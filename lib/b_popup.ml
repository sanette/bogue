(* This file is part of BOGUE, by San Vu Ngoc *)

(* Use a Popup to put a sublayout on top of the main layout. *)
(* Recall that "on top" means "insert_after" layer; change? *)

(* TODO implement the resize function *)

module Box = B_box
module Chain = B_chain
module Draw = B_draw
module L = B_layout
module Main = B_main
module Space = B_space
module Style = B_style
module Theme = B_theme
module Timeout = B_timeout
module Trigger =  B_trigger
module Widget = B_widget

open B_utils

let new_layer_above base =
  printd debug_graphics "Create new layer";
  Chain.insert_after base (Draw.new_layer ())

(* Search top_layer inside the layout. *)
(* One could also use the global toplayer instead (Chain.last) or at
   least the top_layer of the whole connected component (top_layer
   (Layout.top_house layout)). In fact since the top_house is supposed
   to contain all the graphics of the window, and there is one layer
   chain per window, the two choices should give the same answer. Thus
   it's better to use Chain.last layer, see below. *)
let rec top_layer layout =
  let open L in
  match layout.content with
  | Resident _ -> get_layer layout
  | Rooms r ->
     match list_max Chain.compare (List.map top_layer r) with
     | None -> printd debug_error "Error: there should be a top layer";
               get_layer layout
     | Some l -> l

let global_top_layer layout : Draw.layer =
  Chain.last (L.get_layer layout)

(* Register a resize function that will follow (size AND position) the model
   layout. For the position to work correctly, both layouts must be in the same
   house. *)
let resize_same_as model room =
  let resize _ =
    let open L in let open Resize in
    match model.house, room.house with
    | Some h1, Some h2 when L.equal h1 h2 ->
      let (w, h) = get_size model in
      let x = xpos model in
      let y = ypos model in
      setx room x;
      sety room y;
      set_size room ~w ~h
    | _ -> printd debug_error
             "[resize_same_as] must apply to two rooms in the same house. Maybe \
              you should use [L.resize_follow_house]."
  in
  room.resize <- resize

(* Create a box of the dimension of the layout. *)
let filter_screen ?color ?layer ?keyboard_focus layout =
  let w,h = L.(width layout, height layout) in
  printd debug_graphics "Create filter screen (%d,%d)" w h;
  let b = match color with
    | None -> Widget.empty ~w ~h ()
    | Some color ->
      let style = Style.create ~background:(Style.Solid color) () in
      Widget.box ~w ~h ~style () in
  let screen =
    L.(resident_with_layer ?layer ~name:"_filter" ?canvas:layout.canvas b) in
  screen.L.keyboard_focus <- keyboard_focus;
  screen

(* Add a screen on top of the layout. This can be useful to make the
   layout clickable as a whole. To make sure this works as expected,
   it should be called dynamically and not statically before running
   the board, because if other layers are created afterwards, the
   screen might endup not being on top of everything. *)
let add_screen ?(color = Draw.(transp pale_grey) (* DEBUG *) ) layout =
  let base_layer = top_layer layout in
  let screen_layer = new_layer_above base_layer in
  let screen = filter_screen ~color ~layer:screen_layer
                 ~keyboard_focus:true layout in
  L.add_room ~dst:layout screen;
  L.resize_follow_house screen;
  screen

(* TODO add dx dy *)
let attach_on_top ?(dx=0) ?(dy=0) house layout =
  let open L in
  setx layout (getx layout + dx);
  sety layout (gety layout + dy);
  rec_set_layer layout (global_top_layer house);
  add_room ~dst:house layout;
  resize_same_as house layout


(** add two layers on top of the house: one for the screen to hide the house,
    one for the layout on top of the screen. Return the screen. *)
(* TODO: use add_screen to reduce code *)
let attach ?bg ?(show=true) house layout =
  let base_layer = global_top_layer house in  (* eg. 10 *)
  let filter_layer = new_layer_above base_layer in (* eg. 20 *)
  let top_layer = new_layer_above filter_layer in  (* eg. 30 *)
  (* We change layer for layout and all its children: *)
  L.rec_set_layer layout top_layer;
  let screen = filter_screen ?color:bg ~keyboard_focus:true house in
  L.set_layer screen filter_layer;
  L.add_room ~dst:house screen;
  L.resize_keep_margins screen;
  L.add_room ~halign:Draw.Center ~valign:Draw.Center ~dst:house layout;
  L.resize_keep_margins layout;
  screen.L.show <- show;
  layout.L.show <- show;
  Trigger.push_keyboard_focus (screen.L.id);
  Trigger.push_mouse_focus (screen.L.id); (* redundant *)
  (* When inserting new elements on the fly, one needs to ask to mouse to
     refresh its focus, see b_main.ml. *)
  screen


(* some predefined popup designs *)

let add_button_line ?background ?w ?h content buttons =
  let room = L.tower ~resize:L.Resize.Disable
      ~align:Draw.Max ?background [content; buttons] in
  L.resize_keep_margins content;
  Space.keep_bottom_sync ~reset_scaling:true buttons;
  Space.keep_right_sync ~reset_scaling:false buttons;
  L.set_size room ?w ?h;
  room

let slide_in ~dst ?bg ?w ?h ?screen_color content buttons =
  let background = default bg (Style.Solid Draw.(opaque white)) in
  let style = let open Style in create
      ~border:(mk_border (mk_line ~color:Draw.(opaque grey) ()))
      ~shadow:(mk_shadow ())
      (* Warning: currently shadow is quite CPU intensive *)
      ~background () in
  let background = L.Box (Box.create ~style ()) in
  let popup = add_button_line ~background ?w ?h content buttons in
  let screen_bg = default screen_color (Draw.(set_alpha 200 (pale grey))) in
  let screen = attach ~bg:screen_bg dst popup in
  (* L.slide_in ~dst popup; *)
  popup, screen

let one_button ?w ?h ?on_close ~button ~dst content =
  let close_btn = Widget.button ~border_radius:3 button in
  let popup, screen = slide_in ~dst content (L.resident ?w ?h close_btn) in
  let close _ =
    L.hide popup;
    L.hide screen;
    L.fade_out screen;
    let _ = Timeout.add L.default_duration (fun () ->
        L.detach screen;
        L.detach popup) in
    do_option on_close run in
  Widget.on_button_release ~release:close close_btn

(* a text and a close button. *)
(* TODO the ?w and ?h define the size of the text_display (not automatically
   detected). It should also include the size of the close button *)
let info ?w ?h ?button_w ?button_h ?(button="Close") text dst =
  let td = Widget.text_display ?w ?h text
           |> L.resident in
  one_button ?w:button_w ?h:button_h ~button ~dst td

(* TODO check that we don"t resize to 0 size! cf example21ter "*)
(* ?button_w and ?button_h to specify a common size for both buttons *)
let two_buttons ?dst ?board ?button_w ?button_h ?w ?h ?screen_color
    ~label1 ~label2 ~action1 ~action2 ?connect2 content =
  let btn1 = Widget.button ~border_radius:3 label1 in
  let btn2 = Widget.button ~border_radius:3 label2 in
  let buttons = let w, h = button_w, button_h in
    L.(flat ~vmargin:0 ~sep:(2*Theme.room_margin)
         [resident ?w ?h btn1; L.resident ?w ?h btn2]) in
  let close = match dst with
    | Some dst -> (* We create a popup *)
      let popup, screen = slide_in ~dst ?w ?h ?screen_color content buttons in
      fun () ->
        let _ = Timeout.add L.default_duration (fun () ->
            L.detach screen;
            L.detach popup) in
        (*L.hide popup;*)
        L.fade_out ~hide:true popup;
        (*L.hide screen*)
        L.fade_out ~hide:true screen
    | None -> (* We create a new window *)
      let frame = add_button_line ?w ?h content buttons in
      let () = match board with
        | Some b -> ignore (Main.add_window b frame)
        | None -> L.add_window frame in
      fun () ->
        printd debug_board "Closing [two_buttons] window.";
        L.destroy_window frame
  in
  apply_option connect2 btn2;
  let do1 _ =
    close ();
    action1 () in
  let do2 _ =
    close ();
    action2 () in
  Widget.on_button_release ~release:do1 btn1;
  Widget.on_button_release ~release:do2 btn2

let yesno ?w ?h ?button_w ?button_h ?(yes="Yes") ?(no="No")
      ~yes_action ~no_action text dst =
  let dst =
    if L.has_resident dst
    then begin
        printd (debug_error + debug_user)
          "The [yesno] popup requires a destination layout which is not a single \
           resident (as %s). We're trying anyways, but please correct your code."
          (L.sprint_id dst);
        match L.guess_top () with
        | Some r -> L.top_house r
        | None -> failwith "Cannot find a valid layout!"
      end
    else dst in
  let content =
    Widget.text_display ?w ?h text
    |> L.resident in
  two_buttons ?w:button_w ?h:button_h ~label1:yes ~label2:no
    ~action1:yes_action ~action2:no_action
    content ~dst

(* tooltips *)

(* tooltips are small popups which are displayed on a specified layout, close to
   a specified target. The target should be a room inside the layout. Tooltips
   don't have 'screens' like usual popups because they don't prevent the user to
   do anything. Tooltips are displayed when the "mouse_at_rest" event is
   triggered to the specified widget, and removed when the "mouse_leave" event is
   triggered. *)

(* the content of the tooltips is usually a simple text, but it can be any
   layout *)

type position =
  | LeftOf
  | RightOf
  | Above
  | Below
  | Mouse

let tooltip ?background ?(position = Below) text ~target widget layout =
  let t = Widget.label ~size:Theme.small_font_size text in
  let background = match background with
    | Some b -> b
    | None ->
      let style = Style.(create ~border:(mk_border ~radius:5 (mk_line ()))
                           ~background:(Solid Draw.(opaque (pale grey))) ()) in
      L.Box (Box.create ~style ()) in
  let tooltip = L.tower_of_w ~sep:3 ~background [t] in
  attach_on_top layout tooltip;
  tooltip.L.show <- false;

  let show_tooltip _ _ _ =
    let open L in
    if not tooltip.show then begin
      let x,y = pos_from layout target in
      (* print_endline (Printf.sprintf "(%i,%i)" x y); *)
      let x',y' = match position with
        | Below -> x, y+(height target)+2
        | Above -> x, y-(height tooltip)-2
        | LeftOf -> x-(width tooltip)-2, y
        | RightOf -> x+(width target)+2, y
        | Mouse -> let x,y = Mouse.pos () in (x+8,y+8) in
      sety tooltip y';
      setx tooltip x';
      tooltip.show <- true;
      L.fade_in tooltip
    end in
  let to_show = ref true in
  let hide_tooltip b =
    to_show := false;
    ignore (Timeout.add 200 (fun () ->
        tooltip.L.show <- !to_show;
        Trigger.push_redraw (Widget.id b)))
  in
  let enter _ =
    if tooltip.L.show
    then to_show := true in (* this amounts to cancelling the timeout, which is
                               what we want to do when we re-enter the target *)

  Widget.mouse_over ~enter ~leave:hide_tooltip widget;
  let c = Widget.connect_main widget widget show_tooltip [Trigger.mouse_at_rest] in
  Widget.add_connection widget c
