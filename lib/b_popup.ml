(** put a sublayout on top of the main layout *)
(* recall that "on top" means "insert_after" layer; change ? *)

open B_utils;;
module Layout = B_layout
module Widget = B_widget
module Chain = B_chain
module Theme = B_theme
module Timeout = B_timeout
module Trigger =  B_trigger
module Draw = B_draw
module Style = B_style
module Box = B_box
  
let new_layer_above base =
  printd debug_graphics "Create new layer";
  Chain.insert_after base (Draw.new_layer ());;

(* search top_layer inside the layout. *)
(* use the global toplayer instead (Chain.last) or at least the top_layer of the
   whole connected component ? (top_layer (Layout.top_house layout)). In fact
   since the top_house is supposed to contain all the graphics of the window,
   and there is one layer chain per window, the two choices should give the same
   answer. Thus it's better to use Chain.last layer. OK see below ... *)
let rec top_layer layout =
  let open Layout in
  match layout.content with
  | Resident _ -> get_layer layout
  | Rooms r -> match list_max Chain.compare (List.map top_layer r) with
    | None -> printd debug_error "Error: there should be a top layer"; get_layer layout
    | Some l -> l;;

let global_top_layer layout : Draw.layer =
  Chain.last (Layout.get_layer layout);;

(* create a box of the dimension of the layout *)
let filter_screen ?color ?layer ?keyboard_focus layout =
  let w,h = Layout.(width layout, height layout) in
  printd debug_graphics "Create filter screen (%d,%d)" w h;
  let b = match color with
    | None -> Widget.empty ~w ~h ()
    | Some color -> Widget.box ~w ~h ~background:(Style.Solid color)() in
  let screen = (* Layout.(flat_of_w ~sep:0 layout.canvas [b]) in *)
    Layout.(resident ~name:"filter" ?canvas:layout.canvas b) in
  (* Layout.(screen.geometry <- {screen.geometry with w; h}); *)
  do_option layer (Layout.set_layer screen);
  screen.Layout.keyboard_focus <- keyboard_focus;
  screen;;

(** add a screen on top of the layout. This can be useful to make the whole
   layout clickable as a whole. To make sure this works as expected, it should
   be called dynamically and not statically before running the board, because if
   other layers are created afterwards, the screen might endup not being on top
   of everything. *)
let add_screen ?(color = Draw.(transp red) (* DEBUG *) ) layout =
  let base_layer = top_layer layout in
  let screen_layer = new_layer_above base_layer in
  let screen = filter_screen ~color ~layer:screen_layer layout in
  Layout.add_room ~dst:layout screen;
  screen;;

(* TODO add dx dy *)
let attach_on_top ?(dx=0) ?(dy=0) house layout =
  let open Layout in
  setx layout (getx layout + dx);
  sety layout (gety layout + dy);
  global_set_layer layout (global_top_layer house);
  add_room ~dst:house layout;;

(** add two layers on top of the house: one for the screen to hide the house,
    one for the layout on top of the screen. Return the screen. *)
(* TODO: use add_screen to reduce code *)
let attach ?bg ?(show=true) house layout =
  let base_layer = global_top_layer house in  (* eg. 10 *)
  let filter_layer = new_layer_above base_layer in (* eg. 20 *)
  let top_layer = new_layer_above filter_layer in  (* eg. 30 *)
  (* We change layer for layout and all its children: *)
  Layout.global_set_layer layout top_layer;
  let screen = filter_screen ?color:bg house in
  Layout.set_layer screen filter_layer;
  Layout.add_room ~dst:house screen;
  Layout.add_room ~halign:Draw.Center ~valign:Draw.Center ~dst:house layout;
  screen.Layout.show <- show;
  layout.Layout.show <- show;
  Trigger.push_keyboard_focus (screen.Layout.id);
  Trigger.push_mouse_focus (screen.Layout.id); (* redundant *)
  (* When inserting new elements on the fly, one needs to ask to mouse to
     refresh its focus, see b_main.ml. *)
  screen;;


(* some predefined popup designs *)

let slide_in ~dst content buttons =
  let border = Style.(border (line ~color:Draw.(opaque grey) ())) in
  let shadow = Style.shadow () in
  let background = Layout.Box
      (Box.create ~shadow ~background:(Style.Solid Draw.(opaque (pale grey)))
         ~border ()) in
let popup = Layout.tower ~align:Draw.Center ~background [content; buttons] in
  let screen = attach ~bg:(Draw.(set_alpha 200 (pale grey))) dst popup in
  (* Layout.slide_in ~dst popup; *)
  popup, screen;;
  
let one_button ?w ?h ~button ~dst content =
  let close_btn = Widget.button ~border_radius:3 button in
  let popup, screen = slide_in ~dst content (Layout.resident ?w ?h close_btn) in
  let close _ =
    Layout.hide popup;
    Layout.hide screen;
    Layout.fade_out screen in
  Widget.on_release ~release:close close_btn;;

(* a text and a close button. *)
(* TODO the ?w and ?h define the size of the text_display (not automatically
   detected). It should also include the size of the close button *)
let info ?w ?h ?(button="Close") text dst =
  let td = Widget.text_display ?w ?h text
           |> Layout.resident in
  one_button ?w ?h ~button ~dst td;;

(* ?w and ?h to specify a common size for both buttons *)
let two_buttons ?w ?h ~label1 ~label2 ~action1 ~action2
    content dst =
  let btn1 = Widget.button ~border_radius:3 label1 in
  let btn2 = Widget.button ~border_radius:3 label2 in
  let buttons = Layout.(flat ~vmargin:0 ~sep:(2*Theme.room_margin) [resident ?w ?h btn1; resident ?w ?h btn2]) in
  let popup, screen = slide_in ~dst content buttons in
  let close () =
    (*Layout.hide popup;*)
    Layout.fade_out ~hide:true popup; 
    (*Layout.hide screen*)
    Layout.fade_out ~hide:true screen in
  let do1 _ =
    close ();
    action1 () in
  let do2 _ =
    close ();
    action2 () in
  Widget.on_release ~release:do1 btn1;
  Widget.on_release ~release:do2 btn2;;

let yesno ?w ?h ?(yes="Yes") ?(no="No") ~yes_action ~no_action text dst =
  let td = Widget.text_display ?w ?h text
           |> Layout.resident in
  two_buttons ?w ?h ~label1:yes ~label2:no ~action1:yes_action ~action2:no_action
    td dst;;


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
  let border = Style.(border ~radius:5 (line ())) in
  let background = default background
      (Layout.Box (Box.create ~background:(Style.Solid Draw.(opaque (pale grey)))
                     ~border ())) in
  let tooltip = Layout.tower_of_w ~sep:3 ~background [t] in
  attach_on_top layout tooltip;
  tooltip.Layout.show <- false;

  let show_tooltip _ _ _ =
    let open Layout in
    if not tooltip.show then begin
      let x,y = pos_from layout target in
      (* print_endline (Printf.sprintf "(%i,%i)" x y); *)
      let x',y' = match position with
        | Below -> x, y+(height target)+2
        | Above -> x, y-(height tooltip)-2
        | LeftOf -> x-(width tooltip)-2, y
        | RightOf -> x+(width target)+2, y
        | Mouse -> let x,y = Trigger.mouse_pos () in (x+8,y+8) in
      sety tooltip y';
      setx tooltip x';
      tooltip.show <- true;
      Layout.fade_in tooltip
    end in
  let to_show = ref true in
  let hide_tooltip b =
    to_show := false;
    ignore (Timeout.add 200 (fun () ->
        tooltip.Layout.show <- !to_show;
        Trigger.push_redraw (Widget.id b)))
  in
  let enter _ =
    if tooltip.Layout.show
    then to_show := true in (* this amounts to cancelling the timeout, which is
                               what we want to do when we re-enter the target *)

  Widget.mouse_over ~enter ~leave:hide_tooltip widget;
  let c = Widget.connect_main widget widget show_tooltip [Trigger.mouse_at_rest] in
  Widget.add_connection widget c;;
