(** a generic menu layout with submenus *)
(* can be used with entries (layouts) at arbitrary locations *)

open B_utils
open Tsdl
module Layout = B_layout
module Widget = B_widget
module Avar = B_avar
module Chain = B_chain
module Time = B_time
module Var = B_var
module Timeout = B_timeout
module Trigger =  B_trigger
module Draw = B_draw
module Mouse = B_mouse
module Label = B_label
module Button = B_button
module Popup = B_popup
  
type action = Layout.t -> unit;;

(* formatted entry *)
type layout_entry = {
  layout : Layout.t;
  selected : bool Var.t; (* true if the mouse is over this entry *)
  (* NOTE: in this code, we seem to allow several entries to be selected at the
     same time, why ? See activate_subentry. *)
  action : action;
  (* the action will be executed when the mouse is clicked AND released on the
     entry (at mouse_button_up) *)
  submenu : t option
  (* The layouts of the submenu belong to the layout of this entry, hence the
     position of the submenu should be calculated relative to the entry. *)
}

(* menu. It would be safer to make this a private type, but it is not completely
   private, because some functions like drop_down (called by the Select module)
   will generate layout_entry, which links to this type. *)
and t = {
  entries : layout_entry array;
  (* the menu items. Here we call them "entries". Recall that an entry can also
     contain a submenu. *)
  show : bool Var.t;
  (* show = current state of the menu. More precisely, the "coming soon" state,
     in case of animation. *)
  depth : int;
  (* depth = submenu depth. This is only used for dealing with the screen layer *)
  mutable mouse_pressed : bool; (* was the mouse pressed on any of the entries ? *)
  mutable room : Layout.t option;
  (* The room field will contain the computed layout of this menu. *)
  mutable stamp : int Var.t (* used to add a delay before closing the menu *)
  (* TODO we don't want this anymore ? *)
}

let create_menu ?(depth=0) entries show =
  { entries;
    show = Var.create show;
    depth;
    room = None;
    mouse_pressed = false;
    stamp = Var.create 0 };;

let rec is_selected t =
  let list = Array.to_list t.entries in
  List.exists (fun e ->
      Var.get e.selected || default (map_option e.submenu is_selected) false) list;;

(* Layout creation *)

(** close the menu unless it is selected *)
let rec try_close ?(force=false) screen t =
  (* Sdl.log "TRY_CLOSE depth=%d" t.depth; *)
  if (force || (not (is_selected t))) && (Var.get t.show)
  then begin
    (* print_endline "OK CLOSE"; *)
    let layout = remove_option t.room in
    if t.depth = 1 then screen.Layout.show <- false;
    Layout.hide ~towards:Avar.Top layout;
    Layout.fade_out ~duration:250 layout;
    (* this fade_out animation maybe will not be finished if show/hide is
           very quick => will print an error. Cf WARNING in Layout.hide. *)
    printd debug_thread "Hiding menu #%u" layout.Layout.id;
    Var.set t.show false;
    (* now we close submenus *)
    Array.iter (fun e -> if force then Var.set e.selected false;
                 do_option e.submenu (try_close ~force screen)) t.entries
  end;;

let show_submenu mouse_pressed screen t =
  (* print_endline "SHOW SUBMENU"; *)
  Var.set t.show true;
  (* We propagate the mouse_pressed *)
  t.mouse_pressed <- mouse_pressed;
  let layout = remove_option t.room in
  (* show_submenu is only called for submenus, not for the main menu. Hence we
     know that the layout.house is precisely the dst layout given as argument of
     create. Of course the user has to possibility to mess this up, because she
     has access to dst. We could instead pass dst as a parameter. *)
  let dst = remove_option layout.Layout.house in
  (*print_endline (Print.layout_down dst);*) (* DEbuG *)
  screen.Layout.show <- true;
  Layout.show ~duration:200 layout;
  Layout.fade_in ~duration:200 layout; 
  if Layout.(getx layout + width layout > width dst) ||
     Layout.(gety layout + height layout > height dst)
  then begin
    printd debug_warning "Destination layout too small for displaying menu.";
    let dx = imax 0 (Layout.(getx layout + width layout - width dst)) in
    (*let dy = imax 0 (Layout.(gety layout + height layout - height dst)) in*)
    Layout.(slide_to layout (getx layout - dx, gety layout (*- dy*) ));
    if Layout.(gety layout + height layout > height dst)
    then begin
      let clipped = Layout.(make_clip ~h:(height dst - gety layout)
                              ~scrollbar_inside:true
                              ~scrollbar_width:4 layout) in
      Layout.replace_room ~by:clipped ~house:dst layout;
      t.room <- Some clipped
      (* TODO sub-submenus of "layout" should be relocated to the house
         "clipped" ?... *)
    end
  end;;

(* return index and menu of the first activated entry *)
let rec get_selected_entry t =
  let rec loop i =
    if i = Array.length t.entries then None
    else if Var.get t.entries.(i).selected then Some (t,i)
    else match t.entries.(i).submenu with
      | None -> loop (i+1)
      | Some t' -> (match get_selected_entry t' with
          | None -> loop (i+1)
          | a -> a) in
  loop 0;;

let select ?bg entry =
  Layout.set_background entry.layout bg;
  Var.set entry.selected true;;
(* TODO make sure the entry is fully visible (show menu if hidden, scroll if
   necessary) *)

let unselect ?bg entry =
  Layout.set_background entry.layout bg;
  Var.set entry.selected false;;

(* run the action of the selected entries of the menu or submenus *)
(* typically before closing the menu after mouse_button_up *)
let rec activate_subentry t =
  let list = Array.to_list t.entries in
  List.iter (fun e ->
      if Var.get e.selected
      then e.action e.layout;
      do_option e.submenu activate_subentry) list;;


(** create a layout for the given menu attached to the given house="dst" *)
(* NO positions are calculated here, only the connections *)
(* hide = true if we want the menu to disappear after click *)
(* we make a layout, add the entries, add connections to the entries, and to
   the filter layer (click the filter = quit the menu  TODO) *)
(* select_bg = background for selected entries *)
(* This function is recursive because it is used also to create submenus *)
(* The function returns the layout of the main menu, it also sets the room entry
   of t with it. WARNING: the submenus are not included in this layout. Instead,
   they are added to the ~dst layout, and start in a hidden state. That's
   because you usually want the submenus to appear somewhere else. *)
let rec create_loop ?(hide = false) ?name ?background ?select_bg
    ~screen ~layers ~dst t =

  assert (t.room = None); (* it is the role of this function to create t.room *)

  let entry_layer, coating_layer = layers in
  let menu = t.entries in
  let l = Array.length menu in
  if l = 0 then failwith "Menu should not be empty"; (* or return an empty box ? *)
  let rooms = Array.to_list menu
              |> List.map (fun e -> e.layout) in
  (* We create the menu layout and store it in the room field. Note that the
     positions of all elements should be already indicated in the geometry. *)
  let canvas = dst.Layout.canvas in
  let layout = Layout.superpose ?name ?canvas rooms in
  t.room <- Some layout;
  (*Layout.set_layer layout dst_layer;*) (* useful ? *)
  Layout.set_background layout background;
  if hide then ((*Layout.hide ~towards:Avar.Top ~duration:0 layout;*)
    layout.Layout.show <- false;
    Var.set t.show false);

  (* We create the connections: *)
  for i = 0 to l-1 do
    let room = menu.(i).layout in
    let bg = Layout.get_background room in
    Layout.global_set_layer room entry_layer; (* TODO translate (shift) layers instead *)
    (* We translate back all the entries to the origin of the container
       layout: *)
    Layout.(setx room (getx room - getx layout));
    Layout.(sety room (gety room - gety layout));
    (* We need a coat to get mouse focus on the whole length of the menu entry,
       not only on the area of the text itself (label): *)
    let coat = Popup.filter_screen ~layer:coating_layer room in
    coat.Layout.keyboard_focus <- Some false;
    Layout.add_room ~dst:room coat;
    (* we don't use Popup.add_screen to avoid creating too many layers. *)
    let wg = Layout.widget coat in
    wg.Widget.cursor <- Some (go (Draw.create_system_cursor Sdl.System_cursor.hand));
    let action_button_up () =
      (* REMARK: one could have implemented the mechanism with a W.button
         ~kind:Button.Switch. Cf Example 16 *)
      (* let no_submenu = menu.(i).submenu = None in *)
      (* if hide then try_close ~force:no_submenu t; *)
      (* TODO close parents too *)
      t.mouse_pressed <- false;
      if not !Trigger.too_fast then begin
        match menu.(i).submenu with
        | None -> (* mouse focus is on a standard entry *)
          (* the user is allowed to select an entry which has mouse_over but not
             mouse_focus, because one can click on an item (which gets then
             mouse_focus), and then let the button pressed and move to another
             item: *)
          activate_subentry t;
          if hide then try_close ~force:true screen t;
          (*printd debug_thread "Selecting menu action #%d" (i+1);
            menu.(i).action room; *)
        | Some sub -> begin
            (* Recall than an action is attached to the widget that has mouse
               focus. Here the focus widget has a submenu, which means that
               (probably) some subentry was selected with mouse button kept
               down. We activate it. *)
            activate_subentry sub;
            (* we assume that if this is a single click it means the mouse_up
               was on the same entry as the mouse_down (of course, we could
               check this rigorously, but this more complicated and maybe not
               worth). In this case, the menu was shown on mouse_down, so we
               don't hide it now! On the contrary if it is not a single click,
               then the mouse button was kept down to select another entry, and
               hence we close the submenu. *)
            (* TODO sometimes scrolling with the touch pad generates
               Mouse_up... we should filter this out! *)
            if not (Trigger.was_single_click ()) then try_close ~force:true
                screen sub
          end
      end
    in
    (* TODO merge this with button_up: *)
    let action_activate _ _ ev =
      match Trigger.event_kind ev with
      | `Mouse_button_up
      | `Finger_up -> action_button_up (); 
      | `Key_down -> let c = Sdl.Event.(get ev keyboard_keycode) in
        if c = Sdl.K.return || c = Sdl.K.return2 || c = Sdl.K.kp_enter
        then begin
          action_button_up ();
          if hide then try_close ~force:true screen t;
          do_option menu.(i).submenu (fun sub -> try_close ~force:true screen sub)
        end
      | _ -> () in

    let c = Widget.connect wg wg action_activate
        Sdl.Event.[mouse_button_up; key_down; finger_up] in
    Widget.add_connection wg c;

    let enter _ (* w *) =
      (* print_endline ("ENTER " ^ (string_of_int w.Widget.wid)); *) (* DEBUG *)
      select ?bg:select_bg menu.(i);
      (*Sdl.set_cursor
        (Some (Utils.go (Draw.create_system_cursor Sdl.System_cursor.hand))); *)
      (* We push the keyboard_focus event, so that the main loop can update its
         status. But this is quite slow at the moment. The only use of this (?) is
         to treat the TAB key... because I don't know how to do it better. *)
      (*Trigger.push_keyboard_focus coat.Layout.id *)
    in
    let leave _ (* w *) =
      (* print_endline ("LEAVE " ^ (string_of_int w.Widget.wid)); *) (* DEBUG *)
      unselect ?bg menu.(i);
      (* if hide && not (Trigger.wait_value ~timeout:0.5 ev t.active true) *)
      (* which means t.active remained false for 0.5sec *)

      if Trigger.mouse_left_button_pressed () then begin
        let stamp = Time.now () in
        Var.set t.stamp stamp; (* TODO save timeout instead in order to kill previous timeouts... *)
        let _ = Timeout.add 300 (fun () ->
            (*Trigger.nice_delay ev 0.5;*)
            (* the problem is that we may leave one entry and enter another
               simultaneously. We check that the stamp has not been modified by
               another thread. *)
            if hide && (Var.get t.stamp = stamp) then begin
              try_close screen t;
              do_option menu.(i).submenu (try_close screen); (* TODO verify that submenu is of "hide" type... *)
              Trigger.push_action (); (* or push_redraw ? *)
            end) in ()
      end
    in
    Widget.mouse_over ~enter ~leave wg;
    let action_keyboard w _ ev = (* TODO: this doesn't work well because multiple selections are made with keyboard + mouse *)
      match Sdl.Event.(get ev keyboard_keycode) with
      | c when c = Sdl.K.up || c = Sdl.K.down ->
        do_option (get_selected_entry t) (fun (t',j) ->
            let j' = (if c = Sdl.K.up then j+1 else j-1 + (Array.length t'.entries)) mod (Array.length t'.entries) in
            unselect ?bg t'.entries.(j);
            select ?bg:select_bg t'.entries.(j')
          )
      | c when c = Sdl.K.tab ->
        do_option (get_selected_entry t) (fun (t',j) ->
            unselect ?bg t'.entries.(j));
        enter w
      | _ -> () in
    let c = Widget.connect_main wg wg action_keyboard [Sdl.Event.key_down] in
    Widget.add_connection wg c;

    (* we treat submenus *)
    do_option menu.(i).submenu (fun t' ->
        let name = map_option name (fun s -> s ^ ":sub") in
        let submenu = create_loop ?name ~hide:true ?background ?select_bg
            ~layers ~screen ~dst t' in
        (*Layout.(submenu.background <- Some (Solid Draw.(transp green)));*)
        (* =DEBUG *)

        (* The submenus are added to the dst layout: *)
        (* Popup.attach_on_top dst submenu; ?? *)
        Layout.add_room ~dst submenu;
        let action_show _ _ ev =
          if (Trigger.mouse_left_button_pressed () &&
              (Trigger.event_kind ev = `Mouse_button_down || t.mouse_pressed))
          || Trigger.event_kind ev = `Finger_down
          || (Trigger.event_kind ev = `Key_down &&
              let c = Sdl.Event.(get ev keyboard_keycode) in
              c = Sdl.K.return || c = Sdl.K.return2 || c = Sdl.K.kp_enter)
          then if Var.get t'.show
            then try_close screen t'
            else begin
              if Trigger.event_kind ev = `Mouse_button_down &&
                 Trigger.mouse_left_button_pressed ()
              then t.mouse_pressed <- true;
              show_submenu t.mouse_pressed screen t'
            end in

        let c = Widget.connect_main wg wg action_show
            Sdl.Event.[Trigger.mouse_enter; mouse_button_down;
                       finger_down; key_down] in
        Widget.add_connection wg c
      )
  done;
  layout;;

let close_all_submenus screen t =
  Array.iter (fun e ->
      do_option e.submenu (try_close ~force:true screen))
    t.entries;;

let create ?(hide = false) ?name ?background ?select_bg ~dst t =
  let dst_layer = Chain.last (Layout.get_layer dst) in
  let entry_layer = Popup.new_layer_above dst_layer in
  let coating_layer = Popup.new_layer_above entry_layer in
  let layers = entry_layer, coating_layer in
  (* the screen is used to grab all mouse focus while the submenus are open *)
  let screen = Popup.filter_screen ~layer:entry_layer
                 (*~color:Draw.(more_transp (transp green))*) (* DEBUG*) dst in
  screen.Layout.show <- false;
  Layout.add_room ~dst screen;

  let w = Layout.widget screen in
  Widget.on_click ~click:(fun _ -> (* print_endline "CLICK SCREEN"; *)
      close_all_submenus screen t) w;

  create_loop ?name ~hide ~layers ~screen ?background ?select_bg ~dst t;;


(* Now we create the geometries (not the connections) *)

(* Some types for standard menu types, from the simplest to the most
   elaborate *)

(* Just text *)
type simple_entry = {
  text : string;
  action : unit -> unit;
};;

(* The text can be modified by the action *)
type mutable_text = {
  mutable text : string;
  action : string -> unit;
};;

(* A check box before the label, and the text is modifiable *)
type check_entry = {
  mutable state : bool;
  mutable text : string;
  action : string -> bool -> unit
};;


type label =
  | Label of simple_entry
  | Dynamic of mutable_text
  | Check of check_entry
  | Layout of layout_entry;;

type entry = {
    label : label;
    submenu : (entry list) option;
  };;

(** create an entry layout and action suitable for the menu type *)
let create_action ?submenu ?canvas ?(margin=3) entry =
  match entry.label with
  | Label l ->
    let res = Layout.resident ?canvas (Widget.label l.text) in
    (* : here we cannot use a resident as is because we will need to add another
       room later. we need to wrap it: *)
    let layout = Layout.flat ?canvas ~hmargin:margin ~vmargin:margin [res] in
    let action = fun _ -> l.action () in
    { layout; action; submenu; selected = Var.create false }
  | Dynamic _ -> (* TODO *) failwith "Not_implemented"
  | Check _ -> (* TODO *) failwith "Not_implemented"
  | Layout l -> l

let submenu_sep_margin = 4;;

(** create a vertical menu with simple entries *)
let rec drop_down ~x ~y ?canvas ?(depth=0) ?(hmargin=submenu_sep_margin) entries =
  let submenu_icon = "caret-right" in (* the icon used to indicate submenus *)
  (* first pass: we need to render all the entries to find the max width *)
  let rec loop0 wmax hmax has_submenu entries formatted_entries =
    match entries with
    | [] -> wmax, hmax, has_submenu, formatted_entries
    | entry :: rest ->
      let f_entry = create_action ?canvas entry in
      let has_submenu = has_submenu || (entry.submenu <> None) in
      let r = f_entry.layout in
      loop0 (max wmax Layout.(getx r + width r)) 
        Layout.(max hmax (gety r + height r))
        has_submenu rest (f_entry :: formatted_entries)
  in
  let wmax, _, has_submenu, formatted_entries = loop0 0 0 false entries [] in
  (* hmax is not used for this version... *)
  let wmax = if has_submenu
    then let si = Label.icon submenu_icon in
      let (si_size,_) = Label.size si in
      wmax + si_size + 2*hmargin
    else wmax + 2*hmargin in

  (* second (and final) pass: now we adjust size and add submenus *)
  let rec loop y' entries old_f_entries formatted_entries =
    match entries, old_f_entries with
    | [], _ -> formatted_entries
    | entry :: rest, f_entry :: f_rest ->
      (* let f_entry = create_action canvas entry in *)
      let layout = f_entry.layout in
      Layout.setx layout x;
      Layout.sety layout y';
      Layout.set_width layout wmax;
      let submenu = map_option entry.submenu (fun list ->
          let f_entries = drop_down ~x:(x+wmax) ~y:y' ~depth:(depth+1)
              ?canvas list in
          let submenu_indicator = Layout.resident ?canvas
              (Widget.icon submenu_icon) in
          Layout.add_room ~dst:layout ~valign:Draw.Center ~halign:Draw.Max
            submenu_indicator;
          (* let w = Layout.width layout in *)
          create_menu ~depth:(depth+1) f_entries false)
      in
      let f_entry = { f_entry with submenu } in
      let h = Layout.height layout in
      loop (y' + h) rest f_rest (f_entry :: formatted_entries)
    | _ -> failwith "Menu.drop_down loop: This case should not happen because old_f_entries and formatted_entries should have same length."
  in
  Array.of_list (loop y entries (List.rev formatted_entries) []);;
(* = formatted_entries *)


  (* let menu = create ~background:(Layout.Solid Draw.none) *)
  (*     { entries = formatted_entries; show = true } in *)
  (* Layout.(menu.background <- Some (Solid Draw.(transp pale_grey))); *)
  (* menu;; *)


(* create a menu bar = horizontal labels with vertical submenus, attached to
   dst *)
(* This function returns the layout of the main menu = the horizontal menu bar. *)
(* all submenus are added as rooms inside dst *)
(* The dst layout should be big enough to contain the submenus. Any item flowing
   out of dst will not get focus. The system will automatically try to shift the
   submenus if they are too wide, or add a scrollbar if they are too tall. *)
(* the dst can contain other widgets. In principle they should not interfere
   with the menus, because the menu layouts are on a different layer. *)
(* TODO use window_size *)
let bar ?(background = Layout.Solid Draw.(set_alpha 175 menu_bg_color))
    ?(name="menu_bar") dst entries =
  let canvas = dst.Layout.canvas in
  let rec loop x entries formatted_entries =
    match entries with
    | [] -> formatted_entries
    | entry :: rest ->
      let submenu_entries = map_option entry.submenu
          (drop_down ~depth:1 ~x ~y:0 ?canvas) in
      let submenu = map_option submenu_entries (fun entries ->
          create_menu ~depth:1 entries false) in
      let f_entry = create_action ?submenu ?canvas entry in
      let layout = f_entry.layout in
      Layout.setx layout x;
      let w = Layout.width layout in
      loop (x+w) rest (f_entry :: formatted_entries)
  in
  (* not necessary to do a List.rev... *)
  (* print_endline "Now calling menu"; *)
  let formatted_entries = Array.of_list (loop (Layout.getx dst) entries []) in
  let menu = create ~name ~select_bg:(Layout.Solid Draw.(transp menu_hl_color)) 
      ~background ~dst (create_menu formatted_entries true) in
  Layout.(set_width menu (width dst));
  (*  Layout.(set_background menu (Some (Solid Draw.(transp red)))); *)

  menu;;
(* TODO Layout.tower ~sep:0 ? *)
