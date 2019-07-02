(** Select list *)
(* based on the Menu module *)
(* This is a simple select list with no submenus *)

(* TODO: handle keyboard *)

open B_utils
module Layout = B_layout
module Widget = B_widget
module Avar = B_avar
module Theme = B_theme
module Var = B_var
module Tvar = B_tvar
module Trigger =  B_trigger
module Draw = B_draw
module Label = B_label
module Update = B_update
module Menu = B_menu
  
type t =  {
  selected : int Var.t;
  action : (int -> unit) Var.t;
  (* action i is executed each time entry #i is selected *)
  layout : Layout.t;
  widget : Widget.t (* the label widget of the selected entry *)
}

let layout t =
  t.layout;;

(* we create a menu with a single entry having a submenu. The submenu is the
   select-list, and the main entry is the selected item. The destination layout
   is the layout over which the list will be drawn. Typically it is just an
   "empty" layout of the form Layout.empty ~w ~h (). (A Layout.resident will not
   work because we need a list of rooms inside). Il will be placed just below
   the selected entry. (Hence it should not be added a second time!). It should
   be large enough...It not specified, we take the top layout of the room that
   will contain the menu. *)
(* a special case, for convenience: if dst is not provided, the submenu will be
   attached to the top_layout. This works well (and actually more economical) in
   most cases, but is static: if you move the menu, the submenu will not move
   accordingly... *)
(* action : int -> () can be used to do something each time a new entry is
   selected. It is also possible to "connect to" the selected_label, as a target
   widget (not source, because this widget doesn't get focus) *)
(* TODO ?? replace action by a "selected" Tvar ?? *)
let create ?dst ?(name = "select") ?(action = fun _ -> ()) ?fg
    ?(hmargin = Theme.room_margin/2) entries selected =
  let selected = Var.create selected in
  let action = Var.create action in
  (* let select_bg = Layout.Solid Draw.(lighter (transp menu_hl_color)) in *)

  (* We define the main entry: *)
  let selected_label = Widget.label ?fg entries.(Var.get selected) in
  let selected_layout = Layout.flat_of_w ~sep:0
      [selected_label] in

  (* We transform the list of strings into a suitable list of Menu.entry *)
  let open Menu in
  let rec loop i menu_entries =
    if i < 0  then menu_entries
    else 
      let text = entries.(i) in
      let action () = 
        Label.set (Widget.get_label selected_label) text;
        Var.set selected i;
        Var.get action i
        (* Widget.update selected_label *) in
      let label = Label ({ text; action }) in
      let entry = { label; submenu = None } in

      loop (i-1) (entry :: menu_entries) in

  (* Now we create the drop-down menu: *)
  let submenu_entries = loop (Array.length entries - 1) [] in
  let submenu_fentries = drop_down ~x:0 ~y:0 ~hmargin submenu_entries in
  (* Var.set submenu_fentries.(Array.length entries - 1 - selected).selected
     true; *)
  (* Layout.set_background submenu_fentries.(Array.length entries - 1 - selected).layout (Some select_bg); *)

  (* We set the width of the Select layout (and its children: the label and the
     screen) to the max of all entries *)
  let w = List.fold_left (fun m e -> imax m (Layout.width e.layout))
      0 (Array.to_list submenu_fentries) in
  Layout.set_width selected_layout w;
  List.iter (fun l -> Layout.set_width l w) (Layout.get_rooms selected_layout);

  let submenu = Menu.create_menu ~depth:1 submenu_fentries false in

  (* We can complete the main entry: *)
  let main_entry = {
    layout = selected_layout;
    selected = Var.create false;
    action = (fun _ -> ());
    submenu = Some submenu
  } in

  (* We set the main entry's layout's width (and its children: the label and the
     screen) to the largest width: *)
  (* let w = Layout.width (submenu.entries.(0).layout) in *)
  (* Layout.set_width selected_layout w; *)
  (* let w = Layout.width selected_layout in *)
  (* List.iter (fun l -> Layout.set_width l w) (Layout.get_rooms selected_layout); *)

  (* Just a horizontal line *)
  let line = Widget.empty ~w ~h:1 () in
  let background = Layout.Solid(Draw.(transp grey)) in (* DEBUG *)
  let tmp_dst = default dst (Layout.flat_of_w ~sep:0 ~background [line]) in

  (* The callback to be executed at startup. See why below. *)
  (* TODO: this won't work if the Select List is created dynamically after
     startup... *)
  let relocate _ _ _ =
    match dst with
    | Some _ -> () (* we do nothing if the dst was provided *)
    (* TODO: clip here too, as below *)
    | None -> begin
        let open Layout in
        let rooms = Layout.get_rooms tmp_dst in
        if !debug then assert (List.length rooms = 3);
        let layout = List.hd (List.rev rooms) in 
        (* the content of tmp_dst should contain only three layouts = the line
           +its filter and the submenu. We remove the submenu and keep only the
           line. *)
        let dst = top_house layout in
        (* we need to force computation of layout, because it might very well
           never have been rendered before: *)
        let x1,y1 = Layout.compute_pos layout in
        tmp_dst.content <- Rooms [List.hd rooms];
        let x = x1 - xpos dst in
        let y = y1 - ypos dst in
        let layout = if y + height layout <= height dst
          then layout
          else begin
            (* We clip and add a scrollbar if the dst is too small *)
            let clipped = make_clip ~h:(height dst - y)
                ~scrollbar_inside:true ~scrollbar_width:4 layout in
            hide ~towards:Avar.Top ~duration:0 clipped;
            Layout.show ~duration:0 layout;
            do_option main_entry.submenu
              (fun sbm -> sbm.room <- Some clipped);
            clipped 
          end in
        add_room ~dst layout; (* the submenu is added to the top layout *)
        setx layout x;
        sety layout y;
      end
  in
  let c = Widget.connect_main line line relocate [Trigger.startup] in
  Widget.add_connection line c;

  (* We finally create the whole thing, and we also return the selected_label,
     which can be used to add connections. *)
  let menu = Menu.create ~name:"select_menu" (* TODO name ok ? *) 
      ~select_bg:(Layout.Solid Draw.(transp menu_hl_color)) 
      ~background:(Layout.Solid Draw.(lighter (transp menu_bg_color))) 
      ~dst:tmp_dst
      (Menu.create_menu [| main_entry |] true) in
  let layout = if dst = None
    then Layout.tower ~name ~sep:0 ~vmargin:0 ~hmargin [menu; tmp_dst]
    else menu in
  { layout;
    action;
    selected;
    widget = selected_label
  };;
(* Note: first the list is attached to the line (tmp_dst). This will trigger a
   Warning: "room too tall". And indeed if it stays here, it will never have
   mouse focus because it belongs to a small layout. We need to relocate the
   submenu to the top_house. The problem: we don't know the top_house at this
   point... it has to be done dynamically after startup... This is why we added
   a connection to the line widget that reacts to the Startup Event.*)

let selected t =
  Var.get t.selected;;

let set_label t text =
  Label.set (Widget.get_label t.widget) text;
  Update.push t.widget;;

let set_action t action =
  Var.set t.action action;;
