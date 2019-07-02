(* This module provides some tools to auto-adjust space and size of layouts at
   startup or when resizing window. They use the animation mechanism (Avar) with
   duration=0. *)

(* The order of invocation of the fill functions is not important. The structure
   of the layouts prevail: from top house to inner rooms. *)

open B_utils
open Printf
module Layout = B_layout
module Widget = B_widget
module Avar = B_avar
module Theme = B_theme
module Trigger =  B_trigger
module Draw = B_draw
module Update = B_update
  
(* we store here rooms that should be updated in case of layout resizing (ie
   when calling Space.update *)
let rooms_to_update = Layout.WHash.create 50;;


let update_room r =
  let open Layout in
  Avar.reset r.geometry.w;
  Avar.reset r.geometry.y;
  Avar.reset r.geometry.h;;

(* Use "update" to force recomputation of all layouts (by resetting animations);
   for instance this is called by Layout.resize when resizing a window. *)
let update_all room =
  Layout.iter update_room room;;

(* reset only those that have a Space.something *)
(* TODO reset only those belonging to the layout-window that we resize *)
let update () =
  Layout.WHash.iter update_room rooms_to_update;;
  
(* split a list into two lists: the one before and the one after the first
   element for which test is true. This element is not included in the
   result. *)
let split_at test list =
  let rec loop before = function
    | [] -> failwith "split_at: no matching element was not found in the list."
    | a::rest -> if test a
      then List.rev before, rest
      else loop (a::before) rest in
  loop [] list;;

let hfill_action margin house layout rooms =
  let open Layout in
  let before,after = split_at (equal layout) rooms in
  let bx,_,bw,_ = bounding_geometry before in
  let ax,_,aw,_ = bounding_geometry after in
  let available_width = width house - aw - bw - bx - margin in
  if available_width > 0 then begin
    printd debug_graphics "hfill (%s, margin=%i) will use %u pixel%s."
      (sprint_id layout) margin available_width
      (if available_width > 1 then "s" else "");
    set_width layout available_width;
    setx layout (bx+bw);
    List.iter (fun r -> setx r (getx r - ax + width house - aw - margin)) after
  end
  else printd (debug_graphics+debug_warning)
      "hfill cannot operate since available_width=%d" available_width;;

let vfill_action margin house layout rooms =
  let open Layout in
  let before,after = split_at (equal layout) rooms in
  let _,by,_,bh = bounding_geometry before in
  let _,ay,_,ah = bounding_geometry after in
  let available_height = height house - ah - bh - by - margin in
  if available_height > 0 then begin
    printd debug_graphics "vfill (%s, margin=%i) will use %u pixel%s."
      (sprint_id layout) margin available_height
      (if available_height > 1 then "s" else "");
    set_height layout available_height;
    sety layout (by+bh);
    List.iter (fun r -> sety r (get_oldy r - ay + height house - ah - margin)) after
  end
  else printd (debug_graphics+debug_warning)
      "vfill cannot operate since available_height=%d" available_height;;

let full_width_action margin layout house =
  let open Layout in
  let w = width house in
  set_width layout (w - 2*margin);
  setx layout margin;;

let full_height_action margin layout house =
  let open Layout in
  let h = height house in
  set_height layout (h - 2*margin);
  sety layout margin;;

let vcenter_action layout house =
  let open Layout in
  let hh = height house in
  let h = height layout in
  let y = Draw.center 0 hh h in
  sety layout y;;

(* the "make_" versions apply the fill animation to already existing layouts in
   a Layouts.flat structure. *)

let make_avar l action =
  let open Layout in
  let update _ _ =
    match l.house with
    | None -> printd debug_error "hfill: the room must have a house.";
      0
    | Some house -> begin
        match house.content with
        | Resident _ ->
          failwith (sprintf
                      "hfill: This house %s cannot contain a Resident, \
                       since it must containt the Layout %s amongst its Rooms."
                      (sprint_id house) (sprint_id l))
        | Rooms rooms ->
          action house l rooms
      end in
  Avar.create ~duration:0 ~update 0;;

let make_hfill ?(margin = Theme.room_margin) l =
  let avar = make_avar l (fun house l rooms ->
      hfill_action margin house l rooms; Layout.width l) in
  Layout.animate_w l avar;
  Layout.WHash.add rooms_to_update l;;

let make_vfill ?(margin = Theme.room_margin) l =
  let avar = make_avar l (fun house l rooms ->
      vfill_action margin house l rooms; Layout.height l) in
  Layout.animate_h l avar;
  Layout.WHash.add rooms_to_update l;;

(* hfill ~margin () will create an empty layout that expands at startup in order to
   fill the available width in the parent house. It will only work in a
   "Layout.flat" structure. See example 39. *)
(* rooms to the right will be shifted further right until they reach the right
   boundary, just leaving a right margin of "margin" pixels; it won't work for
   rooms that are animated. *)
let hfill ?(margin = Theme.room_margin) () =
  let l = Layout.empty ~w:0 ~h:0 ~name:"hfill-a" () in
  make_hfill ~margin l;
  l;;

(* vfill will only work in a Layout.tower structure *)
let vfill ?(margin = Theme.room_margin) () =
  let l = Layout.empty ~w:0 ~h:0 ~name:"vfill-a" () in
  make_vfill ~margin l;
  l;;

(* set width + margin to fit house width *)
let full_width ?(margin = Theme.room_margin) l =
  let open Layout in
  let update _ _ =
    match l.house with
    | None -> printd debug_error "hfill: the room must have a house.";
      0
    | Some house -> begin
        full_width_action margin l house;
        width l
      end in
  let avar = Avar.create ~duration:0 ~update 0 in
  animate_w l avar;
  Layout.WHash.add rooms_to_update l;;



let full_height ?(margin = Theme.room_margin) l =
  let open Layout in
  let update _ _ =
    match l.house with
    | None -> printd debug_error "full_height: the room must have a house.";
      0
    | Some house -> begin
        full_height_action margin l house;
        height l
      end in
  let avar = Avar.create ~duration:0 ~update 0 in
  animate_h l avar;
  Layout.WHash.add rooms_to_update l;;

let vcenter l =
  let open Layout in
  let update _ _ =
    match l.house with
    | None -> printd debug_error "vcenter: the room must have a house.";
      0
    | Some house -> begin
        vcenter_action l house;
        get_oldy l
      end in
  let avar = Avar.create ~duration:0 ~update 0 in
  animate_y l avar;
  Layout.WHash.add rooms_to_update l;;


















(** old **)

(* (h/v)fill are placeholder layouts that expand at startup, and optionally on
   update *)
(* TODO in fact, startup is not really necessary. However, be careful when
   create a Space on-the-fly with ~update: it will be updated at next sync, and
   if the layout has no canvas, it will maybe crash (?). To avoid this: make
   sure you immediately attach it to a displayed layout. *)
(* TODO add "resize" event. The problem is that events are only sent to widgets
   that have focus. Maybe we should make an exception for some events like
   resize ? (send them to all widgets ?) Cf: Bogue.resize *)

let add_action update action w l =
  let open Layout in
  let startup _ _ _ =
    do_option l.house (fun h ->
        match h.content with
        | Resident _ ->
          failwith (sprintf "This house %s cannot contain a Resident, \
                             since it must containt the Layout %s amongst \
                             its Rooms."
                      (sprint_id h) (sprint_id l))
        | Rooms rooms ->
          action h l rooms 
      ) in
  let events = if update
    then Trigger.[startup; update; mouse_enter (* DEBUG*) ]
    else [Trigger.startup] in
  let c = Widget.connect_main w w startup events in
  Widget.add_connection w c;
  if update then Update.push w;;

let create update action =
  let w = 0 and h = 0 in
  let e = Widget.empty ~w ~h () in
  let l = Layout.resident ~name:"hfill" e in
  add_action update action e l;
  l;;

let add_room_action update action w l =
  let open Layout in
  let startup _ _ _ =
    do_option l.house (action l) in
  let events = if update
    then Trigger.[startup; update; mouse_enter (* DEBUG*)]
    else [Trigger.startup] in
  let c = Widget.connect_main w w startup events in
  Widget.add_connection w c;
  if update then Update.push w;;

let hfill_old ?(sep = Theme.room_margin/2) ?(update=false) () =
  create update (hfill_action sep);;

let vfill_old ?(sep = Theme.room_margin/2) ?(update=false) () =
  create update (vfill_action sep);;

let full_width_old ?(margin = Theme.room_margin) ?(update=false) ?widget room =
  let widget = default widget (Layout.first_show_widget room) in
  add_room_action update (full_width_action margin) widget room;;

let full_height_old ?(margin = Theme.room_margin) ?(update=false) ?widget room =
  let widget = default widget (Layout.first_show_widget room) in
  add_room_action update (full_height_action margin) widget room;;


(* in example 39: the order of the following commands: full_width and
    make_hfill is important (both will be executed at Sync): first we set the
    full_width to line2, and then the hfill can have some effect.  However if
    we manually set the width of line2 (instead of calling full_width), then
    the hfill can be before or after (because set_width is immediate, while
    hfill is Sync.) *)

(* Note that the connection has to be attached to a widget. Either the widget is
   specified (safer) or the first one appearing in the room will be selected. *)
let make_hfill_old ?(sep = Theme.room_margin/2) ?(update=false) ?widget room =
  let widget = default widget
      (Layout.first_show_widget room) in
  add_action update (hfill_action sep) widget room;;


let make_hfill_bis ?(sep = Theme.room_margin/2) l =
  let open Layout in
  let update _ _ =
    match l.house with
    | None -> printd debug_error "hfill: the room must have a house.";
      0
    | Some house -> begin
        match house.content with
        | Resident _ ->
          failwith (sprintf
                      "hfill: This house %s cannot contain a Resident, \
                       since it must containt the Layout %s amongst its Rooms."
                      (sprint_id house) (sprint_id l))
        | Rooms rooms ->
          hfill_action sep house l rooms;
          width l
      end in
  let avar = Avar.create ~duration:0 ~update 0 in
  animate_w l avar;;

let make_vfill_bis ?(sep = Theme.room_margin/2) l =
  let open Layout in
  let update _ _ =
    match l.house with
    | None -> printd debug_error "vfill: the room must have a house.";
      0
    | Some house -> begin
        match house.content with
        | Resident _ ->
          failwith (sprintf
                      "vfill: This house %s cannot contain a Resident, \
                       since it must containt the Layout %s amongst its Rooms."
                      (sprint_id house) (sprint_id l))
        | Rooms rooms ->
          vfill_action sep house l rooms;
          height l
      end in
  let avar = Avar.create ~duration:0 ~update 0 in
  animate_h l avar;;
