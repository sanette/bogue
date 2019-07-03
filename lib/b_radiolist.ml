(* a radiolist set of widgets *)

(* we create an array of widgets consisting of a radio button and a label (or
   another widget); the callback (action) is a little bit complicated because we
   have to make sure only one button is checked at a time. *)

(* in principle this should be just a particular case of Menu, but this is
   not what we do here. *)

open Tsdl
open B_utils
module W = B_widget
module Layout = B_layout
module Var = B_var
module Trigger =  B_trigger
module Draw = B_draw
module Check = B_check
module Update = B_update
  
type widgets = {
  index : (int option) Var.t; (* the index of selected entry *)
  data : (W.t * W.t) array
}

type t = {
  widgets : widgets;
  layout :  Layout.t;
  click_on_label : bool
}

(* returns all widgets that are active for selecting an entry. Useful for adding
   further connections, cf Example30 *)
let active_widgets t =
  if t.click_on_label then
    t.widgets.data
    |> Array.to_list
    |> List.map (fun (a,b) -> [a;b])
    |> List.flatten
  else t.widgets.data
       |> Array.to_list
       |> List.map (fun (a,_) -> a);;

(* return the checlk widget of the selected entry, or None *)
let get_button widgets =
  map_option (Var.get widgets.index) (fun i ->
      let (b,_) = widgets.data.(i) in b);;

(* string -> label *)
let make_label ?(click_on_label=true) entry =
  let l = W.label entry in
  if click_on_label
  then l.W.cursor <- Some (go (Draw.create_system_cursor Sdl.System_cursor.hand));
  l;;

let make_button () =
  let style = Check.Circle in
  W.create_empty  (W.Check (Check.create ~style ()));;

(* mettre "b w" n'est pas vraiment nécessaire car on peut faire "let (b,w) =
   widgets.data.(i)", mais bon... *)
let select_action widgets i b _=
  if not (W.get_state b)
  then begin
      W.set_check_state b true;
      do_option (get_button widgets) (fun old_b ->
          W.set_check_state old_b false);
      Var.set widgets.index (Some i);
      (*Update.push b*)
    end;;

let make_connections widgets =
  for i = 0 to Array.length widgets.data - 1 do
    let (b,w) = widgets.data.(i) in
    let action = select_action widgets i in
    let c = W.connect_main b w (fun b w _ -> action b w) Trigger.buttons_down in
    let c' = W.connect_main w b (fun w b _ -> action b w) Trigger.buttons_down in
    W.add_connection b c;
    W.add_connection w c'
  done;;

let make_widgets ?selected ?(click_on_label=true) entries =
  let data = Array.map (fun entry ->
      (make_button (), make_label ~click_on_label entry)) entries in
  do_option selected (fun i ->
      let (b,_) = data.(i) in
      W.set_check_state b true);
  let widgets = { index = Var.create selected; data } in
  make_connections widgets; widgets;;

(* create a vertical (ie. standard) layout *)
let vertical ?(name = "radiolist") ?(click_on_label=true) ?selected entries =
  let widgets = make_widgets ?selected ~click_on_label entries in
  let layout = Layout.tower ~sep:0 ~margins:0 ~name
      (List.map (fun (b,l) -> 
           Layout.flat_of_w ~sep:2 ~align:Draw.Center [b;l])
          (Array.to_list widgets.data)) in
  {widgets; layout; click_on_label};;

(* get index of selected entry, or None *)
let get_index t =
  Var.get t.widgets.index;;

(* sets the selected entry to i and directly activate the button's connections
   with the var_changed event. *)
let set_index t i =
  let (b,w) = t.widgets.data.(i) in
  select_action t.widgets i b w;
  (* This will wake up the widget b even if it doesn't have mouse focus *)
  Update.push b;;
(* another possibility, if using Update sounds like a bad idea, is to directly
   wake the widget up with *)
(* let e = Trigger.(create_event var_changed) in List.iter *)
(*   (W.wake_up e) b.W.connections;; *)
(* but then it is possible that the connections be triggered too many times *)


let layout t =
  t.layout;;
