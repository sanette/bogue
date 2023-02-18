(* a radiolist set of widgets *)

(* This module offers a general way to connect a set of widgets so that thay
   behave like a radiolist: at most one is selected at any time. Of course it
   can also create from scratch a layout containg a standard radiolist with
   round button and labels.  *)

(* TODO offer a horizontal layout too *)

open Tsdl
open B_utils
module W = B_widget
module Layout = B_layout
module Var = B_var
module Trigger =  B_trigger
module Draw = B_draw
module Check = B_check
module Update = B_update

type toggle_widget = {
  widget : W.t;
  trigger : Trigger.t;
  set_state : bool -> unit;
  get_state : unit -> bool
}

type t = {
  index : (int option) Var.t; (* the index of selected entry, starting form 0. *)
  widgets : (toggle_widget * toggle_widget option) array;
  (* For a standard radiolist, each entry of the [widgets] array is (check_button, label) *)
  layout :  Layout.t option;
  (* This module optionally creates a layout containing the widgets *)
  click_on_label : bool;
  allow_no_choice : bool
}

(* Return all widgets that are active for selecting an entry. Useful for adding
   further connections, cf Example30 *)
let active_widgets t =
  if t.click_on_label then
    t.widgets
    |> Array.to_list
    |> List.map (fun (a, b) -> match b with
        | Some b -> [a.widget; b.widget] | None -> [a.widget])
    |> List.flatten
  else t.widgets
       |> Array.to_list
       |> List.map (fun (a, _) -> a.widget)

(* Return the first toogle_widget of the selected entry, or None if nothing is
   selected *)
let get_button t =
  map_option (Var.get t.index)
    (fun i -> fst t.widgets.(i))


(* Note that [b] is redundant since it can be obtained via [fst t.widgets.(i)]
   *)
let select_action t i b =
  match b.get_state (), Var.get t.index <> Some i with
  | true, true -> (* We select a new choice *)
    do_option (get_button t) (fun old_w ->
        old_w.set_state false);
    Var.set t.index (Some i);
    (*Update.push b*)
  | false, false ->
    (* We deselect current choice *)
    if t.allow_no_choice then Var.set t.index None
    else b.set_state true
  | true, false ->
    (* we are already on the selected widget *) ()
  | false, true -> (* the click was probably invalid *)
    ()

let make_connections t =
  for i = 0 to Array.length t.widgets - 1 do
    let (b, w) = t.widgets.(i) in
    let action = select_action t i in
    if t.click_on_label
    then do_option w (fun w ->
        let c = W.connect_main w.widget b.widget
            (fun _ _ _ ->
               b.set_state (not (b.get_state ()));
               (* TODO ça pourrait dépendre du fonctionnement de b: mouse_down
                  ou up, etc...*)
               action b)
            [w.trigger] in
        W.add_connection w.widget c);
    let w = default w b in
    (* if there is no [w] we use [b] itself as target of the connection *)
    let c' = W.connect_main b.widget w.widget (fun _ _ _ -> action b)
        [b.trigger] in
    W.add_connection b.widget c'
  done

(* If click_on_label is true, we add a connection on the label with
   Trigger=mouse_button_down to un/select the radio button. Nothing prevents you
   to do this by hand, hence not using this option. *)
let create ?selected ?layout ?(click_on_label=true) widgets =
  Array.iteri (fun i (tw, two) ->
      let state = selected = Some i in
      tw.set_state state;
      do_option two (fun tw -> tw.set_state state)) widgets;
  let t = { index = Var.create selected;
            widgets;
            layout;
            click_on_label;
            allow_no_choice = (selected = None)} in
  make_connections t;
  t

(* Create a radio-like behaviour by connecting a list of toggle_widgets *)
let of_toggle_widgets ?selected tws =
  let widgets = List.map (fun tw -> (tw, None)) tws
                |> Array.of_list in
  create ?selected ~click_on_label:false widgets

let toggle_widget_of_widget widget =
  let set_state, get_state, trigger = match widget.W.kind with
    | W.Button _ ->
      W.set_state widget,
      (fun () -> W.get_state widget),
      Trigger.E.mouse_button_up
    | W.Check _ ->
      W.set_state widget,
      (fun () -> W.get_state widget),
      Trigger.E.mouse_button_down
    | W.Label _ ->
      (fun _ ->
         printd (debug_error) "[Radiolist] Cannot set state of a Label"),
      (fun () ->
         printd debug_error "[Radiolist] Label does not have any state";
         false),
      Trigger.E.mouse_button_down
    | _ -> invalid_arg "[Radiolist.toggle_widget_of_widget] widget %i"
             (W.id widget) in
  (* W.remove_trigger widget trigger; *) (* We remove possibly conflicting connections *)
  { widget; trigger; set_state; get_state }

(* from a bare Button.t *)
let make_button_widget b =
  let open W in
  let b = create_empty (Button b) in
  let press = fun _ _ _ -> Button.press (get_button b) in
  let c = connect_main b b press Trigger.buttons_down in
  add_connection b c;
  let c = connect_main b b (fun b _ _ -> Button.mouse_enter (get_button b))
      [Trigger.mouse_enter] in
  add_connection b c;
  let c = connect_main b b (fun b _ _ -> Button.mouse_leave (get_button b))
      [Trigger.mouse_leave] in
  add_connection b c;
  b

(* Create a radio-like behaviour from a list of already existing buttons. TODO
   create layout! *)
let of_buttons ?selected buttons =
  let tws = List.map make_button_widget buttons
            |> List.map toggle_widget_of_widget  in
  of_toggle_widgets ?selected tws

let of_widgets ?selected widgets =
  let tws = List.map toggle_widget_of_widget widgets in
  of_toggle_widgets ?selected tws

(* string -> label toggle_widget *)
let make_label ?(click_on_label=true) entry =
  let l = W.label entry in
  if click_on_label
  then l.W.cursor <- Some (go (Draw.create_system_cursor Sdl.System_cursor.hand));
  toggle_widget_of_widget l

let make_check () =
  let style = Check.Circle in
  (* W.create_empty (W.Check (Check.create ~style ())) *)
  W.check_box ~style ()
  |> toggle_widget_of_widget

(* Create widgets from string entries *)
let make_widgets ?(click_on_label=true) entries =
  Array.map (fun entry ->
      (make_check (), Some (make_label ~click_on_label entry))) entries

(* create a vertical (ie. standard) layout *)
let vertical ?(name = "radiolist") ?(click_on_label=true) ?selected entries =
  let widgets = make_widgets ~click_on_label entries in
  let layout = Layout.tower ~sep:0 ~margins:0 ~name
      (List.map (function
           | (b, Some l) ->
             Layout.flat_of_w ~sep:2 ~align:Draw.Center [b.widget; l.widget]
           | _ -> invalid_arg "[Radiolist.vertical] this should not happen")
          (Array.to_list widgets))
               |> Option.some in
  create ?selected ?layout ~click_on_label widgets

(* get index of selected entry, or None *)
let get_index t =
  Var.get t.index

(* Set the selected entry to i and directly activate the button's connections *)
let set_index t io =
  let ioo, state = match io with
    | Some i -> Some i, true
    | None -> Var.get t.index, false in
  do_option ioo (fun i ->
    let (b, _w) = t.widgets.(i) in
    b.set_state state;
    select_action t i b;
    (* This will wake up the widget b even if it doesn't have mouse focus: *)
    Update.push b.widget)

(* another possibility, if using Update sounds like a bad idea, is to directly
   wake the widget up with *)
(* let e = Trigger.(create_event var_changed) in List.iter *)
(*   (W.wake_up e) b.W.connections;; *)
(* but then it is possible that the connections be triggered too many times *)

let layout t =
  match t.layout with
  | Some l -> l
  | None -> printd (debug_error + debug_user)
              "This type of radiolist doesn't carry its own layout";
    failwith "This type of radiolist doesn't carry its own layout"
