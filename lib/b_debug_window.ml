(** an interactive window to select the Debug level *)
open Tsdl
open B_utils
module W = B_widget
module L = B_layout
module Draw = B_draw
  
let is_set code =
  code land !debug_code <> 0;;

let toggle code =
  debug_code := !debug_code lxor code;;

let set code b =
  if b then debug_code := !debug_code lor code
  else debug_code := !debug_code land (lnot code);;

let create () =
  let save_layer = Draw.get_current_layer () in 
  Draw.use_new_layer (); (* TODO this should be saved to the window, not global, otherwise layouts that are created after this but in an older window will be drawn on this layer, and thus not shown at all... *)
  let b = W.check_box ~state:!W.draw_boxes () in
  let l = W.label "Turn on debug rectangles" in
  let dbg_boxes = L.flat_of_w ~align:Draw.Center [b;l] in

  let action w _ _ =
    W.draw_boxes := W.get_state w in
  let c_boxes = W.connect b b action [Sdl.Event.mouse_button_down; Sdl.Event.finger_down] in

  let b = W.check_box ~state:!debug () in
  let l = W.label "Turn on debugging trace" in
  let dbg_button = L.flat_of_w ~align:Draw.Center [b;l] in

  let title = W.label "Debug Variables" in
  let action code w _ _ =
    set code (W.get_state w)
  in
  let rec loop vars rooms connections =
    match vars with
    | [] -> rooms, connections
    | (var,code)::rest ->
      let bb = W.check_box ~state:(is_set code) () in
      let ll = W.label var in
      let btn = L.flat_of_w ~sep:0 [bb;ll] in
      let c = W.connect bb bb (action code) [Sdl.Event.mouse_button_down] in
      loop rest (btn :: rooms) (c :: connections) in

  let rooms, connections = loop debug_vars [] [] in

  let panel = L.tower ~sep:0 ((L.flat_of_w ~sep:10 [title]) :: rooms) in
  let action w _ _ =
    let ok = W.get_state w in
    debug := ok;
    if ok
    then (L.show panel; L.fade_in panel)
    else (L.hide panel; L.fade_out panel)  in
  let c = W.connect b b action [Sdl.Event.mouse_button_down] in

  List.iter (fun c -> W.(add_connection c.source c)) (c_boxes :: c :: connections);
  panel.L.show <- !debug;
  let layout = L.tower ~sep:0 [dbg_boxes; dbg_button; panel] in
  Draw.set_current_layer save_layer; (* DEBUG: not here ! *)
  layout;;

(* let _ = *)
(*   let layout = create () in *)
(*   let board = make [] [layout] in *)
(*   run board; *)
(*   Draw.quit;; *)
