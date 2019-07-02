open Tsdl
open B_utils
module Theme = B_theme
module Trigger =  B_trigger
module Draw = B_draw
  
(* mouse position. If the mouse goes over a second window, the new origin
   immediately shifts *)
(* it doesn't work for touchscreen (only the first touch, not motion) *)
let pos = Trigger.mouse_pos;;

(* the mouse position registered in the mouse_motion event *)
(* Inutile: apparemment ça ne change rien *)
let motion_pos ev =
  match Trigger.event_kind ev with
  | `Mouse_motion ->
    let x = Sdl.Event.(get ev mouse_motion_x)
            |> Theme.unscale_int in
    let y = Sdl.Event.(get ev mouse_motion_y)
            |> Theme.unscale_int in
    let px,py = pos () in
    if (x,y) <> (px,py) then printd debug_event " ! Mouse_pos (%d,%d) <> Motion_pos (%d,%d) ! " px py x y;
    Some (x,y)
  | _ -> None

let button_pos ev =
  match Trigger.event_kind ev with
  | `Mouse_motion
  | `Mouse_button_down
  | `Mouse_button_up ->
    let x = Sdl.Event.(get ev mouse_button_x) in
    let y = Sdl.Event.(get ev mouse_button_y) in
    (Draw.unscale_size (x, y))
  | _ ->  failwith "WRONG EVENT";;

let finger_pos ev =
  match Trigger.event_kind ev with
  | `Finger_down
  | `Finger_up
  | `Finger_motion ->
    let x = Sdl.Event.(get ev touch_finger_x) in
    let y = Sdl.Event.(get ev touch_finger_y) in
    Theme.(unscale_f x,unscale_f y)
  | _ ->  failwith "WRONG EVENT";;


let pointer_pos = Trigger.pointer_pos;;
    
(* the mouse_pos with respect to the given window, using window position if
   necessary *)
let window_pos =
  let x' = ref 0 in
  let y' = ref 0 in
  fun window ->
    let x,y = pos () in
    match Sdl.get_mouse_focus () with
    | None -> (!x', !y')
    | Some w -> if Sdl.get_window_id w = Sdl.get_window_id window
      then (x' := x; y' := y; (x,y))
      else let x0,y0 = Sdl.get_window_position window in
        let x1,y1 = Sdl.get_window_position w in
        (x':= x1-x0 + x; y' := y1-y0 + y;
         (!x',!y'));;
