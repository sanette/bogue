open Tsdl
open B_utils
module Theme = B_theme
module Trigger =  B_trigger
module Draw = B_draw
module E = Sdl.Event
         
(* mouse position. If the mouse goes over a second window, the new origin
   immediately shifts *)
(* it doesn't work for touchscreen (only the first touch, not motion) *)
let pos = Trigger.mouse_pos;;

(* the mouse position registered in the mouse_motion event *)
(* Inutile: apparemment ça ne change rien *)
let motion_pos ev =
  match Trigger.event_kind ev with
  | `Mouse_motion ->
    let x = E.(get ev mouse_motion_x)
            |> Theme.unscale_int in
    let y = E.(get ev mouse_motion_y)
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
    let x = E.(get ev mouse_button_x) in
    let y = E.(get ev mouse_button_y) in
    (Draw.unscale_size (x, y))
  | _ ->  failwith "WRONG EVENT";;

(* return the SDL window where the mouse focus is. If not, return the last
   one. TODO check if window does exist (was not destroyed) *)
let get_window =
  let last_win = ref None in
  fun () ->
  do_option (Sdl.get_mouse_focus ()) (fun win -> last_win := Some win);
  !last_win;;

let compute_finger_pos ev =
  (* WARNING as of tsdl version??? this is now normalized in 0..1 *)
  let fx = E.(get ev touch_finger_x) in
  let fy = E.(get ev touch_finger_y) in
  match get_window () with
  | None -> failwith "Cannot find window for finger position"
  (* TODO don't fail for this? *)
  | Some win ->
     let w,h = Sdl.get_window_size win in
     Theme.(unscale_f (fx *. float w),unscale_f (fy *. float h));;
    
let finger_pos ev =
  match Trigger.event_kind ev with
  | `Finger_down
  | `Finger_up
  | `Finger_motion -> compute_finger_pos ev
  | _ ->  failwith "WRONG EVENT";;

(* guess where the pointer is, trying mouse first and then touch *)
(* in logical pixels *)
(* TODO retrieve also from mouse_at_rest *)
let pointer_pos ev =
  match Trigger.event_kind ev with
  | `Mouse_motion
  | `Mouse_button_down
  | `Mouse_button_up ->
    let x = E.(get ev mouse_button_x) in
    let y = E.(get ev mouse_button_y) in
    Theme.(unscale_int x, unscale_int y)
  | `Finger_down
  | `Finger_up
  | `Finger_motion ->
     let x,y = compute_finger_pos ev in
     (round x, round y)
  | _ -> begin
      printd debug_error
        "The event for pointer_pos should be a mouse or touch event";
      Trigger.mouse_pos ()
    end;;










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
