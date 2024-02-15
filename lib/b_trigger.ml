(* we use Sdlevent plus some additions *)
(* Warning, SDL is not thread-safe, so we implement some mutexes and
   don't use wait_event *)
(* Doc wiki:
   NOTE: You should not expect to be able to create a window, render, or
   receive events on any thread other than the main one *)
(* It is not clear to me if a thread is authorized to *send* an event
   to SDL *)

(* WARNING: just loading this module will execute some Sdl functions (bad?).
   Thus, it will initialize SDL *)

open Tsdl
open B_utils
module E = Sdl.Event
module Var = B_var
open Result

(* We initialize SDL with only the events subsystem *)
let () =
  Sdl.(init Init.nothing) |> go;
  printd debug_warning "SDL initialized";
  let a,b,c = Sdl.get_version () in
  Sdl.log "Using SDL %u.%u.%u" a b c;
  Sdl.(init_sub_system Init.events) |> go;
  printd debug_event "SDL Events initialized"

type t = Sdl.event_type

let event_names : (t,string) Hashtbl.t = Hashtbl.create 20

let name_list = [
  E.finger_down, "finger_down";
  E.finger_motion, "finger_motion";
  E.finger_up, "finger_up";
  E.key_down, "key_down";
  E.key_up, "key_up";
  E.mouse_button_down, "mouse_button_down";
  E.mouse_button_up, "mouse_button_up";
  E.mouse_motion, "mouse_motion";
  E.mouse_wheel, "mouse_wheel";
  E.sys_wm_event, "sys_wm_event";
  E.text_editing, "text_editing";
  E.text_input, "text_input";
  E.display_event, "display_event";
  E.window_event, "window_event"
]

let () =
  List.iter (fun (e,n) -> Hashtbl.add event_names e n) name_list


(* this will be set by the main loop in Bogue *)
let main_tread_id = ref (-1)

let new_event_type name =
  match Sdl.register_event () with
  | None -> failwith "Cannot register event."
  | Some t -> (
      Hashtbl.add event_names t name;
      printd debug_event "Register new event type:%s (%d)" name t;
      t)

let create_event t =
  let e = E.create () in
  E.(set e typ t);
  e

let create_window_event w_id =
  let e = create_event E.window_event in
  E.(set e window_event_id w_id);
  e

(* we create new user types. The first one should be the predefined
   E.user_event *)

let user_type = new_event_type "user"

let () = assert (user_type = E.user_event)  (* 32768 *)

let user_event = E.user_event

let stop = new_event_type "stop"

let stopped = new_event_type "stopped"

let mouse_enter = new_event_type "mouse_enter"

let mouse_leave = new_event_type "mouse_leave"
(* The mouse_leave and mouse_enter events are sent when the layout under the
   mouse changes, and if the mouse button is *not* clicked* -- because in this
   case, the clicked widget should keep the focus until the button is
   released. However, in the current implementation, if the button is down, the
   mouse_leave event will be sent when the mouse leaves the Bogue window. See
   [Main.check_mouse_motion]. *)

let redraw = new_event_type "redraw" (* TODO: select only a particular canvas *)

(* let refresh = new_event_type () *)

let mouse_at_rest = new_event_type "mouse_at_rest"

let startup = new_event_type "startup"

(* The var_changed event can be send to notify that some widget made a change to
   a global variable. Not used yet. *)
let var_changed = new_event_type "var_changed"

(* The update event can be used to trigger some actions when a widget is
   updated, even if it doesn't get mouse focus. It is filtered early in
   b_main.ml and not sent to the mouse_focus. At this point it is not clear
   whether we need both var_changed and update *)
let update = new_event_type "update"

let sync_action = new_event_type "sync_action"

let keyboard_focus = new_event_type "keyboard_focus"

let mouse_focus = new_event_type "mouse_focus"

let remove_layout = new_event_type "remove_layout"

let destroy_window = new_event_type "destroy_window"

let not_used = new_event_type "not_used"

(* Some aliases. Beware that in case of finger events, the OS or SDL will most
   likely trigger the corresponding mouse event as well, so it's not necessary
   to react to both. *)

let buttons_down = E.[mouse_button_down; finger_down]
let buttons_up = E.[mouse_button_up; finger_up]
let pointer_motion = E.[mouse_motion; finger_motion]

(* This event is used by the main loop, and sometimes we need to send it to
   other threads. In this case we create a new event for the main loop, using
   renew_my_event below. *)
let my_event = ref (E.create ())

let renew_my_event () =
  let e = create_event not_used in (* this should be innocuous *)
  my_event := e

let of_event ev =
  E.(get ev typ)

(* See tsdl.mli *)
(* TODO when we switch to Tsdl 0.9.8, we should use their 'Event.enum' type
   instead, so that we don't become incompatible every time they add a new
   variant...  See PR https://github.com/dbuenzli/tsdl/pull/54 *)
(* Or, one could copy here the function 'enum' from tsdl.ml *)

type sdl_event =
[ `App_did_enter_background
| `App_did_enter_foreground
| `App_low_memory
| `App_terminating
| `App_will_enter_background
| `App_will_enter_foreground
| `Clipboard_update
| `Controller_axis_motion
| `Controller_button_down
| `Controller_button_up
| `Controller_device_added
| `Controller_device_remapped
| `Controller_device_removed
| `Dollar_gesture
| `Dollar_record
| `Drop_file
| `Finger_down
| `Finger_motion
| `Finger_up
| `Joy_axis_motion
| `Joy_ball_motion
| `Joy_button_down
| `Joy_button_up
| `Joy_device_added
| `Joy_device_removed
| `Joy_hat_motion
| `Key_down
| `Key_up
| `Mouse_button_down
| `Mouse_button_up
| `Mouse_motion
| `Mouse_wheel
| `Multi_gesture
| `Quit
| `Sys_wm_event
| `Text_editing
| `Text_input
| `Unknown of int
| `User_event
| `Window_event
| `Display_event
| `Sensor_update
(* Added in Tsdl 1.0.0 *)
| `Audio_device_added
| `Audio_device_removed
| `Drop_begin
| `Drop_complete
| `Drop_text
| `Keymap_changed
| `Render_device_reset
| `Render_targets_reset ]

type bogue_event =
  [ `Bogue_startup
  | `Bogue_stop
  | `Bogue_stopped
  | `Bogue_mouse_at_rest
  | `Bogue_mouse_enter
  | `Bogue_mouse_leave
  | `Bogue_var_changed
  | `Bogue_keyboard_focus
  | `Bogue_mouse_focus
  | `Bogue_remove_layout
  | `Bogue_destroy_window
  | `Bogue_update
  | `Bogue_sync_action
  | `Bogue_redraw
  | `Bogue_keymap_changed (* SDL event, which was missing in Tsdl. *)
  ]

let generalize_sdl_event ev = (ev : sdl_event :> [> sdl_event | bogue_event])
let generalize_bogue_event ev = (ev : bogue_event :> [> sdl_event | bogue_event])

let event_kind ev : [> sdl_event | bogue_event] =
  match E.(enum (get ev typ)) with
  (* It would be nice that Tsdl puts the event enum type as refinable variant
     type with [>...]. Then we could add our own variant tags. *)
  | `Unknown x ->
    begin
      match x with (* TODO association list or Imap like tsdl.ml *)
       | i when i = startup -> `Bogue_startup
       | i when i = stop -> `Bogue_stop
       | i when i = stopped -> `Bogue_stopped
       | i when i = mouse_at_rest -> `Bogue_mouse_at_rest
       | i when i = mouse_enter -> `Bogue_mouse_enter
       | i when i = mouse_leave -> `Bogue_mouse_leave
       | i when i = var_changed -> `Bogue_var_changed
       | i when i = keyboard_focus -> `Bogue_keyboard_focus
       | i when i = mouse_focus -> `Bogue_mouse_focus
       | i when i = remove_layout -> `Bogue_remove_layout
       | i when i = destroy_window -> `Bogue_destroy_window
       | i when i = update -> `Bogue_update
       | i when i = redraw -> `Bogue_redraw
       | i when i = sync_action -> `Bogue_sync_action
       | i when i = 0x304 -> `Bogue_keymap_changed
       (* keymap_changed was forgotten, but corrected in recent Tsdl.
          https://github.com/dbuenzli/tsdl/issues/76#event-6708707941 See also:
          https://github.com/libsdl-org/SDL/issues/5520 *)
       | _ -> printd debug_event "UNKNOWN EVENT=0x%x" x;
              `Unknown x
     end
  | e -> generalize_sdl_event e

(* Query replace regexp (default `\([a-z_]+\)\1): `\([a-z]+\)`\1 -> "\1" *)

let window_event_name = function
  | `Close -> "Close"
  | `Enter -> "Enter"
  | `Exposed -> "Exposed"
  | `Focus_gained -> "focus_gained"
  | `Focus_lost -> "Focus_lost"
  | `Hidden -> "Hidden"
  | `Hit_test -> "Hit_test"
  | `Leave -> "Leave"
  | `Maximized -> "Maximized"
  | `Minimized -> "Minimized"
  | `Moved -> "Moved"
  | `Resized -> "Resized"
  | `Restored -> "Restored"
  | `Shown -> "Shown"
  | `Size_changed -> "Size_changed"
  | `Take_focus -> "Take_focus"
  | `Unknown _ -> "Unknown"

(* Some dumb code to duplicate an event. Probably much easier directly in
   C... *)
type field =
  | Button of Sdl.button_state E.field
  | Finger of Sdl.finger_id E.field
  | Float of float E.field
  | Gesture of Sdl.gesture_id E.field
  | Hat of Sdl.Hat.t E.field
  | Int of int E.field
  | Int16 of Sdl.int16 E.field
  | Int32 of int32 E.field
  | Joystick of Sdl.joystick_id E.field
  | Keycode of Sdl.keycode E.field
  | Keymod of Sdl.keymod E.field
  | Scancode of Sdl.scancode E.field
  | String of string E.field
  | Timestamp of Sdl.uint32 E.field
  | Touch of Sdl.touch_id E.field
  | Type of Sdl.event_type E.field
  | Uint32 of Sdl.uint32 E.field
  | Uint8 of Sdl.uint8 E.field
  | Window of E.window_event_id E.field

let common_fields = let open E in
[ Type typ; Timestamp timestamp ]

let touch_finger_fields = let open E in
  [ Touch touch_finger_touch_id;
    Finger touch_finger_finger_id;
    Float touch_finger_x;
    Float touch_finger_y;
    Float touch_finger_dx;
    Float touch_finger_dy;
    Float touch_finger_pressure;
  ]

let controller_button_fields = let open E in
  [ Joystick controller_button_which;
    Uint8 controller_button_button;
    Button controller_button_state ]

let dollar_gesture_fields = let open E in
  [ Touch dollar_gesture_touch_id;
    Gesture dollar_gesture_gesture_id;
    Int dollar_gesture_num_fingers;
    Float dollar_gesture_error;
    Float dollar_gesture_x;
    Float dollar_gesture_y ]

let joy_button_fields = let open E in
  [ Joystick joy_button_which;
    Uint8 joy_button_button;
    Button joy_button_state ]

let joy_device_fields = let open E in
  [ Joystick joy_device_which ]

let keyboard_fields = let open E in
  [ Int keyboard_window_id;
    Button keyboard_state;
    Int keyboard_repeat;
    Scancode keyboard_scancode;
    Keycode keyboard_keycode;
    Keymod keyboard_keymod ]

let mouse_button_fields = let open E in
  [ Int mouse_button_window_id;
    Uint32 mouse_button_which;
    Uint8 mouse_button_button;
    Button mouse_button_state;
    Uint8 mouse_button_clicks;
    Int mouse_button_x;
    Int mouse_button_y ]

let mouse_motion_fields = let open E in
  [ Int mouse_motion_window_id;
    Uint32 mouse_motion_which;
    Uint32 mouse_motion_state;
    Int mouse_motion_x;
    Int mouse_motion_y;
    Int mouse_motion_xrel;
    Int mouse_motion_yrel ]

let user_fields = let open E in
  [ Int user_window_id;
    Int user_code ]

let special_fields_per_type = let open E in
  [
    controller_axis_motion,
    [ Joystick controller_axis_which;
      Uint8 controller_axis_axis;
      Int16 controller_axis_value ];

    controller_button_down, controller_button_fields;
    controller_button_up, controller_button_fields;

    controller_device_added,
    [ Joystick controller_device_which ];
    controller_device_remapped, (* idem *)
    [ Joystick controller_device_which ];
    controller_device_removed, (* idem *)
    [ Joystick controller_device_which ];

    dollar_gesture, dollar_gesture_fields;
    dollar_record, dollar_gesture_fields;

    (* warning drop_file_file cannot be copied *)

    finger_down, touch_finger_fields;
    finger_motion, touch_finger_fields;
    finger_up, touch_finger_fields;

    joy_axis_motion,
    [ Joystick joy_axis_which;
      Uint8 joy_axis_axis;
      Int16 joy_axis_value ];

    joy_ball_motion,
    [ Joystick joy_ball_which;
      Uint8 joy_ball_ball;
      Int joy_ball_xrel;
      Int joy_ball_yrel ];

    joy_button_down, joy_button_fields;
    joy_button_up, joy_button_fields;

    joy_device_added, joy_device_fields;
    joy_device_removed, joy_device_fields;

    joy_hat_motion,
    [ Joystick joy_hat_which;
      Uint8 joy_hat_hat;
      Hat joy_hat_value ];

    key_down, keyboard_fields;
    key_up, keyboard_fields;

    mouse_button_down, mouse_button_fields;
    mouse_button_up, mouse_button_fields;

    mouse_motion, mouse_motion_fields;

    mouse_wheel,
    [ Int mouse_wheel_window_id;
      Uint32 mouse_wheel_which;
      Int mouse_wheel_x;
      Int mouse_wheel_y ];

    multi_gesture,
    [ Touch multi_gesture_touch_id;
      Float multi_gesture_dtheta;
      Float multi_gesture_ddist;
      Float multi_gesture_x;
      Float multi_gesture_y;
      Int multi_gesture_num_fingers ];

    text_editing,
    [ Int text_editing_window_id;
      String text_editing_text;
      Int text_editing_start;
      Int text_editing_length ];

    text_input,
    [ Int text_input_window_id;
      String text_input_text ];

    user_event, user_fields;

    window_event,
    [ Int window_window_id;
      Window window_event_id;
      Int32 window_data1;
      Int32 window_data2 ];
  ]

let copy_field e1 e2 field =
  let x = E.get e1 field in E.set e2 field x

let copy_field e1 e2 field =
  match field with
  | Button f -> copy_field e1 e2 f
  | Finger f -> copy_field e1 e2 f
  | Float f -> copy_field e1 e2 f
  | Gesture f -> copy_field e1 e2 f
  | Hat f -> copy_field e1 e2 f
  | Int f -> copy_field e1 e2 f
  | Int16 f -> copy_field e1 e2 f
  | Int32 f -> copy_field e1 e2 f
  | Joystick f -> copy_field e1 e2 f
  | Keycode f -> copy_field e1 e2 f
  | Keymod f -> copy_field e1 e2 f
  | Scancode f -> copy_field e1 e2 f
  | String f -> copy_field e1 e2 f
  | Timestamp f -> copy_field e1 e2 f
  | Touch f -> copy_field e1 e2 f
  | Type f -> copy_field e1 e2 f
  | Uint32 f -> copy_field e1 e2 f
  | Uint8 f -> copy_field e1 e2 f
  | Window f -> copy_field e1 e2 f

let copy_fields e1 e2 fields =
  List.iter (copy_field e1 e2) fields

(* pourquoi est-ce qu'on ne ferait pas juste un push/pop event ? *)
let copy_event e =
  let e2 = E.create () in
  copy_fields e e2 common_fields;
  let t = E.(get e typ) in
  let fields = if t > E.user_event
    (* all event types greater than user_event are user created events *)
    then user_fields
    else try List.assoc t special_fields_per_type
      with Not_found -> [] in
  copy_fields e e2 fields;
  e2

(* val enum : Tsdl.Sdl.event_type -> *)
(*        [ `App_did_enter_background *)
(*        | `App_did_enter_foreground *)
(*        | `App_low_memory *)
(*        | `App_terminating *)
(*        | `App_will_enter_background *)
(*        | `App_will_enter_foreground *)
(*        | `Clipboard_update *)
(*        | `Controller_axis_motion *)
(*        | `Controller_button_down *)
(*        | `Controller_button_up *)
(*        | `Controller_device_added *)
(*        | `Controller_device_remapped *)
(*        | `Controller_device_removed *)
(*        | `Dollar_gesture *)
(*        | `Dollar_record *)
(*        | `Drop_file *)
(*        | `Finger_down *)
(*        | `Finger_motion *)
(*        | `Finger_up *)
(*        | `Joy_axis_motion *)
(*        | `Joy_ball_motion *)
(*        | `Joy_button_down *)
(*        | `Joy_button_up *)
(*        | `Joy_device_added *)
(*        | `Joy_device_removed *)
(*        | `Joy_hat_motion *)
(*        | `Key_down *)
(*        | `Key_up *)
(*        | `Mouse_button_down *)
(*        | `Mouse_button_up *)
(*        | `Mouse_motion *)
(*        | `Mouse_wheel *)
(*        | `Multi_gesture *)
(*        | `Quit *)
(*        | `Sys_wm_event *)
(*        | `Text_editing *)
(*        | `Text_input *)
(*        | `Unknown *)
(*        | `User_event *)
(*        | `Window_event ] *)

(* USER 0 = ask for refresh *)
(* let user id =  *)
(*   let () = match Sdl.register_event () with (\* useful ?? *\) *)
(*     | None -> failwith "Cannot register event. Bla?" *)
(*     | Some t -> print_debug "Event type:%d (%d)" t E.user_event in *)

(*   let open Sdl.Event in *)
(*       let e = create () in *)
(*       set e typ user_event; *)
(*       set e user_code id; *)
(*       e;; *)

(* let user0 = user 0;; *)

(* let add ev =  *)
(*   print_debug "Add USER 0"; *)
(*   if not (go (Sdl.push_event ev)) then print_debug "Warning: Event filtered";; *)

(* let add_user x = if x = 0 then add user0;; *)

let text_event ev =
  match event_kind ev with
    | `Clipboard_update
    | `Key_down
    | `Key_up
    | `Text_editing
    | `Text_input -> true
    | _ -> false

let flush kind =
  Sdl.flush_event kind

let sprint_ev ev =
  let open Printf in
  let t = E.(get ev typ) in
  let name = try sprintf " (%s)" (Hashtbl.find event_names t)
    with Not_found -> "" in
  sprintf "0x%x%s" t name

let push_event ev =
  printd debug_event "Pushing event %s" (sprint_ev ev);
  if not (go (Sdl.push_event ev))
  then printd debug_event "Warning: Event filtered"

(* There seems to be a problem with [Sdl.poll_event None] on some platforms/SDL
   versions: on the macOS VirtualBox with SDL 2.0.18, it returns false
   positives. See https://discourse.libsdl.org/t/pollevent-inconsistency/34358/3
   and the fix in
   https://github.com/libsdl-org/SDL/commit/dca281e810263f1fbf9420c2988932b7700be1d4
   Meanwhile, we avoid using [Sdl.poll_event None].  *)
let _has_no_event_old () =
  not (Sdl.poll_event None)

(* SDL_PeepEvents is currently not bound by Tsdl unfortunately. *)
(* We don't use this anymore, because for some reason (SDL 2.0.10) when the
   event is 0x702 SDL_FINGERMOTION, [push_event] will push instead an event of
   type 0x802 SDL_MULTIGESTURE, (even if we filter this out using
   Sdl.set_event_state E.multi_gesture Sdl.disable !!).  This can easily cause
   an accumulation of thousands of events in the main event_loop... !!  *)
let _has_no_event_old () =
  let e = E.create () in
  if Sdl.poll_event (Some e)
  then begin
    printd debug_event "Event remaining: %s" (sprint_ev e);
    push_event e;
    false
  end
  else true

let has_no_event () =
  Sdl.has_events E.first_event E.last_event |> not

(** get the list of all events, and remove them from the queue *)
(* the first of the list is the oldest, ie the one to be popped at the next
   wait_event *)
let get_all_events () =
  let rec loop list =
    let e = E.create () in
    if Sdl.poll_event (Some e) then loop (e::list)
    else List.rev list in
  loop []

(* Leave only those events that satisfy the filter test and return the others *)
let filter_events filter =
  let filter = if !debug
    then fun ev -> let result = filter ev in
      printd debug_event "Filter on event #%u = %b" E.(get ev typ) result;
      result
    else filter in
  let list = get_all_events () in
  let keep, remove = List.partition filter list in
  List.iter push_event keep;
  remove

(* Remove all events of this kind and return the last one (=most recent).
   Warning: treating the last event without deleting other events changes the
   logical order of emitted events.  *)
let get_last kind =
  if Sdl.has_event kind
  then let remove = filter_events (fun ev -> E.(get ev typ) <> kind) in
    if remove = [] then failwith "[Trigger.get_last]: list should not be empty."
    else Some (List.hd (List.rev remove))
  else None
(* TODO optimize by using a get_all_events which does not do List.rev *)


(* Leave at most n events in the queue, can also filter *)
(* this is brutal: we pop all events, and then push back only n *)
(* if there is a filter, only those events that satisfy the filter are left in
   the queue *)
let flush_but_n ?filter n =
  let all_events = get_all_events () in
  let rec loop i list =
    if i <= 0 then ()
    else match list with
      | [] -> ()
      | e::rest -> match filter with
        | Some f -> if f e then (push_event e; loop (i-1) rest)
          else loop i rest
        | None -> (push_event e; loop (i-1) rest)
  in
  printd debug_event "Number of events:%u" (List.length all_events);
  loop n all_events

let flush_all () =
ignore (get_all_events ())

let flush_n n =
  let e = E.create () in
  let rec loop i =
    if i > 0 && Sdl.poll_event (Some e) then loop (i-1)
  in loop n


(* some aliases *)
let text_input = E.text_input
let key_down = E.key_down
let key_up = E.key_up

(* we save here the id of the room corresponding to the event *)
let room_id = E.user_code
let widget_id = E.user_code

(* TODO: it would be more efficient to store directly the room, rather that
   storing the id, and then painfully search the room corresponding to id...
   (not to mention that this can cause serious problems when the event lives
   longer than the room...) But this implies creating a new event type, not
   using the Sdl.Event directly. *)

(* Some remarks about the mouse_enter/leave events, in relation to
   animation. (To test this, use example 28 where there is a Select list
   combined with a scrolling.) In the functions push_mouse_enter/leave, we
   create a new event each time because it IS possible that several mouse_enter
   events get in the queue, eg. in the following scenario:

   - trigger mouse_enter
   - trigger another event (eg. mouse_motion)
   - new main loop iteration
   - the mouse_motion event is treated
   - the mouse leaves the widget and enters another one: we trigger a new mouse_enter
   (it is easy to do this in case of animation, because then there is a "long"
   FPS wait in the loop... change this?)

   - end of the loop. Now we have two mouse_enter events in the queue.

   What makes this possible is

   1. only one event is treated per iteration (2022: not true anymore)
   2. there might be quite a long delay in the loop in case of animation

   This behaviour has a drawback: during an animation, mouse_enter/leave events
   may lag behing real time.

   In fact, the design of 2 is questionable: one could design the main loop
   another way: the animation does not use a FPS delay, but instead would react
   to a repetitive event that would be triggered at 60FPS. Or move the code of
   Timer.fps inside the main loop without blocking.
*)

(* push a event and store the id in the user_code *)
let push_from_id ev_type id =
let e = create_event ev_type in
  E.(set e user_code id);
  push_event e

(* use this when the mouse enters a new widget. id is the id of the room
   containing the widget. *)
let push_mouse_enter = push_from_id mouse_enter

(* see push_mouse_enter *)
let push_mouse_leave = push_from_id mouse_leave

(* use this to request a redraw of the widget, id is the widget_id *)
(* in the current implementation, it asks the whole Window to refresh *)
let push_redraw = push_from_id redraw

(* use this when the layout claims keyboard focus *)
(* the id is of the layout, which should contain a Widget. *)
let push_keyboard_focus = push_from_id keyboard_focus

(* use this when the layout claims mouse focus *)
(* the id is of the layout, which should contain a Widget. *)
let push_mouse_focus = push_from_id mouse_focus

(* use this when a Tvar (or dynvar, not used) has changed *)
(* at this time, the significance of the id argument is not specified. (not used
   yet). It could be a widget_id, room_id, or dynavar_id... TODO create a
   specific event for each of them *)
let push_var_changed = push_from_id var_changed

(* the widget_id is stored in the update event *)
let push_update = push_from_id update

(* The layout id is stored but not used. Only one push is necessary
   currently. *)
let push_remove_layout id =
  if not (Sdl.has_event remove_layout)
  then push_from_id remove_layout id

let push_destroy_window ~window_id id =
  let e = create_event destroy_window in
  E.(set e user_code id);
  E.(set e user_window_id window_id);
  push_event e

let push_close id =
  let e = create_event E.window_event in
  E.(set e window_window_id id);
  E.(set e window_event_id window_event_close);
  push_event e

(* send the `Quit event *)
let push_quit () = push_from_id E.quit 0

let get_update_wid e =
  if E.(get e typ) <> update
  then failwith "Event should be an update event"
  else E.(get e user_code)

(* use this to inform that there is a new Sync.action to execute *)
(* the event doesn't contain any special data, so we always use the same *)
let push_action =
  let action_event = create_event sync_action in
  fun () -> push_event action_event


(** tell if the current thread should exit. This should be called within a
    widget action. We communicate via the event to decide if the thread should
    exit *)
let should_exit ev =
  (* printd debug_thread "should exit ? event type = %d" (E.(get ev typ)); *)
  E.(get ev typ) = stop

let will_exit ev =
  E.(set ev typ) stopped

(** a delay that can be stopped via the event *)
let nice_delay ev sec =
  let t = sec +. Unix.gettimeofday () in
  let rec loop () =
    if (Unix.gettimeofday () >= t) || (should_exit ev) then ()
    else (Thread.delay 0.003; loop ()) in
  loop ()

(** wait until the value is observed, or timeout is elapsed. Return true is the
    value was observed *)
let wait_value ?timeout ev var value =
  let t0 =  Unix.gettimeofday () in
  while not (Var.get var = value) &&
        (default (map_option timeout
                    (fun t -> Unix.gettimeofday () < t +. t0)) true)
        && not (should_exit ev)
  do
    Thread.delay 0.003;
  done;
  Var.get var = value

(** wait until condition returns true *)
let wait_for ?timeout ?ev cond =
  let ev = default ev !my_event in
  let t0 =  Unix.gettimeofday () in
  while not (cond ())
        && (default (map_option timeout
                       (fun t -> Unix.gettimeofday () < t +. t0)) true)
        && not (should_exit ev)
  do
    Thread.delay 0.01;
  done


(* WARNING: in the current implementation of widget.ml, all events that can be
   sent to a widget are likely to be *mutated* by a connection/thread to the
   widget. Hence it is not safe to use global event variables. We create a new
   event each time we need it, or at least each time we send it to the
   Widget. *)
(* EDIT: now we duplicate the event before sending it to a thread, so this
   should be safer, see widget.ml/add_action *)

let full_click_magic = 255

(* A full_click event is actually a mouse_button_up but whose clicks fields is
   set to full_click_magic. This way we can use the x,y fields, which would be
   difficult with a user_event. *)
let set_full_click e =
  E.(set e mouse_button_clicks) full_click_magic

let has_full_click ev =
  E.(get ev typ = mouse_button_up)
  && E.(get ev mouse_button_clicks) = full_click_magic

let startup_event () = create_event startup

let is_mouse_at_rest = ref false

(* Get mouse position in OS pixels *)
let mouse_pos () =
  snd (Sdl.get_mouse_state ())

(* check if mouse didn't move for a while *)
(* TODO use get_touch_finger *)
let check_mouse_rest =
  let pos0 = ref (0,0)
  and t = ref (Some 0.) in
  fun () ->
    match !t with
    | None -> (* we start timer *)
      t := Some (Unix.gettimeofday ());
      pos0 := mouse_pos ();
      0.
    | Some t0 ->
      let p = mouse_pos () in
      if p <> !pos0 (* we have moved *)
      then t := None;
      Unix.gettimeofday () -. t0

(* Wait for next event. Returns the SAME event structure e (modified) *)
(* Remark: (Sdl.wait_event (Some e); Some e) is supposed to to the job, but
   (quoted from DOC) as of SDL 2.0, this function does not put the application's
   process to sleep waiting for events; it polls for events in a loop
   internally. This may change in the future to improve power savings. *)
(* ME: as a result, it seems that Sdl.wait_event prevents other threads from
   executing nicely *)
let rec wait_event ?(action = nop) e =
  action ();
  if Sdl.poll_event (Some e) then e
  (* TODO send an event instead, and reset mouse *)
  else begin
    let t = check_mouse_rest () in (* TODO use Timeout instead *)
    if t > 1. && not !is_mouse_at_rest
    then (is_mouse_at_rest := true;
          push_event (create_event mouse_at_rest))
    (* TODO save mouse position in event *)
    else if t < 1. && !is_mouse_at_rest
    then is_mouse_at_rest := false; (* the mouse has moved *)
    Thread.delay 0.01;
    wait_event ~action e
  end

let mm_pressed ev =
  Int32.logand E.(get ev mouse_motion_state) (Sdl.Button.lmask) <> 0l

(* Maybe all the *_window_id fields in fact are the same int? *)
let window_id ev =
  match event_kind ev with
  | `Key_down
  | `Key_up -> E.(get ev keyboard_window_id)
  | `Mouse_button_down
  | `Mouse_button_up -> E.(get ev mouse_button_window_id)
  | `Mouse_motion -> E.(get ev mouse_motion_window_id)
  | `Mouse_wheel -> E.(get ev mouse_wheel_window_id)
  | `Text_editing -> E.(get ev text_editing_window_id)
  | `Text_input -> E.(get ev text_input_window_id)
  | `User_event -> E.(get ev user_window_id)
  | `Bogue_destroy_window -> E.(get ev user_window_id)
  | `Window_event -> E.(get ev window_window_id)
  | _ -> (* TODO mouse_enter/leave *)
    printd debug_event "Warning! this event has no window id; fallback on mouse_focus";
    match Sdl.get_mouse_focus () with
    | Some w -> Sdl.get_window_id w
    | None -> printd debug_event "Hmm, no mouse_focus either, trying keyboard_focus";
      match Sdl.get_keyboard_focus () with
      | Some w -> Sdl.get_window_id w
      | None -> printd debug_event "Ah. no keyboard_focus. trying any window";
        let rec loop id =
          if id >=1024 then (printd debug_event "No window found ! giving 0 and crossing fingers..."; 0)
          else match Sdl.get_window_from_id id with
            | Ok _ -> id
            | Error _ -> loop (id+1)
        in loop 0

(* treatment of clicks *)
(***********************)

(* We just copy the Sdl event structure *)
type bc_static =
  { window_id : int;
    button_which : Tsdl.Sdl.uint32;
    (* the mouse instance id, or SDL_TOUCH_MOUSEID; see Remarks for details *)
    button_button  : Tsdl.Sdl.uint8
  }

type bc_dynamic =
    { mutable timestamp : int; (* Warning!  not Tsdl.Sdl.uint32 *)
      mutable button_state : Tsdl.Sdl.button_state;
      mutable button_x : int;
      mutable button_y : int;
    }

type button_click =
    { mutable static : bc_static;
      dynamic : bc_dynamic }

let empty_click () =
  let static = {
    window_id = 0;
    button_which = 0l;
    button_button = 0 } in
  let dynamic = {
    timestamp = 0;
    button_state = Sdl.released;
    button_x = 0;
    button_y = 0 } in
  { static; dynamic }

let button_down_event = empty_click ()
let single_click_delay = 300 (* between button_down and button_up *)
let double_click_delay = 400 (* between first button_up and second button_up *)
let single_click = ref None
let double_click = ref None
let too_fast = ref false

let copy_from_event ev bc =
  let open Sdl.Event in
  let static = {
    window_id = get ev mouse_button_window_id;
    button_which = get ev mouse_button_which;
    button_button = get ev mouse_button_button } in
  bc.static <- static;
  bc.dynamic.timestamp <- Int32.to_int (get ev timestamp);
  bc.dynamic.button_state <- get ev mouse_button_state;
  bc.dynamic.button_x <- get ev mouse_button_x;
  bc.dynamic.button_y <- get ev mouse_button_y

(* Should be called on every button_down. Remark: on my machine, any
   SDL_FINGERDOWN is preceeded by SDL_MOUSEMOTION + SDL_MOUSEBUTTONDOWN (in this
   order). *)
let button_down ev =
  flush E.mouse_button_down;
  flush E.finger_down;
  printd debug_event "Mouse button down...";
  copy_from_event ev button_down_event

(* Should be called on every button_up.  Remark: on my machine, any SDL_FINGERUP
   is preceeded by SDL_MOUSEBUTTONUP *)
let button_up ev =
  flush E.mouse_button_up;
  flush E.finger_up;
  let b_up = empty_click () in
  copy_from_event ev b_up;
  if b_up.static = button_down_event.static then
    let t = b_up.dynamic.timestamp in
    let t0 = button_down_event.dynamic.timestamp in
    too_fast := t - t0 <= 2;
    if !too_fast
    then (printd debug_event "Click was too fast. We disregard it";
          single_click := None;
          double_click := None)
    else
      (match !single_click with
       | None -> if t - t0 < single_click_delay
         then (printd debug_event "Single click %d ms" (t - t0);
               single_click := Some t;
               double_click := None)
         else (printd debug_event "No click: too late";
               single_click := None;
               double_click := None)
       | Some st -> if t - st < double_click_delay
         then (printd debug_event "Double click";
               double_click := Some t;
               single_click := None)
         else if t - t0 < single_click_delay
         then (printd debug_event "Still Single click %d ms" (t - t0);
               single_click := Some t;
               double_click := None)
         else (printd debug_event "No click: too late";
               single_click := None;
               double_click := None))
  else printd debug_event "No click: not the same button"

let was_double_click () = !double_click <> None

let was_single_click () = (!single_click <> None) && (!double_click = None)

(* text input *)

(* test if the shift mod (and only it) is pressed *)
let shift_pressed () =
  let m = Sdl.get_mod_state () in
  m = Sdl.Kmod.lshift
  || m = Sdl.Kmod.rshift

(* only ctrl *)
let ctrl_pressed () =
  let m = Sdl.get_mod_state () in
  m = Sdl.Kmod.ctrl
  || m = Sdl.Kmod.lctrl
  || m = Sdl.Kmod.rctrl

let ctrl_shift_pressed () =
  let m = Sdl.get_mod_state () in
  m = Sdl.Kmod.lctrl lor Sdl.Kmod.lshift
  || m = Sdl.Kmod.lctrl lor Sdl.Kmod.rshift
  || m = Sdl.Kmod.rctrl lor Sdl.Kmod.rshift
  || m = Sdl.Kmod.rctrl lor Sdl.Kmod.lshift

let mouse_left_button_pressed () =
  let m, _ = Sdl.get_mouse_state () in
  Int32.logand m Sdl.Button.lmask = Sdl.Button.lmask
