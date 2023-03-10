(** BOGUE *)
(** A GUI Library for Ocaml, using SDL2 *)

(** Vu Ngoc San, December 2013 -- now *)

(* doc on threads:
https://ocaml.github.io/ocamlunix/threads.html
*)

open B_utils
open Tsdl
module E = Sdl.Event
module Layout = B_layout
module Widget = B_widget
module Shortcut = B_shortcut
module Avar = B_avar
module Time = B_time
module Timeout = B_timeout
module Trigger =  B_trigger
module Sync = B_sync
module Draw = B_draw
module Mouse = B_mouse
module Update = B_update
module Print = B_print
module Window = B_window

exception Exit

type board = {
  mutable windows: Window.t list;
  (* : one layout per window. This is (mostly) redundant with the next field
     'windows_house' *)
  windows_house: Layout.t;
  (* : a special Layout containing the layouts of the board defined above in the
     layouts field. Rarely used, in fact just the list of windows is enough. But
     sometimes, it's convenient to use operations directly on the
     windows_house. Warning, the layouts should NOT indicate the windows_house
     in their House field. *)
  mutable mouse_focus: Layout.t option;
  (* : the room containing the mouse. It must contain a Widget. *)
  mutable keyboard_focus: Layout.t option;
  (* : the room with keyboard focus. It must contain a Widget *)
  (* It is important that keyboard focus may be different from mouse focus, cf
     example 12: one wants to be able to continue typing even when the mouse
     goes out of the text entry widget. *)
  mutable button_down: Layout.t option;
  (* : the room where the button_down has been registered. Used to trigger
     full_click event *)
  mutable shortcuts: board Shortcut.t;
  (* Global keyboard shortcuts. TODO some shortcuts should be executed only on
     Key_up to avoid auto-repeat. => create 2 maps, shortcuts_down et
     shortcuts_up. Or maybe all? *)
  mutable shortcut_pressed: bool;
  mutable mouse_alive: bool;
  (* True as soon as the mouse has moved. Because SDL will report position 0,0
     when the window opens, but we dont want to activate a widget if it is
     located at 0,0...*)
  on_user_event : (Sdl.event -> unit) option
}

type shortcuts = board Shortcut.t

let exit_on_escape = (Sdl.K.escape, Sdl.Kmod.none, fun (_ : board) -> raise Exit)

let get_frame () = !Avar.frame

let get_layouts board =
  List.map Window.get_layout board.windows
(* should be the same as getting the Rooms content of the windows_house *)

(* We return the mouse_focus. Sometimes it does not belong to the active tree,
   see example 19 (tabs) *)
let get_mouse_focus board =
  board.mouse_focus

let set_windows board windows =
  board.windows <- windows;
  board.windows_house.Layout.content <-
    Layout.Rooms (List.map Window.get_layout windows)
(* TODO connections? widgets? *)

let close_window window =
  let layout = Window.get_layout window in
  printd debug_board "** Closing window #%u (Layout %s)"
    (Window.id window) (Layout.sprint_id layout);
  (* TODO: stop all animations ? *)
  if !Avar.alive_animations > 0
  then begin
    printd debug_warning "%d animation%s not stopped. We reset the counter."
      !Avar.alive_animations (if !Avar.alive_animations = 1
                              then " was" else "s were");
    Avar.alive_animations := 0
  end;
  List.iter Widget.remove_active_connections (Layout.get_widgets layout);
  if Sdl.is_text_input_active () then Sdl.stop_text_input ();
  (* DEBUG: clipboard sometimes causes problems *)
  (* if Sdl.has_clipboard_text ()  *)
  (* then begin let text = go(Sdl.get_clipboard_text ()) in *)
  (*   printd debug_warning "Clipboard has [%s]" text *)
  (* end; *)
  Layout.delete_textures layout;
  (* now we destroy the canvas (renderer and window): *)
  Draw.destroy_canvas ~bogue:window.Window.bogue (Layout.get_canvas layout);
  Layout.remove_canvas layout

(* only for debugging *)
let check_cemetery () =
  let nzombies = List.length !Layout.cemetery in
  if Layout.check_cemetery ()
  then printd debug_memory "All zombies have been killed! Congratulations!"
  else if !debug then begin
    printd debug_memory "==> Still some living deads around. Invoking GC";
    Gc.compact ();
    if Layout.check_cemetery ()
    then printd debug_memory "All zombies have been killed! Congratulations!"
    else printd debug_memory "Percentage of killed zombies = %u%% (out of %u)."
        (round (100. -. 100. *. (float (List.length !Layout.cemetery) /.
                                 (float nzombies)))) nzombies
  end

(* Call this to close everything. Don't use the layouts after this! *)
(* However in principle you can run the board again, and then the layouts are
   usable(?) *)
let exit_board board =
  if Sync.execute 1000
  then printd debug_warning "Some Sync action was queueing and hence executed \
                             before exiting board.";
  Update.clear ();
  Timeout.clear ();
  check_cemetery ();
  List.iter close_window board.windows;
  board.mouse_focus <- None;
  board.keyboard_focus <- None;
  board.button_down <- None;
  Draw.destroy_textures (); (* en principe inutile: déjà fait *)
  (* Layout.clear_wtable (); *)
  Draw.check_memory ();
  Trigger.flush_all ();
  flush_log ()

(* TODO: this is not enough cleaning. + one needs an "init" function to
   restart. *)
let quit = Draw.quit


(** redisplay the layouts to the layers *)
(* there is no clear screen here *)
(* it will display a layout only if window.refresh = true *)
(* it should NOT be used more than once per loop (because of transparency) *)
let display board =
  (* We flush the events asking for redrawing. TODO: make it window-specific. *)
  Trigger.(flush redraw);
  List.iter (fun w ->
      if not (Window.is_fresh w) then Window.render w)
            board.windows

(* or Layout.render board.windows_house *)

(** Render all layers and flip window buffers, only for windows with the
   is_fresh=false field *)
let flip ?clear board =
  List.iter (Window.flip ?clear) board.windows;
  Draw.destroy_textures ()

(** Update layout of window that was resized by user *)
let resize window =
  let layout = Window.get_layout window in
  if Window.size window <> Layout.get_physical_size layout
  then begin
    printd debug_graphics "Resize window (Layout #%u)" layout.Layout.id;
    Layout.resize_from_window ~flip:false layout;
    Window.to_refresh window
  end

(* Dynamically add a new window (given by the layout) to the board, while
   running. *)
let add_window board layout =
  Layout.move_to_new_stack layout;
  let window = Window.create layout in
  Window.make_sdl_window window;

  (* update board *)
  let windows = List.rev (window :: (List.rev board.windows)) in
  set_windows board windows;

  (* show *)
  Sdl.show_window (Layout.window layout);
  Draw.update_background (Layout.get_canvas layout);

  (* run *)
  display board;
  do_option (get_mouse_focus board) Layout.set_focus;
  flip board;
  (* We send the startup_event to all widgets *)
  List.iter (Widget.wake_up (Trigger.startup_event ()))
    (List.flatten (List.map Widget.connections (Layout.get_widgets layout)));
  Trigger.renew_my_event ();
  window

let same_window w1 w2 = Sdl.(get_window_id w1 = get_window_id w2)

(** get window (layout) by id. Not used... (layout_event can do it somehow) *)
let get_window_by_id board id =
  let rec loop = function
    | [] -> printd debug_error "There is no window with this id#%d" id;
      List.hd board.windows;
    | w::rest -> if id = Window.id w then w
      else loop rest in
  loop board.windows

(* Remove window and raise Exit if there are no more active windows. *)
let remove_window board window =
  let windows = List.filter (fun w -> not (Window.equal window w))
      board.windows in
  set_windows board windows;
  close_window window;
  (* We reset all focus for safety. TODO: one could reset only those that
     belonged to the removed window. *)
  board.mouse_focus <- None;
  board.keyboard_focus <- None;
  board.button_down <- None;
  if board.windows = [] then begin
    printd debug_board "No more windows. We quit.";
    (* The `Quit` event seems to be emitted when the user clicks on the Close
       button of the last open window, but not when we delete windows
       manually. Hence we have to raise Exit.*)
    (* TODO instead we should check if there are Sync actions or Timeouts
       waiting?*)
    raise Exit
  end else if not (List.exists Window.is_shown board.windows) then begin
    printd (debug_board + debug_user)
      "Some windows are alive, but all windows are hidden. We quit.";
    (* Is there a scenario where we would not like to quit here? *)
    raise Exit
  end

(*************)
let show board =
  List.iter (fun w ->
      Window.show_maybe w;
      Window.to_refresh w;
      Draw.update_background (Window.get_canvas w)) board.windows

(* return the widget with mouse focus *)
let mouse_focus_widget board =
  map_option (get_mouse_focus board) Layout.widget

(* return the widget with keyboard_focus *)
let keyboard_focus_widget board =
  map_option board.keyboard_focus Layout.widget

let button_down_widget board =
  map_option board.button_down Layout.widget

(* which layout (ie window) has mouse focus? *)
let layout_focus board =
  match Sdl.get_mouse_focus () with
  | None -> None (* the mouse is outside of the SDL windows *)
  | Some w -> (* we return the first corresponding window (there should be only
                 one anyway) *)
     list_check_ok (fun l -> same_window (Layout.window l) w) (get_layouts board)

(* What is the window containing this house *)
let top_house board room =
  let top = Layout.top_house room in
  list_check_ok (fun l -> Layout.(top == l)) (get_layouts board)

(* which Window.t corresponds to the event? *)
let window_of_event board ev =
  try
    let ido = match Trigger.event_kind ev with
      | `Bogue_redraw ->
         let wid = E.(get ev user_code) in
         map_option (Layout.of_wid wid) (fun r ->
             let id = Sdl.get_window_id (Layout.window r) in
             printd debug_event "Redraw event window_id=%d" id;
             id)
      | _ -> Some (Trigger.window_id ev) in
    check_option ido (fun id ->
        list_check_ok (fun w -> id = Window.id w) board.windows)
  with Not_found ->
    printd debug_error "Search window for event %s caused an error"
      (Trigger.sprint_ev ev);
    None

(* Detect layout under mouse, with top layer (= largest "depth") *)
let check_mouse_focus board =
  if board.mouse_alive
  then let (x,y) = Mouse.pos () in
    printd debug_board "Mouse pos:(%u,%u)" x y;
    check_option (layout_focus board) (Layout.top_focus x y)
  else None

(* detect layout (room or widget) under mouse; only used for testing *)
let check_mouse_hover board =
  let (x,y) = Mouse.pos () in
  check_option (layout_focus board) (Layout.hover x y)

(* [check_mouse_motion] deals with sending the mouse_enter/mouse_leave events *)
(* The optional [target] argument can be used to specify the layout that should
   be considered as the new layout "under mouse", instead of really checking
   mouse position. Used for keyboard interaction. *)
(* This also sets the cursor. Overriding cursor is always possible by reacting
   to the mouse_leave/enter events. We try to keep at most one mouse_leave event
   and at most one mouse_enter event in the queue (however see remark in
   trigger.ml). The rule is that this function is called only when
   there is no pending event, which means that no mouse_enter/leave event will
   be sent until the previous ones are dealt with.  Therefore it is NOT
   guaranteed that all widgets receive their due mouse_enter/leave events, in
   case many of them are triggered at the same time. The program should not rely
   on this.  *)
let check_mouse_motion ?target board =
  let open Layout in
  let open Trigger in
  let mf = match target with
    | Some _ -> target
    | None -> check_mouse_focus board in
  let () = match (get_mouse_focus board), mf with
    (* on compare l'ancien et le nouveau. See remarks in trigger.ml *)
    | None, None -> ()
    | Some r, None ->
      unset_focus r;
      push_mouse_leave (r.Layout.id);
      set_cursor None;
    | None, Some r ->
      set_focus r;
      push_mouse_enter (r.Layout.id);
      set_cursor (Some r);
    | Some w1, Some w2 ->
      if not (Widget.equal (widget w1) (widget w2))
      then (set_focus w2;
            unset_focus w1;
            (* we send mouse_leave to w1 *)
            push_mouse_leave (w1.Layout.id);
            (* we send mouse_enter to w2 *)
            push_mouse_enter (w2.Layout.id);
            set_cursor (Some w2)
           ) in
  board.mouse_focus <- mf
(* Rm: in case of triggered action, this is already done by the redraw/refresh
   event *)

let dragging = ref None (* the initial position of the dragged room *)
(* put this in board, or local to the drag & drop functions? *)

(* guess which widget the event should be targetted to (or None) *)
let target_widget board ev =
  let roomo =
    if E.(get ev typ) = Trigger.mouse_enter ||
       E.(get ev typ) = Trigger.mouse_leave
    then let id = E.get ev Trigger.room_id in
      Layout.of_id_opt ~not_found:(fun () ->
          printd debug_error "The room #%u has disappeared but was pointed by \
                              the mouse enter/leave event" id) id
    else match board.button_down with
      | Some r (*when !dragging*) ->
        printd debug_board "Target: select button_down (%s)"
          (Layout.sprint_id r);
        Some r
      (* when dragging, the board.button_down has priority over all *)
      (* TODO: it happens also for push buttons, scroll bars, etc... *)
      (* OR: give board.button_down priority for ALL but for menus (find
         something else, like activate what was selected in the menu...) *)
      | None ->
        if Trigger.text_event ev
        || map_option board.button_down Layout.has_keyboard_focus = Some true
           (* if the button remains down, the initial text event keeps
              listening to events *)
           (* TODO: idem for mouse_button_up? *)
        then (printd debug_board "Target: select keyboard widget";
              board.keyboard_focus)
        else (printd debug_board "Target: select mouse widget";
              (get_mouse_focus board)) in
  map_option roomo Layout.widget

(* Are all the widgets rendered up-to-date? *)
let is_fresh board =
  (* List.fold_left (fun yes b -> yes && (Layout.is_fresh b)) true
     board.layouts *)
  Layout.is_fresh board.windows_house

(** display only widgets that need to be updated *)
(* because of transparency effects, this is almost impossible to use *)
let update_old board =
  List.iter (fun w ->
      if not (Window.is_fresh w) && Draw.window_is_shown (Window.sdl_window w)
      then (Window.to_refresh w;
            Layout.update_old (Window.get_layout w))
      else printd debug_board "Window is hidden")
    board.windows
(* without the shown test, one could do directly: Layout.update
   board.windows_house *)

let has_anim board =
  (* !Avar.alive_animations > 0 || *)
  (* useful? only if we have some animated variables that are not used in the
     Layout.display *)
  (List.fold_left (fun b w ->
       let h = Layout.has_anim (Window.get_layout w) in
       if h then Window.to_refresh w;
       h || b ) false board.windows)
  (* ou bien: Layout.has_anim board.windows_house *)

(* the "drop" part of drag-and-drop. It is only called by "drag" *)
let drop board =
  match board.button_down with
  | None -> ()
  | Some room -> begin
      printd debug_board " ----> Drop";
      (*board.button_down <- None;*)
      let open Layout in
      do_option !dragging (slide_to room);
      dragging := None;
    end

(* to drag, we use the anim mechanism *)
(* TODO: drag Rooms layouts, not only Residents *)
let drag board ev room =
  let open Layout in
  match Trigger.event_kind ev with
  | `Mouse_motion when not (!dragging <> None) && Trigger.mm_pressed ev ->
    (* if room.keyboard_focus <> Some true (* TODO use a "dragable" property
       instead *) then *)
    (* save initial position: *)
    dragging := Some (getx room, gety room);
    follow_mouse room;
    board.button_down <- Some room;
    printd debug_board " ----> Drag";

    None (* drag *)
  | `Mouse_button_up when !dragging <> None -> drop board; Some ev
  (* : we pass the button_up event *)
  | `Mouse_motion when (!dragging <> None && E.(get ev mouse_motion_state) = 0l)
    (* : button is not pressed *)
    -> drop board; None
  (* we do this because the mousebuttonup event might have been deleted before
     treated... Problem: if the window initially has no focus, and you drag
     something directly, and move the cursor out of the window, and then release
     button, the mouse_button_up event is NOT registered...??  *)
  (* TODO: drag and drop to another window *)
  | _ -> Some ev

let activate board roomo =
  board.button_down <- roomo;
  (match board.keyboard_focus, roomo with
   | Some kr, Some mr when not Layout.(kr == mr) ->
     Layout.remove_keyboard_focus kr;
     Layout.ask_update kr (* TODO à déplacer en button_up *)
   | Some kr, None -> Layout.remove_keyboard_focus kr;
     Layout.ask_update kr (* TODO idem -- et regrouper *)
   | _ -> ())

(* Impose mouse focus, and trigger mouse_enter/leave events as a consequence
   (regardless of actual mouse position.) *)
let set_mouse_focus board target =
  check_mouse_motion ?target board

let set_keyboard_focus board ro =
  activate board ro;
  board.keyboard_focus <- ro;
  do_option ro (fun r ->
      Layout.set_keyboard_focus r;
      check_mouse_motion ~target:r board)
  (* = selecting something via the keyboard should also set this as mouse focus
     (to get highlight, to trigger mouse_leave, etc. but without moving the
     mouse cursor...) *)


(* react to the TAB key. We look for the next room that is shown in one of the
   windows and give it keyboard focus. Rooms with show=false OR in a detached
   layout cannot be selected for keyboard focus. *)
(* TODOO this should not permit to activate items that are hidden behind a
   popup... Maybe we could restrict TAB nagivation to a unique layer? (or layers
   above the current one?)*)
let tab board =
  let current_room = match board.keyboard_focus with
    | Some r -> r
    | None -> match (get_mouse_focus board) with
      | Some r -> r
      | None -> match layout_focus board with
        | Some l -> l
        | None -> Window.get_layout (List.hd board.windows) in
  let top = match top_house board current_room with
    | None -> printd (debug_board+debug_custom)
                "Current keyboard focus %s has no Window..." (Layout.sprint_id current_room);
              Window.get_layout (List.hd board.windows)
    | Some top -> printd debug_custom "Current window is %s" (Layout.sprint_id top);
                  top in
  printd debug_board "Current room #%u" current_room.Layout.id;
  Layout.keyboard_focus_before_tab := Some current_room;
  match Layout.next_keyboard ~top current_room with
  | None -> printd debug_board " ==> No keyboard focus found !"
  | Some r as ro -> printd debug_board "Activating next keyboard focus (room #%u)" r.Layout.id;
    set_keyboard_focus board ro

(** open/close the debugging window *)
let toggle_debug_window =
  let window = ref None in fun board ->
    match !window with
    | None ->
      print_endline "OPENING DEBUG WINDOW";
      let debug_window = B_debug_window.create () in
      let w = add_window board debug_window in
      window := Some w
    | Some w ->
      remove_window board w;
      window := None

let add_debug_shortcuts shortcuts =
  shortcuts
  |> Shortcut.add_ctrl (Sdl.K.d, fun _ ->
      debug := not !debug;
      print_endline (Printf.sprintf "Debug set to %b" !debug))
  |> Shortcut.add_ctrl_shift (Sdl.K.d, toggle_debug_window)
  |> Shortcut.add_ctrl_shift (Sdl.K.i, fun board ->
      print_endline "Mouse Focus Layout parents:";
      print_endline Print.(option layout_up board.mouse_focus))
  |> Shortcut.add_ctrl (Sdl.K.i, fun board ->
      print_endline "Hover Layout children (don't trust this):";
      print_endline Print.(option layout_down (check_mouse_hover board)))
  |> Shortcut.add_ctrl_shift (Sdl.K.s, fun board -> (* snapshot *)
      Print.dump board.windows_house)
  |> Shortcut.add_ctrl (Sdl.K.m, fun _ ->
      print_endline "Garbage collecting...";
      Gc.compact ();
      Draw.memory_info ())

let refresh_custom_windows board =
  List.iter (fun w ->
      printd debug_board "BOGUE WINDOW=%b" w.Window.bogue;
      if not w.Window.bogue then w.Window.is_fresh <- false)
    board.windows

let check_removed board =
  do_option board.mouse_focus (fun r ->
      if Layout.is_removed r || not r.Layout.show then begin
        printd debug_board "Re-setting mouse_focus because layout %s was removed"
          (Layout.sprint_id r);
        check_mouse_motion board
      end);
  do_option board.keyboard_focus (fun r ->
      if Layout.is_removed r || not r.Layout.show then begin
        printd debug_board "Unsetting keyboard_focus because layout %s was removed"
          (Layout.sprint_id r);
        board.keyboard_focus <- None
      end);
  do_option board.button_down (fun r ->
      if Layout.is_removed r || not r.Layout.show then begin
        printd debug_board "Unsetting button_down because layout %s was removed"
          (Layout.sprint_id r);
        board.button_down <- None
      end)


  (* EVENT LOOP *)

  (* First: we treat the events that should be filtered or modified. This returns
     the evo_layout that the layout (& widget) is authorized to treat
     thereafter. Returning None means that widgets will never react to such
     event. Currently all events are returned, except for the Update and
     Remove_layout events. *)
let filter_board_events board e =
  let open E in
  printd debug_event "1==> Filtering event type: %s" (Trigger.sprint_ev e);
  match Trigger.event_kind e with
  | `Finger_motion ->
    if Sdl.has_event E.finger_motion then None else Some e
  (* There is (probably?) no use in treating finger motion if there is another
     one in the queue. [This is NOT true for Mouse_motion, see below.] If really
     the system was slow to treat this, we could check if the 'count' of the
     event loop is not too high (TODO?). *)
  | `Bogue_keyboard_focus ->
    set_keyboard_focus board
      (Layout.of_id_opt (get e user_code)
         ~not_found:(fun () ->
             printd debug_error "Room #%u pointed by event %s has disappeared"
               (get e user_code) (Trigger.sprint_ev e)));
    Some e (* ou None? *)
  | `Bogue_mouse_focus ->
    printd debug_event "Require Mouse FOCUS";
    (* we filter and treat only the last event *)
    (* let e' = default (Trigger.get_last (Trigger.mouse_focus)) e in *)
    set_mouse_focus board (Layout.of_id_opt (get e user_code));
    Some e
  | `Bogue_mouse_enter ->
    printd debug_event "Mouse ENTER";
    (* by design, only one mouse_enter event can exist in the queue if there is
       no mouse_leave. *)
    Some e
  | `Bogue_mouse_leave ->
    printd debug_event "Mouse LEAVE";
    (* by design, only one mouse_leave event can exist in the queue if there is
       no mouse_enter. *)
    Some e
  | `Bogue_update ->
    printd debug_event "Update";
    Update.execute e;
    None
  | `Bogue_remove_layout ->
    printd debug_event "Layout removed";
    Trigger.(flush remove_layout); (* not necessary in principle *)
    check_removed board;
    None
  | `Render_targets_reset
  | `Render_device_reset ->
    printd (debug_graphics + debug_error) "TODO! Reset all textures";
    Sdl.log "reset event";
    None
  | _ -> Some e


(* Treat events that should be used before being sent to the layout & widget,
   but without filtering. *)
let treat_layout_events board e =
  let open E in
  printd debug_event "2==> Treating event type: %s" (Trigger.sprint_ev e);
  begin match Trigger.event_kind e with
    | `Bogue_sync_action ->
      (* This one should be executed before anything else. *)
      (* We run the actions in the Queue, and stop if the Queue is empty or
         time has exceeded 10ms *)
      printd debug_event "Sync";
      if not (Sync.execute 10)
      then Trigger.flush (Trigger.sync_action) (* probably not useful *)
    (* Here we treat key events that have priority over the widgets *)
    | `Key_up -> board.shortcut_pressed <- false;
      (* I assume auto-repeat will never trigger Key_up, but it seems that
         it's not always the case... (can happen when a new window opens) *)
    | `Key_down ->
      let pair = get e keyboard_keycode, get e keyboard_keymod in
      if not board.shortcut_pressed
      then do_option (Shortcut.find board.shortcuts pair)
          (fun a -> board.shortcut_pressed <- true; a board)
    | `Mouse_button_down
    | `Finger_down ->
      (* For finger down and moving, the following events can be emitted quasi
         simultaneously: (0x400) (0x401) (0x700) (0x400) (0x702) (0x400) (0x702)
         (0x400) (0x702) *)
      Trigger.button_down e;
      activate board (get_mouse_focus board)
    (* | `Mouse_button_up when Trigger.has_full_click e ->
     *   printd debug_event "Full click" *)
    | `Mouse_button_up
    | `Finger_up ->
      printd debug_event "Mouse button up !";
      Trigger.button_up e;
      if not !Trigger.too_fast
      && (map2_option board.button_down (check_mouse_focus board) Layout.equal
          = Some true
          || map_option board.button_down Layout.has_keyboard_focus = Some true)
      then begin
        printd debug_event "full click";
        Trigger.set_full_click e;
        (* full click means that the press and released were done on the same
           widget. It does not mean that the click was "quick". For this, check
           Trigger.single_click. *)
        (* Now we set keyboard_focus on "admissible" widgets: *)
        do_option (get_mouse_focus board) (fun x ->
            printd debug_board "Mouse focus: %d" x.Layout.id);
        do_option board.keyboard_focus (fun x ->
            printd debug_board "Keyboard focus: %d" x.Layout.id);
        do_option board.button_down (fun x ->
            printd debug_board "Button down on #%d" x.Layout.id;
            Layout.set_keyboard_focus x);
        board.keyboard_focus <- board.button_down (* OK?? *)
      end
    | `Mouse_wheel ->
      (* TODO change. mouse_wheel should be captured by the widget itself. *)
      do_option (get_mouse_focus board) (fun room ->
          do_option (Layout.find_clip_house room)
            (fun room ->
               (* now we add up the number of wheel events in the queue. With
                    a standard mouse wheel one can easily add up to 5
                    events. With a touchpad, this can add up to 10 or more *)
               let list = Trigger.filter_events (fun e ->
                   Trigger.event_kind e <> `Mouse_wheel) in
               let total = List.fold_left
                   (fun s ev -> s + get ev mouse_wheel_y)
                   E.(get e mouse_wheel_y) list in
               printd debug_event "Total mouse wheels=%d" total;
               let dy = - total * 50 in
               Layout.scroll ~duration:500 dy room;
               check_mouse_motion board;
               Trigger.push_var_changed room.Layout.id))
    | `Bogue_destroy_window ->
      printd (debug_board+debug_event) "Destroy window request";
      do_option (window_of_event board e) (remove_window board);
    | `Window_event ->
      (* https://github.com/libsdl-org/SDL/blob/main/include/SDL_video.h *)
      let wid = get e window_event_id in
      printd debug_event "Window event [%d]" wid;
      (* Warning: when resizing window by mowing the top-left corner, we trigger
         trigger 6 = resize, and also 4 = "window_event_moved"... and sometimes
         3 = exposed *)
      (* Some window events may come by pair; for instance if you middle_click
         on the maximize button, it can trigger 10 (mouse enter) and then 6
         (resize). *)
      begin
        match window_event_enum wid with
        | `Resized ->
          printd debug_event "(ignored) Resized => to (%lu,%lu)"
            (get e window_data1) (get e window_data2)
        (* https://wiki.libsdl.org/SDL_WindowEventID *)
        | `Size_changed ->
          (* For resizing, we usually have wid = (4-moved), 6-size-changed,
             5-resized, 3-exposed.*)
          printd debug_event "Size_changed => Resize to (%lu,%lu)"
            (get e window_data1) (get e window_data2);
          do_option (window_of_event board e) resize
        | `Exposed ->
          (* The exposed event is triggered by X11 when part of the
             window is lost and should be re-rendered. Sometimes several
             exposed events are triggered because they correspond to
             several regions of the window. This seems to be unreachable
             via SDL. *)
          printd debug_event "Exposed";
          (* for some reason, when the size changes quickly, Exposed is
             triggered but not Resized nor `Size_changed...*)
          do_option (window_of_event board e) (fun w ->
              let l = Window.get_layout w in
              if Window.size w <> Layout.get_physical_size l
              then Layout.resize_from_window ~flip:false l;
              (* the renderer changed, we need to recreate all
                 textures *)
              Window.to_refresh w)
        | `Close ->
          printd (debug_board+debug_event) "Asking window to close";
          do_option (window_of_event board e) (fun w ->
              if not w.Window.bogue then (Trigger.push_event e);
              (* : we relay the close event to the manually created window. *)
              let action = default w.on_close (remove_window board) in
              action w)
        | _ as enum ->
          printd debug_event "%s" (Trigger.window_event_name enum);
          do_option (window_of_event board e) (fun w ->
              Window.to_refresh w;
              check_mouse_motion board;
              (* Otherwise we don't get mouse_leave when the mouse leaves the
                   window. OK here ? *)
              (* Warning: the behaviour of SDL seems to be the following:
                   when the window has no focus and the user click on it,
                   there is NO Button_down NEITHER Button_up event, instead
                   there is a Window "Take_focus" event. We follow this
                   here. It means that the user has to click a second time to
                   activate a button.*) )
      end;
      (* TODO just display the corresponding window, not all of them *)
    | `User_event ->
      printd (debug_event+debug_board+debug_user) "User event";
      do_option board.on_user_event (fun f -> f e)
    | `Quit -> printd (debug_event+debug_board) "Quit event"; raise Exit
    | _ -> ()
  end

let filter_drag_and_drop_event board e =
  match board.button_down with
  | Some room -> if Layout.draggable room
    then drag board e room (*Layout.drag_n_drop e room*)
    else Some e
  | None -> printd debug_board "No board.button_down"; Some e
(* This happens for instance when you drag outside the SDL window and then
   release mouse button. Still, the event will be treated by the original widget
   below (in case of a keyboard_focus). *)

(* The board can use the event that was filtered by the widget. Empty for the
   moment. *)
let treat_remaining_events _board e =
  match Trigger.event_kind e with
  (* For instance | `Key_down when E.(get e keyboard_keycode) = Sdl.K.tab -> tab board e *)
  | _ -> ()

(* What to do with the original event before flip. This should not modify
   widgets, only update board status. *)
let final_events board anim e =
  match Trigger.event_kind e with
  (* | `Mouse_motion when not anim && has_no_event ->
   *   if not board.mouse_alive then board.mouse_alive <- true;
   *   check_mouse_motion board; *)
  (* do_option (window_of_event board e) do_display; *)
  (* List.iter Window.to_refresh board.windows; *)
  (* TODO? display? *)
  (* = ou seulement ce qui a été (dé)sélectionné ? *)
  | `Mouse_motion ->
    (* Even if there are several mouse_motion in the queue, it is important to
       treat the ones that are followed by button_down (otherwise the focus is
       not updated). Hence we don't flush them. In case of performance hit, one
       could check Sdl.has_event E.mouse_motion && not (Sdl.has_event
       E.mouse_button_down). *)
    printd debug_disable "MOTION anim=%b"
      anim;
    if not board.mouse_alive then board.mouse_alive <- true;
    if (* has_no_event && *) not (Trigger.mm_pressed e)
    then check_mouse_motion board
  (* In most situations, when the button is pressed, we don't want to lose the
     initial focus, and we don't want to activate anything else. There is one
     (common) exception: when clicking a menu entry, we would like to navigate
     menus while mouse button is down. How to handle this particular case? If we
     remove the mm_pressed test it works nicely for menus, but it not usual for
     other things (like scroll bar). I don't see any other solution than adding
     a new flag 'allow_focus_change_when_mm_pressed' somewhere... TODO? *)
  | `Mouse_button_up
  | `Finger_up ->
    board.button_down <- None;
    check_mouse_motion board
  | `Window_event -> () (* done above *)
  | `Bogue_redraw ->
    (* Sometimes there are too many redraw events in the queue, this would cause
       a noticeable delay if only one can be treated by iteration. Cf example
       28/bis. Hence we leave at most one. Flushing all here is NOT recommended,
       it can prevent the correct detection of new animations (ex: adding
       sliding popups). *)
    (* do_option Trigger.(get_last redraw) (fun ev -> Trigger.push_event ev); *)
    if not anim then begin
      printd debug_event "Redraw";
      do_option (window_of_event board e) Window.to_refresh
    end
  (* board.mouse_focus <- (check_mouse_focus board); *)  (* ? *)
  (* could use window_of_event instead *)
  (* do_option board.mouse_focus Layout.set_focus *) (* ? *)
  (* | `Unknown when not !anim && E.(get e typ) = Trigger.refresh -> *)
  (*   printd debug_event "Refresh"; (\* not used anymore *\) *)
  (* update board; *)
  (* board.mouse_focus <- (check_mouse_focus board); *) (* ? *)
  (* could use window_of_event instead *)
  (* do_option board.mouse_focus Layout.set_focus *) (* ? *)
  | `Bogue_mouse_at_rest ->
    printd debug_event "Mouse AT REST"; (* TODO *)
  | _ -> ()

let max_events = 128

let event_loop anim new_anim board =
  (* let redraw_next = ref false in *)

  let rec loop e count =
    printd debug_event "Event loop %i" count;

    (* Note, do not flush var_changed, it is used by radiolist.ml, cf. example30
    *)
    let evo_layout = filter_board_events board e in
    do_option evo_layout (treat_layout_events board);

    (* Second, the event is treated by the global layout for drag-and-drop,
       and we return the evo_widget for the widgets *)
    let evo_widget = check_option evo_layout (filter_drag_and_drop_event board)
    in
    (* Now, the widget has the event (TODO sortir de la boucle ? ou dans une autre?) *)
    (* note that the widget will usually emit a redraw event *)
    do_option evo_widget (fun ev ->
        do_option (target_widget board ev) (Widget.wake_up_all ev));
    (* Now, the board again. *)
    do_option evo_widget (treat_remaining_events board);

    (* Finally we do final updates before flip with the original, unfiltered
       event "e" *)
    (* TODO we should not use 'e' if there is an anim, because it will be an
       old event *)
    final_events board new_anim e;
    continue e (count + 1)

  and continue e count =
    if count > max_events
    then begin
      printd (debug_event + debug_error + debug_user)
        "Too many events accumulate. Maybe your system is too slow.";
      Sdl.flush_events E.first_event E.last_event
    end
    else if Sdl.poll_event (Some e)
    then loop e count
    else if anim || new_anim
    then printd debug_event
        "Animation: we end the event loop, %i events processed." count
        (* : we exit [event_loop] *)
    else if count = 0
    then loop (Trigger.wait_event ~action:Timeout.run e) count
    (* While we wait for events, we execute the Timeout Queue. *)
    else printd debug_event "No more events, %i events processed." count
    (* exit the loop (we want to wait only once) *)
  in

  let e = !Trigger.my_event in
  continue e 0


(* [one_step] is what is executed during the main loop *)
let one_step ?before_display anim (start_fps, fps) ?clear board =
  Timeout.run ();
  let new_anim = has_anim board in
  if new_anim && not anim then start_fps ();
  event_loop anim new_anim board;
  do_option before_display run;

  let anim = new_anim in
  (* now some specifics in case of animation *)
  if anim then begin
    (* if board.mouse_focus <> None then board.mouse_focus <- None; *)
    (* = we desactivate focus during animation?? *)
    printd debug_graphics " * Animation running...";

    (* Warning: Finally we chose this behaviour: mouse_enter/leave events are
       sent only when the mouse really moves. If a widget hits the idle mouse
       cursor because of an animation, these events are NOT sent. For instance
       this can happen when scrolling a Select list. It is NOT ideal (what is?),
       for instance when inserting new elements on the fly, like popups, one has
       to tell the mouse to update its focus, even if it didn't move (otherwise
       it will still connect to the widget that's below the popup...). For this
       we have to push mouse_focus event.

       TODO: ça affecte drad-and-drop, cf exemple5 à corriger. ça affecte aussi
       example24 (cliquer sur un objet animé)

       Read below for various trys... *)

    (* Comment this for Menu2 keyboard navigation... *)

    (* : even if the mouse doesn't actually move, some animated widget can
       collide the mouse and become (un)selected. *)
    (* display board; *)
    List.iter Window.to_refresh board.windows;
    (* : we could select only the one which really has an animation *)
    (*fps 60*)
  end;
  (* else *)

  if anim then fps () else Thread.delay 0.005;
  (* even when there is no anim, we need to to be nice to other treads, in
     particular when an event is triggered very rapidly (mouse_motion) and
     captured by a connection, without anim. Should we put also here a FPS?? *)

  let t = Time.now () in
  flip ?clear board; (* This is where all rendering takes place. *)
  printd debug_graphics "==> Rendering took %u ms" (Time.now () - t);
  Avar.new_frame (); (* This is used for updating animated variables. *)
  printd debug_graphics "---------- end of loop -----------";
  anim

(* Create an SDL window for each top layout. *)
(* One can use predefined windows, they will be used by the layouts in the
   order they appear in the list. If there are fewer windows than layouts, new
   windows are created. If there are more, the excess is disregarded. *)
let make_sdl_windows ?windows board =
  match windows with
  | None -> List.iter Window.make_sdl_window board.windows
  | Some list ->
     let rec loop sdl ws =
       match sdl, ws with
       | _, [] -> ()
       | [], rest -> List.iter Window.make_sdl_window rest
       | s::srest, w::wrest -> begin
           Window.use_sdl_window s w;
           loop srest wrest
         end in
     loop list board.windows

(* Create the board. *)
let create ?shortcuts ?(connections = []) ?on_user_event windows =
  (* let canvas = match layouts with *)
  (*   | [] -> failwith "At least one layout is needed to make the board" *)
  (*   | l::_ -> Layout.get_canvas l in *)
  (* if adjust then List.iter (Layout.adjust_window ~display:false) layouts; *)
  (* TODO add "adjust" property in layout. NO this should be enforced *)
  (* TODO one could use the position of the top layout to position the window *)
  let layouts = List.map Window.get_layout windows in
  let windows_house = Layout.create_win_house layouts in
  let widgets = (* List.flatten (List.map Layout.get_widgets layouts) *)
    Layout.get_widgets windows_house in
  do_option (repeated Widget.equal widgets) (fun w ->
      print_endline (Print.widget w);
      failwith (Printf.sprintf "Widget is repeated: #%u" (Widget.id w)));
  List.iter (fun c -> Widget.(add_connection c.source c)) connections;
  (* = ou bien dans "run" ? (ça modifie les widgets) *)
  let shortcuts = default_lazy shortcuts (lazy (Shortcut.create [])) in
  let shortcuts =
    (if !debug then add_debug_shortcuts shortcuts else shortcuts)
    |> Shortcut.add (Sdl.K.tab, tab)
    |> Shortcut.add_ctrl
      (Sdl.K.l,
       fun board ->
         print_endline "User Redraw";
         display board;
         show board) in
  { windows;
    windows_house;
    mouse_focus = None;
    keyboard_focus = None;
    button_down = None;
    shortcuts; shortcut_pressed = false;
    mouse_alive = false;
    on_user_event }

let of_windows = create

(* Create a board from layouts. Each layout in the list will be displayed in a
   different window.*)
let of_layouts ?shortcuts ?connections ?on_user_event layouts =
  create ?shortcuts ?connections ?on_user_event (List.map Window.create layouts)

let of_layout ?shortcuts ?connections ?on_user_event layout =
  of_layouts ?shortcuts ?connections ?on_user_event [layout]

(* for backward compatibility. Use [create], [of_windows] or [of_layouts]
   instead. *)
let make ?shortcuts connections layouts =
  printd (debug_user + debug_warning)
    "Bogue.make is deprecated. Use Bogue.create, Bogue.of_layout, \
     Bogue.of_layouts, or Bogue.of_windows instead.";
  of_layouts ?shortcuts ~connections layouts

(** The main function that loops indefinitely *)
(* one can insert code to be executed at two different places: "before_display"
   means after Sync was executed and before Layout.display (except for manual
   CTRL-L which would occur before it. "after_display" means just after all
   textures have been calculated and rendered. Of course these two will not be
   executed at all if there is no event to trigger display. *)
let run ?before_display ?after_display board =
  printd debug_board "==> Running board!";
  Trigger.flush_all ();
  if not (Sync.is_empty ()) then Trigger.push_action ();
  if not (Update.is_empty ()) then Update.push_all ();
  Trigger.main_tread_id := Thread.(id (self ()));
  let fps = Time.adaptive_fps 60 in
  make_sdl_windows board;
  show board;
  Thread.delay 0.01; (* we need some delay for the initial Mouse position to be detected *)
  Sdl.pump_events ();
  Sdl.stop_text_input ();
  (* List.iter (Widget.set_canvas canvas) board.widgets; *)
  (* Warning: layouts may have different canvas because of different layers *)

  (* We have to display the board in order to detect mouse focus
     (otherwise the 'show' field of layouts are not set). *)
  display board;
  board.mouse_focus <- check_mouse_focus board;
  printd debug_board "Has focus: %s"
    (if board.mouse_focus = None then "NO" else "YES");
  do_option (get_mouse_focus board) (fun l ->
      Layout.set_focus l;
      (* we send mouse_enter event to the widget where the mouse is
         positionned at startup *)
      (* Widget.wake_up_all Trigger.(create_event mouse_enter) (Layout.widget l);
       * display board *)
      Trigger.push_mouse_enter (l.Layout.id));

  if not (Sync.execute 50) (* The first Sync is given more time *)
  then Trigger.flush (Trigger.sync_action); (* probably not useful *)

  flip ~clear:true board;

  (* We send the startup_event to all widgets *)
  (* List.iter (fun l -> List.iter (Widget.wake_up ev) *)
  (*               (List.flatten (List.map Widget.connections (Layout.get_widgets l)))) *)
  (*   board.layouts; *)
  List.iter (Widget.wake_up (Trigger.startup_event ())) (* TODOOOOO this event can be modified by a thread??!!! *)
    (List.flatten (List.map Widget.connections (Layout.get_widgets board.windows_house)));
  Trigger.renew_my_event ();
  let rec loop anim =
    let anim' = one_step ?before_display ~clear:true anim fps board in
    do_option after_display (fun f -> f ()); (* TODO? *)
    loop anim' in
  try
    loop false
  with
  | Exit -> exit_board board
  | e ->
    let sdl_error = Sdl.get_error () in
    if sdl_error <> "" then print_endline ("SDL ERROR: " ^ sdl_error);
    print_endline (Print.layout_down board.windows_house);
    raise e


(*************)
(* Shortcuts *)
(*************)

type shortcut_action = board Shortcut.action

let shortcuts_empty () : shortcuts =
  Shortcut.empty ()

let shortcuts_add map ?(keymod = Sdl.Kmod.none) keycode action : shortcuts =
  Shortcut.add_map map (keycode, keymod, action)

let shortcuts_add_ctrl map keycode action : shortcuts =
  Shortcut.add_ctrl (keycode, action) map

let shortcuts_add_ctrl_shift map keycode action : shortcuts =
  Shortcut.add_ctrl_shift (keycode, action) map

let shortcuts_of_list list : shortcuts =
  Shortcut.create list
