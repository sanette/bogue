(* each widget contains its personal data, and the list of connections from
   it *)

open Tsdl
open B_utils
module Avar = B_avar
module Utf8 = B_utf8
module Var = B_var
module Tvar = B_tvar
module Timeout = B_timeout
module Trigger =  B_trigger
module Draw = B_draw
module Empty = B_empty
module Image = B_image
module Box = B_box
module Label = B_label
module Button = B_button
module Slider = B_slider
module Check = B_check
module Text_display = B_text_display
module Text_input = B_text_input
  
type kind =
  | Empty of Empty.t
  | Box of Box.t
  | Button of Button.t
  | Check of Check.t
  | TextDisplay of Text_display.t
  | Label of Label.t
  | Image of Image.t
  | Slider of Slider.t
  | TextInput of Text_input.t

(** what to do when the same action (= same connection id) is already running ? *)
type action_priority = 
  | Forget (** discard the new action *)
  | Join (** execute the new after the first one has completed *)
  | Replace (** kill the first action (if possible) and execute the second one *)
  | Main (** run in the main program. So this is blocking for all subsequent actions *)

type active = {
  thread : Thread.t; (** this is the thread launched by the connection with given id *)
  event : Sdl.event; (* this is the event passed to the "action".  It is used
                        also for communication *)
  connect_id : int
};;

type action = t -> t -> Sdl.event -> unit

and connection = {
  source : t;
  target : t;
  action : action;
  priority : action_priority;
  triggers : Trigger.t list;
  id : int;
}

and t = {
  kind : kind;
  (*  receiver : action Event.channel; *) (* TODO: pas nécessaire ? *)
  actives : (active list) Var.t;
  (** all active threads/connections for this widget. Most recent come first in
      the list *)
  mutable connections : connection list;
  (** all possible connections from this widget. In the order to be
      executed. Particular case: the local actions are connection from
      and to the same widget. *)
  (* mutable à cause de définition cyclique *)
  wid : int;
  mutable fresh : bool Var.t; (* is the display up-to-date ? *)
  (* not really used anymore. TODO: check if this flag is still used *)
  mutable room_id : int option; (* will be filled by the room id when inserted in that room *)
  mutable cursor : Sdl.cursor option (* use this to override the default mouse cursor *)
};;

let draw_boxes = ref false;;
(* for debugging: draws a red rect rectangle around each widget layout, (fill
   when it has mouse focus (might need a redraw: CTRL-l) and a blue rect around
   container layouts *)

let id w = w.wid;;

let get_room_id w = match w.room_id with
  | None -> failwith "The widget does not belong to a room yet"
  | Some id -> id;;

let equal w1 w2 =
  w1.wid = w2.wid;;
let (==) = equal;;

type widget = t;;

module Hash = struct
  type t = widget
  let equal = equal
  let hash = id
end

module WHash = Weak.Make(Hash);;
let widgets_wtable = WHash.create 100;;

(* When to use this ??? *)
(* in particular, when this function is called, the widget w in principle has
   already been removed from widgets_wtable *)
let free w =
  printd debug_memory "Freeing widget #%u" w.wid;
  match w.kind with
  | Empty e -> Empty.free e
  | Box b -> Box.free b
  | Check b -> Check.free b
  | Button b -> Button.free b
  | TextDisplay t -> Text_display.free t
  | Image img -> Image.free img
  | Label l -> Label.free l
  | Slider s -> Slider.free s
  | TextInput ti -> Text_input.free ti

let is_fresh w = Var.get w.fresh;;

(* let canvas w = match w.canvas with *)
(*   | Some c -> c *)
(*   | None -> failwith "Canvas not defined";; *)

(* let renderer w = *)
(*   (canvas w).Draw.renderer;; *)

(* let set_canvas canvas w = *)
(*   w.canvas <- Some canvas;; *)

let fresh_id = fresh_int ();;
let fresh_wid = fresh_int ();;

let create_empty kind =
  let wid = fresh_wid () in
  let w =
    { kind;
      wid;
      actives = Var.create [];
      fresh = Var.create false;
      connections = [];
      room_id = None;
      cursor = None;
    } in
  WHash.add widgets_wtable w;
  (*Gc.finalise free w;*) (* TODO: NOT A GOOD IDEA as this will ask to destroy
                             textures that maybe were already destroyed when the
                             window was closed *)
  (* However if we don't do this there is a risk that some textures are never
     freed (as long as the renderer is not destroyed) *)
  w;;

let dummy_widget = create_empty (Empty (Empty.create (0,0)));;
    
(*let of_id wid = Hashtbl.find widgets_table wid;;*)
let of_id wid : t =
  try WHash.find widgets_wtable {dummy_widget with wid} with
  | Not_found -> (printd debug_error "Cannot find widget with wid=%d" wid;
                  raise Not_found);;

(* unload all textures but the widget remains usable. (Rendering will recreate
   all textures) *)
let unload_texture w =
  printd debug_memory "Unloading texture for widget #%u" w.wid;
  match w.kind with
  | Empty b -> Empty.unload b
  | Box b -> Box.unload b
  | Check b -> Check.unload b
  | Button b -> Button.unload b
  | TextDisplay t -> Text_display.unload t
  | Image img -> Image.unload img
  | Label l -> Label.unload l
  | Slider s -> Slider.unload s
  | TextInput ti -> Text_input.unload ti


let default_size w =
  match w.kind with
  | Empty e -> Empty.size e
  | Check b -> Check.size b
  | Box b -> Box.size b
  | TextDisplay td -> Text_display.size td
  | Label l -> let x,y = Label.size l in (x+2,y+2)
  | Image img -> Image.size img
  | Button b -> Button.size b
  | Slider s -> Slider.default_size s
  | TextInput ti -> Text_input.size ti
                      
let get_cursor w =
  default w.cursor
    (match w.kind with
     | Empty _
     | Box _
     | Label _
     | TextDisplay _
     | Image _ -> go (Draw.create_system_cursor Sdl.System_cursor.arrow)
     | Button _ 
     | Check _
     | Slider _ -> go (Draw.create_system_cursor Sdl.System_cursor.hand)
     | TextInput _ -> go (Draw.create_system_cursor Sdl.System_cursor.ibeam)
    );;

let set_cursor w cursor =
  w.cursor <- cursor

let display canvas layer w geom =
  Var.set w.fresh true;
  let geom = Draw.scale_geom geom in
  match w.kind with
  | Empty e -> printd debug_board "empty box";
    Empty.display canvas layer e geom
  | Box b -> printd debug_board "draw box";
    Box.display canvas layer b geom
  | Check b -> printd debug_board "check button: %b" (Check.state b);
    Check.display canvas layer b geom
  | Button b -> printd debug_board "button [%s]" (Button.text b);
    Button.display canvas layer b geom
  | TextDisplay td -> printd debug_board "text display: %s" (Text_display.text td);
    Text_display.display canvas layer td geom
  | Image img -> printd debug_board "image: %s" (Var.get img.Image.file);
    Image.display canvas layer img geom
  | Label l -> printd debug_board "label: %s" (Label.text l);
    Label.display canvas layer l geom
  | Slider s -> printd debug_board "slider: %d" (Slider.value s);
    Slider.display canvas layer s geom
  | TextInput ti -> printd debug_board "Input: %s" (Text_input.text ti);
    Text_input.display canvas layer ti geom

(** ask for refresh *)
(* Warning: this is frequently called by other threads *)
(* Warning: this *resets to 0* the user_window_id *)
(* anyway, it is not clear if the user_window_id field for created event types
   is really supported by (T)SDL *)
let update w =
  printd debug_board "Please refresh";
  Var.set w.fresh false;
  (* if !draw_boxes then Trigger.(push_event refresh_event) *)
  (* else *) 
  Trigger.push_redraw w.wid;; (*TODO... use wid et/ou window_id...*)
(* refresh is not used anymore. We redraw everyhting at each frame ... *)
(* before, it was not very subtle either: if !draw_boxes is false, we ask for
   clearing the background before painting. Maybe some widgets can update
   without clearing the whole background. But those with some transparency
   probably need it. This should not be necessary in case we draw a solid
   background -- for instance if draw_boxes = true *)


(** create new connection *)
(* if ~join:c, on donne le même id que la connexion c, ce qui permet
   d'effectuer l'action conjointement avec celle de c (avec en général
   la priorité Join pour effectuer à la suite de c). Attention dans ce
   cas, ne pas déclancher plein de ces connexions à la suite... elles
   s'attendent ! *)
let connect source target action ?(priority=Forget) ?(update_target=true) ?join triggers =
  if update_target && (List.mem Sdl.Event.user_event triggers)
  then printd debug_warning "one should not 'connect' with 'update_target'=true if the trigger list contains 'user_event'. It may cause an infinite display loop";
  let action = if update_target
    then fun w1 w2 ev -> (action w1 w2 ev; update w2) (* TODO ajouter Trigger.will_exit ev ?? *)
    else action in
  let action = if !debug
    then fun w1 w2 ev ->
      (printd debug_thread "Executing action";
       let t = Unix.gettimeofday () in
       action w1 w2 ev;
       printd debug_thread "End of action with time=%f" (Unix.gettimeofday () -. t))
    else action in
  let id = match join with
    | None -> fresh_id ()
    | Some c -> c.id in
  { source;
    target;
    action;
    priority;
    triggers;
    id };;

let connect_after source target action triggers =
  match List.rev source.connections with
    | [] -> connect source target action ~priority:Join triggers
    | c::_ -> connect source target action ~priority:Join ~join:c triggers;;

let connect_main = connect ~priority:Main;;

let connections t =
  t.connections;;

(* TODO: vérifier qu'on n'ajoute pas deux fois la même *)
(* TODO à faire automatiquement après "connect" ? *)
let add_connection w c =
  w.connections <- List.rev (c :: List.rev w.connections);;

(* TODO: remove connection *)
let get_box w =
  match w.kind with
    | Box b -> b
    | _ -> failwith "Expecting a box";;

let get_check w =
  match w.kind with
    | Check b -> b
    | _ -> failwith "Expecting a check box";;

let get_label w =
 match w.kind with
    | Label l -> l
    | _ -> failwith "Expecting a label";;

let get_button w =
  match w.kind with
    | Button b -> b
    | _ -> failwith "Expecting a button";;

let get_slider w =
 match w.kind with
    | Slider s -> s
    | _ -> failwith "Expecting a slider";;

let get_text_display w =
 match w.kind with
    | TextDisplay td -> td
    | _ -> failwith "Expecting a text display";;


let get_text_input w =
 match w.kind with
    | TextInput ti -> ti
    | _ -> failwith "Expecting a text input";;

(** creation of simple widgets *)
let check_box ?state ?style () =
  let b = create_empty  (Check (Check.create ?state ?style ())) in
  let action = fun w _ _ -> Check.action (get_check w) in
  let c = connect_main b b action Trigger.buttons_down in
  add_connection b c;
  b;;


(*let get_check_state b =
  Check.state (get_check b);;
*)

let set_check_state b s =
  Check.set (get_check b) s;;

let empty ~w ~h () =
  create_empty (Empty (Empty.create (w,h)));;

let text_display ?w ?h text =
  create_empty (TextDisplay (Text_display.create_from_string ?w ?h text));;

let rich_text ?size ?w ?h paragraphs =
  create_empty (TextDisplay (Text_display.create ?size ?w ?h paragraphs));;

let lines_display ?w ?h lines =
  create_empty (TextDisplay (Text_display.create_from_lines ?w ?h lines));;

let verbatim text =
  create_empty (TextDisplay (Text_display.create_verbatim text));;

let box ?w ?h ?background ?border ?shadow () =
  create_empty (Box (Box.create ?width:w ?height:h ?background ?border ?shadow ()));;

let label ?size ?fg ?font text =
  create_empty (Label (Label.create ?size ?fg ?font text));;

(* alias for fontawesome icon labels *)
let icon ?size ?fg name =
  create_empty (Label (Label.icon ?size ?fg name));;

let image ?w ?h ?bg ?noscale file =
  create_empty (Image (Image.create ?width:w ?height:h ?bg ?noscale file));;

let image_from_svg ?w ?h ?bg file =
  let svg = Draw.convert_svg ?w ?h file in
  let w,h = Draw.unscale_size (Draw.image_size svg) in
  image ~w ~h ?bg svg;;
  
let button ?(kind = Button.Trigger) ?label ?label_on ?label_off
    ?fg ?bg_on ?bg_off ?bg_over ?state 
    ?border_radius ?border_color text =
  let b = create_empty
      (Button (Button.create ?label ?label_on ?label_off ?fg
                 ?bg_on ?bg_off ?bg_over
                 ?border_radius ?border_color ?state text)) in
  let press = fun _ _ _ -> Button.press (get_button b) in
  let c = connect_main b b press Trigger.buttons_down in
  add_connection b c;
  let release = match kind with (* move this test to Button ? *)
    | Button.Trigger -> fun _ _ _ -> Button.release (get_button b)
    | Button.Switch -> fun _ _ ev -> Button.switch (get_button b) ev
  in
  let c = connect_main b b release Trigger.buttons_up in
  add_connection b c;
  let c = connect_main b b (fun b _ _ -> Button.mouse_enter (get_button b))
      [Trigger.mouse_enter] in
  add_connection b c;
  let c = connect_main b b (fun b _ _ -> Button.mouse_leave (get_button b))
      [Trigger.mouse_leave] in
  add_connection b c;
  b;;
(* TODO: actions *)

(* use ~lock if the user is not authorized to slide *)
let slider ?(priority=Main) ?step ?value ?kind ?var ?length ?thickness ?tick_size ?(lock = false) maxi =
  let w = create_empty (Slider (Slider.create ?step ?value ?kind ?var ?length ?thickness ?tick_size maxi)) in
  if not lock then begin
    let onbutton_down = fun w _ ev -> Slider.click (get_slider w) ev in
    let c = connect_main w w onbutton_down Trigger.buttons_down in
    add_connection w c;
    (* let onclick = fun w _ ev -> Slider.click_focus (get_slider w) ev in *)
    (* let c = connect_main w w onclick [Sdl.Event.mouse_button_up] in *)
    (* add_connection w c; *)
    let on_release = fun w _ _ -> Slider.release (get_slider w) in
    let c = connect_main w w on_release Trigger.buttons_up in
    add_connection w c;
    let slide = fun w _ ev ->
      let ti = get_slider w in
      if Trigger.mm_pressed ev || Trigger.event_kind ev = `Finger_motion
      then (Slider.slide ti ev; update w)
    in
    let c = connect ~priority ~update_target:false w w slide Trigger.pointer_motion in
    add_connection w c;
    let get_keys = fun w _ ev -> Slider.receive_key (get_slider w) ev
    in
    let c = connect ~priority w w get_keys [Sdl.Event.key_down] in
    add_connection w c
  end;
  w;;

(* create a slider with a simple Tvar that executes an action each time the
   local value of the slider is modified by the slider *)
let slider_with_action ?priority ?step ?kind ~value ?length ?thickness ?tick_size
    ~action max =
  let v = Var.create (Avar.var value) in
  let t_from a = Avar.get a in
  let t_to x = action x; Avar.var x in
  let var = Tvar.create v ~t_from ~t_to in
  slider ?priority ?step ?kind ~var ?length ?thickness ?tick_size max;;
  
let text_input ?(text = "") ?prompt ?size ?filter ?max_size () =
  let ti = Text_input.create ?size ?prompt ?filter ?max_size text in
  let w = create_empty (TextInput ti) in
  let onbutton_down = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    Text_input.button_down ti ev in
  let c = connect_main w w onbutton_down Trigger.buttons_down in
  add_connection w c;
  let onclick = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    Text_input.click ti ev in
  let c = connect_main w w onclick Trigger.buttons_up in
  add_connection w c;
  let ontab = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    Text_input.tab ti ev in
  let c = connect_main w w ontab [Sdl.Event.key_down] in
  add_connection w c;
  let selection = fun w _ ev ->
    let ti = get_text_input w in (* = ti ! *)
    if Trigger.mm_pressed ev then (Text_input.mouse_select ti ev; update w)
  in
  let c = connect_main ~update_target:false w w selection [Sdl.Event.mouse_motion] in
  add_connection w c;
  let get_keys = fun w _ ev -> Text_input.receive_key (get_text_input w) ev
  in
  let c2 = connect_main w w get_keys [Sdl.Event.text_editing; Sdl.Event.text_input; Sdl.Event.key_down; Sdl.Event.key_up] in
  add_connection w c2;
  w;;
(* TODO *)


(* Some generic functions or 'methods' that can make sense for one or several
   types of widgets *)

let get_text w =
  match w.kind with
  | Button b -> Button.text b
  | TextDisplay td -> Text_display.text td
  | Label l -> Label.text l
  | TextInput ti -> Text_input.text ti
  | _ -> (printd debug_error "This type of widget does not have a text function";
          "");;

let set_text w text =
  match w.kind with
  | Button b -> Button.set_label b text
  | TextDisplay td -> let pa = Text_display.paragraphs_of_string text in
    Text_display.update td pa
  | Label l -> Label.set l text
  | TextInput ti -> let k = Utf8.split text in
    Text_input.set ti k
  | _ -> printd debug_error "Cannot set text to this type of widget";;

let get_state w =
  match w.kind with
  | Button b -> Button.state b
  | Check c -> Check.state c
  | _ -> (printd debug_error "This type of widget does not have a state function";
          false);;

  
(** creation of combined widgets *)
let check_box_with_label text =
  let b = check_box () in
  let l = label text in
  let action = fun _ w _ -> Check.action (get_check w) in
  let c = connect_main l b action Trigger.buttons_down in
  add_connection l c;
  b,l;;

(****)

(* some useful connections *)
(* the disadvantage is that these functions do not take advantage of the two
   widgets + event entry. Thus they are less 'functional' and require more
   global variables. Also, they all work with "connect_main", so are ok only for
   very fast actions. *)

let mouse_over ?(enter = nop) ?(leave = nop) w =
  let c = connect w w (fun w _ _ -> enter w) [Trigger.mouse_enter] in
  add_connection w c;
  let c' = connect w w (fun w _ _ -> leave w) [Trigger.mouse_leave] in
  add_connection w c';;

let on_click ~click w =
  let c = connect_main w w (fun w _ _ -> click w) Trigger.buttons_down in
  add_connection w c;;

let on_release ~release w =
  let c = connect_main w w (fun w _ _ -> release w) Trigger.buttons_up in
  add_connection w c;;

(****)

(** check if connection is in the active list, and return the most
    recent (=first in list) active, or None *)
let is_active alist c =
  let rec loop = function
    | [] -> None
    | a::rest -> if a.connect_id = c.id then Some a else loop rest
  in loop alist;;

(** remove an 'active' from the active list of the widget *)
(* it should occur only once in the list *)
let remove widget thread_id =
  let rec loop list acc = match list with
    | [] -> acc
    | a::rest -> (* if a.connect_id = active.connect_id *)
      (* test inutile, le suivant suffit *)
      if Thread.id a.thread = thread_id
      then List.concat [List.rev rest; acc]
      else loop rest (a::acc)
  in Var.set widget.actives (List.rev (loop (Var.get widget.actives) []));;

let add widget active =
  Var.set widget.actives (active :: (Var.get widget.actives));;

(** ask a thread to remove itself from a widget *)
let remove_me c_id widget =
  printd debug_thread "Removing connection #%d" c_id;
  remove widget (Thread.id (Thread.self ()));
  decr threads_created;;

(* check if connection is terminated *)
(* (only if the thread decided to signal this, for instance by setting the event
   to Trigger.stopped) *)
let has_terminated active =
  Sdl.Event.(get active.event typ) <> Trigger.stop;;

(* indicate to an active connection that its thread should terminate *)
(* TODO protect this with mutex or Var *)
let terminate ?(timeout = 50) active =
  printd debug_thread "Ask for terminating connection #%u" active.connect_id;
  Sdl.Event.(set active.event typ) Trigger.stop;
  ignore (Timeout.add timeout (fun () ->
      if not (has_terminated active)
      then begin
        try Thread.kill active.thread;
          printd debug_thread "Killing thread for connection #%u" active.connect_id
        with _ ->
          printd debug_thread "Cannot kill thread for connection #%u (probably Thread.kill not implemented)." active.connect_id
      end
    ));;

(* ask for terminate and wait (blocking) until it really terminates by itself *)
let wait_terminate active =
  terminate active;
  Thread.join active.thread;;

(** activate an action (via a thread) on the connection *)
let add_action c action ev =
  printd debug_thread "Create thread for connection #%d" c.id;
  (* Trigger.renew_my_event (); *)
  (* we used to create a new event for the main loop, so that "ev" can be safely
     sent to the thread, and the thread can examine later, even after several
     main loops, without it being altered (except when exiting is required) *)
  (* Now we use a more natural, solution would be to copy the event before
     sending it to the thread, but there is no "copy_event" function
     available... *)
  (* WARNING: at this point it is not possible to copy the drop_file_file field *)
  let e_copy = Trigger.copy_event ev in
  incr threads_created;
  add c.source
    { thread = Thread.create (action c.source c.target) e_copy;
      event = e_copy;
      connect_id = c.id };;

(** check if the trigger can wake up a connection, and if so, run the action *)
let wake_up event c =
  if List.mem (Trigger.of_event event) c.triggers then
    begin
      printd debug_thread "Activating connection #%d" c.id;
      (* TODO add a more precise ~test before launching the thread ? *)
      if c.priority = Main then c.action c.source c.target event
      (* = direct action, no thread !. Should we still add it to the active list
         ? *)
      else begin
        let action = fun w1 w2 ev ->
          c.action w1 w2 ev;
          remove_me c.id w1 in
        let alist = Var.get c.source.actives in
        let tho = is_active alist c in
        if alist = [] || tho = None then add_action c action event
        else match c.priority, tho with
          | Forget, _ -> printd debug_thread "Forgetting connection #%d" c.id
          | Join, Some a ->
            let action = fun w1 w2 ev -> (Thread.join a.thread; action w1 w2 ev) in
            add_action c action event
          | Replace, Some a -> begin
              (*printd debug_thread "Killing connection #%d" a.connect_id;*)
              (* Thread.kill a.thread; *) (*TODO: change this: Thread.kill is in
                                            fact NOT implemented... ! *)
              terminate a;
              remove c.source (Thread.id a.thread);
              add_action c action event
            end
          | _ -> failwith "This should not happen"
      end
    end;;

let wake_up_all ev w =
  List.iter (wake_up ev) w.connections;;
  
(** remove all active connections from this widget and ask for the threads to
    terminate *)
let remove_active_connections widget =
  let actives = Var.get widget.actives in
  List.iter wait_terminate actives;
  Var.set widget.actives [];;


(*******************)

(* some widgets directly react to a click event to activate themselves. Some,
   like text_input, even react to the TAB key. In fact, keyboard_focus is
   treated globally by the main loop, therefore one could (should ?) rely on
   this function below instead of adding new reactions to TAB & click *)
let set_keyboard_focus w =
  match w.kind with
  | TextInput _ -> () (* already done by the widget *)
  | Slider s -> Slider.set_focus s
  | _ -> ();;

let remove_keyboard_focus w =
  match w.kind with
  | TextInput ti -> Text_input.stop ti
  | Slider s -> Slider.unfocus s
  | _ -> ();;


let guess_unset_keyboard_focus w =
  match w.kind with
  | TextInput _ -> Some false
  | Slider _ -> Some false
  | _ -> None;;
(* TODO: buttons could have keyboard focus... to activate them with TAB or ENTER
   or SPACE... *)
