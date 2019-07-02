(* dynamic variables with immediate or delayed execution *)

(* one could use this for the whole rendering process of everything (like in
   ELM): Layout.t would be replaced by Layout.t Dynvar.t. But I'm not sure it's
   a good idea. If you have a complicated layout and only one small part
   changes, it seems to me you would have to recompute everything et each
   frame. Or, we need a mechanism to impose the scope of the modifications. *)

(* TODO: make thus a functor to specialize equality *)
 
(* this module is not used yet *)

open Tsdl
open B_utils
module Var = B_var
module Trigger =  B_trigger
  
(* just to keep track of created threads... not used ? *)
let threads_created = Var.create [];;

let add_thread t =
  Var.protect_fn threads_created (fun () ->
      Var.unsafe_set threads_created (t :: Var.unsafe_get threads_created));;

let remove_thread t =
  Var.protect_fn threads_created (fun () ->
      let list = List.filter (fun t' -> t' <> t) (Var.unsafe_get threads_created) in
      Var.unsafe_set threads_created list);;

type 'a t = {
  id : int;
  mutable data : 'a;
  mutable changed : bool;  (* not used ? *)
  update : Sdl.event -> 'a -> 'a;
  events : Sdl.event_type list (* the list of events that change this variable *)
};;

(* table (event_type, id) = the id of the variables that are modified by the
   event of this type (multivalued) *)
let var_event_table : (Sdl.event_type, int) Hashtbl.t = Hashtbl.create 100;;

(* "reciprocal table" : table (id, ev): the vars that have changed
   (multivalued), and the corresponding events *)
let var_to_update_table : (int, Sdl.event) Hashtbl.t = Hashtbl.create 100;;

let new_id = fresh_int ();;

(** create a new dynvar which reacts to the event types listed in ~event, and
    update itself by applying the ~update function to each event *)
let create ~update ~events x =
  let id = new_id () in
  let v = { 
    id;
    data = x;
    changed = false;
    update;
    events } in
  List.iter (fun typ -> 
      printd debug_warning "Add (%u, %u)" typ id;
      Hashtbl.add var_event_table typ id) events; 
  (* can have several vars per event type *)
  v;;

let fail _ _ = failwith "This var cannot update itself";;
  
(** create a "manual" dynvar which does react to any event, but which can be
    modified manually with modify *)
let of_value x =
  create ~update:fail ~events:[] x;;

let send_event_var_changed v =
  Trigger.push_var_changed v.id;;
(* do we really need to send several such events ? maybe only one is enough, and
   the user code is not used. Another option would be to store in var.events the
   var_id ... *)

(** manually modify a dynvar *)
let modify v x =
  if x <> v.data then begin (* TODO specialize equality *)
    v.data <- x;
    v.changed <- true;
    send_event_var_changed v
  end;;

let update_var v ev =
  printd debug_warning "Update dynvar #%u" v.id;
  modify v (v.update ev v.data);
  Hashtbl.remove var_to_update_table v.id;;

(** get (or compute) the value of the dynvar *)
let value v =
  let () = try let evs = Hashtbl.find_all var_to_update_table v.id
      in List.iter (update_var v) evs;
    with
    | Not_found -> () in
  v.data;;

(** create a new dynvar by applying a the function f to the value of v.
    If v is modified in the future, then the new dynvar will be updated
    accordingly *)
let apply f v =
  let x = f (value v) in
  let update _ _ = f (value v) in
  let events = Trigger.var_changed :: v.events in (* or already there ? *)
  create ~update ~events x;;

(* this should be called with every new event in the mainloop. *)
let process_event ev =
  let ev_type = Sdl.Event.(get ev typ) in
  let ids = Hashtbl.find_all var_event_table ev_type in
  List.iter (fun id -> 
      printd debug_warning "Add to update: (id:%u, ev:%u)" id ev_type;
      Hashtbl.add var_to_update_table id ev) ids;;

(** create a var with the result of a computation done in another thread *)
(* we need a default value to start with *)
let async_compute f default =
  let v = create ~events:[] ~update:fail default in
  let g = fun () ->
    let result = f () in
    modify v result;
    remove_thread (Thread.self ())
  in
  add_thread (Thread.create g ());
  v;;


(***************************************************)
let test () = 
  let u = of_value 0 in
  let f x = x + 1 in
  let v = apply f u in
  let g x = x * 2 in
  let w = apply g v in
  printd debug_warning "w=%u" (value w);
  modify u 1;
  let ev = Trigger.(wait_event !my_event) in
  process_event ev;
  printd debug_warning "w=%u" (value w);
  let ev = Trigger.(wait_event !my_event) in
  process_event ev;
  printd debug_warning "w=%u" (value w);

  let my_long_computation () =
    printd debug_thread "Starting computation...";
    Thread.delay 3.;
    printd debug_thread "Computation finished !";
    123 in
  let au = async_compute my_long_computation 0 in
  let v = apply f au in
  printd debug_warning "au=%u, v=%u" (value au) (value v);
  let rec loop t0 =
    if Unix.gettimeofday () -. t0 > 4. then ()
    else begin
      let ev = Trigger.(wait_event !my_event) in
      process_event ev;
      printd debug_warning "au=%u, v=%u" (value au) (value v);
      Thread.delay 0.1;
      loop t0 
    end in
  loop (Unix.gettimeofday ());;

(*
   Local Variables:
   tuareg-interactive-program:"./bogue.top"
   typerex-interactive-program:"./threadtop -I +threads"
   compile-command:"make -k"
   End:
*)
