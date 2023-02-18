(* This file is part of BOGUE, by San Vu Ngoc *)

(* Execute actions after a specified timeout *)

(* Warning: the Timeout by itself will not generate any event. Therefore, if the
   action needs an immediate redraw by the main loop, the redraw event should be
   triggered, or the action_event (if just breaking the wait_event loop is
   enough). TODO ? Maybe it's better that all timeouts break the the wait_event
   loop? *)


(* We need an ordered data structure, with very fast folding ( = itering in
   increasing order), but the insertion time is not a problem. *)
(* We chose here an ordered List. Maybe that's not optimal. *)

module Utils = B_utils
module Time = B_time
module Var = B_var

type action = unit -> unit
type t = {
  id : int;
  timeout : Time.t; (* in absolute time units *)
  action : action;
  mutable cancelled : bool
}

let new_id = Utils.fresh_int ()

let iterating = ref false (* for debugging *)

let create timeout action =
  { id = new_id (); timeout; action; cancelled = false}

let execute t =
  if !Utils.debug then assert (not t.cancelled);
  if Time.(now () >> t.timeout)
  then (Utils.(printd debug_board "Executing timeout %i" t.id);
        t.action (); true)
  else false

(* The global stack variable. It should always be sorted by final time of
   execution.  *)
let stack = Var.create []

(* (Not used) Should not be called while iterating... *)
let clear () =
  if !Utils.debug then assert (not !iterating);
  if Var.get stack <> [] then
    begin
      Utils.(printd debug_warning "Clearing the remaining %u Timeouts"
               (List.length (Var.get stack)));
      Var.set stack []
    end

(* Insert t at the right place in list. *)
let insert list t =
  let rec loop before_rev after =
    match after with
    | [] -> List.rev (t :: before_rev)
    | a :: rest -> if a.timeout > t.timeout
      then List.rev_append before_rev (t :: after)
      else loop (a :: before_rev) rest in
  loop [] list

(* Insert a sublist in a list. *)
(* It could certainly be optimised taking into account that lists are ordered: *)
(* once an element of sublist in inserted into list, we know the other elements
   of sublist will fall on the right of it. *)
let insert_sublist sublist list =
  List.fold_left insert list sublist

(* Immediately registers a new timeout and returns it. In general it's better to
   use push in order to get a correct starting time, unless we know this is done
   dynamically during the main loop. *)
let add delay action =
  let timeout = Time.now () + delay in
  let t = create timeout action in
  Utils.(printd debug_board "Adding timeout %i" t.id);
  Var.update stack (fun list -> (insert list t));
  t

let not_equal t1 t2 =
  t1.id <> t2.id

(* (Not used) Remove a Timeout from stack. Should not be called while
   iterating. *)
let remove_old t stack =
  Var.update stack (fun list -> List.filter (not_equal t) list)

(* Cancel a Timeout from the global stack. It will not be executed and will be
   effectively removed from the stack by the next call to [iter]. *)
let cancel t =
  Utils.(printd debug_board "Cancelling Timeout %i" t.id);
  t.cancelled <- true

let iter stack =
  (* We pop the whole list and push back an empty stack in case the actions in
     the list, or some other thread, want to add new timeouts while we are
     processing. *)
  let list =
    Var.with_protect stack (fun list ->
        Var.unsafe_set stack [];
        list) in
  (* Utils.(printd debug_custom "Iter timeout stack of size %i" (List.length
     list)); *)
  let rec loop l =
    match l with
    | [] -> []
    | t :: l' ->
      if t.cancelled || execute t
      then loop l'
      else l (* the action t was not executed, we leave it in the stack *)
  in
  let remaining = loop list in
  (* Utils.(printd debug_custom "Remaining size %i" (List.length remaining)); *)
  Var.update stack (fun modified -> insert_sublist modified remaining)

let run () =
  (* the stack should be empty most of the time, so we add a test to be faster *)
  if Var.get stack <> [] then iter stack
