(** the queue of actions to be executed before everything in the main loop *)
(* NOTE: if some actions take too much time, the remaining ones are postponed to
   next iteration of the main loop. *)
(* FIFO queue: order of actions is preserved *)

(* Warning: the first Sync execution is done slightly before the main loop, with
   a little more time... Make sure that's no problem. *)

open B_utils
module Time = B_time
module Var = B_var
module Trigger =  B_trigger

type action = unit -> unit

let queue : (action Queue.t) Var.t = Var.create (Queue.create ())

let is_empty () =
  Queue.is_empty (Var.get queue)

let push action =
  printd debug_thread "Sync push action";
  Var.protect_fn queue (Queue.push action);
  Trigger.push_action ()

(* If [o] is None, Sync-compute the value with [future] and apply it to [f] *)
let option o future f =
  match o with
  | Some x -> f x
  | None -> push (fun () ->
      let x = future () in f x
    )

(* Returns true if some action was executed *)
(* We assume that the whole process does not need to be mutex protected. It
   should be OK if other treads are adding to the Queue, as long as we (the main
   thread) are the only ones popping from the queue. *)
let execute timeout =
  if is_empty () then false (* a quick test in order to avoid lock *)
  else let t = Time.now () in
    let rec loop () =
      if Var.with_protect queue Queue.is_empty then () (* we exit *)
      else if Time.(now () - t > timeout)
      then (* we exit but also send a action event to finish the rest of the
              queue at next iteration. *)
        begin
          printd debug_thread "Didn't have time to finish Sync queue.";
          Trigger.push_action ()
        end
      else let action = Var.with_protect queue Queue.pop in
        printd debug_thread "Popping one action from the Sync Queue.";
        action ();
        loop ();
    in
    loop ();
    true
