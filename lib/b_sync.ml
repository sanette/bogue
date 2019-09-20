(** the queue of actions to be executed before everything in the main loop *)
(* NOTE: if some actions take too much time, the remaining ones are postponed to
   next iteration of the main loop. *)
(* FIFO queue: order of actions is preserved *)

open B_utils
module Time = B_time
module Var = B_var
module Trigger =  B_trigger
  
type action = unit -> unit;;

let queue : (action Queue.t) Var.t = Var.create (Queue.create ());;

let is_empty () =
  Queue.is_empty (Var.get queue);;

(* Warning: a Sync action cannot push another Sync action, because it will wait
   forever the release of the mutex *)
(* TODO: add a test of this using Mutex.try_lock *)
let push action =
  printd debug_thread "Sync push action";
  Var.protect queue;
  Queue.push action (Var.get queue);
  Trigger.push_action ();
  Var.release queue;;

(** returns true if some action was executed *)
let execute timeout =
  if is_empty () then false (* a quick test in order to avoid lock *)
  else let t = Time.now () in 
       let () = Var.protect queue in
       let q = Var.get queue in
       let rec loop () =
         if Queue.is_empty q then () (* we exit *)
         else if Time.(now () - t > timeout)
         then (* we exit but also send a action event to finish the rest of the
                 queue at next iteration. *)
           Trigger.push_action ()
         else let action = Queue.pop q in
              printd debug_thread "Popping one action from the Sync Queue";
              action ();
              loop ();
       in
       loop ();
       Var.release queue;
       true;;

