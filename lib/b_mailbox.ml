(* This file is part of BOGUE, by San Vu Ngoc *)


(* Contrary to SDL events (which are more like triggers than messages),
   mailboxes can receive any kind of semantic events. For instance:

   [send mbx (`Open_directory "/home")]

   This small messaging API is proposed as a convenience for the reader, but no
   Bogue widget relies on it (and in fact, even for the example above it is
   always possible to create controller widgets without using mailboxes, see the
   tutorial

   https://sanette.github.io/bogue-tutorials/bogue-tutorials/modif_parent.html

   )

   On the other hand, in more complicated cases, using mailboxes is cleary
   easier and reduces the boilerplate.

   WARNING: API not stabilized yet
*)

open B_utils

module Sync = B_sync
module Trigger = B_trigger
module Update = B_update
module Var = B_var
module W = B_widget

type 'a mbx = {
  owner : W.t; (* Only the owner widget should be authorized to read the mail *)
  queue : ('a list) Var.t;
  mutable active : bool;
  (* if not active, sending mail will be loggued as an error, but the messages
     are still stored in the queue and can be read once the mailbox is
     activated.*)
}

(* Create a mailbox for widget [owner] whose messages are of type ['a]. *)
let create ?owner () =
  let owner = match owner with
    | Some widget -> widget
    | None ->
      let w = W.empty ~w:0 ~h:0 () in
      printd (debug_board) "Creating an empty Widget #%u for hosting new Mailbox"
        (W.id w);
      w in
  {
  owner;
  queue = Var.create [];
  active = false
}

(* By default, "the mailman delivers at each frame": messages are handled one by
   one in the Sync queue, in the order of reception (FIFO). Setting [sync=false]
   will on the contrary execute the handler in a separate thread. (Only one
   thread for all messages.) *)
let activate ?(sync = true) mbx handler =
  let c = W.connect mbx.owner mbx.owner (fun _ _ ev ->
      if mbx.active then begin match Trigger.event_kind ev with
        | `Bogue_new_mail ->
          let wid = Trigger.E.(get ev user_code) in
          if wid <> W.id mbx.owner
          then begin
            printd (debug_error)
              "Event sent by widget #%u trying to read a mailbox belonging to #%u!"
              wid (W.id mbx.owner)
          end
          else begin
            let messages = List.rev (Var.get mbx.queue) in
            Var.set mbx.queue [];
            if sync
            then List.iter (fun msg -> Sync.push (fun () -> handler msg)) messages
            else List.iter handler messages
          end
        | _ ->
          print_endline "OTHER EV";
          printd debug_error "Reading a mailbox should be triggered only by \
                              the Bogue_new_mail event. "
      end
      else printd (debug_warning+debug_event)
          "Mailbox #%u is currently inactive. Please re-enable it to handle \
           incoming mails" (W.id mbx.owner)
    ) [Trigger.new_mail] in
  W.add_connection mbx.owner c;
  mbx.active <- true

let send mbx msg =
  Var.update mbx.queue (List.cons msg);
  Trigger.push_new_mail (W.id mbx.owner);
  printd (debug_user) "Message was sent to the (active=%b) mailbox #%u."
    mbx.active (W.id mbx.owner)


(* We should manually feed the widget with the event, because the widget may not
   have focus and hence is not reached by the usual connection strategy. (Like
   we do for the update event.) *)
let reach_widget ev =
  let wid = Trigger.(E.get ev widget_id) in
  try let w = W.of_id wid in
    W.wake_up_all ev w
  with Not_found -> printd debug_error "The mailbox widget #%u has disappeared." wid

let enable mbx =
  mbx.active <- true;
  Trigger.push_new_mail (W.id mbx.owner)

let disable mbx =
  mbx.active <- false

let clear mbx =
  Var.update mbx.queue (function
      | [] -> []
      | _ ->
        printd debug_user "Clearing the Mailbox #%u" (W.id mbx.owner);
        [])
