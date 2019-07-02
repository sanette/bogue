(* this is where we store the widgets that receive the update event *)

(* similar to Sync.ml but we make sure there is no repeated entries in the
   queue *)

(* see: examples/chain *)

(* TODO à vider en sortant ? *)
(* ou à attacher à la board ? *)

open B_utils
module Widget = B_widget
module Var = B_var
module Trigger =  B_trigger
  
let str = Printf.sprintf


let table : (Widget.t list) Var.t = Var.create  [];;

let is_empty () =
  Var.get table = [];;

let clear () =
  if not (is_empty ()) then
    begin
      printd debug_warning "The update queue was not empty"; 
      Var.set table []
    end;;

let mem w =
  List.exists (Widget.equal w) (Var.get table);;
  
let push w =
  if mem w then
    printd debug_event "Widget #%u is already in the Update.table" w.Widget.wid
  else begin
    Var.protect table;
    Var.unsafe_set table (w::(Var.get table));
    Trigger.push_update w.Widget.wid;
    Var.release table
  end;;

let push_all () =
  List.iter
    (fun w -> Trigger.push_update (Widget.id w)) (Var.get table);;

let execute_one e w =
  if w.Widget.wid = Trigger.get_update_wid e
  then (
    Widget.wake_up_all e w;
    Trigger.push_redraw (Widget.id w) (* OK ?? *)
  );;

let execute e =
  match Var.get table with
  | [] -> () (* we don't want to reset the table to [] if it was already empty *)
  | list -> (
      Var.protect table;
      let wid = Trigger.get_update_wid e in
      let list_e, other = List.partition (fun w -> w.Widget.wid = wid) list in
      printd debug_memory "Udpate Table: remaining size=%i" (List.length other);
      (* we keep the widgets that do not correspond to the event e *)
      Var.unsafe_set table other;
      Var.release table;
      (* we release the table before execution so that one can still push to
         the new table while the old one is being executed *)
      printd debug_memory "Update Table: execute size=%i" (List.length list_e);
      List.iter (execute_one e) list_e);;
