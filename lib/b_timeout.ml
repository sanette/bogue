(** Execute actions after a specified timeout *)

(* Warning: the Timemout by itself will not generate any event. Therefore, if
   the action needs an immediate redraw by the main loop, the redraw event
   should be triggered, or the action_event (if just breaking the wait_event
   loop is enough). TODO ? Maybe it's better that all timeouts break the the
   wait_event loop? *)


(* we need an ordered data structure, with very fast folding ( = itering in
   increasing order), but the insertion time is not a problem. *)
(* We chose here an ordered List. Maybe that's not optimal. *)

module Utils = B_utils
module Time = B_time
module Var = B_var
  
type action = unit -> unit;;
type t = { 
  id : int;
  timeout : Time.t;
  action : action
}

let new_id = Utils.fresh_int ();;

let create timeout action =
  { id = new_id (); timeout; action};;

let execute t =
  if Time.(now () >> t.timeout) 
  then (Utils.(printd debug_board "Executing timeout");
        t.action (); true)
  else false;;

(* the global stack variable *)
let stack = Var.create [];;

let clear () =
  if Var.get stack <> [] then
    begin
      Utils.(printd debug_warning "Clearing the remaining %u Timeouts"
               (List.length (Var.get stack)));
      Var.set stack []
    end;;

(* insert t at the right place in list *)
let insert t list =
  let rec loop before_rev after =
    match after with
    | [] -> List.rev (t :: before_rev)
    | a :: rest -> if a.timeout > t.timeout
      then List.rev_append before_rev (t :: after)
      else loop (a :: before_rev) rest in
  loop [] list;;

(* insert a sublist in a list *)
(* it should be slightly more efficient than using "insert" repeatedly *)
(* because once an element of sublist in inserted into list, we know the other
   elements of sublist will fall on the right of it. *)
let insert_sublist sublist list =
  if sublist = [] then list
  else begin
  let rec loop sub final_rev rest =
    let rec insert t before_rev after =
      match after with
      | [] -> (t :: before_rev), []
      | a :: rest -> if a.timeout > t.timeout
        then (t :: before_rev), after
        else insert t (a :: before_rev) rest in
    match sub with
    | [] -> List.rev_append final_rev rest
    | t :: subrest -> let before_rev, after =  insert t final_rev list in
      loop subrest before_rev after in
  loop sublist [] list
  end;;

(* Immediately registers a new timeout and returns it. In general it's better to
   use push in order to get a correct starting time, unless we know this is done
   dynamically during the main loop. *)
let add timeout action =
  let timeout = Time.now () + timeout in
  let t = create timeout action in
  Var.protect_fn stack (fun () ->
      let list = Var.unsafe_get stack in
      Var.unsafe_set stack (insert t list));
  t;;

(* Push a timeout to be registered at the next iteration of the main loop. *)
let push timeout action =
  (fun () -> add timeout action)
  |> Stack.push
       
let not_equal t1 t2 =
  t1.id <> t2.id;;

(* remove a Timeout from stack *)
let remove t stack =
  Var.protect_fn stack (fun () ->
      let list = Var.unsafe_get stack in
      Var.unsafe_set stack (List.filter (not_equal t) list));;

(** cancel a Timeout from the global stack *)
let cancel t = 
  remove t stack;;

let iter stack =
  (* we pop the whole list and push back an empty stack in case some thread want
     to add new timeouts while we are processing *)
  let list = Var.protect_fn stack (fun () ->
      let list = Var.unsafe_get stack in
      Var.unsafe_set stack [];
      list) in
  let rec loop l =
    match l with
    | [] -> []
    | t :: l' -> if execute t
      then loop l' 
      else l (* the action t was not executed, we leave it in the stack *)
  in
  let remaining = loop list in
  Var.protect_fn stack (fun () ->
      let modified = Var.unsafe_get stack in
      Var.unsafe_set stack (insert_sublist modified remaining));;

let run () =
  (* the stack should be empty most of the time, so we add a test to be faster *)
  if Var.get stack <> [] then iter stack;;
