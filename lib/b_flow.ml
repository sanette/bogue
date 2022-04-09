(* Flow data structure

   A flow is a simple FIFO queue like Queue.t which can be rewinded to its
   initial state. As a consequence, an iteration of a flow can be stopped and
   resumed later without destroying the queue.  *)

(* This file is part of BOGUE but can be used independently *)

exception End_reached

type 'a cell_cons = { content: 'a; mutable next: 'a cell }

and 'a cell =
  | Nil
  | Cons of 'a cell_cons

type 'a t = {
  mutable current: 'a cell;
  mutable first: 'a cell;
  mutable last: 'a cell
}

let create () = {
  current = Nil;
  first = Nil;
  last = Nil
}

let is_empty q =
  q.first = Nil (* q.last = Nil would work too *)

let end_reached q =
  q.current = Nil

let clear q =
  q.current <- Nil;
  q.first <- Nil;
  q.last <- Nil

let rewind q =
  q.current <- q.first

(* forget the past of the flow. *)
let forget q =
  q.first <- q.current

(* Add [x] to the queue. If the current pointer is [Nil] (end reached) then it
   will point to the added element. *)
let add x q =
  let cell = Cons {
    content = x;
    next = Nil
  } in
  match q.last with
  | Nil -> (* q is empty *)
    assert (is_empty q);
    q.current <- cell;
    q.first <- cell;
    q.last <- cell
  | Cons last ->
    last.next <- cell;
    q.last <- cell;
    if q.current = Nil then q.current <- cell

(* read and advance in the flow, but do not remove from the queue *)
let read q =
  match q.current with
  | Nil -> raise End_reached
  | Cons { content; next } ->
    q.current <- next;
    content

let read_opt q =
  match q.current with
  | Nil -> None
  | Cons { content; next } ->
    q.current <- next;
    Some content

(* TODO use read_opt? *)
let iter =
  let rec iter q f cell =
    match cell with
    | Nil -> ()
    | Cons { content; next } ->
      q.current <- next;
      f content;
      iter q f next
  in
  fun f q ->
    iter q f q.current;
    q.current <- Nil

(* Stops when the result of [f] evaluated on the queue element is true *)
let iter_until f q =
  let rec loop f cell =
    match cell with
    | Nil -> q.current <- Nil
    | Cons { content; next } ->
      q.current <- next;
      if not (f content) then loop f next
  in
  loop f q.current

(* Check if [q] contains an element [x] for which [f x] returns true (starting
   from current position) *)
let exists f q =
  let rec loop cell =
    match cell with
    | Nil -> false
    | Cons { content; next } ->
      if f content then true else loop next
  in
  loop q.current

(* Could be optimized if necessary *)
let of_list l =
  let q = create () in
  List.iter (fun x -> add x q) l;
  q

(* Removing an arbitrary element is not very adapted to this data structure,
   except of course popping out the first element. The following functions are
   for occasional use.  *)

(* The following functions use physical equality [==] on union types, which
   seems to be implementation dependent according to the manual. Hence we
   redefine [==] to have a better control over it. But it seems to work with the
   original [Stdlib.==] as well. Note that [Cons x == Cons x] is FALSE for
   [Stdlib.(==)] but TRUE for our redefined equality. However we were careful
   not to encounter this case anyway. *)
let (==) cell1 cell2 =
  match cell1, cell2 with
  | Nil, Nil -> true
  | Cons c1, Cons c2 -> Stdlib.(==) c1 c2 (* this one is well-defined because of
                                             the mutable field "next". *)
  | _ -> false

(* [remove_first_match_after f q] removes the first q element (starting from
   *next* to current) for which f evals to true. *)
let remove_first_match_after =
  let rec loop f q previous_cell =
    match previous_cell with
    | Nil -> raise End_reached
    | Cons previous ->
      match previous.next with
      | Nil -> raise Not_found
      | Cons curr ->
        if f curr.content
        then begin
          if q.last == previous.next (* we want to remove the last element *)
          then q.last <- previous_cell;
          previous.next <- curr.next;
      end
      else loop f q previous.next
  in
  fun f q -> loop f q q.current

let remove_current q =
  match q.current with
  | Nil -> raise End_reached
  | Cons curr ->
    if q.current == q.first (* easy case *)
    then begin
      if q.last == q.current then q.last <- Nil;
      q.first <- curr.next;
      q.current <- curr.next;
    end
    else (* bad case, we have to start from top *)
      let rec loop previous =
        match previous.next with
        | Nil -> failwith "Should not happen"
        | Cons c ->
          if previous.next == q.current then begin
            previous.next <- c.next;
            q.current <- c.next
          end
          else loop c in
      match q.first with
      | Nil -> failwith "Empty flow head: should not happen"
      | Cons p -> loop p

(* Remove the first element (starting from the top of the flow) for which f
   evals true. In case the removed element was current, the next one becomes
   current. *)
let remove_first_match f q =
  match q.current with
  | Cons { content; next = _ } ->
    if f content
    then remove_current q
    else remove_first_match_after f q
  | Nil -> raise End_reached

(* Slow. only for debugging *)
let length_from cell =
  let rec loop i cell =
    match cell with
    | Nil -> i
    | Cons { content = _; next } ->
      loop (i+1) next in
  loop 0 cell

let total_length q = length_from q.first

let length q = length_from q.current

let test () =
  let q = of_list [1;2;3] in
  assert (not (is_empty q));
  assert (not (end_reached q));
  assert (read q = 1);
  assert (read q = 2);
  assert (read q = 3);
  assert (end_reached q);
  assert (not (is_empty q));
  add 4 q;
  assert (read q = 4);
  rewind q;
  iter_until (fun x -> x >= 2) q;
  assert (read q = 3);
  assert (read q = 4);
  assert (end_reached q);
  rewind q;
  iter (fun _ -> ()) q;
  assert (end_reached q);
  rewind q;
  iter_until (fun x -> x >= 20) q;
  assert (end_reached q);
  clear q;
  assert (is_empty q);
  assert (read_opt q = None);
  add 1 q;
  remove_current q;
  assert (end_reached q);
  add 1 q;
  add 2 q;
  remove_current q;
  assert (exists (fun x -> x = 2) q);
  assert (read q = 2);
  assert (not (exists (fun x -> x = 2) q))
