(* bidirectional ordered chained lists (mutable, not thread-safe) *)
(* San Vu Ngoc *)

(* This file is part of Bogue but can be used independently *)

(* A chain is morally just a list with a pointer indicating the current element
   we're looking at. The implementation should provide an easy and fast way to
   access the element before and the element after. The whole chain (that is,
   the connected component of any of its elements) is called a stack. There is
   no particular type for a stack. *)

(* TODO we could use a phatom type when we want to ensure the chain is not
   empty. See
   https://blog.janestreet.com/howto-static-access-control-using-phantom-types/*)

let debug = !B_utils.debug

exception Max_insert

type 'a element =
  { id : int; (* [id] identifies the stack (connected component) *)
    mutable value : 'a;
    mutable depth : int;
    (* [depth] (positive integer for non-empty Chain, zero for empty) is a
       redundant information, in order to get faster comparison between
       chains. The rule is that the .next element must have higher depth. A
       consequence is that the number of elements cannot exceed max_int - 2
       (here = 4611686018427387901) *)
    mutable prev : ('a element) option;
    mutable next : ('a element) option
  }

let new_id = B_utils.fresh_int ()

type 'a t = 'a element option (* None = empty chain *)


(* The only non trivial (fun) part of the implementation is to decide what
   "depth" should be attributed to an element when adding or inserting a new
   element in a chain.  In our implementation, the two directions "prev" and
   "next" are not symmetric. In a symmetric implementation, each insertion would
   cut the depth interval in two equal parts, hence, since max_int = 2^62, in
   the (very) worst case, we can roughly only insert 62 elements to a chain
   before we need to reattribute depths. Here we decide that "insert_after" is
   more common than "insert_before" -- we will use the Chains for graphic
   layers, and it's more usual to add a layer on top of the previous one rather
   than inserting a layer "below" an existing one.

   So, when appending a new element, we simply add a constant value to the
   depth: sqrt(max_int). Therefore we may append sqrt(max_int) elements in a
   row.  *)

let dx =
  if debug then 10
  else int_of_float (sqrt (float max_int))
(* Since max_int = 4611686018427387903 on a 64bits machine, dx = 2147483648 *)

let singleton value =
  Some {
    id = new_id ();
    value;
    depth = dx;
    prev = None;
    next = None }

let get_stack_id = function
  | None -> invalid_arg "[Chain.get_stack_id] Empty Chain has no stack id."
  | Some a -> a.id

(* same as Option.iter f o *)
let do_option o f = match o with
  | Some x -> f x
  | None -> ()

let next = function
  | None -> None
  | Some t -> t.next

let prev = function
  | None -> None
  | Some t -> t.prev

let value = function
  | None -> invalid_arg "[Chain.value] Empty chain has no value."
  | Some a -> a.value

let depth = function
  | None -> 0
  | Some a -> a.depth

let rec first = function
  | None -> None
  | Some a -> match a.prev with
    | None -> Some a
    | b -> first b

let rec last =  function
  | None -> None
  | Some a -> match a.next with
    | None -> Some a
    | b -> last b

let same_stack t1 t2 =
  match t1,t2 with
  | None, None
  | None, Some _
  | Some _ , None -> false
  | Some x1, Some x2 -> x1.id = x2.id

let comp (x:int) (y:int) = Stdlib.compare x y

let compare_elements x1 x2 =
  if x1.id <> x2.id
  then failwith "Cannot compare chains in different stacks"
  else comp x1.depth x2.depth

let compare t1 t2 =
  match t1, t2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some x1, Some x2 -> compare_elements x1 x2

let (==) t1 t2 =
  compare t1 t2 = 0

(* t1 >. t2 if [depth t1 > depth t2] (when in the same stack). So ">" means
   "deeper than". *)
let (>.) t1 t2 =
  compare t1 t2 > 0


let min t1 t2 =
  if t1 >. t2 then t2 else t1

(*(* We discard all empty chains when computing the minimum *)*)
(* match t1, t2 with
 * | None, None -> None
 * | Some _, None -> t1
 * | None, Some _ -> t2
 * | Some x1, Some x2 -> if compare_elements x1 x2 > 0 then t2 else t1 *)

let size t =
  let rec loop t i =
    match t with
    | None -> i
    | Some t -> loop t.next (i+1) in
  loop (first t) 0

let is_empty t =
  t = None

(* redistribute depth values *)
let evenize t =
  let dx = max_int / (size t + 2) in
  if dx = 0 then failwith "Chain too large" (* in principe this cannot happen *)
  else let rec loop d t =
    match t with
    | None -> ()
    | Some a -> a.depth <- d; loop (d+dx) a.next in
    loop dx (first t)

(* the return value points to the inserted element *)
let insert_after t value =
  let n = next t in
  let id, depth = match t with
    | None -> new_id (), dx
    | Some x -> match n with
      | None -> x.id, x.depth + dx
      | Some x' -> let d = x'.depth - x.depth in
        if d < 2 then raise Max_insert
        (* TODO: en fait on peut encore décaler le suivant ! *)
        else x.id, x.depth + d / 2 in
  let t' = Some { id; value; depth; prev = t; next = n} in
  B_utils.(printd debug_memory "New layer created with depth: %u\n" depth);
  do_option t (fun x -> x.next <- t');
  do_option n (fun x -> x.prev <- t');
  t'

let insert_after t value =
  try insert_after t value with
  | Max_insert ->
    B_utils.(printd debug_memory "Need to evenize chain...");
    evenize t; insert_after t value
  | e -> raise e

let insert_before t value =
  let p = prev t in
  let id, depth = match t with
    | None -> new_id (), dx
    | Some x ->
      let d' =
        match p with
        | None -> 0
        | Some x' -> x'.depth in
      let d = x.depth - d' in
      if d < 2 then raise Max_insert
      (* TODO: en fait on peut encore décaler le suivant ! *)
      else x.id, x.depth - d / 2 in
  let t' = Some { id; value; depth; prev = p; next = t } in
  B_utils.(printd debug_memory  "New layer created with depth: %u\n" depth);
  do_option t (fun x -> x.prev <- t');
  do_option p (fun x -> x.next <- t');
  t'

let insert_before t value =
  try insert_before t value with
  | Max_insert ->
    B_utils.(printd debug_memory "Need to evenize chain...");
    evenize t; insert_before t value
  | e -> raise e

let replace t value =
  match t with
  | None -> invalid_arg "[Chain.replace] Cannot set value to empty Chain."
  | Some a -> a.value <- value

(* [remove t] removes the element pointed by t in the stack and returns the next
   element. Then [t] becomes isolated and should be discarded. *)
(* not used *)
let remove = function
  | None -> invalid_arg "Cannot remove element of empty Chain."
  | Some a ->
     do_option a.prev (fun p -> p.next <- a.next);
     do_option a.next (fun n -> n.prev <- a.prev);
     a.prev <- None;
     let t = a.next in
     a.next <- None;
     t

(* return the ordered list of values of the whole stack. *)
let to_list t =
  let rec loop x list = match x with
    | None -> list
    | Some a -> loop a.prev (a.value :: list) in
  loop (last t) []

(* Create a chain from a list of values. The return value points to the last
   element of the list *)
let of_list list : 'a t =
  let id = new_id () in
  let t, _ =
    List.fold_left
      (fun (t, depth) value ->
        let t' = Some { id; value; depth; next = None; prev = t } in
        do_option t (fun b -> b.next <- t');
        (t', depth + dx))
      (None, dx) list in
  t

(* iter on values (not elements) starting from the given position *)
let rec iter_down f = function
  | None -> ()
  | Some a -> f a.value; iter_down f a.next

(* iter on values (not elements) of the whole chain *)
let iter f t =
  iter_down f (first t)

let rec iter_up f = function
  | None -> ()
  | Some a -> f a.value; iter_up f a.prev

let iter_up f t =
  iter_up f (last t)

(* iter on 'real elements' (no option) *)
let rec iter_elements_down f = function
  | None -> ()
  | Some a -> f a; iter_elements_down f a.next

let iter_elements f t =
  iter_elements_down f (first t)

let fill t value =
  iter_elements (fun t -> t.value <- value) t

let insert_chain_before ~dst t =
  iter (fun v -> ignore (insert_before dst v)) t
(* of course this could be done more efficiently = in constant time if we didn't
   have to compute depth, or if the depths of the subchain were strictly included
   between t and (next t) *)

let insert_chain_after ~dst t =
  iter_up (fun v -> ignore (insert_after dst v)) t

let print_depths t =
  iter_elements (fun a -> Printf.printf "depth=%d\n" a.depth) t

(* [copy t] returns a copy of the chain in a new stack, still pointing to the
   same (copied) element. *)
(* not used *)
let copy = function
  | None -> None
  | Some a0 ->
     let id = new_id () in
     let a0' = { a0 with id } in

     (* copy t=(Some a, Some b,...) into t'=(Some a', Some b', ...) *)
     let rec loop_down a' = function
       | None -> assert (a'.next = None)
       | Some b ->
          (* One could use [insert_after] but that function is a bit too general
             since here we know we append at the end of the stack.  *)
          let b' = { b with id; prev = Some a' } in
          a'.next <- Some b';
          loop_down b' b.next in

     let rec loop_up a' = function
       | None -> assert (a'.prev = None)
       | Some b ->
          (* Same remark as above, for [insert_before]. *)
          let b' = { b with id; next = Some a' } in
          a'.prev <- Some b';
          loop_up b' b.prev in

     loop_down a0' a0.next;
     loop_up a0' a0.prev;
     Some a0'

(* [copy_into ~dst:t s] copy the element pointed by [s] into a different stack
   [t] with keeping the depth. Its position is determined by its depth. Does
   nothing if the depth is already occupied. Return the copied element. Does not
   modify [s]. Does not copy the whole chain of [s]: see [copy] for this. [t]
   may be empty (then a new stack is created).

   This weird function can serve to extract a subchain from an existing chain,
   in order to move it to another stack: this is why we use it in Bogue for
   sending layouts to a different window. Another possibiliy would be to use
   [copy] and then remove the unwanted elements. Another possibility would be to
   get a list of elements we want to extract (removing doublons), sort it, and
   then use [of_list]. This does not preserve exact depths, but preserve the
   order. *)
let copy_into ~dst:t = function
  | None ->
    B_utils.(printd debug_warning "Copying an empty Chain has no effect.");
    None
  | Some a as s ->
    if same_stack s t
    then invalid_arg
        "[Chain.copy_into] Cannot copy a chain element into the same stack."
    else
      let rec search_position t0 = function
        | None -> t0, None
        | Some b as t1 ->
          if a.depth > b.depth then search_position t1 b.next
          else t0, t1 in  (* t0 < Some a <= t1 *)
      let id = match t with None -> new_id () | Some b -> b.id in
      let a' = { a with id } in
      match search_position None (first t) with
      | None, None ->
        a'.next <- None;
        a'.prev <- None;
        Some a'
      | Some a0 as t0, None ->
        a'.next <- None;
        a'.prev <- t0;
        a0.next <- Some a';
        Some a'
      | None, (Some a1 as t1) ->
        if a1.depth <> a.depth then
          begin
            a'.prev <- None;
            a'.next <- t1;
            a1.prev <- Some a';
            Some a'
          end else t1
      | Some a0 as t0, (Some a1 as t1) ->
        if a1.depth <> a.depth then
          begin
            let tt = Some a' in
            a'.prev <- t0;
            a'.next <- t1;
            a0.next <- tt;
            a1.prev <- tt;
            tt
          end else t1
