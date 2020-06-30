(* bidirectional ordered chained lists (mutable) *)

let debug = !B_utils.debug;;

exception Max_insert;;

type 'a element =
  { id : int; (* identifies the connected component *)
    mutable value : 'a;
    mutable depth : int; (* depth is a redundant information, in order to get
                            faster comparison between chains. The rule is that
                            the .next element must have higher depth. A
                            consequence is that the number of elements cannot
                            exceed max_int - 2 (here = 4611686018427387901) *)
    mutable prev : ('a element) option;
    mutable next : ('a element) option
  }

let new_id = B_utils.fresh_int ();;

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
  else int_of_float (sqrt (float max_int));;
(* Since max_int = 4611686018427387903 on a 64bits machine, dx = 2147483648 *)

let singleton value =
  Some {
    id = new_id ();
    value;
    depth = dx;
    prev = None;
    next = None }

let do_option o f = match o with
  | Some x -> f x
  | None -> ();;

let next = function
  | None -> None
  | Some t -> t.next;;

let prev = function
  | None -> None
  | Some t -> t.prev;;

let value = function
  | None -> raise Not_found
  | Some a -> a.value;;

let depth = function
  | None -> raise Not_found
  | Some a -> a.depth;;

let rec first = function
  | None -> raise Not_found
  | Some a -> match a.prev with
    | None -> Some a
    | b -> first b;;

let rec last =  function
  | None -> raise Not_found
  | Some a -> match a.next with
    | None -> Some a
    | b -> last b;;

let same_component t1 t2 =
  match t1,t2 with
  | None, None
  | None, Some _
  | Some _ , None -> true
  | Some x1, Some x2 -> x1.id = x2.id;;

let comp (x:int) (y:int) = Stdlib.compare x y;;

let compare t1 t2 =
  match t1, t2 with
  | None, None -> (* print_endline "None to compare"; *) 0
  | Some _, None
  | None, Some _ -> raise Not_found
  | Some x1, Some x2 -> (
      if x1.id <> x2.id then failwith "Cannot compare chains in different \
                                      components"
      (* print_endline (Printf.sprintf "depths=%d,%d" x1.depth x2.depth); *)
      else comp x1.depth x2.depth);;

let (==) t1 t2 =
  compare t1 t2 = 0;;

(* t1 > t2 if t1.depth > t2.depth. So ">" means "deeper than". *)
let (>) t1 t2 =
  compare t1 t2 > 0;;

let size t =
  let rec loop t i =
    match t with
    | None -> i
    | Some t -> loop t.next (i+1) in
  loop (first t) 0;;


(* redistribute depth values *)
let evenize t =
  let dx = max_int / (size t + 2) in
  if dx = 0 then failwith "Chain too large" (* in principe this cannot happen *)
  else let rec loop d t =
    match t with
    | None -> ()
    | Some a -> a.depth <- d; loop (d+dx) a.next in
    loop dx (first t);;

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
  t';;

let insert_after t value =
  try insert_after t value with
  | Max_insert -> B_utils.(printd debug_memory "Need to evenize chain...");
                 evenize t; insert_after t value
  | e -> raise e;;

let insert_before t value =
  let p = prev t in
  let id, depth = match t with
    | None -> new_id (), dx
    | Some x -> let d' =
      match p with
      | None -> 0
      | Some x' -> x'.depth in
      let d = x.depth - d' in
      if d < 2 then raise Max_insert
      (* TODO: en fait on peut encore décaler le suivant ! *)
      else x.id, x.depth - d / 2 in
  let t' = Some { id; value; depth; prev = p; next = t } in
  Printf.printf "New layer created with depth: %u\n" depth;
  do_option t (fun x -> x.prev <- t');
  do_option p (fun x -> x.next <- t');
  t';;

let insert_before t value =
  try insert_before t value with
  | Max_insert -> B_utils.(printd debug_memory "Need to evenize chain...");
                 evenize t; insert_before t value
  | e -> raise e;;


let replace t value =
  match t with
  | None -> raise Not_found
  | Some a -> a.value <- value;;

let to_list t =
  let rec loop x list = match x with
    | None -> list
    | Some a -> loop a.prev (a.value :: list) in
  loop (last t) [];;

(* the return value points to the last element of the list *)
let of_list list : 'a t =
  let id = new_id () in
  let t, _ =
    List.fold_left
      (fun (t, depth) value -> let t' = Some { id; value; depth; next = None; prev = t } in
        do_option t (fun b -> b.next <- t');
        (t', depth + dx))
      (None, dx) list in
  t;;

(* iter on values (not elements) starting from the given position *)
let rec iter_down f = function
  | None -> ()
  | Some a -> f a.value; iter_down f a.next;;

(* iter on values (not elements) of the whole chain *)
let iter f t =
  iter_down f (first t);;

let rec iter_up f = function
  | None -> ()
  | Some a -> f a.value; iter_up f a.prev;;

let iter_up f t =
  iter_up f (last t);;

(* iter on 'real elements' (no option) *)
let rec iter_elements_down f = function
  | None -> ()
  | Some a -> f a; iter_elements_down f a.next;;

let iter_elements f t =
  iter_elements_down f (first t);;

let fill t value =
  iter_elements (fun t -> t.value <- value) t;;

let insert_chain_before ~dst t =
  iter (fun v -> ignore (insert_before dst v)) t;;
(* of course this could be done more efficiently = in constant time if we didn't
   have to compute depth, or if the depths of the subchain were strictly included
   between t and (next t) *)

let insert_chain_after ~dst t =
  iter_up (fun v -> ignore (insert_after dst v)) t;;

let print_depths t =
  iter_elements (fun a -> Printf.printf "depth=%d\n" a.depth) t;;
