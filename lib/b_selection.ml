(* Dealing with sets of range of integers -- San Vu Ngoc *)

(* This is used by table.ml. Another interesting application is to detect rows
   and colums in layout.ml *)

(* This module is self-contained and can be used outside Bogue *)

(* Example: (obtained atfer evalulating this buffer, so no types are hidden)

# let r = union [Range (2,6); Range (12,14)] [Range (4,10)]
val r : selected list = [Range (2, 10); Range (12, 14)]

# iter (Printf.printf "[%i]") r
[2][3][4][5][6][7][8][9][10][12][13][14]- : unit = ()

*)

type selected =
    | Range of (int * int)

(* This type has to be made private because only normalized lists are allowed *)
type t = selected list

let empty = []

let card = List.length

let range (a,b) = Range (a,b)
let of_range (Range (a,b)) = (a,b)

let to_list sel =
  List.map of_range sel

let compare (Range (r1,_)) (Range (r2,_)) =
  Stdlib.compare r1 r2

let sort sel =
  List.sort compare sel

(* first step before normalize: remove "syntax errors" *)
let sanitize sel =
  let rec loop sl new_sl =
    match sl with
    | [] -> List.rev new_sl
    | Range (i1,i2)::rest when i2 < i1 -> loop rest new_sl
    | s::rest -> loop rest (s::new_sl) in
  loop sel []

(* Returns a normalized (sorted and unique) selection list with minimal number
   of elements. Such a simplifed list is always strictly increasing.  *)
let normalize sel =
  (* this loop *assumes* sl is sorted *)
  let rec loop current sl new_sl =
    match current, sl with
    | _, [] -> List.rev (current::new_sl)
    | Range (i1,i2), Range (j1,j2)::rest ->
      if i2+1 < j1 then loop (Range (j1,j2)) rest (current::new_sl)
      else loop (Range (i1, max i2 j2)) rest new_sl
  in
  match sort (sanitize sel) with
  | [] -> []
  | first::rest -> loop first rest []

let of_list list =
  List.map range list
  |> normalize

let proj1 (Range (r1, _)) = r1
let proj2 (Range (_, r2)) = r2

(* min element. *)
let first_unsorted = function
  | [] -> invalid_arg "[Selection.first_unsorted] selection should not be empty."
  | (Range (r1, _)) :: rest ->
    List.map proj1 rest
    |> List.fold_left min r1

let first = function
  | [] -> invalid_arg "[Selection.first] selection should not be empty."
  | (Range (r1, _))::_ -> r1

let last = function
  | [] -> invalid_arg "[Selection.last] selection should not be empty."
  | (Range (_, r2)) :: rest ->
    List.map proj2 rest
    |> List.fold_left max r2

(* Check if item i is selected *)
(* sl doesn't need to be sorted or simplified, but it would be faster if it
   was *)
let mem sel i =
  let rec loop sl =
    match sl with
    | [] -> false
    | Range (i1,i2)::rest -> if i1<=i && i<=i2 then true else loop rest
  in
  loop sel

(* Removes an entry from the selection *)
(* works only on simplified lists, returns a simplified list *)
let remove sel i =
  let rec loop sl new_sl =
    match sl with
    | [] -> List.rev new_sl
    | Range (i1,i2)::rest when i=i1 && i=i2 ->
       List.rev_append new_sl rest
    | Range (i1,i2)::rest when i=i1 ->
       List.rev_append new_sl (Range (i1+1,i2)::rest)
    | Range (i1,i2)::rest when i=i2 ->
       List.rev_append new_sl (Range (i1,i2-1)::rest)
    | Range (i1,i2)::rest when i1<i && i<i2 ->
       List.rev_append new_sl (Range (i1,i-1)::(Range (i+1,i2)::rest))
    | Range (i1,i2)::rest -> loop rest (Range (i1,i2)::new_sl)
  in
  loop sel []

(* Adds an entry to the selection *)
(* if the selection was simplified, the result is also simplified *)
let add sel i =
  let rec loop sl new_sl =
    match sl with
    | [] -> List.rev (Range (i,i)::new_sl)
    | Range (i1,_)::_ when i+1<i1 ->
       List.rev_append new_sl ((Range (i,i))::sl)
    | Range (i1,i2)::rest when i+1=i1 ->
       List.rev_append new_sl (Range (i,i2)::rest)
    | Range (i1,i2)::(Range (j1,j2))::rest when i=i2+1 && j1=i2+2 ->
       List.rev_append new_sl (Range (i1,j2)::rest)
    | Range (i1,i2)::rest when i=i2+1 ->
       List.rev_append new_sl (Range (i1,i)::rest)
    | Range (i1,i2)::_ when i1<=i && i<=i2 ->
       List.rev_append new_sl sl
    | r::rest ->
       loop rest (r::new_sl)
  in
  loop sel []

let toggle sel i =
  if mem sel i then remove sel i else add sel i

(* Simple union, not optimized because it also works with non normalized
   entries *)
let union_brute sel1 sel2 =
  normalize (List.rev_append sel2 sel1)

(* Faster (see tests) union for normalized entries. Not tail recursive. *)
let rec union sel1 sel2 =
  match sel1, sel2 with
  | [], _ -> sel2
  | _, [] -> sel1
  | (Range (r1,r2))::rest1, (Range (s1,s2))::rest2 ->
    if r1 > s1 then union sel2 sel1 (* we make sure r1 <= s1 *)
    else if r2 < s1-1 then Range (r1,r2) :: union rest1 sel2
    else if s2 <= r2 then union sel1 rest2
    else union (Range (r1,s2) :: rest2) rest1

(* Not tail recursive. Works only for normalized. *)
let rec intersect sel1 sel2 =
  match sel1, sel2 with
  | [], _
  | _, [] -> []
  | (Range (r1,r2))::rest1, (Range (s1,s2))::rest2 ->
    if r1 > s1 then intersect sel2 sel1 (* we make sure r1 <= s1 *)
    else if r2 < s1 then intersect rest1 sel2
    else if r2 <= s2 then Range (s1,r2) :: intersect rest1 sel2
    else Range (s1,s2) :: intersect sel1 rest2

(* Is [sel1] contained in [sel2]? *)
let contains sel1 sel2 =
  intersect sel1 sel2 = sel1

let (<<=) = contains

let iter (f : int -> unit) sel =
  let rec loop sl =
    match sl with
    | [] -> ()
    | Range (i1,i2)::rest when i2<i1 -> loop rest
    | Range (i1,i2)::rest -> f i1; loop (Range (i1+1,i2)::rest)
  in
  loop sel

let sprint_entry (Range (i1,i2)) =
  if i1=i2 then string_of_int i1
  else Printf.sprintf "%d..%d" i1 i2

let sprint sel =
  List.map sprint_entry sel
  |> String.concat ", "
  |> Printf.sprintf "{%s}"

(****************)
(* Some tests. Need #require "unix";; in the toplevel *)


(* Create a random normalized selection within the interval [0, maxi] with
   average start [maxi]/4, average [gap]+1 between ranges, and average [len]gth
   of ranges. Use bad >0 to create non-normalized ones. *)
let random ?(bad = 0) len gap maxi =
  let gap = max 1 gap in
  let len = max 0 len in
  let rec loop x list =
    let r1 = x + 1 + Random.int (2*gap-1) - bad in
    let r2 = r1 + Random.int (2*len+1) in
    if r2 > maxi then list
    else loop (r2+1) (Range (r1, r2) :: list) in
  List.rev (loop (Random.int (maxi/2+1)) [])

(* return (ieth element, list without that element) *)
let list_remove l i =
  let rec loop j acc = function
    | [] -> invalid_arg "[Selection.list_remove] selection should not be empty."
    | x::rest -> if i = j then x, List.rev_append acc rest
      else loop (j+1) (x::acc) rest in
  loop 0 [] l

let shuffle list =
  let rec loop acc len = function
    | [] -> acc
    | l -> let i = Random.int len in
      let x, rest = list_remove l i in
      loop (x::acc) (len-1) rest in
  loop [] (List.length list) list

let time name f =
  let t0 = Unix.gettimeofday () in
  let y = f () in
  Printf.printf "Time %s = %f\n" name (Unix.gettimeofday () -. t0);
  y

let test () =
  let len, gap, maxi = 1000, 100, 10000000 in
  let r = time "random" (fun () -> random len gap maxi) in
  assert (r = normalize r);
  let rs = shuffle r in
  (* attention shuffle ne va jamais créer des overlaps... *)
  let rn = time "normalize shuffle" (fun () -> normalize rs) in
  assert (r = rn);
  let bad = random ~bad:(gap/2) len gap maxi |> shuffle in
  let bn = time "normalize bad" (fun () -> normalize bad) in
  assert (bn = normalize bn);
  print_endline "Test first";
  assert (first bn = first_unsorted bad);
  let s = random len gap maxi in
  let u1 = time "union" (fun () -> union r s) in
  let u2 = time "union_brute" (fun () -> union_brute r s) in
  assert (u1 = u2);
  let i = time "intersect" (fun () -> intersect r s) in
  assert (intersect i r = intersect i s);
  print_endline "Test toggle";
  let x = Random.int maxi in
  assert (r = toggle (toggle r x) x);
  print_endline "Test iter";
  let e = ref empty in
  let t = random 1000 100 100000 in (* pas trop grand sinon c'est lent...*)
  iter (fun i -> e := add !e i) t;
  assert (!e = t);
  print_endline "Selection Test passed OK."
