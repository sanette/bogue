(* This file is part of BOGUE, by San Vu Ngoc *)

(* Dealing with sets of range of integers *)

(* This is used by table.ml. Another interesting application is to detect rows
   and colums in layout.ml *)

(* This module is self-contained and can be used outside Bogue *)

(* Example: (obtained atfer evalulating this buffer, so no types are hidden)

# let r = union [Range (2,6); Range (12,14)] [Range (4,10)]
val r : selected list = [Range (2, 10); Range (12, 14)]

# iter (Printf.printf "[%i]") r
[2][3][4][5][6][7][8][9][10][12][13][14]- : unit = ()

*)

(* TODO check normalization with type system? cf phantom types
https://www.dicosmo.org/CourseNotes/pfav/1314/cours5.handout.pdf
*)

type selected =
    | Range of (int * int)

(* This type has to be made private because only normalized lists are allowed *)
type t = selected list

let empty = []

let card = List.length

let is_empty sel = sel = []

let single_range (a,b) = Range (a,b)
let of_range (Range (a,b)) = (a,b)
let range (a,b) = [single_range (a,b)]

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
  List.map single_range list
  |> normalize

let proj1 (Range (r1, _)) = r1
let proj2 (Range (_, r2)) = r2

(* min element. *)
let first_unsorted = function
  | [] -> invalid_arg "[Selection.first_unsorted] selection should not be empty."
  | Range (r1, _) :: rest ->
    List.map proj1 rest
    |> List.fold_left min r1

let first = function
  | [] -> invalid_arg "[Selection.first] selection should not be empty."
  | Range (r1, _) ::_ -> r1

let last_unsorted = function
  | [] -> invalid_arg "[Selection.last_unsorted] selection should not be empty."
  | Range (_, r2) :: rest ->
    List.map proj2 rest
    |> List.fold_left max r2

let rec list_last = function
  | [] -> invalid_arg "[list_last]: empty list"
  | [x] -> x
  | _::rest -> list_last rest

let last sel = proj2 (list_last sel)

(* Number of selected integers. For normalized selections only *)
let size sel =
  let rec loop s = function
    | [] -> s
    | Range (i1,i2) :: rest -> loop (i2 + 1 - i1 + s) rest in
  loop 0 sel

(* Check if item i is selected *)
(* sl doesn't need to be sorted or simplified, but it would be faster if it
   was *)
let mem sel i =
  let rec loop sl =
    match sl with
    | [] -> false
    | Range (i1,i2) :: rest -> if i1<=i && i<=i2 then true else loop rest
  in
  loop sel

(* Removes an entry from the selection *)
(* works only on simplified lists, returns a simplified list *)
let remove sel i =
  let rec loop sl new_sl =
    match sl with
    | [] -> List.rev new_sl
    | Range (i1,i2) :: rest when i=i1 && i=i2 ->
       List.rev_append new_sl rest
    | Range (i1,i2) :: rest when i=i1 ->
       List.rev_append new_sl (Range (i1+1,i2) :: rest)
    | Range (i1,i2) :: rest when i=i2 ->
       List.rev_append new_sl (Range (i1,i2-1) :: rest)
    | Range (i1,i2) :: rest when i1<i && i<i2 ->
       List.rev_append new_sl (Range (i1,i-1) :: (Range (i+1,i2) :: rest))
    | Range (i1,i2) :: rest -> loop rest (Range (i1,i2) :: new_sl)
  in
  loop sel []

(* Adds an entry to the selection *)
(* if the selection was simplified, the result is also simplified *)
let add sel i =
  let rec loop sl new_sl =
    match sl with
    | [] -> List.rev (Range (i,i) :: new_sl)
    | Range (i1,_) :: _ when i+1<i1 ->
       List.rev_append new_sl ((Range (i,i)) :: sl)
    | Range (i1,i2) :: rest when i+1=i1 ->
       List.rev_append new_sl (Range (i,i2) :: rest)
    | Range (i1,i2) :: (Range (j1,j2)) :: rest when i=i2+1 && j1=i2+2 ->
       List.rev_append new_sl (Range (i1,j2) :: rest)
    | Range (i1,i2) :: rest when i=i2+1 ->
       List.rev_append new_sl (Range (i1,i) :: rest)
    | Range (i1,i2) :: _ when i1<=i && i<=i2 ->
       sel (* List.rev_append new_sl sl *)
    | r :: rest ->
       loop rest (r :: new_sl)
  in
  loop sel []

let toggle sel i =
  if mem sel i then remove sel i else add sel i

(* complement of [sel] within [first, last] *)
let invert ~first ~last sel =
  let rec loop inv mn = function
    | _ when mn > last -> List.rev inv
    | [] -> List.rev (Range (mn, last) :: inv)
    | Range (a,_) :: _ when a > last -> loop inv mn []
    | Range (_,b) :: rest when b < mn -> loop inv mn rest
    | Range (_,b) :: rest when b = mn -> loop inv (b+1) rest
    | Range (a,b) :: rest when a > mn -> loop (Range (mn, a-1) :: inv) (b+1) rest
    | Range (_,b) :: rest -> loop inv (b+1) rest in
  loop [] first sel

(* Simple union, not optimized because it also works with non normalized
   entries *)
let union_brute sel1 sel2 =
  normalize (List.rev_append sel2 sel1)

(* Faster (see tests) union for normalized entries. Not tail recursive. *)
let rec union sel1 sel2 =
  match sel1, sel2 with
  | [], _ -> sel2
  | _, [] -> sel1
  | (Range (r1,r2)) :: rest1, (Range (s1,s2)) :: rest2 ->
    if r1 > s1 then union sel2 sel1 (* we make sure r1 <= s1 *)
    else if r2 < s1-1 then Range (r1,r2) :: union rest1 sel2
    else if s2 <= r2 then union sel1 rest2
    else union (Range (r1,s2) :: rest2) rest1

(* Not tail recursive. Works only for normalized. *)
let rec intersect sel1 sel2 =
  match sel1, sel2 with
  | [], _
  | _, [] -> []
  | (Range (r1,r2)) :: rest1, (Range (s1,s2)) :: rest2 ->
    if r1 > s1 then intersect sel2 sel1 (* we make sure r1 <= s1 *)
    else if r2 < s1 then intersect rest1 sel2
    else if r2 <= s2 then Range (s1,r2) :: intersect rest1 sel2
    else Range (s1,s2) :: intersect sel1 rest2

let minus sel1 sel2 =
  if sel1 = [] then [] else
    let i1 = first sel1 in
    let i2 = last sel1 in
    intersect sel1 (invert ~first:i1 ~last:i2 sel2)

(* Is [sel1] contained in [sel2]? *)
let contains sel1 sel2 =
  intersect sel1 sel2 = sel1

let ( <<= )  = contains

let iter (f : int -> unit) sel =
  let rec loop sl =
    match sl with
    | [] -> ()
    | Range (i1,i2) :: rest when i2 < i1 -> assert (i1 = i2+1); loop rest
    | Range (i1,i2) :: rest -> f i1; loop (Range (i1+1, i2) :: rest)
  in
  loop sel

let fold f sel x0 =
  let rec loop acc sl =
    match sl with
    | [] -> acc
    | Range (i1,i2) :: rest when i2 < i1 -> assert (i1 = i2+1); loop acc rest
    | Range (i1,i2) :: rest -> loop (f i1 acc) (Range (i1+1, i2) :: rest)
  in
  loop x0 sel

let sprint_entry (Range (i1,i2)) =
  if i1=i2 then string_of_int i1
  else Printf.sprintf "%d..%d" i1 i2

let sprint sel =
  List.map sprint_entry sel
  |> String.concat ", "
  |> Printf.sprintf "{%s}"

(****************)
(* Some tests. Need #require "unix";; in the toplevel *)

module Naive = struct
  (** A naive but robust and easy to check implementation, serves for
      testing. Works only for non-negative integer. *)
  type t = bool array
  let empty = [||]
  let is_empty a = not (Array.mem true a)
  let size a = Array.fold_left (fun s b -> if b then s + 1 else s) 0 a
  let to_list a =
    let rec loop list start i =
      if i >= Array.length a then match start with
        | None -> List.rev list
        | Some i0 -> List.rev ((i0, i-1)::list)
      else match start, a.(i) with
        | None, false -> loop list None (i+1)
        | None, true -> loop list (Some i) (i+1)
        | Some _, true -> loop list start (i+1)
        | Some i0, false -> loop ((i0, i-1) :: list) None (i+1) in
    loop [] (if a.(0) = true then Some 0 else None) 0
  let to_sel a = of_list (to_list a)
  let find_index p a = (* Ocaml 5.1 *)
    let n = Array.length a in
    let rec loop i =
      if i = n then None
      else if p (Array.unsafe_get a i) then Some i
      else loop (succ i) in
    loop 0
  let first a = Option.get (find_index (fun b -> b) a)
  let last a =
    let rec loop i =
      if i < 0 then None else if a.(i) then Some i else loop (i-1) in
    Option.get (loop (Array.length a - 1))
  let of_list list =
    let last = List.map snd list |> List.fold_left max 0 in
    let a = Array.make (last+1) false in
    let rec loop = function
      | [] -> ()
      | (x, y) :: rest -> for i = x to y do a.(i) <- true done; loop rest in
    loop list; a
  let invert ~first ~last a =
    Array.init (Array.length a) (fun i -> i >= first && i <= last && not a.(i))
  let union a b =
    let na = Array.length a and nb = Array.length b in
    let a,b,na,nb = if na < nb then a,b,na,nb else b,a,nb,na in
    Array.init nb (fun i -> if i < na then a.(i) || b.(i) else b.(i))
  let intersect a b =
    let a,b = if Array.length a < Array.length b then a,b else b,a in
    Array.init (Array.length a) (fun i -> a.(i) && b.(i))
  let add a i =
    let n = Array.length a in
    let l = max (i+1) n in
    Array.init l (fun j -> j=i || (j < n && a.(i)))
  let remove a i =
    Array.init (Array.length a) (fun j -> a.(i) && not (j=i))
  let toggle a i =
    let n = Array.length a in
    let l = max (i+1) n in
    Array.init l (fun j -> if j < n then if i <> j then a.(j) else not a.(j) else i = j)
  let random n =
    Array.init n (fun _ -> Random.bool ())

end

module Test = struct
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
      | x :: rest -> if i = j then x, List.rev_append acc rest
        else loop (j+1) (x :: acc) rest in
    loop 0 [] l

  let shuffle list =
    let rec loop acc len = function
      | [] -> acc
      | l -> let i = Random.int len in
        let x, rest = list_remove l i in
        loop (x :: acc) (len-1) rest in
    loop [] (List.length list) list

  let time name f =
    let t0 = Unix.gettimeofday () in
    let y = f () in
    Printf.printf "Time %s = %f\n" name (Unix.gettimeofday () -. t0);
    y

  let to_naive t = Naive.of_list (to_list t)

  let print sel =
    print_endline (Printf.sprintf
                     "Using selection with range [%i,%i], size=%i, and %i \
                      components." (first sel) (last sel) (size sel) (card sel))

  let test () =
    let open Printf in
    for maxi_factor = 1 to 4 do
      print_endline (sprintf "Selection Test %i" maxi_factor);
      let maxi = 1000 * int_of_float (10. ** (float maxi_factor)) in
      let len, gap = 1000, 100 in
      let r = time "random" (fun () -> random len gap maxi) in
      assert (r = normalize r);
      assert (r = Naive.to_sel (to_naive r));
      let rs = shuffle r in
      sprintf "Size r = %i, size rs = %i" (size r) (size rs) |> print_endline;
      (* attention shuffle ne va jamais créer des overlaps... *)
      let rn = time "normalize shuffle" (fun () -> normalize rs) in
      assert (r = rn);
      let bad = random ~bad:(gap/2) len gap maxi |> shuffle in
      print bad;
      let bn = time "normalize bad" (fun () -> normalize bad) in
      assert (bn = normalize bn);
      print_endline "Test first";
      assert (first bn = first_unsorted bad);
      let s = random len gap maxi in
      let u1 = time "union" (fun () -> union r s) in
      let u2 = time "union_brute" (fun () -> union_brute r s) in
      assert (u1 = u2);
      let rn = to_naive r and sn = to_naive s in
      let un = time "union Naive" (fun () -> Naive.union rn sn) in
      assert (u1 = Naive.to_sel un);
      let i = time "intersect" (fun () -> intersect r s) in
      assert (intersect i r = intersect i s);
      let inn = time "intersect Naive" (fun () -> Naive.intersect rn sn) in
      assert (i = Naive.to_sel inn);
      print_endline "Test toggle";
      let x = Random.int maxi in
      assert (r = toggle (toggle r x) x);
      let rn = to_naive r in
      assert (time "Toggle" (fun () -> toggle r x) =
              Naive.to_sel (time "Toggle Naive" (fun () -> Naive.toggle rn x)));
      let first = Random.int maxi and last = Random.int maxi in
      print_endline (sprintf "Invert range [%i, %i]:" first last);
      let iv = time "Invert" (fun () -> invert ~first ~last r) in
      let ivn = time "Invert Naive" (fun () -> Naive.invert ~first ~last rn) in
      assert (iv = Naive.to_sel ivn);
      let rdn = Naive.random maxi in
      let rd = Naive.to_sel rdn in
      print rd;
      let ird = time "Invert bad random" (fun () -> invert ~first ~last rd) in
      let irdn = time "Invert bad random Naive"
          (fun () -> Naive.invert ~first ~last rdn) in
      assert (Naive.to_sel irdn = ird);
      let e = ref empty in
      let t = time "random" (fun () -> random 1000 100 100000) in
      (* pas trop grand sinon c'est lent...*)
      time "iter" (fun () -> iter (fun i -> e := add !e i) t);
      assert (!e = t);
      print_endline (sprintf "Selection Test %i passed OK.\n" maxi_factor);
    done


end
