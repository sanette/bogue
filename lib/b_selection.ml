(* dealing with sets of range of integers *)
(* this is used by table.ml *)

type selected =
    | Range of (int * int)
                 
type t = selected list

let empty = []
            
let compare (Range (r1,_)) (Range (r2,_)) =
  compare r1 r2
          
let sort sel =
  List.sort compare sel

(* first step before simplify: remove "syntax errors" *)
let sanitize sel =
  let rec loop sl new_sl =
    match sl with
    | [] -> List.rev new_sl
    | Range (i1,i2)::rest when i2 < i1 -> loop rest new_sl
    | s::rest -> loop rest (s::new_sl) in
  loop sel []
    
(* Returns a normalized (sorted and unique) selection list with minimal number
   of elements. Such a simplifed list is always strictly increasing *)
let simplify sel =
  (* this loop *assumes* sl is sorted *)
  let rec loop current sl new_sl =
    match current, sl with
    | _,[] -> List.rev (current::new_sl)
    | Range (i1,i2), Range (j1,j2)::rest ->
       if i2+1 < j1 then loop (Range (j1,j2)) rest (current::new_sl)
       else loop (Range (i1, max j1 j2)) rest new_sl
  in
  match sort (sanitize sel) with
  | [] -> []
  | first::rest -> loop first rest []

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
  print_endline "REMOVE";
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
  print_endline "ADD";
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
  if mem sel i then remove sel i else add sel i;;

(* faster if sel1 is already sorted (?) *)
let union sel1 sel2 =
  simplify (List.rev_append sel2 sel1);;
  
let iter f sel =
  let rec loop sl =
    match sl with
    | [] -> ()
    | Range (i1,i2)::rest when i2<i1 -> loop rest
    | Range (i1,i2)::rest -> f i1; loop (Range (i1+1,i2)::rest)
  in
  loop sel

let sprint_entry (Range (i1,i2)) =
  if i1=i2 then string_of_int i1
  else Printf.sprintf "%d-%d" i1 i2;;

let sprint sel =
  List.map sprint_entry sel
  |> String.concat ", "
  |> Printf.sprintf "{%s}";;
