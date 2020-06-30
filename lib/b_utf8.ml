(** Basic UTF8 encoding *)

let uencode c =
  if c < 128 then Bytes.make 1 (Char.chr c)
  else if c < 2048 then let s = Bytes.make 2 '\000' in
    let c1 = 192 lor (c lsr 6) in
    let c2 = (c land 63) lor 128 in
    (Bytes.set s 0 (Char.chr c1);
     Bytes.set s 1 (Char.chr c2);
     s)
  else if c < 65536 then let s = Bytes.make 3 '\000' in
    let c1 = 224 lor (c lsr 12) in
    let c2 = 128 lor ((c lsr 6) land 63) in
    let c3 = 128 lor (c land 63) in
    (Bytes.set s 0 (Char.chr c1);
     Bytes.set s 1 (Char.chr c2);
     Bytes.set s 2 (Char.chr c3);
     s)
  else failwith "Not implemented";;


(** Split utf8 string into list of string letters. Does not work for decomposed
   forms. (the letter and the accent will be separated) *)

(* Find size of utf8 letter starting at position i *)
let bytes_first_letter s i =
  if i >= String.length s then 0
  else let c = Char.code (s.[i]) in
    if c lsr 7 = 0 then 1
    else if c lsr 5 = 6 then 2
    else if c lsr 4 = 14 then 3
    else if c lsr 3 = 30 then 4
    else if c lsr 6 = 2 then failwith
        (Printf.sprintf "Wrong UTF8 at position %d. The string was probably cut \
                         at a wrong position." i)
    else failwith (Printf.sprintf "Wrong UTF8 at position %d." i)

let split s =
  let l = String.length s in
  let rec loop i acc =
    if i = l then acc
    else let n = bytes_first_letter s i in
      loop (i+n) ((String.sub s i n)::acc)
  in
  List.rev (loop 0 []);;
