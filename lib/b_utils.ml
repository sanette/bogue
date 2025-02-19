(** Utilities *)
(* Function names should be unambiguous enough to make it safe to open this
   module anywhere. *)
open Tsdl

exception Sdl_error of string

let nop _ = ()

let debug =
  let d = try
      match Sys.getenv "BOGUE_DEBUG" |> String.capitalize_ascii with
      | "YES"
      | "1"
      | "TRUE" -> true
      | _ -> false
    with Not_found -> false (* set to false for production *)
       | e -> raise e in
  ref d

let log_channel = ref stdout
let close_log () = close_out !log_channel
let flush_log () = flush !log_channel

let debug_thread = 1
let debug_warning = 2
let debug_graphics = 4
let debug_error = 8
let debug_io = 16
let debug_memory = 32
let debug_board = 64
let debug_event = 128
let debug_custom = 256
let debug_user = 512 (* messages that can be of interest to the (non developer)
                        user *)
let debug_disable = 1024 (* use this to disable the debug message *)

let debug_code =
  ref (debug_error
       + debug_warning
       + debug_graphics
       + debug_thread
       + debug_io
       + debug_board
       + debug_memory
       + debug_event
       + debug_custom
       + debug_user)

(* debug_code := !debug_code lor debug_thread;; *)

(* let debug_code = ref 511;; *) (* everything *)

let debug_vars =
  [ "Thread", debug_thread;
    "Warning", debug_warning;
    "Graphics", debug_graphics;
    "ERROR", debug_error;
    "I/O", debug_io;
    "Memory", debug_memory;
    "Board", debug_board;
    "Event", debug_event;
    "Custom", debug_custom;
    "User", debug_user]

let debug_to_string =
  let debug_array = Array.of_list debug_vars in
  fun c ->
    let rec loop i n list =
      if i = 0 || n = 16 then list
      else let code = i land 1 in
        if code = 0 then loop (i lsr 1) (n+1) list
        else let s = if n > 0 && n < 11
               then fst debug_array.(n-1)
               else "Unknown" in
          loop (i lsr 1) (n+1) (s::list) in
    String.concat "; " (loop c 1 [])

(* Should we put this in a Var/Atomic? *)
(* TODO: use this to reduce the number of lock if there is no thread *)
let threads_created = ref 0

(* couleurs xterm, cf : http://linuxgazette.net/issue65/padala.html *)
let xterm_red  = "\027[0;31m"
let xterm_blue = "\027[0;94m"
let xterm_light_grey = "\027[1;30m"
let xterm_nc = "\027[0m"

let print s = Printf.ksprintf print_endline s

let invalid_arg s = Printf.ksprintf invalid_arg s

let print_debug_old s =
  Printf.ksprintf
    (fun s ->
       if !debug
       then print_endline
           (xterm_blue ^ "[" ^ (string_of_int (Int32.to_int (Sdl.get_ticks ()) mod 60000)) ^ "] : " ^ xterm_nc ^ s)) s

let debug_select_old code s =
  if !debug && (code land !debug_code <> 0)
  then print_endline (xterm_red ^ (debug_to_string code) ^ xterm_nc ^ ": " ^ s)

let iksprintf _f = Printf.ikfprintf (fun () -> ()) ()

(*  [printd] is used extensively to trace the execution of Bogue; the syntax is
    [printd some_debug_code "My sprintf style string, like %s=%d." (name_of x)
    (compute x)] It will output log messages only if [!debug] is true. However,
    all arguments are evaluated (like [name_of x] and [compute x] in the example
    above), no matter the value of !debug. If their evaluation is costly, one
    should rather write [if !debug then printd .... ] *)
let printd code =
  let debug = !debug && (code land !debug_code <> 0) in
  let printf = Printf.(if debug then ksprintf else iksprintf) in
  printf (fun s ->
      output_string !log_channel
        (xterm_blue ^
         "[" ^ (string_of_int (Int32.to_int (Sdl.get_ticks ()) mod 60000)) ^ "]" ^
         xterm_light_grey ^ "[" ^
         (string_of_int (Thread.id (Thread.self ()))) ^ "]" ^ xterm_nc ^ " :\t " ^
         xterm_nc ^ xterm_red ^ (debug_to_string code) ^ xterm_nc ^ ": "
         ^ s ^ "\n");
      if !log_channel = stdout then flush !log_channel)

let time_it name f x =
  let t = Sys.time () in f x;
  let dt = Sys.time () -. t in
  printd debug_custom "Timing [%s]: %f ms" name dt;
  dt

let itime_it name f x = ignore (time_it name f x)

(* check if string s starts with string sub *)
let startswith s sub =
  String.length sub = 0 || begin
      String.length s >= String.length sub &&
        String.sub s 0 (String.length sub) = sub
    end

(* create function for generating integers, starting from 1 *)
let fresh_int () =
  let id = ref 0 in
  fun () ->
  if !id < max_int then (incr id; !id)
  else failwith "Too many ids created!"

(* round float to nearest integer: *)
let round x =
  int_of_float (Float.round x)

let pi = Float.pi

let square x = x *. x

let rec pwr_old k x =
  assert (k>=0);
  if k = 0 then 1. else x *. (pwr_old (k-1) x)

let pwr k x = Float.pow x (float k)

(* Use Int.max and Int.min for ocaml >= 4.13 *)
let imax (x:int) (y:int) =
  if x > y then x else y

let imin (x:int) (y:int) =
  if x < y then x else y

let fmax = Float.max

let fmin = Float.min

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let go : 'a Tsdl.Sdl.result -> 'a = function
  | Error _ -> raise (Sdl_error ("SDL ERROR: " ^ (Sdl.get_error ())))
  | Ok r -> r

(* List utilities *)
(******************)

let list_iter list f = List.iter f list

(* Return an option containing the first element of the list for which the
   function f does not return None *)
let rec list_check f l =
  match l with
  | [] -> None
  | x::rest -> begin
      match f x with
      | None -> list_check f rest
      | s -> s
    end

(* Idem where the function f returns true *)
(* let list_check_ok f l = *)
(*   list_check (fun x -> if f x then Some x else None) l *)
(* This is now List.find_opt *)
let list_check_ok = List.find_opt

(* Return the first element of the list satisfying p, and its index *)
let list_findi p l =
  let rec loop i = function
    | [] -> None
    | a::rest -> if p a then Some (a, i)
      else loop (i+1) rest in
  loop 0 l

(* Split l into two lists l1rev,l2, where the first element of l2 is the first
   element of l for which f is true (l2 can be empty). We always have: l =
   List.rev_append l1rev l2. *)
let list_split_first_rev f l =
  let rec loop l1rev = function
    | [] -> l1rev, []
    | x::rest as l2 -> if f x then l1rev, l2
      else loop (x::l1rev) rest in
  loop [] l

let list_split_before l equal x =
  let l1rev, l2 = list_split_first_rev (equal x) l in
  List.rev l1rev, l2

(* Return l1rev, x, l2, where x is the first element for which f x = true and l
   is the concatenation of List.rev l1, x and l2 *)
let list_split_at_rev f l =
  match list_split_first_rev f l with
  | _, [] -> raise Not_found
  | l1, x::l2 -> l1, x, l2

(* Replace the first element for which f returns true by x *)
let list_replace f l x =
  let l1, _, l2 = list_split_at_rev f l in
  List.rev_append l1 (x :: l2)

(* returns the list where the first element for which f is true is removed *)
let list_remove_first f l =
  match list_split_first_rev f l with
  | _, [] -> raise Not_found
  | l1, _::l2 -> List.rev_append l1 l2

(* Splits a list atfer the xth element. (x=0 for first element; the first
   returned list has length x.) *)
let split_list_rev list x =
  let rec loop head tail i =
    if i >= x then (head, tail)
    else match tail with
         | [] -> printd debug_error
                   "Error: position too far in list"; raise Not_found
         | a::rest -> loop (a::head) rest (i+1) in
  loop [] list 0

let split_list list x =
  let daeh, tail = split_list_rev list x in
  List.rev daeh, tail

(* checks if 'a' contained in the list, with 'equal' function *)
let rec mem equal a list =
  match list with
    | [] -> false
    | b::rest -> equal a b || mem equal a rest

(* checks if all elements are different (using the 'equal' function) *)
(* not used, use "repeated" below instead *)
let rec injective equal list =
  match list with
    | [] -> true
    | a::rest -> if mem equal a rest then false else injective equal rest

(* Check if some element is repeated and return the first one. Note this is
   O(n²) in the worse case. One could use sort_uniq instead which should be O(n
   log n). *)
let rec repeated equal list =
   match list with
    | [] -> None
    | a::rest -> if mem equal a rest then Some a else repeated equal rest

(* max of a list *)
(* in case of equal elements, the *first* one is selected *)
let list_max compare list =
  match list with
  | [] -> None
  | a::rest ->
    Some (List.fold_left
            (fun max x ->
               (* printd debug_warning "Compare=%d" (compare x min); *)
               if compare x max > 0 then x else max)
            a rest)

let list_min compare list =
  match list with
  | [] -> None
  | a::rest ->
    Some (List.fold_left
            (fun min x ->
               if compare x min < 0 then x else min)
            a rest)

let rec list_last = function
  | [] -> printd debug_error "[list_last]: empty list"; raise Not_found
  | [x] -> x
  | _::rest -> list_last rest

let rec list_last_opt = function
  | [] -> None
  | [x] -> Some x
  | _::rest -> list_last_opt rest

(* Return the element following the first occurence of x, or None if x is the
   last element. It can be equal to x if x is repeated. *)
let rec list_next equal x = function
  | [] -> printd debug_error "[list_next]: empty list"; raise Not_found
  | [a] when equal a x -> None
  | [_] -> printd debug_error "[list_next] does not contain x"; raise Not_found
  | a::b::rest -> if equal a x then Some b else list_next equal x (b::rest)

let list_next_check equalx ~check list =
  let rec loop ok = function
    | []  -> if ok then None
      else (printd debug_error "[list_next_check]: empty list"; raise Not_found)
    | a::rest -> if ok && check a then Some a
      else loop (ok || equalx a) rest in
  loop false list

let test_list_next_check () =
  assert (list_next_check (Int.equal 4) ~check:(fun x -> x mod 3 = 0) [1;2;3;4;5;6;7;8;9]
          = Some 6);
  assert (list_next_check (Int.equal 6) ~check:(fun x -> x mod 3 = 0) [1;2;3;4;5;6;7;8;9]
          = Some 9);
  assert (list_next_check (Int.equal 9) ~check:(fun x -> x mod 3 = 0) [1;2;3;4;5;6;7;8;9]
          = None)

(* Return the element preceeding the first occurence of x, or None if x is the
   first element. It cannot be equal to x. *)
let rec list_prev equalx = function
  | [] -> printd debug_error "[list_prev]: empty list"; raise Not_found
  | a::_ when equalx a -> None
  | [_] -> printd debug_error "[list_prev] does not contain x"; raise Not_found
  | a::b::rest -> if equalx b then Some a else list_prev equalx (b::rest)

(* Return the last element that satisfy [check] BEFORE the first element that
   satisfy [equalx], or None. At lease one element must satisfy [equalx]
   otherwise it will raise Not_found. *)
let list_prev_check equalx ~check list =
  let rec loop last = function
    | [] -> printd debug_error "[list_prev_check]: empty list"; raise Not_found
    | x::_ when equalx x -> last
    | a::rest -> if check a then loop (Some a) rest else loop last rest in
  loop None list

let test_list_prev () =
  assert (list_prev ( Int.equal 2 ) [2;1;2;3] = None);
  let () = try ignore (list_prev ( Int.equal 1 ) [2]) with
    | Not_found -> ()
    | _ -> assert (false) in
  assert (list_prev ( Int.equal 1 ) [2;1] = Some 2);
  assert (list_prev ( Int.equal 3 ) [1;2;3] = Some 2)

let test_list_prev_check () =
  assert (list_prev_check (Int.equal 6) ~check:(fun x -> x mod 2 = 0) [1;2;3;4;5;6;7]
          = Some 4);
  assert (list_prev_check (Int.equal 5) ~check:(fun x -> x mod 2 = 0) [1;2;3;4;5;6;7]
          = Some 4);
  assert (list_prev_check (Int.equal 2) ~check:(fun x -> x mod 2 = 0) [1;2;3;4;5;6;7]
          = None)

let list_hd_opt = function [] -> None | a::_ -> Some a

let run f = f ()

let apply x f = f x

(* Monadic operations. Starting with ocaml 4.08 we can use the Option module.
   Warning, all arguments being evaluated even if not used, for costly argments
   it is better to use the original pattern matching | Some | None *)

exception None_option
(* used when the option should not be None. *)

(* let map_option o f = match o with
 *   | Some x -> Some (f x)
 *   | None -> None *)
let map_option o f = Option.map f o

(* let do_option o f = match o with
 *   | Some x -> f x
 *   | None -> () *)
let do_option o f = Option.iter f o

let apply_option fo x = match fo with
  | Some f -> f x
  | None -> ()

(* let check_option o f = match o with
 *   | Some x -> f x
 *   | None -> None *)
let check_option = Option.bind

(* Warning the "d" is always evaluated, so it's not always a good idea to use
   this...use the lazy or fn version instead.  *)
let default o d = match o with
  | Some x -> x
  | None -> d

let default_lazy o d = match o with
  | Some x -> x
  | None -> Lazy.force d

let default_fn o f = match o with
  | Some x -> x
  | None -> f ()

let default_option o od = match o with
  | None -> od
  | o -> o

let default_option_fn o f = match o with
  | None -> f ()
  | o -> o

let opt_map fo x = match fo with
  | Some f -> f x
  | None -> x

let map2_option o1 o2 f = match o1, o2 with
  | Some x1, Some x2 -> Some (f x1 x2)
  | _ -> None

let one_of_two o1 o2 = match o1, o2 with
  | None, None -> None
  | _, None -> o1
  | None, _ -> o2
  | _ -> printd debug_warning "one_of_two takes first of two options"; o1

let remove_option = function
  | Some x -> x
  | None -> raise None_option

let string_of_option f = function
  | Some x -> f x
  | None -> "None"

let (let@) f x = f x
(**  This can be used to write, for instance,
     [let@ x = Var.with_protect v in foo] instead of
     [Var.with_protect v (fun x -> foo)],
     where [foo] is any expression using [x].

     {b Warning:} the whole sequence of expressions is used. For instance
     [let@ x = Var.with_protect v in foo; bar]
     will use the function
     [x -> foo; bar]
     and hence is not the same as
     [Var.with_protect v (fun x -> foo); bar].
     It can be wise to write [begin let@ ... in .. end]

     See also https://github.com/ocaml/ocaml/pull/9887  *)

(* memo *)
(* standard memo fns. Don't use when the variable is mutable, it would store the
   old value for ever when the variable changes. *)

let memo ~name f =
  let store = Hashtbl.create 100 in
  fun x -> match Hashtbl.find_opt store x with
    | Some y -> y
    | None -> let result = f x in
      Hashtbl.add store x result;
      printd debug_memory "##Size of %s Hashtbl : %u" name (Hashtbl.length store);
      result

let memo2 f =
  let store = Hashtbl.create 100 in
  fun x y -> match Hashtbl.find_opt store (x,y) with
    | Some y -> y
    | None -> let result = f x y in
      Hashtbl.add store (x,y) result;
      printd debug_memory "##Size of Hashtbl2 : %u" (Hashtbl.length store);
      result

let memo3 f =
  let store3 = Hashtbl.create 100 in
  (fun x y z -> match Hashtbl.find_opt store3 (x,y,z) with
     | Some y -> y
     | None -> let result = f x y z in
       Hashtbl.add store3 (x,y,z) result;
       printd debug_memory "###Size of Hashtbl3 : %u" (Hashtbl.length store3);
       result),
  store3

(* inutile ? *)
let list_sum list =
  List.fold_left (+) 0 list

(* let find_file list_list = *)

let one_line_command_ouput command =
  try let s = Unix.open_process_in (command) in
    let res = try Some (input_line s) with _ -> None in
    begin match Unix.close_process_in s with
      | Unix.WEXITED 0 -> res
      | Unix.WEXITED 1 -> None (* in principle this is redundant since `res`
                                  is already None at this point *)
      | _ -> printd (debug_error + debug_io)
               "The `%s` command exited with error." command;
        None
    end
  with
  | _ -> printd (debug_error + debug_io) "Cannot use the `%s` command." command;
    None

(* Ocaml >= 4.13 *)
(* let string_starts_with ~prefix s = *)
(*   let len_s = length s *)
(*   and len_pre = length prefix in *)
(*   let rec aux i = *)
(*     if i = len_pre then true *)
(*     else if unsafe_get s i <> unsafe_get prefix i then false *)
(*     else aux (i + 1) *)
(*   in len_s >= len_pre && aux 0 *)

(* Result of [uname -s] for Unixes. Can be:
    "FreeBSD"
    "Linux"
    "Darwin"
    "NetBSD"
    "OpenBSD"
    "SunOS"
    "AIX"
    "HP-UX"
    "CYGWIN_NT-*" ?
    "MSYS_NT-**" ?
    "Windows_NT" (si uname est utilisé via un environnement Unix sur un système Windows).
    "Minix"
    "DragonFly"
   etc.
*)
let os_type =
  let res = ref None in fun () ->
    match !res with
    | Some s -> s
    | None -> let s = match Sys.os_type with
        | "Unix" -> begin
            match one_line_command_ouput "uname -s" with
            | None -> "Unix_unknown"
            | Some s -> s
          end
        | s -> s in
      res := Some s; s

let which command =
  (* BETTER: (specially for portability to WIN/MAC) use
     https://opam.ocaml.org/packages/fileutils/ *)
  let cmdline command =
    if Sys.win32 then
      "where " ^ command ^ " 2> NUL"
    else
      "which " ^ command ^ " 2>/dev/null"
  in
  one_line_command_ouput (cmdline command)
