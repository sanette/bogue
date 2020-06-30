(** Utilities *)
(* Function names should be unambiguous enough to make it safe to open this
   module anywhere. *)
open Tsdl
open Result

exception Sdl2_error of string

let nop _ = ();;
             
let debug = ref false;; (* set to false for production *)
let debug_thread = 1;;
let debug_warning = 2;;
let debug_graphics = 4;;
let debug_error = 8;;
let debug_io = 16;;
let debug_memory = 32;;
let debug_board = 64;;
let debug_event = 128;;
let debug_custom = 256;;

let debug_code = ref ((*debug_thread +*) debug_warning (* + debug_graphics *) + debug_error + debug_io + debug_board (* + debug_memory *) + debug_event + debug_custom);;

(* debug_code := !debug_code lor debug_thread;; *)

(* let debug_code = ref 511;; *) (* everything *)

let debug_vars = [ "Thread", debug_thread;
                   "Warning", debug_warning;
                   "Graphics", debug_graphics;
                   "ERROR", debug_error;
                   "I/O", debug_io;
                   "Memory", debug_memory;
                   "Board", debug_board;
                   "Event", debug_event;
                   "Custom", debug_custom];;

let debug_to_string =
  let debug_array = Array.of_list debug_vars in
  fun c ->
    let rec loop i n list =
      if i = 0 || n = 16 then list
      else let code = i land 1 in
        if code = 0 then loop (i lsr 1) (n+1) list
        else let s =
          if n > 0 && n < 10 then fst debug_array.(n-1)
          (* if n = 1 then "Thread" *)
          (* else if n = 2 then "Warning" *)
          (* else if n = 3 then "Graphics" *)
          (* else if n = 4 then "ERROR" *)
          (* else if n = 5 then "I/O" *)
          (* else if n = 6 then "Memory" *)
          (* else if n = 7 then "Board" *)
          (* else if n = 8 then "Event" *)
          else "Unknown" in
          loop (i lsr 1) (n+1) (s::list) in
    String.concat "; " (loop c 1 []);;


(** should we put this in a Var ? *)
(* TODO: use this to reduce the number of lock if there is no thread *)
let threads_created = ref 0;;

(* couleurs xterm, cf : http://linuxgazette.net/issue65/padala.html *)
let xterm_red  = "\027[0;31m";;
let xterm_blue = "\027[0;94m";;
let xterm_light_grey = "\027[1;30m";;
let xterm_nc = "\027[0m";;

let print s = Printf.ksprintf print_endline s;;
let print_debug_old s = Printf.ksprintf
  (fun s -> if !debug then print_endline
      (xterm_blue ^ "[" ^ (string_of_int (Int32.to_int (Sdl.get_ticks ()) mod 60000)) ^ "] : " ^ xterm_nc ^ s)) s;;
let debug_select_old code s = if !debug && (code land !debug_code <> 0)
  then print_endline (xterm_red ^ (debug_to_string code) ^ xterm_nc ^ ": " ^ s);;
let printd code s = Printf.ksprintf
    (fun s -> if !debug && (code land !debug_code <> 0)
      then print_endline
          (xterm_blue ^
           "[" ^ (string_of_int (Int32.to_int (Sdl.get_ticks ()) mod 60000)) ^ "]" ^
           xterm_light_grey ^ "[" ^
           (string_of_int (Thread.id (Thread.self ()))) ^ "]" ^ xterm_nc ^ " :\t " ^
           xterm_nc ^ xterm_red ^ (debug_to_string code) ^ xterm_nc ^ ": " ^ s)) s;;

(* check if string s starts with string sub *)
let startswith s sub =
  String.length sub = 0 || begin
    String.length s >= String.length sub &&
    String.sub s 0 (String.length sub) = sub
  end;;
     
(* create function for generating integers, starting from 1 *)
let fresh_int () =
  let id = ref 0 in
  fun () -> if !id < max_int then (incr id; !id)
    else failwith "Too many ids created!";;

(* round float to nearest integer: *)
let round x = if x >= 0. 
  then int_of_float (x +. 0.5)
  else int_of_float (x -. 0.5)

let pi = 4. *. atan 1.;;

let square x = x *. x;;

let rec pwr k x =
  assert (k>=0);
  if k = 0 then 1. else x *. (pwr (k-1) x);;

let imax (x:int) (y:int) =
  if x > y then x else y;;

let imin (x:int) (y:int) =
  if x < y then x else y;;

let fmax (x:float) (y:float) =
    if x > y then x else y;;

let fmin (x:float) (y:float) =
    if x > y then x else y;;

let go = function
  | Error _ -> failwith ("SDL ERROR: " ^ (Sdl.get_error ()))
  | Ok r -> r;;

(* returns an option containing the first element of the list for which the
    function f does not return None *)
let rec list_check f l =
  match l with
    | [] -> None
    | x::rest -> begin
      match f x with
        | None -> list_check f rest
        | s -> s
    end;;

(* Return the first element of the list satisfying p, and its index *)
let list_findi p l =
  let rec loop i = function
    | [] -> None
    | a::rest -> if p a then Some (a, i)
      else loop (i+1) rest in
  loop 0 l
  
(* idem where the function f returns true *)
let list_check_ok f l =
  list_check (fun x -> if f x then Some x else None) l;;

(* splits a list atfer the xth element *)
let split_list_rev list x =
  let rec loop head tail i =
    if i >= x then (head, tail)
    else match tail with
      | [] -> printd debug_error "Error: position too far in list"; raise Not_found
      | a::rest -> loop (a::head) rest (i+1) in
  loop [] list 0;;

let split_list list x =
  let daeh, tail = split_list_rev list x in
  List.rev daeh, tail;;


(* checks if 'a' contained in the list, with 'equal' function *)
let rec mem equal a list =
  match list with
    | [] -> false
    | b::rest -> if equal a b then true else mem equal a rest;;

(* checks if all elements are different (using the 'equal' function) *)
(* not used, use "repeated" below instead *)
let rec injective equal list =
  match list with
    | [] -> true
    | a::rest -> if mem equal a rest then false else injective equal rest;;

let rec repeated equal list =
   match list with
    | [] -> None
    | a::rest -> if mem equal a rest then Some a else repeated equal rest;;

(* max of a list *)
(* in case of equal elements, the *first* one is selected *)
let list_max compare list =
  match list with
  | [] -> None
  | a::rest -> Some (List.fold_left
                       (fun max x ->
                          (* printd debug_warning "Compare=%d" (compare x min); *)
                          if compare x max > 0 then x else max)
                       a rest);;

(* included in ocaml 4.03.0 *)
let cons x list =
  x::list;;

let run f = f ();;

(* monadic operations *)

exception None_option
(* used when the option should not be None. *)

let map_option o f = match o with
  | Some x -> Some (f x)
  | None -> None;;

let do_option o f = match o with
  | Some x -> f x
  | None -> ();;

let check_option o f = match o with
  | Some x -> f x
  | None -> None;;

(* Warning the "d" is always evaluated, so it's not always a good idea to use
   this... *)
let default o d = match o with
  | Some x -> x
  | None -> d;;

let default_option o od = match o with
  | None -> od
  | o -> o;;

let map2_option o1 o2 f = match o1, o2 with
  | Some x1, Some x2 -> Some (f x1 x2)
  | _ -> None

let one_of_two o1 o2 = match o1, o2 with
  | None, None -> None
  | _, None -> o1
  | None, _ -> o2
  | _ -> printd debug_warning "one_of_two takes first of two options"; o1;;

let remove_option = function
  | Some x -> x
  | None -> raise None_option;;


(* memo *)
(* standard memo fns. Don't use when the variable is mutable, it would store the
   old value for ever when the variable changes. *)

let memo f =
  let store = Hashtbl.create 100 in
  fun x -> try Hashtbl.find store x with
    | Not_found -> let result = f x in
                   Hashtbl.add store x result;
                   printd debug_memory "##Size of Hashtbl : %u" (Hashtbl.length store);
                   result;;

let memo2 f =
  let store = Hashtbl.create 100 in
  fun x y -> try Hashtbl.find store (x,y) with
    | Not_found -> let result = f x y in
                   Hashtbl.add store (x,y) result;
                   printd debug_memory "##Size of Hashtbl2 : %u" (Hashtbl.length store);
                   result;;

let memo3 f =
  let store3 = Hashtbl.create 100 in
  (fun x y z -> try Hashtbl.find store3 (x,y,z) with
     | Not_found -> let result = f x y z in
       Hashtbl.add store3 (x,y,z) result;
       printd debug_memory "###Size of Hashtbl3 : %u" (Hashtbl.length store3);
       result),
  store3;;

(* inutile ? *)
let list_sum list =
  List.fold_left (+) 0 list;;

(* let find_file list_list = *)

let which command =
(* BETTER: (specially for portability to WIN/MAC) use
   https://opam.ocaml.org/packages/fileutils/ *)
  try
    let s = Unix.open_process_in ("which " ^ command) in
    let res = try 
        Some (input_line s)
      with
      | _ -> None in begin
        match Unix.close_process_in s with
        | Unix.WEXITED 0 -> res
        | Unix.WEXITED 1 -> None (* in principle this is redundant since `res`
                                    is already None at this point *)
        | _ -> printd (debug_error + debug_io)
                 "The `which` command exited with error.";
               None
      end
  with
  | _ -> printd (debug_error + debug_io) "Cannot use the `which` command.";
         None;;
