(* This file is part of BOGUE. *)

(*-------------------------------------------------------*)
(* How to make Bogue work in an OCaml toplevel (or REPL) *)
(*-------------------------------------------------------*)

(* This file can be sent directly to an OCaml toplevel *)

#thread;;
#require "bogue";;
open Bogue;;
module W = Widget;;
module L = Layout;;

(*-------------------------------------------------------*)
(* a simple check button *)
let b = W.check_box ();;
let layout = L.resident ~w:100 b;;
let board = Main.make [] [layout];;
Main.run board;;
(*-------------------------------------------------------*)


(*-------------------------------------------------------*)
(* a table with selectable rows *)
let list = [
      ["English"; "French"];
      [ "hello"; "salut" ];
      [ "bye bye"; "salut"];
      [ "see you"; "à plus"];
      [ "darn"; "zut"];
      [ "holy cow"; "oh punaise"]];;
let table, sel = Table.of_list ~h:200 list;;
(* Let's preselect row#2 *)
Tvar.set sel (Selection.toggle (Tvar.get sel) 2);;
let layout = L.tower [L.resident (W.label "This is a nice table"); table];;
Main.(run (make [] [layout]));;

(* Here is what row numbers the user has selected *)
Selection.sprint (Tvar.get sel);;

(* And the corresponding rows *)
Selection.iter (fun i ->
    String.concat ";" (List.nth list (i+1)) |> print_endline) (Tvar.get sel);;

(*-------------------------------------------------------*)
