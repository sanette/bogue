(* Connecting a text input with a text display in "immediate" mode *)

(* This file is part of Bogue documentation. *)
(* http://sanette.github.io/bogue/Principles.html *)

(* It's easy to write because no connection (event/callback) is used. However
   for more complicated programs, this type of programming can impact
   performance since the widgets are updated at every frame. *)

open Bogue
module W = Widget
module L = Layout

let main () =
  let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in
  let label = W.label ~size:40 "Hello!" in
  let layout = L.tower [L.resident ~w:400 input;
                        L.resident ~w:400 ~h:200 label] in

  let before_display () =
    let text = W.get_text input in
    W.set_text label ("Hello " ^ text ^ "!") in

  let board = Bogue.make [] [layout] in
  Bogue.run ~before_display board;;

let _ = main ();
  Bogue.quit ();;
