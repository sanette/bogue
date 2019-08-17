(* A standard way to connect a text input to a text display *)

(* This file is part of Bogue documentation. *)
(* http://sanette.github.io/bogue/Principles.html *)

open Tsdl
open Bogue
module W = Widget
module L = Layout

let main () =
  let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in
  let label = W.label ~size:40 "Hello!" in
  let layout = L.tower [L.resident ~w:400 input;
                       L.resident ~w:400 ~h:200 label] in
  let action ti l _ =
    let text = W.get_text ti in
    W.set_text l ("Hello " ^ text ^"!") in
  let c = W.connect input label action Sdl.Event.[text_input; key_down] in
    
  let board = Bogue.make [c] [layout] in
  Bogue.run board;;

let _ = main ();
  Draw.quit ();;
