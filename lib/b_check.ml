(** a check button, optional label *)

(* TODO: label. For the moment this is done by combining widgets, see
   Widget.check_box_with_label *)
(* TODO: keyboard focus *)

open B_utils
module Var = B_var
module Draw = B_draw
  
type style = 
  | Square
  | Circle (* circle is used for radio buttons *)

type t =
    { style : style;
      state : bool Var.t;
      mutable size : (int * int) option}

let create ?(state=false) ?(style=Square) () =
  { style; state = Var.create state;
    size = None; (* will be initialized at first rendering *) 
  };;

let state b = Var.get b.state;;

let set b s = Var.set b.state s;;

(** when one clicks on the button *)
(* TODO This will work with button_down. But if we want to change state only on
button_up, we need to check the mouse is still over the widget *)
let action b =
  let s = state b in
  Var.set b.state (not s);;

let free _ =
  ();;

let unload _ =
  ();;

(************* display ***********)

let default_circle_size = (12,14);; (* TODO compute at run-time *)
let default_square_size = (17,18);;

(* TODO load the symbol at run-time so that we can change color *)
let size t =
  default t.size (match t.style with
      | Circle -> default_circle_size
      | Square -> default_square_size);;
                
let display canvas layer b g =
  printd debug_graphics "Display button";
  let open Draw in
  let texture_on = match b.style with
    | Square -> canvas.textures.check_on
    | Circle -> canvas.textures.radio_on
  in
  let texture_off = match b.style with
    | Square -> canvas.textures.check_off
    | Circle -> canvas.textures.radio_off
  in
  let tex = if state b
    then texture_on
    else texture_off in
  if b.size = None
  then b.size <- (let w,h = tex_size tex in
                  Some (unscale_size (w, h)));

  (* DEBUG *)
  (* let (w,h) = tex_size tex in *)
  (* Printf.printf "TEX SIZE=(%u,%u)\n" w h; *)

  [center_tex_to_layer ~horiz:false canvas layer tex g];;
(* we could center horizontally, but then first one should change textures so
   that check_off and check_on have same width. *)
