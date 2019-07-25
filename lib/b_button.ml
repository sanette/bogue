(** a clickable button *)
(* TODO click on an image *)

(* TODO in case of Switch, dim the label when not selected *)
(* ==> label_on, label_off ? *)

open B_utils
open Tsdl
module Theme = B_theme
module Var = B_var
module Draw = B_draw
module Style = B_style
module Box = B_box
module Label = B_label
  
type kind =
  | Trigger (* one action when pressed. TODO, better to avoid name clash with
               Trigger module*)
  | Switch (* two states *)

type t =
  { label_on : Label.t;
    label_off : Label.t;
    state : bool Var.t;
    pressed : bool Var.t;
    mutable mouse_over : bool;
    box_on : Box.t; (* TODO Var.t ? *)
    box_off : Box.t; (* TODO Var.t ? *)
    box_over : Box.t option
  }

let color_on = Draw.find_color Theme.button_color_on
let color_off = Draw.find_color Theme.button_color_off

(* if label_on and/or label_off is provided, then label is ignored *)
let create ?size ?border_radius ?border_color ?fg
    ?(bg_on = Style.color_bg Draw.(opaque color_on))
    ?(bg_off = Style.color_bg Draw.(opaque color_off))
    ?bg_over ?label ?label_on ?label_off ?(state=false) text =
  let label_on, label_off = match label, label_on, label_off with
    | None, None, None -> let l = Label.create ?size ?fg text in l,l
    | Some l, None, None -> l,l
    | None, _, _ -> 
      default label_on (Label.create ?size ?fg text),
      default label_off (Label.create ?size ?fg text)
    | _ -> printd debug_warning
             "label argument was ignored because label_on and/or \
              label_off was provided";
      default label_on (Label.create ?size ?fg text),
      default label_off (Label.create ?size ?fg text) in
  let border_on, border_off = match border_color, border_radius with
    | None, None -> None, None
    | None, Some radius ->
      Some Style.(border ~radius (line ~color:(Style.get_color bg_on) () )),
      Some Style.(border ~radius (line ~color:(Style.get_color bg_off) () ))
    | _ ->
      let s = Style.(border ?radius:border_radius (line ?color:border_color ())) in
      Some s, Some s
  in
  { label_on;
    label_off;
    state = Var.create state;
    pressed = Var.create state;
    mouse_over = false;
    box_on = Box.(create ~background:bg_on ?border:border_on ());
    box_off = Box.(create ~background:bg_off ?border:border_off ());
    box_over = map_option bg_over (fun bg -> Box.create ~background:bg ())
  };;

let unload l =
  Label.unload l.label_on;
  Label.unload l.label_off;
  Box.unload l.box_on;
  Box.unload l.box_off;
  do_option l.box_over Box.unload;;

(* TODO *)
let free = unload;;

let state b =
  Var.get b.state;;

let text b =
  if state b
  then Label.text b.label_on
  else Label.text b.label_off;;

let set_label b text =
  if state b
  then Label.set b.label_on text
  else Label.set b.label_off text;;

let is_pressed b =
  Var.get b.pressed;;

let press b =
  Var.set b.pressed true;;

let reset b =
  Var.set b.pressed false;
  Var.set b.state false;;

let release b =
  (* TODO: verify true click *)
  if is_pressed b then begin
    Var.set b.pressed false;
    Var.set b.state (not (Var.get b.state)) (* TODO; this is not exactly what we want with Trigger *)
  end;;

(* called by button_up in case of kind=Switch *)
let switch b ev =
  if Sdl.Event.(get ev mouse_button_state) = Sdl.pressed
  (* = DIRTY trick, see bogue.ml *)
  then begin
    Var.set b.state (not (Var.get b.state));
    printd debug_event "Switch button to [pressed=%b] [state=%b]" (is_pressed b) (Var.get b.state);
  end;
  Var.set b.pressed (Var.get b.state);;

let mouse_enter b =
  b.mouse_over <- true;;

let mouse_leave b =
  b.mouse_over <- false;;

(************* display ***********)

let button_margin = 6;; (* logical size - TODO theme this var ? *)
let bm = Theme.scale_int button_margin;;

let size b =
  let (w,h) = Label.size b.label_on in
  let (w',h') = Label.size b.label_off in
  let w = imax w w' and h = imax h h' in
  (w + 2*button_margin, h + 2*button_margin);;

let display canvas layer b g =
  let (dx,dy) = if is_pressed b then (0, 1) else (0, 0) in
  let box = if is_pressed b
    then b.box_on
    else if b.mouse_over
    then default b.box_over b.box_off
    else b.box_off in
  (*let margin = if is_pressed b then 0 else button_margin in*)
  (*  Draw.box canvas.Draw.renderer ~bg (x+margin) (y+margin) (w-2*margin) (h-2*margin); *)
  let box_blit = Box.display canvas layer box
      Draw.( { x = g.x (* + margin *);
               y = g.y (* + margin *);
               w = g.w;
               h = g.h;
               voffset = g.voffset } ) in
  let label_blit = Label.display canvas layer
      (if state b then b.label_on else b.label_off)
      Draw.( { x = g.x + bm + dx;
               y = g.y + bm + dy;
               w = g.w - 2*bm;
               h = g.h - 2*bm;
               voffset = g.voffset } )
  in List.concat [box_blit; label_blit];;
