(** a clickable button *)
(* TODO click on an image *)

(* TODO in case of Switch, dim the label when not selected *)
(* ==> label_on, label_off ? *)

open B_utils
module Theme = B_theme
module Var = B_var
module Draw = B_draw
module Style = B_style
module Box = B_box
module Label = B_label
module Trigger = B_trigger

type kind =
  | Trigger (* one action when pressed. TODO, better to avoid name clash with
               Trigger module*)
  | Switch (* two states *)

type action = bool -> unit

type t =
  { kind : kind;
    label_on : Label.t;
    label_off : Label.t;
    state : bool Var.t;
    pressed : bool Var.t;
    mutable mouse_over : bool;
    keyboard_focus : bool Var.t;
    box_on : Box.t; (* TODO Var.t ? *)
    box_off : Box.t; (* TODO Var.t ? *)
    box_over : Box.t option;
    action : action option
  }

let color_on = Draw.find_color Theme.button_color_on
let color_off = Draw.find_color Theme.button_color_off
let bg_over = Style.gradient
    Draw.[opaque color_off; opaque color_off; opaque color_on]

(* if label_on and/or label_off is provided, then label is ignored *)
let create ?size ?border_radius ?border_color ?fg
    ?(bg_on = Style.color_bg Draw.(opaque color_on))
    ?(bg_off = Style.color_bg Draw.(opaque color_off))
    ?(bg_over = Some bg_over) ?label ?label_on ?label_off ?(state=false)
    ?action kind text =
  let label_on, label_off = match label, label_on, label_off with
    | None, None, None -> let l = Label.create ?size ?fg text in l,l
    | Some l, None, None -> l,l
    | None, _, _ ->
      default_lazy label_on (lazy (Label.create ?size ?fg text)),
      default_lazy label_off (lazy (Label.create ?size ?fg text))
    | _ -> printd debug_warning
             "label argument was ignored because label_on and/or \
              label_off was provided";
      default_lazy label_on (lazy (Label.create ?size ?fg text)),
      default_lazy label_off (lazy (Label.create ?size ?fg text)) in
  let border_on, border_off = match border_color, border_radius with
    | None, None -> None, None
    | None, Some radius ->
      Some Style.(mk_border ~radius
                    (mk_line ~color:(Style.get_color bg_on) () )),
      Some Style.(mk_border ~radius
                    (mk_line ~color:(Style.get_color bg_off) () ))
    | _ ->
      let s = Style.(mk_border ?radius:border_radius
                       (mk_line ?color:border_color ())) in
      Some s, Some s
  in
  let style_on = Style.create ~background:bg_on ?border:border_on () in
  let style_off = Style.create ~background:bg_off ?border:border_off () in
  { kind;
    action;
    label_on;
    label_off;
    state = Var.create state;
    pressed = Var.create state;
    mouse_over = false;
    keyboard_focus = Var.create false;
    box_on = Box.(create ~style:style_on ());
    box_off = Box.(create ~style:style_off ());
    box_over = map_option bg_over (fun bg ->
        let style = Style.with_bg bg style_off in
        Box.create ~style ())
  }

let unload l =
  Label.unload l.label_on;
  Label.unload l.label_off;
  Box.unload l.box_on;
  Box.unload l.box_off;
  do_option l.box_over Box.unload

(* TODO *)
let free = unload

let has_keyboard_focus b = Var.get b.keyboard_focus

let set_focus b = Var.set b.keyboard_focus true

let unfocus b = Var.set b.keyboard_focus false

let state b =
  Var.get b.state

let text b =
  if state b
  then Label.text b.label_on
  else Label.text b.label_off

let set_label b text =
  if state b
  then Label.set b.label_on text
  else Label.set b.label_off text

let is_pressed b =
  Var.get b.pressed

(* called on button_down *)
let press b =
  Var.set b.pressed true

let reset b =
  Var.set b.pressed false;
  Var.set b.state false

let set b s =
  Var.set b.pressed s;
  Var.set b.state s

(* called on button up for Trigger *)
let release b =
  (* TODO: verify true click *)
  if is_pressed b then begin
    Var.set b.pressed false;
    let s = not (Var.get b.state) in
    Var.set b.state s;
    do_option b.action (fun f -> f s)
    (* TODO; this is not exactly what we want with Trigger *)
  end

(* called by button_up in case of kind=Switch *)
let switch ?(keyboard=false) b ev =
  if keyboard || Trigger.has_full_click ev
  then begin
    let s = not (Var.get b.state) in
    Var.set b.state s;
    printd debug_event "Switch button to [pressed=%b] [state=%b]"
      (is_pressed b) s;
    do_option b.action (fun f -> f s)
  end;
  Var.set b.pressed (Var.get b.state)

let mouse_enter b =
  b.mouse_over <- true;
  set_focus b

let mouse_leave b =
  b.mouse_over <- false;
  unfocus b

let check_key b ev =
  has_keyboard_focus b &&
  Tsdl.Sdl.Event.(get ev keyboard_keycode) = Tsdl.Sdl.K.return

(* TODO use also TAB or ENTER or SPACE...? *)
let receive_key b ev =
  if check_key b ev
  then match Trigger.event_kind ev with
    | `Key_down -> press b
    | `Key_up -> begin match b.kind with
        | Trigger -> release b
        | Switch -> switch ~keyboard:true b ev
      end
    | _ -> printd (debug_event+debug_error)
             "Wrong event (%s) for Button.receive_key" (Trigger.sprint_ev ev)


(************* display ***********)

let button_margin = 6;; (* logical size - TODO theme this var ? *)
let bm = Theme.scale_int button_margin

(* The size of the widget is dictated by the size of the labels *)
let size b =
  let (w,h) = Label.size b.label_on in
  let (w',h') = Label.size b.label_off in
  let w = imax w w' and h = imax h h' in
  (w + 2*button_margin, h + 2*button_margin)

(* For safety (?), if the size is too small, the check icon is not clipped (see
   [display] below). *)
let resize (w,h) b =
  let size = w - 2*button_margin, h - 2*button_margin in
  List.iter (Label.resize size) [b.label_on; b.label_off];
  List.iter (Box.resize (w,h)) [b.box_on; b.box_off];
  do_option b.box_over (Box.resize (w,h))

let display canvas layer b g =
  let (dx,dy) = if is_pressed b then (0, 1) else (0, 0) in
  let box = if is_pressed b
    then b.box_on
    else if b.mouse_over || has_keyboard_focus b
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
  in List.concat [box_blit; label_blit]
