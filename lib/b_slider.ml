open B_utils
open Tsdl
module Avar = B_avar
module Button = B_button
module Theme = B_theme
module Time = B_time
module Var = B_var
module Tvar = B_tvar
module Trigger =  B_trigger
module Draw = B_draw
module Mouse = B_mouse

type kind =
  | Horizontal (* Horizontal bar with a small slider; values increase from left
                  to right. No background *)
  | HBar (* Horizontal bar filled up to the value *)
  | Vertical (* Warning: values increase from bottom to top *)
  | Circular (* Origin is on the positive real axis. *)

type t = {
    (* The value of the slider is a Tvar, which means that it can share a global
       variable. This is used for instance for scrolling bars. When the scroll
       bar is moved, the voffset of the layout is automatically updated, and
       conversely if the layout is scrolled, the scrollbar is automatically
       updated. *)
    var : (int Avar.t, int) Tvar.t;
    (* The local value of the Tvar is the local slider value: an integer between
       0 and max. The remote value is an arbitrary 'external' integer Avar. *)
    (* TODO: (int Avar.t) is here to make smoother transitions. not done yet *)
    cache : int Var.t;
    (*  [cache] is used to avoid computing too many times the same local
        value *)
    mutable pointer_motion : bool;
    clicked_value : (int option) Var.t;
    offset : int Var.t;
    (* If offset=0, the tick will place itself with the mouse pointer exactly in
       its middle point. Offset is used to not move the tick if one clicks at
       any other position of the tick. *)
    mutable max: int;
    (* Slider can take values from 0 to max, both included. Must be non zero. *)
    step: int;
    mutable size : int * int; (* size in pixels (use Var.t ?). *)
    mutable thickness : int; (* in pixels *)
    mutable tick_size : int; (* in pixels. Size of the handle *)
    room_x : int Var.t; (* we store here the room position (unscaled) *)
    room_y : int Var.t;
    mutable first_time : bool; (* we have to discard [room_*] if the slider has
                                  not been displayed yet. TODO reset this when
                                  moving the room? (a reset/init function?) *)
    kind : kind;
    keyboard_focus : bool Var.t;
    (* we need to replicate here the keyboard_focus field of the layout, because
       we use it to render the widget differently if it has keyboard_focus. It
       acts similarly as the .active field of Text_input. It is set by
       Widget.set_keyboard_focus. *)
    key_speed : float Var.t;
    key_time : Time.t Var.t;
    render : (Draw.texture option) Var.t;
    (* render is only used for circular. Otherwise all textures are created and
       destroyed on the fly. Change this ? *)
  }

(* Usual events for a Slider *)
let triggers =
  let open Trigger in
  List.flatten [buttons_down; buttons_up; pointer_motion; [E.key_down]]

let length s =
  let w,h = s.size in
  match s.kind with
  | HBar
    | Horizontal -> w
  | Circular -> imin w h
  | Vertical -> h

let check_max m =
  if m <= 0
  then (printd debug_error "Max value of slider must be positive, not %i." m; 1)
  else m

(* value is ignored if a var is provided *)
let create ?step ?(kind = Horizontal) ?(value = 0) ?(length = 200)
      ?(thickness = 20) ?w ?h ?tick_size ?var m =

  let tick_size = default tick_size
                    (match kind with
                     | HBar -> 4
                     | _ -> 50) in
  let size = match kind with
    | HBar
      | Horizontal
      | Circular -> default w length, default h thickness
    | Vertical -> default w thickness, default h length in
  let thickness = match kind with
    | HBar
      | Horizontal -> snd size
    | Vertical -> fst size
    | Circular -> thickness in
  let step = default step (max 1 (m/100)) in
  let var = default_lazy var
              (lazy (Tvar.create
                       (Var.create (Avar.var value))
                       ~t_from:(Avar.get)
                       ~t_to:(Avar.var))) in
  { var;
    cache = Var.create (Tvar.get var);
    pointer_motion = false;
    max = check_max m;
    clicked_value = Var.create None;
    offset = Var.create 0;
    step;
    size;
    thickness;
    tick_size;
    kind;
    room_x = Var.create 0;
    room_y = Var.create 0;
    first_time = true;
    keyboard_focus = Var.create false;
    key_speed = Var.create 1.;
    key_time = Var.create (Time.now ());
    render = Var.create None
  }

(* create a slider with a simple Tvar that executes an action each time the
   local value of the slider is modified by the slider *)
(* let create_with_action ?step ?kind ~value ?length ?thickness ?tick_size *)
(*     ~action max = *)
(*   let v = Var.create (Avar.var value) in *)
(*   let t_from a = Avar.get a in *)
(*   let t_to x = action x; Avar.var x in *)
(*   let var = Tvar.create v ~t_from ~t_to in *)
(*   create ?step ?kind ~var ?length ?thickness ?tick_size max;; *)

let unload s =
    match Var.get s.render with
  | None -> ()
  | Some tex -> begin
      Draw.forget_texture tex;
      Var.set s.render None
    end

(* TODO *)
let free = unload

(* One might need this function when the room containing the slider has been
   moved and we don't want to wait 2 frames for the slider to automatically
   adapt to its new position... *)
let reset s =
  s.first_time <- true;
  s.pointer_motion <- false;
  Var.set s.clicked_value None;
  Var.set s.offset 0

let has_keyboard_focus s = Var.get s.keyboard_focus

let set_focus s = Var.set s.keyboard_focus true

let unfocus s = Var.set s.keyboard_focus false

let value s =
  Var.get s.cache
(*  Tvar.get s.var;;*)

let get_max s = s.max

let clicked_value s =
  Var.get s.clicked_value

let set s value =
  let value =
    if value < 0 || value > s.max
    then begin
        printd debug_error "Slider value %i is invalid. Clipping." value;
        imax 0 (imin s.max value)
      end
    else value in
  Tvar.set s.var value;
  Var.set s.cache value

(* This has to be done for each external call to this module *)
(* It will update the position of the slider by looking at the s.var. Hence, if
   the s.var has a nontrivial transformation, it might be that the value differs
   from the value initially computed from the mouse position. See example 34. *)
let update_value s =
  Var.set s.cache (Tvar.get s.var)

let refresh _ =
  () (* TODO *)

(* TODO: we could use some animation to make it smoother *)
let increase ?step s =
  let step = default step s.step in
  set s (min s.max (value s + step));
  refresh s

let decrease ?step s =
  let step = default step s.step in
  set s (max 0 (value s - step));
  refresh s

(* events *)

(* Compute the pre-value (in principle between 0 and s.max, but sometimes can be
   outside if the tick is large) from the mouse position *)
let compute_value s ev =
  let w,h = s.size in
  let x,y = Mouse.pointer_pos ev in
  let v = match s.kind with
    | Horizontal ->
       if s.tick_size = w then 0 (* the value should be undefined here *)
       else Var.get s.offset + (s.max * (x - s.tick_size/2 - (Var.get s.room_x))) / (w - s.tick_size)
    | HBar ->
       (s.max * (x - (Var.get s.room_x))) / w
    | Vertical ->
       if s.tick_size = h then 0 (* undefined *)
       else Var.get s.offset + s.max - (s.max * (y - s.tick_size/2 - (Var.get s.room_y))) / (h - s.tick_size)
    | Circular ->
       let x0 = Var.get s.room_x + w/2 in
       let y0 = Var.get s.room_y + h/2 in
       if x = x0 then if y>y0 then 3 * s.max / 4 else s.max / 4
       else
         let a = (float s.max) *. atan (float (y0-y) /. (float (x-x0))) /. pi /. 2. in
         let a' = if x > x0 then if y <= y0 then a else a +. (float s.max)
                  else a +. (float  s.max) /. 2. in
         round a' in
  (* printd debug_custom "Mouse (%d,%d), value=%d" x y v; *)
  v

(* This should be called on mouse_button_down. *)
(* If the click is over the tick, we do *not* change value: this is the standard
   behavious in most GUIs, and is a good idea imho. This requires storing an
   offset. *)
let click s ev =
  if !debug then assert (Var.get s.offset = 0);
  (* in some fast mouse motion it can happen that button_up is lost, so this
     assertion fails. *)
  let old = value s in
  let mouse_v = compute_value s ev in
  let v =
    if abs (mouse_v - old) * (length s - s.tick_size) <= s.max * s.tick_size/2
    then begin (* test à revoir: mouse over tick *)
      (* printd debug_custom "OVER TICK"; *)
      Var.set s.offset (old - mouse_v);
      old
    end
    else (max 0 (min mouse_v s.max)) in
  printd debug_board "Slider value : %d" v;
  Var.set s.clicked_value (Some v);
  (* Var.set s.keyboard_focus true; *)
  set s v
  (* we add an animation to the original Avar. For this we need some
     gymnastic to get the current and final value for it *)
  (* TODO this works only for scrolling, because has_anim is detected for
     scrolling.  Otherwise, has_anim does not detect this animation yet *)
  (* let avar = s.var.Tvar.var in *)
  (* let final = Avar.get (s.var.Tvar.t_to v) in *)
  (* Var.set avar (Avar.fromto (Avar.get (Var.get avar)) final);; *)

(* This should be called on mouse_button_up: *)
let release s =
  Var.set s.clicked_value None;
  s.pointer_motion <- false;
  Var.set s.offset 0

(* on mouse motion: *)
let slide s ev =
  let v = compute_value s ev in
  let v = (max 0 (min v s.max)) in
  printd debug_board "Slider value : %d" v;
  s.pointer_motion <- true;
  set s v

(* Use this to increase the step when using keyboard. *)
let change_speed s =
  let t = Time.now () in
  if Time.(t - (Var.get s.key_time)) > 200
  (* delay too long, we return to initial speed. TODO: check that this is bigger
     than system delay between two key repeats, otherwise this will always
     apply *)
  then Var.set s.key_speed 1.
  else Var.set s.key_speed (Var.get s.key_speed *. 1.1);
  Var.set s.key_time t;
  let step = s.step * (round (Var.get s.key_speed)) in
  step

(* This should be called on key_down. *)
let receive_key s ev =
  update_value s;
  if has_keyboard_focus s then
    begin
      match Trigger.event_kind ev with
      | `Key_down ->
         (match Sdl.Event.(get ev keyboard_keycode) with
          | c when c = Sdl.K.left -> decrease ~step:(change_speed s) s
          | c when c = Sdl.K.down -> decrease ~step:(change_speed s) s
          | c when c = Sdl.K.right -> increase ~step:(change_speed s) s
          | c when c = Sdl.K.up -> increase ~step:(change_speed s) s
          | c -> (printd debug_event "==> Key down event discarded.";
                 printd debug_event "Key=[%s], mod=%u, Keycode:%u" (Sdl.get_key_name c) (Sdl.get_mod_state ()) c))
      | _ -> printd debug_event "Warning: Event should not happen here"
    end


let set_max s m =
  s.max <- check_max m

let set_tick_size s x =
  s.tick_size <- x

let min_tick_size s =
  match s.kind with
  | HBar -> 4
  | Horizontal -> 25
  | Vertical -> 20
  | Circular -> 15

(* display *)

let size s =
  s.size

let resize (w,h) s =
  unload s;
  s.size <- (w,h);
  let thickness = match s.kind with
    | HBar
      | Horizontal -> h
    | Vertical -> w
    | Circular -> s.thickness in
  s.thickness <- thickness

(* internal *)
let x_pos s =
  Var.get s.room_x + (value s) * (length s - s.tick_size) / s.max

(* internal *)
let y_pos s =
  Var.get s.room_y + length s - s.tick_size - (value s)
  * (length s - s.tick_size) / s.max


let make_box_blit ~dst ?(shadow=true) ~focus voffset canvas layer box =
  (* Let's see if it is nice to add a "shadow" to the tick *)
  let box_blit = Draw.make_blit ~voffset ~dst canvas layer box in
  if shadow && focus then
    let shadow_blits = Draw.box_shadow ~offset:(0,0) ~size:(Theme.scale_int 6)
                         canvas layer dst in
    List.rev (box_blit :: shadow_blits)
  else [box_blit]

let display canvas layer s g =
  (* We use y_pos before updating to display a gradient box at the real mouse
     position, in case of non-linear (vertical) slider (see example 34)...  TODO
     do the same for Horizontal sliders. *)
  let oldy = y_pos s in
  update_value s;
  let scale = Theme.scale_int in
  let tick_size = scale s.tick_size
  and thickness = scale s.thickness in
  let open Draw in
  let renderer = canvas.renderer in
  let gx = Theme.unscale_int g.x
  and gy = Theme.unscale_int g.y in
  if Var.get s.room_x <> gx then Var.set s.room_x gx;
  if Var.get s.room_y <> gy then Var.set s.room_y gy;
  let focus = has_keyboard_focus s in
  let shadow = true (* for testing *) in
  let c = if shadow then opaque Button.color_on
          else set_alpha 200 Button.color_on in
  let color = if has_keyboard_focus s && not shadow
              then Draw.(darker c)
              else c in
  let x0 = scale (x_pos s) in
  (*   set_color renderer (opaque color); *)
  match s.kind with
  | Horizontal ->
     (* let rect = Sdl.Rect.create ~x:x0 ~y:g.y ~w:thickness ~h:width in *)
     (* go (Sdl.render_fill_rect renderer (Some rect)); *)
     let box = texture canvas.renderer ~color ~w:tick_size ~h:thickness in
     let dst = Sdl.Rect.create ~x:x0 ~y:g.y ~w:tick_size ~h:thickness in
     forget_texture box; (* or save ? but be careful color may change *)
     make_box_blit ~dst ~shadow ~focus g.voffset canvas layer box
  | HBar ->
     (* horizontal gradient for the slider *)
     let colors = [opaque Button.color_on; opaque Button.color_off] in
     let box = gradient_texture canvas.renderer ~w:(x0 - g.x + tick_size)
                 ~h:thickness ~angle:90. colors in
     let dst = Sdl.Rect.create ~x:g.x ~y:g.y ~w:(x0 - g.x + tick_size)
                 ~h:thickness in
     forget_texture box; (* or save ? *)
     make_box_blit ~dst ~shadow ~focus g.voffset canvas layer box
  (* [make_blit ~voffset:g.voffset ~dst canvas layer box] *)
  | Vertical ->
     let y0 = scale (y_pos s) in
     let dy = if s.first_time then (s.first_time <- false; 0)
       else scale oldy - y0 in
     let y = imin y0 (y0 + dy) in
     (* printd debug_custom "Slider y0=%i y=%i dy=%i" y0 y dy; *)
     let h = imax tick_size (abs dy) in (* see example 34 .*)
     let box = if abs dy <= 3 || not s.pointer_motion
               (* the 3 is completely heuristic. See example 35. Ideally we want
                  0. *)
               then texture canvas.renderer ~color ~h ~w:thickness
               else let colors = [opaque Button.color_on;
                                  opaque Button.color_off] in
                    (* let _ = print_endline (Printf.sprintf "dy = %i" dy) in *)
                    let colors = if dy < 0 then colors else List.rev colors in
                    gradient_texture canvas.renderer ~h ~w:thickness colors in
     let dst = Sdl.Rect.create ~x:g.x ~y ~h ~w:thickness in
     forget_texture box; (* or save ? *)
     make_box_blit ~dst ~shadow ~focus g.voffset canvas layer box
  | Circular ->
     let radius = imax 1 ((imin g.w g.h)/2 - 2) in
     let tex = match Var.get s.render with
       | Some t -> t
       | None ->
          let t' = ring_tex renderer ~color:(lighter (transp grey))
                     ~radius ~width:thickness (g.w/2) (g.h/2) in
          (* j'ai essayé de mettre une taille double puis de réduire avec
           render_copy, mais apparemment ça ne fait pas d'antialiasing *)
          (* let t' = convolution ~emboss:false renderer *)
          (*            (gaussian_blur ~radius:3) 3 t in *)
          Var.set s.render (Some t'); t' in
     let w',h' = tex_size tex in
     let dst = Sdl.Rect.create ~x:(g.x) ~y:(g.y) ~w:w' ~h:h' in
     (* go (Sdl.render_copy ~dst renderer tex); *)
     let sbox = make_blit ~voffset:g.voffset ~dst canvas layer tex in
     (* ring renderer ~bg:(lighter (opaque grey)) ~radius:(w/2-2)
       ~width (x+w/2) (y+h/2); *)
     let tick = ray_to_layer canvas layer ~voffset:g.voffset ~bg:color
                  ~thickness:tick_size
                  ~angle:(360. *. (float (s.max - value s)) /. (float s.max)) ~radius
                  ~width:thickness (g.x + g.w/2) (g.y + g.h/2) in
     [sbox; tick]

let display canvas layer s g =
  let open Draw in
  if g.w = 0 || g.h = 0 then begin
    printd (debug_warning + debug_graphics)
      "Not displaying slider because geometry (%i,%i) has zero area." g.w g.h;
    []
  end
  else display canvas layer s g

(* this function can be used for the ~t_to function to slow down the slider when
   the range of values is big (bigger than the number of pixels of the slider).
   When the user first move the slider, the slope will be 1 (1 pixel => 1 step),
   and then of course the slope becomes higher in order to catch up.  The price
   to pay is that the slider position has to be corrected when mouse_button is
   up. And the function has to be changed for each new starting value x0.  *)
(* x and x0 are between 0 and 1. Range of values is [0,M]. This is only needed
   when M>1 *)
(* k>=2 is the nonlinearity, it has to be even. k=2 should be enough *)
(* TODO modify the formula to allow k odd *)
let slow k m x0 x =
  if x >= x0
  then if (float k) *. (1. -. x0) *. m >= float (k-1) (* we have to slow down *)
       then let t = (x -. x0) /. (1. -. x0) in
            (m -. 1. -. x0 *. m) *. (pwr k t) +. t +. x0 *. m
       else x *. m (* just linear *)
  else
    if (float k) *. m *. x0 >= float (k-1) (* we have to slow down *)
    then let t = (x -. x0) /. x0 in
         (1. -. x0 *. m) *. (pwr k t) +. t +. x0 *. m
    else x *. m
