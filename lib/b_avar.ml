(** Animates variables *)
(* This is a special case of dynamic variables, where we know that the variable
   will be used (and thus, updated) at every iteration of the main loop *)
(* If there is an active Avar, the "anim" flag should be set so that the main
   loop does not wait for events *)

(* We could probably do the same thing using standard Dynvar, and emitting
   another event at each iteration. This would be less efficient (?) *)

open B_utils
module Time = B_time
module Mouse = B_mouse
  
type callback = unit -> unit

type 'a t = {
  mutable value : 'a; (* current value *)
  mutable starting_time : Time.t option;
  mutable finished : bool; 
  (* this flag is set to true just *before* the computation of the last value *)
  (* one can create a var with finished = true to behave like a normal var *)
  mutable frame : int; (* the frame when the value was computed *)
  mutable progress : float;
  (* = float in [0,1] giving the percentage of the animation when the last
     v.value was computed. In case of inifinite animation, this is juste the
     elapsed Time (in ms). *)
  init : callback; (* function to be called before the animation starts *)
  ending : callback; (* function to be called when the animation is finished *)
  update : 'a t -> float -> 'a; 
  (* update is a function v --> [0,1] --> 'a which gives the new value of the
     variable given the old value, where the interval [0,1] represents the whole
     duration of the animation. In case of inifinite animation, it is a function
     of the elapsed Time in ms. *)
  (* TODO: the first argument ('a t) is not currently used. Remove ? *)
  duration : Time.t; (* a negative duration means an infinite animation *)
}

(* This global variable keeps track of the number of animations that are not
finished. At this point, this is only for debugging. We cannot rely on it for
programming, because if an animation was started on a layout that is not used
anymore, it will never "finish".  Moreover some animations can belong to layouts
that are still alive but hidden (maybe clipped, maybe in a hidden window): in
this case they should not be considered "alive" by the renderer. *)             
(* not used *)
let alive_animations = ref 0;;

(* For the moment, in order to indicate that a var is changed, in case it is not
   detected by Bogue.has_amin, one should use: Trigger.push_var_changed *)

(** this global variable counts the number of frames displayed *)
(* of course it should be increased by the main loop *)
let frame = ref 0;;

let new_frame () =
  incr frame;;

let nop () = ();;

let fail _ _ = failwith "This variable cannot update itself";;

let create ?(duration=1000) ?(init=nop) ?(ending=nop) ?(finished=false) 
    ?(update=fail) value =
  { value;
    starting_time = None;
    finished;
    frame = !frame;
    progress = 0.;
    init;
    ending;
    update;
    duration
  };;

let constant x _ _ = x;;

(** simulate a mutable normal variable with a fixed value. The value can be
    changed by changing v.value. 

    <OLD>But it cannot update itself: thus v.finished
    should never be set to false. </OLD> *)
(* in fact, one could use update v u = v.value *)
(* this would solve the problem that setting v.value directly doesn't trigger
   has_anim, and thus can become unnoticed by the main event loop *)
let var value =
  let update v _ = v.value in
  create ~finished:true ~duration:0 ~update value;;

(** create a fixed value. Behaves a bit like var, with important differences: it
    is declared as a new animation (and thus reports "has_anim"), it will always
    have this value when initialized (even if v.value was manually changed) and
    if v.finished is set to false, the initial value will be set again *)
let fixed value =
  create ~duration:0 ~update:(constant value) value;;

(* one could use this instead of the global variable
   alive_animations *)
let has_anim v = not v.finished;;

let finished v = v.finished;;

let started v = v.starting_time <> None;;

(* this should not be called directly (done by get v) *)
(* in particular, it assumes that v.finished = false *)
let start v =
  if !debug then assert (not v.finished);
  v.init ();
  let t = Time.now () in
  incr alive_animations;
  printd debug_event "New animation started. Total=%d" !alive_animations;
  v.starting_time <- Some t;
  t;;

let progress v = v.progress;;

let in_progress v = v.starting_time <> None && not v.finished;;

let elapsed v = 
  if v.duration < 0 
  then round v.progress
  else round (v.progress *. (float v.duration));;
 
(** return the final value, or the current value if v was stopped. This does not
    stop the animation and does not trigger 'ending' *)
let final_value v =
  if v.finished then v.value
  else
  if v.duration < 0 then begin
    printd debug_error "Cannot compute the final value for an infinite animation !";
    v.value end
  else v.update v 1.;;

(** stop the animation, but doesn't change the value *)
(* can be called directly *)
let stop v =
  if v.finished then () 
  else begin
    v.ending ();
    v.finished <- true;
    decr alive_animations;
    printd debug_event "Animation finished. Total remaining=%d" !alive_animations
  end;;

(** finish the animation and set the value to the expected final value *)
let finish v =
  let final = final_value v in
  stop v;
  v.value <- final;;

(* reset so that the animation will start again *)
let reset v =
  if v.finished
  then printd debug_warning "Resetting animation."
  else if v.starting_time <> None
  then begin
    printd debug_warning "Animation was reset before ending.";
    decr alive_animations
  end;
  v.starting_time <- None;
  v.finished <- false;
  v.frame <- !frame;
  v.progress <- 0.;;
  
  
(** start the animation and compute the current value of the variable *)
let get v =
  if v.finished || (started v && v.frame = !frame) then v.value
  else
    let u = (* the rescaled time from 0. to 1. *)
      let t = Time.now () in
      let t0 = match v.starting_time with
        | Some t0 -> t0
        | None -> start v
      in
      if v.duration < 0 then float t (* no rescale in this case: infinite animation! *)
      else let t = if Time.(t - t0 >= v.duration)
        then (stop v;
              Time.(t0 + v.duration))
        else t in
        (* here v.duration should not be 0 *)
        Time.(float (t - t0) /. (float v.duration)) in

    (* we compute the new value: *)
    let x = v.update v u in
    v.value <- x;
    v.frame <- !frame;
    v.progress <- u;
    x;;
  
(** get the old value. This is the way to get the value if one doesn't want to
    start the animation, or if one doesn't want to make any calculation *)
let old v =
  v.value;;

(** sets the value *)
(* if there is an anim running, this has (almost) no effect, since the new value
   will be computed anyway. v.progress is *not* modified *)
let set v value =
  v.value <- value;
  v.frame <- !frame;;

(** create a new Avar by composing with f; the old Avar is still active *)
(* this doesn't start the animation *)
(* TODO if this one stop just a msec before the old one, and the old one is only
   active through the new one, then the old one will never "stop"... but maybe
   it is not a problem ? *)
let apply_old f v =
  let value = f (old v) in
  let update _ _ = f (get v) in
  let av = create ~duration:(v.duration-20) ~finished:v.finished ~update value in
  av.starting_time <- v.starting_time;
  av.progress <- v.progress;
  if not v.finished && started v then
    (incr alive_animations;
     printd debug_event "New composite animation started. Total=%d"
       !alive_animations;
    );
  av;;

let apply f v =
  let value = f (old v) in
  let update _ _ = f (get v) in
  let duration = v.duration - (elapsed v) in
  create ~duration ~finished:v.finished ~update value;;

type direction =
  | No
  | Left
  | Right
  | Top
  | Bottom
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | Random;;


let slowdown_old u =
  (* between 0 and 1, with speed from 1.8 to 0 *)
  2. *. (sin ((1. +. 2. *. u) *. pi /. 6.) -. 0.5);;

let slowdown u =
  (* between 0 and 1, with speed from 2 to 0 *)
  u *. (2. -. u);;

let fmin a b : float = min a b;;

(* for 0 to 1 with prescribed initial speed *)
let initial_slope ~slope =
  let u1 = 2. /. slope in
  if slope >= 2. then fun u -> (* this one is constant = 1 for u >= 2/slope *)
    if u < u1 then (slope *. u *. (1. -. slope *. u /. 4.)) else 1.
  else fun u ->
    u *. (slope +. (1. -. slope) *. u);;

(* from x1 to x2 with given initial and final slopes *)
let interpol3 ~slope1 ~slope2 x1 x2 u =
  let dx = x2 -. x1 in
  x1 +. u *. (slope1 
              +. u *. (3. *. dx -. 2. *. slope1 -. slope2 
                       +. u *. (slope1 +. slope2 -. 2. *. dx)));;

(* from x1 to x2 *)
let affine x1 x2 u =
  x1 *. (1. -. u) +. x2 *. u;;

(* from 0 to x *)
let linear x u =
  x *. u;;

let reverse u = 
  1. -. u;;

let concat ?(weight=0.5) g1 g2 =
  assert (weight >= 0. && weight <= 1.);
  if weight = 0. then g2
  else if weight = 1. then g1
  else fun u ->
    if u < weight then g1 (u /. weight)
    else g2 ((u -. weight) /. (1. -. weight));;

(******** examples of animated variables *********)


(** create a (slowdowned) integer Avar from x1 to x2 *)
let fromto_old ?(duration=300) x1 x2 =
  let update _ u =
    let t = slowdown u in
    round (float x1 *. (1. -. t) +. float x2 *. t) in
  create ~duration ~update x1;;

(** create a (slowdowned) integer Avar from x1 to x2 *)
let fromto ?(duration=300) ?ending x1 x2 =
  if x1 = x2 then fixed x1
  else let update _ u =
    initial_slope ~slope:1.2 u 
    |> affine (float x1) (float x2)
    |> round in
    create ~duration ~update ?ending x1;;

let fromto_float ?(duration=300) ?ending x1 x2 =
  if x1 = x2 then fixed x1
  else let update _ u =
    initial_slope ~slope:1.2 u 
    |> affine x1 x2 in
    create ~duration ~update ?ending x1;;

(** piecevise linear, with 2 pieces *)
let pl2 ?(duration=300) ~via x1 x3 =
  let (weight, x2) = via in
  if x1 = x2 && x2 = x3
  then fixed x1
  else let g1 = affine (float x1) (float x2) in
    let g2 = affine (float x2) (float x3) in
    let update _ u = 
      concat ~weight g1 g2 u
      |> round in
    create ~duration ~update x1;;

(** oscillate around the initial position *)
let oscillate ?(duration = 10000) ?(frequency=5.) amplitude x0 =
  let f = frequency *. 2. *. pi in
  let update _ u =
    x0 + round (float amplitude *. (sin (f *. u))) in
  create ~duration ~update 0;;

(** linear slide-in animation *)
let slide_in ?(from=Right) ~pos ~size =
  let w,h = size in
  let x0, y0 = pos in
  let dx,dy = match from with
    | No -> 0, 0
    | Top -> 0, -h
    | Bottom -> 0, h
    | Right -> w, 0
    | Left -> -w, 0
    | TopLeft -> -w, -h
    | TopRight -> w, -h
    | BottomLeft -> -w, h
    | BottomRight -> w, h
    | Random -> let t = Random.float (2. *. pi) in
      round (float w *. cos t), round (float h *. sin t) in
  let x = fromto (x0 + dx) x0 in
  let y = fromto (y0 + dy) y0 in
  (x,y);;

(** hoffset animation from h1 to h2 *)
(* for fun, one could use 'apply' instead to compose several Dynvar, but
   certainly less optimized... *)
let show ?(duration = 300) ?init ?ending h1 h2 =
  let update _ u =
    slowdown u
    |> affine (float h1) (float h2)
    |> round in
  create ~duration ~update ?init ?ending h1;;

let hide ?(duration = 300) ?init ?ending h1 h2 =
  let update _ u =
    reverse u
    |> slowdown
    |> affine (float h1) (float h2)
    |> round in
  create ~duration ~update ?init ?ending h1;;

(** fade_in animation *)
(* same as fade_out, but accell curve is reversed *)
let fade_in ?(duration = 300) ?(from_alpha = 0.) ?(to_alpha = 1.) () =
  let update _ u =
    reverse u
    |> slowdown 
    |> affine to_alpha from_alpha in
  create ~duration ~update from_alpha;;

(** fade_out animation *)
let fade_out ?ending ?(duration = 300) ?(from_alpha = 1.) ?(to_alpha = 0.) () =
  let update _ u =
    slowdown u
    |> affine from_alpha to_alpha in
  create ~duration ~update ?ending from_alpha;;

(* (\** mouse position relative to starting position *\)
 * let mouse_motion_x_old ?(threshold=7) window =
 *   let resist = ref true in
 *   let x0 = ref 0 in
 *   let init () = x0 := fst (Mouse.window_pos window) in
 *   let update _ u =
 *     let x = fst (Mouse.window_pos window) in
 *     if !resist then begin
 *       if abs (x - !x0) > threshold then resist := false;
 *       0
 *     end 
 *     else x - !x0 in
 *   create ~duration:(-1) ~init ~update 0;; *)

(* 'resist threshold' creates a function which, if x stays close to 0 then
   returns 0 otherwise returns x (even if later x come back close to 0 ) *)
let resist threshold =
  let resist = ref true in
  fun x ->
    if !resist then begin
      if abs x > threshold then resist := false;
      0 end
    else x;;

(* mouse x position relative to starting position *)
let mouse_motion_x_old ?(threshold=7) ?(dx = 0) window =
  let resist = resist threshold in
  let x0 = ref 0 in
  let init () = x0 := fst (Mouse.window_pos (Lazy.force window)) in
  let update _ _ =
    resist (fst (Mouse.window_pos (Lazy.force window)) - !x0)
    |> (+) dx in
  create ~duration:(-1) ~init ~update dx;;
    
(* mouse y position relative to starting position *)
let mouse_motion_y_old ?(threshold=7) ?(dy = 0) window =
  let resist = resist threshold in
  let y0 = ref 0 in
  let init () = y0 := snd (Mouse.window_pos (Lazy.force window)) in
  let update _ _ =
    resist (snd (Mouse.window_pos (Lazy.force window)) - !y0)
    |> (+) dy in
  create ~duration:(-1) ~init ~update dy;;

(** create a new avar from the current position to x2 with C^1 glue *)
(* warning: this is not guaranteed to stay between x1 and x2 *)
let extendto ~duration v x2 =
  let x1 = v.value in
  if v.finished || not (started v)
  then fromto ~duration x1 x2
  else let slope1 = (* we compute the slope at the current point of v *)
    (* if v has a different duration, the slope (in terms of u) has to be
       rescaled *)
    (* it is difficult to compute the slope this way, since v.update has
       integer values; this is why we take du=0.1 quite large. It would be
       better to have a 'float update' *)
    let du = 0.1 in
    let u1 = v.progress in
    let dx = if u1 < du
      then v.update v (u1 +. du) - x1
      else x1 - (v.update v (u1 -. du)) in
    if duration >= 0 && v.duration >= 0 
    then (float (v.duration * dx)) /. (du *. float duration)
    else if duration < 0 && v.duration >= 0
    then (float dx) /. (du *. float duration)
    else if duration >= 0
    then (float (v.duration * dx)) /. du
    else (* both durations are negative *)
      (float dx) /. du
    in
    let update _ u =
      interpol3 ~slope1 ~slope2:(0.) (float x1) (float x2) u
      |> round  in
    print_endline ("SLOPE=" ^ (string_of_float slope1)); (* DEBUG *)
    create ~duration ~update x1;; 

