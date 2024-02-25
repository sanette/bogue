(*

Warning: (from the SDL wiki)

SDL_GetTicks

This function is not recommended as of SDL 2.0.18; use SDL_GetTicks64() instead, where the value doesn't wrap every ~49 days. There are places in SDL where we provide a 32-bit timestamp that can not change without breaking binary compatibility, though, so this function isn't officially deprecated.

*)


open Tsdl
open B_utils

type t = int (* 1/1000 sec *)

let (+) t1 t2 = t1 + t2

let (-) t1 t2 = t1 - t2

let add t1 t2 = t1 + t2

let length t1 t2 = t2 - t1

let compare (t1 : t) (t2 : t) =
  Stdlib.compare t1 t2

let (>>) (t1 : t) (t2 : t) =
  t1 > t2

let float t = float t

(* Do not use! it is NOT nice to other threads *)
let delay_old d = Sdl.delay (Int32.of_int d);; (* attention ça freeze si c'est négatif *)

(* we use this instead *)
let delay x = Thread.delay (float x /. 1000.)

(* in principle one should use Int32.unsigned_to_int. This is ok until 2^31 -1,
   ie. about 24 days. TODO change this? *)
let now () : t = Int32.to_int (Sdl.get_ticks ())

let make_fps ?(min_delay=5) () =
  assert (min_delay >= 0);
  let start = ref 0 in
  (fun () -> start := now ()),
  fun fps ->
    if !start = 0 then (delay min_delay; start := now ())
    else
      let round_trip = now () - !start in begin
        let wait = max min_delay ((1000 / fps) - round_trip) in
        printd debug_graphics "FPS:%u (round_trip=%u)\n" (1000 / (round_trip + wait)) round_trip;
        if wait > 0 then
          delay wait;
        start := now ();
      end

let set_swap_interval =
    let swap_interval = ref min_int in
    fun desired ->
      if !swap_interval <> desired then begin
        match Sdl.gl_set_swap_interval desired with
        | Ok () ->
          swap_interval := desired;
          true;
        | Error (`Msg m) ->
          printd (debug_graphics+debug_warning) "Failed to set desired swap interval to %u: %s" desired m;
          false
      end else true

let adaptive_fps ?(vsync=false) fps =
  let start = ref 0 in
  let frame = ref 1 in
  let total_wait = ref 0 in (* only for debugging *)
  let vsync_used = ref false in

  (* the start function *)
  (fun () ->
    start := now ();
    total_wait := 0;
    if vsync then begin
        vsync_used := set_swap_interval 1;
        printd debug_graphics "VSync used: %b" !vsync_used;
      end;
    frame := 1),

  (* the main function *)
  fun () ->
  if !start = 0 then (delay 5; start := now (); assert(false))
  else
    let elapsed = now () - !start in
    let theoric = 1000 * !frame / fps in (* theoric time after this number of frames *)
    let wait = theoric - elapsed in
    total_wait := !total_wait + wait;
    let wait =
      if wait < 5
      then (printd debug_graphics "Warning: cannot keep up required FPS=%u (wait=%d)" fps wait;
            (* this can also happen when the animation was stopped; we reset
               the counter *)
            frame := 0;
            total_wait := 0;
            start := now ();
            if !vsync_used then
              (* turn on adaptive vsync if supported *)
              if set_swap_interval (-1) then
                printd (debug_graphics + debug_warning) "Adaptive VSync enabled"
              else begin
                  printd (debug_graphics + debug_warning) "Disabling VSync";
                  (* fall back to turning vsync off *)
                  let (_:bool) = set_swap_interval 0 in
                  vsync_used := false
                end;
            5)
      else if !vsync_used then
        (* trust VSync and the released runtime lock in Sdl.render_present
           to maintain FPS, but use the usual 5ms to allow other OCaml code
           to run if needed
         *)
        5
      else (printd debug_graphics "Wait=%u, Avg.=%u" wait (!total_wait / !frame);
            wait) in
    delay wait;
    incr frame;
    if !frame > 1000000 (* set lower? *)
    then (printd debug_graphics "Reset FPS counter";
          frame := 1;
          total_wait := 0;
          start := now ())
