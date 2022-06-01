(* a 'window' is in fine just a layout *)
(* Do not mistake it with a hardware (SDL) window *)
open Tsdl
open B_utils
module Layout = B_layout
module Draw = B_draw
module Chain = B_chain

type t =
  { layout : Layout.t;
    mutable is_fresh : bool;
    mutable bogue : bool;
    (* if bogue = false this means that bogue didn't create the corresponding
       SDL window, and we should neither clear it before rendering, nor
       RenderPresent it after rendering *)
    mutable on_close : (t -> unit) option (* None means destroy window *)
  }

let create ?on_close layout =
  if Layout.get_house layout <> None
  then begin
    printd (debug_error + debug_user)
      "Cannot construct a Window from room %s because it is already contained in \
       a house." (Layout.sprint_id layout);
    raise (Invalid_argument
             "[Window.create] Cannot create a Window from a Layout that belongs \
              to a house.")
  end;
  let g = layout.Layout.current_geom in
  Layout.(layout.current_geom <- { g with x = not_specified; y = not_specified });
  { layout; is_fresh = false; bogue = true; on_close}

let get_layout w =
  w.layout

let is_fresh w =
  w.is_fresh

let set_fresh w =
  w.is_fresh <- true

let to_refresh w =
  w.is_fresh <- false

let on_close w f =
  w.on_close <- f

(* This is not very efficient because it will search for the Window.t from the
   Layout.id... while we already have the Window.t at hand! Well, this function
   doesn't have to be efficient anyways. *)
let destroy w =
  Layout.destroy_window w.layout

let sdl_window w =
  Layout.window w.layout

let is_shown w =
  w.layout.Layout.show

let show_maybe w =
  match Layout.window_opt w.layout with
  | Some win ->
    if is_shown w
    then Sdl.show_window win
    else Sdl.hide_window win
  | None -> printd (debug_error + debug_board)
              "[Window.show_maybe] the SDL window does not exist for layout %s"
              (Layout.sprint_id w.layout)

(* physical size *)
let size w =
  Draw.get_window_size (sdl_window w)

let set_size ~w ~h win =
  do_option (Layout.window_opt win.layout) (Draw.set_window_size ~w ~h)

let maximize_width win =
  do_option (Layout.window_opt win.layout) (fun sdl_win ->
      let id = go (Sdl.get_window_display_index sdl_win) in
      let rect = go (Sdl.get_display_bounds id) in
      let w = Sdl.Rect.w rect in
      printd debug_graphics
        "[maximize_width] Detected display size for layout %s: (%i,%i)."
        (Layout.sprint_id win.layout) w (Sdl.Rect.h rect);
      let _w, h = Draw.get_window_size sdl_win in
      Draw.set_window_size sdl_win ~w ~h)

let get_canvas w =
  Layout.get_canvas w.layout

(** get SDL windows id, in case the canvas was created *)
let id w =
  Sdl.get_window_id (Layout.window w.layout)

let equal w1 w2 =
  Layout.equal w1.layout w2.layout

let render w =
  Layout.render w.layout

let flip ?clear w =
  if not (is_fresh w)
  then begin
    render w;
    let clear = default clear w.bogue in
    printd debug_graphics "clear=%b" clear;
    let present = w.bogue in
    Layout.flip ~clear ~present w.layout;
    set_fresh w
  end
  else Draw.clear_layers (Layout.get_layer w.layout)

(* Span an SDL window controlled by Bogue *)
let make_sdl_window w =
  printd debug_board "Make window for layout %s (stack %d)."
    (Layout.sprint_id w.layout) (Chain.get_stack_id (Layout.get_layer w.layout));
  Layout.make_window w.layout;
  w.bogue <- true

let use_sdl_window sdl_win w =
  printd debug_board "Use existing SDL Window for layout %s."
    (Layout.sprint_id w.layout);
  Layout.make_window ~window:sdl_win w.layout;
  w.bogue <- false
