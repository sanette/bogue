(* Module Draw. This file is part of BOGUE

San Vu Ngoc --2022

Low-level graphics using SDL,
layer mechanism, etc.

*)
open Printf
open Tsdl
open B_utils
module Chain = B_chain
module Theme = B_theme
module Var = B_var
open Result
module TImage = Tsdl_image.Image

let draw_error = debug_error + debug_graphics

type color = int * int * int * int (* RGBA *)
type rgb = int * int * int
type texture = Sdl.texture

type fill =
  | Pattern of texture
  | Solid of color

(* the list of textures available to the canvas *)
type textures = { (* use hashtbl ? *)
  check_on : texture;
  check_off : texture;
  radio_on : texture;
  radio_off : texture;
  mutable background : texture option;
  (* in principle background is always None if canvas.fill = Solid color *)
}

type align =
  | Min
  | Center
  | Max

type transform =
  { angle : float;
    center : Sdl.point option;
    flip : Sdl.flip;
    alpha : float; (* alpha multiplier: 0..1 *)
  }

let textures_in_memory = ref 0
let surfaces_in_memory = ref 0
let ttf_surfaces_in_memory = ref 0

let textures_to_destroy = Var.create (Queue.create ())
(* TODO this should be attached to a window. Make sure all textures that
   belonged to a renderer are removed from this queue when the renderer is
   detroyed. Otherwise we will be destroying a new texture! *)

(* the generic window icon *)
let icon  : (Sdl.surface option) ref = ref None

let check_memory () =
  if !textures_in_memory <> 0
  then printd (debug_memory+debug_error) "Textures remaining: %i"
      !textures_in_memory;
  if !surfaces_in_memory > 1 + !ttf_surfaces_in_memory
  then printd (debug_memory+debug_error) "Surfaces remaining: %i"
      (!surfaces_in_memory - 1 - !ttf_surfaces_in_memory);
  (* there is always the icon surface in memory, that's ok *)
  if !ttf_surfaces_in_memory > 0
  then printd debug_memory "TTF surfaces in memory: %i" !ttf_surfaces_in_memory

(* SDL wrappers *)

let create_color (r,g,b,a) =
  Sdl.Color.create ~r~g ~b ~a

let create_rgb_surface ~w ~h ~depth (r,g,b,a) =
  incr surfaces_in_memory;
  printd debug_memory "Create rgb_surface (%i,%i)" w h;
  go (Sdl.create_rgb_surface ~w ~h ~depth r g b a)

let create_surface_like surf ~w ~h =
  let format = Sdl.get_surface_format_enum surf in
  let depth,r,g,b,a = go (Sdl.pixel_format_enum_to_masks format) in
  create_rgb_surface ~w ~h ~depth (r,g,b,a)

(* see also create_surface below *)

let create_surface_from ~like:surf bigarray =
  let format = Sdl.get_surface_format_enum surf in
  let pitch = Sdl.get_surface_pitch surf in
  (* This is the usual SDL pitch in number of bytes per row *)
  let depth,r,g,b,a = go (Sdl.pixel_format_enum_to_masks format) in
  let w,h = Sdl.get_surface_size surf in
  printd debug_memory "CReate surface_from (%i,%i)" w h;
  incr surfaces_in_memory;
  go(Sdl.create_rgb_surface_from bigarray ~w ~h ~depth ~pitch r g b a)

let free_surface surface =
  let w,h = Sdl.get_surface_size surface in
  printd debug_memory "Freeing surface (%i,%i)" w h;
  Sdl.free_surface surface;
  decr surfaces_in_memory

(* Sdl.get_surface_format. See issue in tsdl.ml *)
(* the resulting format should be freed after use *)
let copy_surface_format surface =
  go(Sdl.alloc_format (Sdl.get_surface_format_enum surface))

let max_texture_size renderer =
  let info = go(Sdl.get_renderer_info renderer) in
  info.Sdl.ri_max_texture_width, info.Sdl.ri_max_texture_height

let rec create_texture renderer format access ~w ~h =
  let w,h =
    if w <= 0 || h <= 0
    then (printd draw_error
            "Texture dimensions must be positive. We change to 1.";
          imax w 1, imax h 1)
    else w,h in
  match Sdl.create_texture renderer format access ~w ~h with
  | Error _ ->
    printd draw_error "create_texture (%i,%i) error: %s" w h (Sdl.get_error ());
    let wmax, hmax = max_texture_size renderer in
    if wmax < w || hmax < h
    then (printd draw_error "The requested texture size (%u,%u) exceeds the max size (%u,%u)." w h wmax hmax;
          create_texture renderer format access ~w:(imin w wmax) ~h:(imin h hmax))
    else exit 1
  | Ok t ->
    incr textures_in_memory;
    t

let rec create_texture_from_surface renderer surface =
  match Sdl.create_texture_from_surface renderer surface with
  | Error _ ->
    printd draw_error "create_texture_from_surface error: %s" (Sdl.get_error ());
    let w,h = Sdl.get_surface_size surface in
    let wmax, hmax = max_texture_size renderer in
    if wmax < w || hmax < h
    then (printd draw_error "The requested texture size (%u,%u) exceeds the max \
                             size (%u,%u)." w h wmax hmax;
          (* now we scale the surface (loosing quality of course)--- and cross
             fingers *)
          let rect = Sdl.Rect.create ~x:0 ~y:0 ~h ~w in
          let w = imin w wmax in
          let h = imin h hmax in
          let new_surf = create_surface_like surface ~w ~h in
          go(Sdl.blit_scaled ~src:surface (Some rect) ~dst:new_surf None);
          let t = create_texture_from_surface renderer new_surf in
          free_surface new_surf;
          t)
    else exit 1
  | Ok t ->
    incr textures_in_memory;
    t

let create_system_cursor = memo ~name:"cursor" Sdl.create_system_cursor

let set_system_cursor sdl_cursor =
  Sdl.set_cursor (Some (go (create_system_cursor sdl_cursor)))

let sdl_image_load file =
  printd debug_memory "Create surface_load (%s)" file;
  incr surfaces_in_memory;
  go (TImage.load file)

(* SDL TTF *)

(* TODO: use a proper cache (delete unused fonts ?) *)
let font_cache : (((string * int), Tsdl_ttf.Ttf.font) Hashtbl.t) Var.t =
  Var.create (Hashtbl.create 10)
(* obviously we don't need the mutability of Var.t here for a Hashtbl... *)

let rec open_font file size =
  (* first we check if it is available in memory *)
  try let f = Hashtbl.find (Var.get font_cache) (file,size) in
      printd debug_memory "Font %s (%u) was found in cache" file size; f
  with
  | Not_found -> begin
      printd debug_io "Loading font %s (%u)" file size;
      match Tsdl_ttf.Ttf.open_font file size with
      | Result.Ok f ->
        let@ fc = Var.with_protect font_cache in
        Hashtbl.add fc (file,size) f;
        f;
      | Result.Error _ ->  (* use default font if error *)
         if file = !Theme.label_font
         then begin
             printd (debug_io + debug_error)
               "(FATAL) default font %s (%u) cannot be loaded" file size;
             print_endline "Font not found";
             raise Not_found
           end
         else begin
             printd (debug_io + debug_error)
               "Font %s (%u) could not be loaded. Using default font instead"
               file size;
             open_font !Theme.label_font size
           end
    end

let ttf_render font text color =
  if text = ""
  then create_rgb_surface ~w:0 ~h:0 ~depth:32
      (Int32.zero,Int32.zero,Int32.zero,Int32.zero)
  else begin
    incr surfaces_in_memory;
    printd debug_memory "Create surface_ttf (%s)" text;
    go (Tsdl_ttf.Ttf.render_utf8_blended font text color)
  end

let ttf_texture renderer font text color =
  let surf = ttf_render font text color in
  let tex = create_texture_from_surface renderer surf in
  free_surface surf;
  tex

let ttf_set_font_style font style =
  let open Tsdl_ttf in
  (* From SDL_ttf doc: NOTE: This will flush the internal cache of previously
     rendered glyphs, even if there is no change in style, so it may be best to
     check the current style using TTF_GetFontStyle first.
  *)
  if Ttf.get_font_style font <> style
  then Ttf.set_font_style font style

(* return a new rectangle translated by the vector (x0,y0) *)
let rect_translate r (x0,y0) =
  Sdl.Rect.(create ~x:(x r + x0) ~y:(y r + y0) ~w:(w r) ~h:(h r))



(* this function should only be called by the main loop *)
(* TODO try to remove all of this, and instead invoque Gc.finalise on every
   texture creation ? Bad idea in fact. See the 'finalise_textures' branch.  *)
let destroy_textures () =
  let queue = Var.get textures_to_destroy in
  let rec loop i =
    if Queue.is_empty queue
    then (if i > 0
          then printd debug_memory "%u texture%s destroyed" i
              (if i>1 then "s" else ""))
    else (let () =
            try
              Sdl.destroy_texture (Queue.pop queue)
            with _ ->
              printd debug_warning
                "The texture to destroy was invalid. It might be normal, \
                 because it is possible that the program decided to free \
                 a texture, and at the same time the Gc decided to free \
                 the object that contained this texture. If there is \
                 a Gc.finalise asking to destroy the texture, it will be \
                 destroyed twice, which raises an SDL error. But it might also \
                 mean that there is a design flaw in the program..." in
          decr textures_in_memory;
          loop (i+1)) in
  loop 0


(* --- *)

(* Call [forget_texture] to destroy the texture after the next iteration of the
   main loop. *)
(* This function should be used whenever one wants to change the texture of a
   widget without changing the pointer to the widget itself (in other words,
   when we just change the texture field of a widget). Then one should call
   forget_texture on the old texture, so that it will be freed by Sdl.

   If a widget is not used anymore, it is necessary to call forget_texture. *)
(* This is thread-safe. *)
let forget_texture tex =
  let@ queue = Var.with_protect textures_to_destroy in
  Queue.push tex queue

(* prints some memory info *)
let memory_info () =
  let open Printf in
  printf
    "Memory info:\n Textures: %d\n Surfaces: %d \nThreads: %d\nSystem RAM: \
     %uMb\t Allocated kbytes: %02f\n"
    !textures_in_memory
    !surfaces_in_memory
    !threads_created
    (Sdl.get_system_ram ())
    (Gc.allocated_bytes () /. 1024.);
  Gc.print_stat stdout;
  flush_all ()

let make_transform ?(angle = 0.) ?center ?(flip = Sdl.Flip.none)
    ?(alpha = 1.) () =
  { angle; center; flip; alpha }

(** compute the transform corresponding to t2 *after* t1 *)
(* TODO: of course this is not correct, need to check rotation center and
   translation *)
let compose_transform t1 t2 =
  if t2.angle = 0. && t2.alpha = 1. && t2.center = None &&
     t2.flip = Sdl.Flip.none
  then t1 (* This test is not necessary; is it useful for speeding up?  One
             could check t1 too. *)
  else
    let angle = t1.angle +. t2.angle in
    let alpha = t1.alpha *. t2.alpha in
    let center = one_of_two t1.center t2.center in
    let flip = Sdl.Flip.(t1.flip + t2.flip) in
    make_transform ~angle ?center ~flip ~alpha ()

(* a texture ready to be blit onscreen *)
type blit = {
  texture : texture;
  rndr : Sdl.renderer;
  dst : Sdl.rect option; (* destination rect *)
  src : Sdl.rect option; (* source rect *)
  clip : Sdl.rect option;
  transform : transform;
  to_layer : layer;
}
and layer = blit Queue.t Chain.t
(* Layers are organized in a chain. Not thread safe: only the main thread should
   modify layers.  Adding a new layer to a chain will put it "on top" of the
   previous layer, as in most drawing programs. It can be confusing because it
   does not fit well with the vocabulary ('depth') used in the Chain module: the
   most visible layer is the *last* (= last added = highest 'depth') layer of a
   Chain. *)

type canvas = {
  renderer : Sdl.renderer;
  window : Sdl.window;
  fill : fill; (* background *)
  textures : textures;
  mutable gl_context : Sdl.gl_context option
  (* layers : layer; *) (* not necessary ? *)
  (* There is one layer chain per canvas. The chain element "layers" referenced
     here may end up not being the first or last layer of the chain, since we
     are allowed to add new layers before or after it. One should use
     (Chain.last layer) to get the top layer (ie the most visible layer) *)
  (* This field is in fact not necessary, since any layout in this canvas points
     to the same set of layers. *)
}

(* There should be one (and only one) canvas per window *)
let canvas_equal c1 c2 =
  c1.window = c2.window

(** test if window is shown *)
let window_is_shown w =
  let flags = Sdl.get_window_flags w in
  Sdl.Window.(test flags shown)

let max_texture_size_old ?canvas () =
  match canvas with
  | Some c -> let info = go(Sdl.get_renderer_info c.renderer) in
    info.Sdl.ri_max_texture_width, info.Sdl.ri_max_texture_height
  | None -> (* go(Sdl.get_render_driver_info 1) in *)
    (* does not work, gives (0,0) *) 4096,4096

(* not used ? *)
type overlay =
  | Shrink
  | Clip
  | TopRight
  | Xoffset of int

let cleanup = ref []

let at_cleanup f =
  cleanup := f :: !cleanup

let destroy_canvas ?(bogue = true) c =
  Sdl.hide_window c.window;
  let t = c.textures in
  List.iter forget_texture [t.check_on; t.check_off; t.radio_on; t.radio_off ];
  do_option t.background forget_texture;
  (match c.fill with
   | Pattern t -> forget_texture t
   | _ -> ());
  destroy_textures ();
  Gc.full_major ();
  do_option c.gl_context Sdl.gl_delete_context;
  c.gl_context <- None;

  if bogue then begin
    printd (debug_graphics + debug_memory) "Destroying renderer";
    Sdl.destroy_renderer c.renderer;
    (* Note: this will destroy all textures attached to the renderer. And their id
       will be available for new textures: BEWARE: If ocaml refers to a texture
       that was destroyed this way, it will in fact most probably refer to a new
       texture with same id that was created after this... Hence the
       Gc.full_major, I didn't test whether this is sufficient. *)
    Sdl.destroy_window c.window;
    (* The following is a workaround for the weird bug on Mac OS 13.0.1 with
       cocoa video driver which prevents SDL windows to close in an interactive
       toplevel session. For some reason the window will close if we initialise a
       subsystem that was not already initialised, here joystick. *)
    if !Sys.interactive && Sdl.get_current_video_driver () = Some "cocoa"
    then begin
      printd (debug_memory + debug_graphics) "Cocoa workaround";
      Sdl.delay 100l;
      go @@ Sdl.(init Init.joystick);
      Sdl.(quit_sub_system Init.joystick)
    end
  end

type geometry = {
  x : int;
  y : int;
  w : int;
  h : int;
  voffset : int;
}

(* get "physical size" in pixel from the geometry *)
let scale_geom g =
  let open Theme in
  { x=(scale_int g.x); y=(scale_int g.y);
    w=(scale_int g.w); h=(scale_int g.h);
    voffset=(scale_int g.voffset) }

(* From Bogue logical pixels to OS pixels *)
(* TODO check whether @inline is really good. It can actually be bad. See also
   below. *)
let[@inline] scale_pos (x,y) =
  (Theme.scale_int x, Theme.scale_int y)

let scale_size = scale_pos

let unscale_pos (x,y) =
  (Theme.unscale_int x, Theme.unscale_int y)

let unscale_size = unscale_pos

let geom_to_rect g =
  Sdl.Rect.create ~x:g.x ~y:g.y ~w:g.w ~h:g.h

let make_geom ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(voffset=0) () =
  { x; y; w; h; voffset }

let window_id canvas =
  Sdl.get_window_id canvas.window

(* colors *)
let black = (0,0,0)
let grey = (100,100,100)
let pale_grey = (150,150,150)
let dark_grey = (75,75,75)
let white = (255,255,255)
let red = (255,0,0)
let blue = (0,0,255)
let green = (0,255,0)
let magenta = (255,0,255)
let cyan = (0,255,255)
let yellow = (255,255,0)
let sienna = (160,82,45)
let none = (0,0,0,0)
let colors =
  [ "black", black;
    "grey", grey;
    "pale_grey", pale_grey;
    "dark_grey", dark_grey;
    "white", white;
    "red", red;
    "blue", blue;
    "green", green;
    "sienna", sienna
  ]

let colors = List.flatten [colors; Theme.color_names]
(* we add all colors from: *)
(* http://www.rapidtables.com/web/color/html-color-codes.htm *)

let color_of_int24 i =
  (i lsr 16) land 255, (i lsr 8) land 255, i land 255

(* convert a string of the form "grey" or "#FE01BC" to a color code (r,g,b) *)
let find_color c =
  if String.length c <> 0 && c.[0] = '#' then try
      color_of_int24 (int_of_string ("0x" ^ (String.sub c 1 (String.length c - 1))))
    with
    | Failure _ -> (* int_of_string *)
      printd debug_error "Cannot extract color code from '%s'" c;
      grey
    | e -> raise e
  else
    try List.assoc c colors
    with
    | Not_found ->
      printd debug_error "Color '%s' unknown" c;
      grey

(* alpha=0 means totally transparent, alpha=1 means totally opaque *)
let set_alpha alpha (r,g,b) : (*Tsdl.Sdl.uint8 * Tsdl.Sdl.uint8 * Tsdl.Sdl.uint8 * int *) color =
  (r,g,b,alpha)

let bg_color = find_color Theme.bg_color
let cursor_color = find_color Theme.cursor_color
let faint_color = find_color Theme.faint_color
let text_color = ref (find_color Theme.text_color)
let sel_bg_color = find_color Theme.sel_bg_color
let sel_fg_color = find_color Theme.sel_fg_color
let label_color = find_color Theme.label_color
let menu_hl_color = find_color Theme.menu_hl_color
let menu_bg_color = find_color Theme.menu_bg_color
(* TODO put in VAR: *)
let scrollbar_color = set_alpha 20 blue

let set_text_color c =
  text_color := c

let opaque = set_alpha 255

let color_of_rgb = opaque

let transp = set_alpha 127

let more_transp (r,g,b,a) : color =
  (r,g,b, a/2)

let random_color () : color =
  let r () = Random.int 256 in
  (r(), r(), r(), r())

let sqrt_color x = round (255. *. sqrt (float x /. 255.))

(* non linear increase of color *)
(* f(x) = a - exp(-bx), f(0)=0.1, f(1)=1  => a = 1.1, b = - ln 0.1 = 2.3... *)
let incr_color x = min 255 (round (255. *. (1.1 -. exp (-.2.3 *. float x /. 255.))))

let pale (r,g,b) = (incr_color r, incr_color g, incr_color b)

let darker (r,g,b,a) : color =
  (3*r/4, 3*g/4, 3*b/4, a)

let component_lighter x =
  min 255 ((4*x)/3 + 80)

let lighter (r,g,b,a) : color =
  (component_lighter r, component_lighter g, component_lighter b, a)

let median (r1,g1,b1,a1) (r2,g2,b2,a2) : color =
  (r1+r2)/2, (g1+g2)/2, (b1+b2)/2, (a1+a2)/2

let set_color renderer (r,g,b,a) =
  go (Sdl.set_render_draw_color renderer r g b a)

(* get the color mask for creating textures *)
let mask renderer =
  let info = go (Sdl.get_renderer_info renderer) in
  let px = List.hd info.Sdl.ri_texture_formats in (* we take the first pixel format available... is it the right thing to do ?? *)
  let depth,r,g,b,a = go (Sdl.pixel_format_enum_to_masks px (* Sdl.Pixel.format_argb8888 *)) in
  depth,(r,g,b,a)

let pixel_format_old = go (Sdl.alloc_format Sdl.Pixel.format_argb8888)
(* TODO: init? *)

let color_to_int32 ?format surf (r,g,b,a) =
  (* Warning: the tsdl source says I should not use get_surface_format *)
  let format' = default_lazy format (lazy (copy_surface_format surf)) in
  let r = Sdl.map_rgba format' r g b a in
  if format = None then Sdl.free_format format';
  r

(* TODO this won't work in 32bits systems. Use Int32 for pixel instead *)
(* not used *)
(* let color_of_int surf pixel =
 *   Sdl.get_rgba (Sdl.get_surface_format surf) (Int32.of_int pixel);; *)

let tex_size tex =
  let _,_,(w,h) = go (Sdl.query_texture tex) in w,h

(**** Layers ****)

(* A layer can be seen as a number assigned to each layout, indicating its
   'depth', ie which one should be drawn first. As such it can be used at the
   stage of creation of layouts, in order to indicate which one should 'stay
   above' which one. But... a layer is actually implemented as a queue that will
   contain all 'blits' of the same depth. This, of course, belongs to the stage
   of rendering. Finally, a layer is also an entry to the whole stack of layers,
   since they are organized as a Chain. Each window shoud contain only one stack
   of layers. This causes a difficulty: at the layout creation stage, no window
   is created, and the user is allowed to define layouts that she will then send
   to different windows...  Hence the concept of "current_layer" below is not
   correct. (Currently one has to call [Draw.use_new_layer ();] when defining
   layouts for another window.) See for instance in "b_debug_window.ml": we have
   to store the current_layer and restore it afterwards. *)

(* Warning: Layers are part of a Stack of layers, which uses the Chain
   datatype. Hence, a layer is in fact always referenced as a Chain element. A
   layer should never be Chain.None *)

let new_layer () : blit Queue.t =
  Queue.create ()

(* [current_layer] is a global variable that should be initialized as soon as
   Bogue starts, because creating layouts necessitates the existence of a base
   layer. *)
let current_layer = Var.create (Chain.singleton (new_layer ()))
(* the mutex is used in Layout.flip *)

let get_current_layer () = Var.get current_layer

let set_current_layer layer = Var.set current_layer layer

let new_stack () = Chain.singleton (new_layer ())

let use_new_layer () = Var.set current_layer (new_stack ())

let set_get_current_layer layer =
  set_current_layer layer;
  layer

let layer_insert_above layer =
  printd debug_graphics "Create new layer above";
  let l = Chain.insert_after layer (new_layer ()) in
  set_get_current_layer l

let layer_above layer =
  let l = match Chain.next layer with
    | None -> printd debug_graphics "Create new layer above";
      Chain.insert_after layer (new_layer ())
    | t -> t
  in set_get_current_layer l

let layer_insert_below layer =
  printd debug_graphics "Create new layer below";
  let l = Chain.insert_before layer (new_layer ()) in
  set_get_current_layer l

let layer_below layer =
  let l = match Chain.next layer with
    | None -> printd debug_graphics "Create new layer below";
      Chain.insert_before layer (new_layer ())
    | t -> t
  in set_get_current_layer l

let top_layer () =
  set_get_current_layer (Chain.last (Var.get current_layer))

let deepest_layer () =
  set_get_current_layer (Chain.first (Var.get current_layer))

(* compute src and dst for a texture *)
(* voffset can be positive or negative *)
(* positive : texture will be shifted downward *)
let apply_offset ?src ?dst voffset tex =
  match voffset with
  | 0 -> src, dst
  (* Warning: if there is an voffset, the src is ignored *)
  | vo -> let x,y,w,h = match dst with
      | None -> let w,h = tex_size tex in 0, 0, w, h
      | Some rect -> Sdl.Rect.(x rect, y rect, w rect, h rect) in
    if vo >= 0
    (* TODO: check height >= 0 ? *)
    then Some (Sdl.Rect.create ~x:0 ~y:vo ~w ~h:(h-vo)),
         Some (Sdl.Rect.create ~x ~y ~w ~h:(h-vo))
    else Some (Sdl.Rect.create ~x:0 ~y:0 ~w ~h:(h+vo)),
         Some (Sdl.Rect.create ~x ~y:(y-vo) ~w ~h:(h+vo))

(* Prepare a blit *)
let make_blit ?src ?dst ?clip ?transform ?(voffset=0) canvas to_layer tex =
  let transform = default_fn transform make_transform in
  let src, dst = apply_offset ?src ?dst voffset tex in
  { src; dst; clip; rndr = canvas.renderer; texture = tex; transform; to_layer }

(* Saves the blit into its layer *)
(* Warning: not thread safe (uses Queues) *)
(* don't call this when rendering with render_blits *)
let blit_to_layer blit =
  (* let layer = match layer with *)
  (*   | None -> canvas.layer (\* the default current layer *\) *)
  (*   | Some l -> l in *)
  let queue = Chain.value blit.to_layer in
  Queue.add blit queue

(* Render a blit onscreen *)
(* WARNING: this does NOT free the texture, because often we want to keep it for
   re-use. In case of a one-time texture, use [forget_texture] before calling
   make_blit, or [unload_blit] after creating the blit: that's ok, because the
   texture will be destroyed only after rendering. *)
let render_blit blit =
  (* if no transform = go (Sdl.render_copy ?src:blit.src ?dst:blit.dst blit.rndr
     blit.texture) *)
  let t = blit.transform in
  let alpha = round (255. *. t.alpha) in
  let orig_alpha = go (Sdl.get_texture_alpha_mod blit.texture) in
  go (Sdl.set_texture_alpha_mod blit.texture alpha);
  go (Sdl.render_set_clip_rect blit.rndr blit.clip);
  go (Sdl.render_copy_ex ?src:blit.src ?dst:blit.dst blit.rndr
        blit.texture t.angle t.center t.flip);
  go (Sdl.render_set_clip_rect blit.rndr None);
  (* : this seems necessary in some cases, see example 35bis. For (extreme)
     optimization we might try to factor this out. *)

  (* BUG/WORKAROUND. Something is fishy with (un)setting clip_rect. Not sure
     why, but if I don't draw a dummy thing like a point or a rect, then the
     texture gets corrupted. It becomes unproperly offset, and has some random
     glitches. Hence the following lines where we draw a transparent point at
     0,0. For more debug information, one can also draw the clip rectangle as
     follows: *)
  (* set_color blit.rndr (random_color ()); *)
  (* go (Sdl.render_draw_rect blit.rndr blit.clip); *)
  set_color blit.rndr none;
  go (Sdl.render_draw_point blit.rndr (-1) (-1));
  (* END WORKAROUND *)

  go (Sdl.set_texture_alpha_mod blit.texture orig_alpha)
(* : we do this in case the texture is used at several places onscreen *)

(* render all blits in one layer. first in, first out *)
let render_blits blits =
  Queue.iter render_blit blits;
  Queue.clear blits

(* render all layers and empty them *)
let render_all_layers (layer : layer) =
  Chain.iter render_blits layer

(* TODO it could be convenient (for a probably very small cost) to render the
   blits onto a target texture instead of directly to the renderer, so that we
   could do image processing (blur...) more easily *)

let unload_blit b =
  forget_texture b.texture

(**********)

(* Draw a thin rectangle of width=1. See also "rectangle" for specifying
   width. As of SDL 2.0.10 to SDL 2.0.14, we don't want to rely upon
   SDL_RenderDrawRect, see:
   https://discourse.libsdl.org/t/sdl-renderdrawrect-function-broken/28756 On
   the other hand, render_draw_line(s) also has
   problems...https://github.com/libsdl-org/SDL/issues/3521.  We will
   reimplement this.

   SDL_RenderDrawLine bug: with SDL 2.0.10, the end point is drawn at a random
   location found in memory. This can be clearly seen in the /examples/drawing
   example: while drawing a new line, you can see ghost points forming a line
   that was drawn in a previous session! (the array somewhat stayed in memory).

   *)
let draw_rect ?color renderer (x,y) w h =
  do_option color (set_color renderer);
  go (Sdl.render_draw_lines renderer
        [Sdl.Point.create ~x ~y;
         Sdl.Point.create ~x:(x + w - 1) ~y;
         Sdl.Point.create ~x:(x + w - 1) ~y:(y + h - 1);
         Sdl.Point.create ~x ~y:(y + h - 1);
         Sdl.Point.create ~x ~y])

(* see also "box" for renderer *)
let fill_rect surf recto color =
(*  let ml = Sdl.must_lock surf in
  if ml then (
    printd debug_graphics "Locking surface";
    go (Sdl.lock_surface surf)
    );*)
  go (Sdl.fill_rect surf recto (color_to_int32 surf color))
  (* if ml then ( *)
  (*   printd debug_graphics "Unlocking surface"; *)
  (*   Sdl.unlock_surface surf *)
  (* ) *)

(** create a surface of the same pixel format as surf, filled with color *)
let create_surface ?like:surf ?renderer ?color w h =
  if w=0 || h=0 then failwith "Error: surface has zero size. You could maybe use a Layout.empty?";
  let depth, color_mask = match surf with
    | Some surf ->
      let d,r,g,b,a = go (Sdl.pixel_format_enum_to_masks
                            (Sdl.get_surface_format_enum surf)) in
      d, (r,g,b,a)
    | None ->
      (match renderer with
       | Some renderer -> mask renderer
       | None ->
         failwith "Creating surface needs either a surface or a renderer")
  in
  let surf = create_rgb_surface ~w ~h ~depth color_mask in
  do_option color (fun c ->
      fill_rect surf None c);
  surf

let create_target ?(format = Sdl.Pixel.format_argb8888) renderer w h =
  create_texture renderer format Sdl.Texture.access_target ~w ~h
(* should clear here? this done in push_target *)

(* read pixel in surface with format_argb8888 *)
(* TODO check bounds *)
(* tested with tests/mask_surface *)
let get_pixel_color surface ~x ~y =
  (* in principle one should check with MUST_LOCK if the surface is RLE encoded
     (otherwise there is no need to lock, see
     https://wiki.libsdl.org/SDL_LockSurface). But that's ok, if not, call to
     Sdl.lock_surface does almost nothing. *)
  go(Sdl.lock_surface surface);
  let pixels = Sdl.get_surface_pixels surface Bigarray.int8_unsigned in
  let pitch = Sdl.get_surface_pitch surface in
  let format_enum = Sdl.get_surface_format_enum surface in
  if !debug then
    if format_enum <> Sdl.Pixel.format_argb8888
    then printd draw_error "get_pixel_color: surface has wrong format";
  (*let format = Sdl.get_surface_format surface in*)
  let w,_ = Sdl.get_surface_size surface in
  let byte_per_pixel = pitch / w in (* just to confirm... *)
  let i0 = y * pitch +  x * byte_per_pixel in
  printd debug_graphics "Getting pixel: surface format=%s; width=%u; pitch=%u; byte_per_pixel=%u; i0=%u" (Sdl.get_pixel_format_name format_enum) w pitch byte_per_pixel i0;
  let open Bigarray in
  let b = Array1.get pixels i0 in (* TODO check order *)
  let g = Array1.get pixels (i0+1) in
  let r = Array1.get pixels (i0+2) in
  let a = Array1.get pixels (i0+3) in
  Sdl.unlock_surface surface;
  printd debug_graphics "color r,g,b,a= %u,%u,%u,%u" r g b a;
  r,g,b,a


(** Sdl quit *)
let quit () =
  printd debug_board "Quitting...";
  List.iter run !cleanup;
  do_option !icon free_surface;
  icon := None;
  printd debug_event "Quitting SDL Events";
  Sdl.quit_sub_system Sdl.Init.events;
  printd debug_graphics "Exit SDL...";
  Sdl.quit ();
  printd debug_graphics
    "Done."

let sdl_flip = Sdl.render_present

let default_dpi = 110

(* The variables [dpi_x/yscale] will be updated at initialization. On MacOS and
   iOS they can be larger than 1, meaning that the user resolution reported by
   the OS is in fact lower than the true pixel resolution. BOGUE will use the
   true pixel resolution in order to produce sharp graphics. *)
let dpi_xscale = ref 1.
let dpi_yscale = ref 1.
let[@inline] dpi_rescalex x = round (float x *. !dpi_xscale)
let[@inline] dpi_rescaley y = round (float y *. !dpi_yscale)

(* From OS pixels to true physical pixels *)
let[@inline] dpi_rescale (x,y) =
  (round (float x *. !dpi_xscale), round (float y *. !dpi_yscale))

(* This takes the "High-DPI" pixels as reported by the OS (iOS or MacOS) and
   return the BOGUE logical pixels. *)
let dpi_unscale_pos (x,y) =
  round (!dpi_xscale *. float x /. !Theme.scale),
  round (!dpi_yscale *. float y /. !Theme.scale)

(** From BOGUE logical pixels to physical pixels *)
let[@inline] to_pixels (x,y) =
  dpi_rescale (scale_pos (x,y))

(* Get the window size in true physical pixels *)
let get_window_size win =
  dpi_rescale (Sdl.get_window_size win)

 (* [set_window_size] will emit E.window_event_size_changed event. *)
let set_window_size win ~w ~h =
  Sdl.set_window_size win ~w:(round (float w /. !dpi_xscale))
      ~h:(round (float h /. !dpi_yscale))

let get_window_position win =
  dpi_rescale (Sdl.get_window_position win)

let set_window_position win x y =
  let x, y = if !dpi_xscale *. !dpi_yscale = 0.
    then begin
      printd (debug_error + debug_graphics)
        "[set_window_position] should not be called when the DPI scale has not \
         been detected";
      x, y
    end
    else round (float x /. !dpi_xscale), round (float y /. !dpi_yscale) in
  Sdl.set_window_position win ~x ~y

(** get the canvas window size *)
let window_size canvas =
  get_window_size canvas.window

let get_dpi () =
  match Sdl.get_display_dpi 0 with
  | Ok (x,_,_) -> Some (round x)
  | Error (`Msg m) ->
    printd (debug_error+debug_graphics)
      "SDL get DPI error: %s" m;
    try
      (* Try to obtain the monitor's DPI on linux systems. Does not work with
         multiple monitors. *)
      let proc = Unix.open_process_in
          "xdpyinfo | grep resolution | awk '{print $2}'" in
      let res = input_line proc in
      match Unix.close_process_in proc with
      | Unix.WEXITED 0 ->
        let i = String.index res 'x' in
        let dpi =int_of_string (String.sub res 0 i) in
        printd debug_graphics "Detected DPI=%u" dpi;
        Some dpi
      | _ -> printd debug_warning
               "Cannot get monitor's DPI from [%s]." res;
        None
    with
    | _ -> printd debug_warning
             "Cannot get monitor's DPI from xdpyinfo.";
      None

(* Choose a reasonable scale. Probably not OK in case of multiple monitors. *)
let detect_set_scale () =
  let dpi = default (get_dpi ()) default_dpi in
  printd debug_graphics "DPI from system: %d" dpi;
  let dpi =
   if dpi < default_dpi && Sdl.get_current_video_driver () = Some "wayland" then
   match Sdl.create_window ~w:10 ~h:10 "SCALE detect"
          Sdl.Window.(windowed + resizable + hidden +
                      opengl + allow_highdpi) with
   | Ok win ->
      let w, h = Sdl.get_window_size win in
      let w', h' = Sdl.gl_get_drawable_size win in
      Sdl.destroy_window win;
      printd debug_graphics "Autodetect multiplier: window(%d,%d), drawable(%d,%d)" w h w' h';
      float dpi *. Float.(max (float w' /. float w) 1.)
   | Error (`Msg m) ->
    printd (debug_error+debug_graphics)
      "SDL autodetect DPI window creation error: %s" m;
    float dpi
   else float dpi
  in
  let s = if dpi <= float default_dpi then 1. else (dpi /. (float default_dpi)) in
  let s = Float.round (4. *. s) /. 4. in (* 0.25 increments for scale *)
  Theme.set_scale s;
  printd (debug_graphics+debug_warning) "Using SCALE=%f" !Theme.scale

let video_init () =
  if Sdl.was_init (Some Sdl.Init.video) = Sdl.Init.video
  then printd debug_graphics "SDL Video already initialized"
  else begin
    let () = match Sdl.init_sub_system Sdl.Init.video with
      | Ok () ->
        printd debug_graphics "SDL Video initialized";
        at_cleanup (fun () ->
            printd debug_graphics "Quitting SDL Video";
            Sdl.quit_sub_system Sdl.Init.video);
      | Error (`Msg msg) ->
        Sdl.log "%s" msg;
        printd (debug_error+debug_graphics)
          "SDL Video init failed with: %s\nYou will not be able to open any \
           window." msg;
        go (Sdl.init_sub_system Sdl.Init.nothing) in
    if !icon = None
    then icon := Some (sdl_image_load (Theme.current ^ "/bogue-icon.png"))
  end;
  if !Theme.scale = 0. then detect_set_scale ()

let ttf_init () =
  let open Tsdl_ttf in
  let () = video_init () in
  if not (Ttf.was_init ()) then
    (go (Ttf.init ());
     at_cleanup (fun () ->
         printd debug_graphics "Quitting SDL TTF";
         Ttf.quit ());
     printd debug_graphics "SDL TTF initialized")

(* Initialize SDL_Image. this is not really necessary, as the SDL_Image doc says
   that the system will be initialized at the first use of a function. The only
   advantage of using init is to avoid doing it in the main event loop (which
   might cause delay) *)
let img_init () =
  if TImage.(init Init.empty = Init.empty) (* not initted *)
  then begin
    let () = video_init () in
    let flags = TImage.Init.(jpg + png + tif) in
    let initted = TImage.(init flags) in
    if initted <> flags
    then printd draw_error "SDL Image could not be initialized"
    else printd debug_graphics "SDL Image initialized";
    at_cleanup (fun () ->
        printd debug_graphics "Quitting SDL Image";
        TImage.quit ()
      )
  end

(* ici ? *)
let load_image renderer file =
  let file = Theme.get_path file in
  printd debug_io "Loading image file %s" file;
  img_init ();
  let tex = go (TImage.load_texture renderer file) in
  printd debug_io "Done loading %s" file;
  tex

(* either load an image (eg: "images.png") or a font-awesome symbol (eg:
   "fa:circle") into a texture *)
let load_image_or_fa ?(fg = opaque menu_hl_color) renderer path =
  if startswith path "fa:"
  then let fa = String.(sub path 3 (length path - 3)) in
    let fa_font = open_font Theme.fa_font Theme.(scale_int fa_font_size) in
    ttf_texture renderer fa_font (Theme.fa_symbol fa) (create_color fg)
  else load_image renderer path (* TODO SCALE texture with Theme *)

(* convert a string (like Theme.background) to a fill *)
  (* "file:themes/paper/paper.png" *)
  (* "file:themes/textures/grey_wash_wall/grey_wash_wall.png" *)
let fill_of_string renderer s =
  let open String in
  if startswith s "file:"
  then Pattern (load_image renderer (Theme.get_path (sub s 5 (length s - 5))))
  else if startswith s "color:"
  then let r,g,b = find_color (sub s 6 (length s - 6)) in
    printd debug_graphics "Fill color = %u,%u,%u" r g b;
    Solid (opaque (r,g,b))
  else (printd draw_error "Wrong background format. Expecting color:... or file:..., got %s instead" s;
        Solid (opaque pale_grey))


let svg_loader =
  if which "rsvg-convert" <> None then "rsvg-convert"
  else if which "rsvg" <> None then "rsvg"
  else begin
    printd (debug_warning + debug_io)
      "Cannot find rsvg converter. You will not be able to load SVG images.";
    ""
  end

(* load svg using rsvg from command-line. Return name of output png file *)
(* rsvg -w 1024 -h 1024 input.svg -o output.png *)
(* maybe better (but slower) with inkscape: *)
(* inkscape w3c-logo-white.svg -w 400 -e aaa.png *)
(* w,h are logical (=scaled) sizes *)
(* We check rsvg or rsvg-convert. On error, simply returns the original file
   name.  *)
let convert_svg ?w ?h file =
  let file = Theme.get_path file in
  printd debug_io "Rendering png file %s" file;
  let tmp = Filename.temp_file "bogue" ".png" in
  at_exit (fun () -> Sys.remove tmp);
  let args = match w,h with
    | None, None -> ""
    | Some w, None -> (sprintf "-w %u -a " (Theme.scale_int w))
    | None, Some h -> (sprintf "-h %u -a " (Theme.scale_int h))
    | Some w, Some h -> (sprintf "-w %u -h %u "
                           (Theme.scale_int w) (Theme.scale_int h)) in
  let ret = match svg_loader with
    | "rsvg" -> Sys.command (sprintf "rsvg %s %s %s" args file tmp)
    | "rsvg-convert" ->
       Sys.command (sprintf "rsvg-convert %s %s > %s" args file tmp)
    | _ -> printd (debug_error + debug_io)
             "You should install rsvg or rsvg-convert to be able to load SVG \
              images."; -1
  in
  if ret <> 0
  then begin
      printd (debug_io + debug_error)
        "Converting %s to %s via rsvg failed with exit code %u." file tmp ret;
      file
    end
  else tmp

(* true pixel size *)
let image_size file =
  let file = Theme.get_path file in
  printd debug_io "Checking image file size %s" file;
  img_init ();
  let surf = sdl_image_load file in
  let size = Sdl.get_surface_size surf in
  free_surface surf;
  size

(** create a texture filled with a color *)
(* One could also create a target texture and clear it with color, but tests in
   tests/line suggest it's no faster. *)
let texture ?(color = opaque grey) renderer ~w ~h =
  let surf = create_surface ~renderer ~color w h in
  let tex = create_texture_from_surface renderer surf in
  free_surface surf;
  tex

(** draw a filled rectangle *)
let box renderer ?bg x y w h =
  if w * h = 0
  then printd debug_graphics "Not drawing empty box at (%d, %d)" x y
  else begin
      printd debug_graphics "Drawing box (%d, %d) (%d, %d)" x y (x+w-1) (y+h-1);
      let r = Sdl.Rect.create ~x ~y ~w ~h in
      do_option bg (set_color renderer);
      go (Sdl.render_fill_rect renderer (Some r))
    end

(** create a "blit" of a filled rectangle *)
let box_to_layer canvas layer ?(bg = opaque grey) ?voffset x y w h =
  let tex = texture canvas.renderer ~color:bg ~w ~h in
  let dst = Sdl.Rect.create ~x ~y ~w ~h in
  forget_texture tex;
  make_blit ?voffset ~dst canvas layer tex

(** save and reset some useful settings before setting a render target *)
(* TODO : not thread safe !*)
let push_target ?(clear=true) ?(bg=none) renderer target =
  (* we save the clip rectangle of the current target *)
  let clip = if Sdl.render_is_clip_enabled renderer
             then Some (Sdl.render_get_clip_rect renderer)
             else None in
  let color = go (Sdl.get_render_draw_color renderer) in
  let old_target = Sdl.get_render_target renderer in
  (* now switch to the new target *)
  go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_blend);
  go (Sdl.set_render_target renderer (Some target));
  (* go (Sdl.render_set_clip_rect renderer None); *)
  set_color renderer bg;
  if clear then go (Sdl.render_clear renderer);
  clip, old_target, color (* TODO include the renderer here *)

(** restore the settings saved by "push_target" *)
let pop_target renderer (clip, old_target, (r,g,b,a)) =
  go (Sdl.set_render_target renderer old_target);
  go (Sdl.render_set_clip_rect renderer clip);
  go (Sdl.set_render_draw_color renderer r g b a)

(* fill Some target with a pattern. If target is None, use the default target *)
(* WARNING: the render-target method does NOT work if the window is hidden *)
let fill_pattern ?rect renderer target pattern =
  let x0, y0, w, h = match rect with
    | None -> let w,h = match target with
        | None -> go (Sdl.get_renderer_output_size renderer)
        | Some tex -> tex_size tex
      in (0,0,w,h)
    | Some r -> Sdl.Rect.(x r, y r, w r, h r) in
  printd debug_graphics "Target size (%i,%i; %i,%i)" x0 y0 w h;
  let pw, ph = tex_size pattern in
  let save_target = map_option target (push_target renderer) in
  (* Is push_target necessary? it's done in b_box.ml *)
  let rec loop x y =
    printd debug_graphics "LOOP (%i,%i)" x y;
    if x >= x0 + w then loop x0 (y + ph)
    else if y >= y0 + h then ()
    else let rw = min pw (x0 + w - x) in
      let rh = min ph (y0 + h - y) in
      let src = Sdl.Rect.create ~x:0 ~y:0 ~w:rw ~h:rh in
      let dst = Sdl.Rect.create ~x ~y ~w:rw ~h:rh in
      go (Sdl.render_copy ~src ~dst renderer pattern);
      loop (x + pw) y in
  loop x0 y0;
  do_option save_target (pop_target renderer)

(* create a texture filled with the repeated pattern *)
let generate_background window renderer pattern =
  let flags = Sdl.get_window_flags window in
  if Sdl.Window.(test flags hidden)
  (* WARNING: target won't work if window is hidden *)
  then (printd debug_graphics "Background cannot be generated because the window is hidden";
        None)
  else begin
    let w,h = (* go (Sdl.get_renderer_output_size renderer) *)
      get_window_size window
    in
    let target = Some (create_target renderer w h) in
    printd debug_graphics "Creating background (%d,%d)" w h;
    fill_pattern renderer target pattern;
    target
  end

(* Use [update_background] to recreate the main background, typically when
   window size has changed. For a simple clear, use [clear_canvas] *)
let update_background canvas =
  printd debug_graphics "Update background";
  match canvas.fill with
  | Solid color ->
    set_color canvas.renderer color;
    go (Sdl.render_clear canvas.renderer)
  | Pattern t ->
    do_option canvas.textures.background forget_texture;
    canvas.textures.background <- generate_background canvas.window
                                    canvas.renderer t

(* return a new copy of the rotated texture (around its center) *)
let copy_rotate_texture renderer angle tex =
  let w, h = tex_size tex in
  let target = create_target renderer w h in
  let push = push_target renderer target in
  go (Sdl.set_texture_blend_mode tex Sdl.Blend.mode_none);
  go (Sdl.render_copy_ex renderer tex angle None Sdl.Flip.none);
  pop_target renderer push;
  target

(* TODO: better to save surfaces instead of textures ? otherwise setting
   eg. alpha on one texture will affect everywhere it is blitted. Cf for example
   check buttons *)
let load_textures window renderer fill = (* use hashtbl ? *)
  (* let check_on = load_image renderer Theme.check_on in *)
  (* let check_off = load_image renderer Theme.check_off in *)
  ttf_init ();
  (* TODO store font in the table *)
  let check_on = load_image_or_fa renderer Theme.check_on in
  let check_off = load_image_or_fa renderer Theme.check_off in
  (* let symbol_font = go (Tsdl_ttf.Ttf.open_font Theme.fa_font
   *                         Theme.(scale_int fa_font_size)) in *)
  let sdl_grey = Sdl.Color.create ~r:70 ~g:70 ~b:70 ~a:255 in
  (* let check_on = ttf_texture renderer symbol_font
   *     (Theme.fa_symbol "check-square-o") sdl_grey in
   * let check_off = ttf_texture renderer symbol_font
   *     (Theme.fa_symbol "square-o") sdl_grey in *)
  (* the symbol for circles is too big. we reduce: *)
  let size = 7 * Theme.(scale_int fa_font_size) / 10 in
  let symbol_font = go (Tsdl_ttf.Ttf.open_font Theme.fa_font size) in
  let radio_on = ttf_texture renderer symbol_font
      (Theme.fa_symbol "dot-circle-o") sdl_grey in
  let radio_off = ttf_texture renderer symbol_font
      (Theme.fa_symbol "circle-thin") sdl_grey in
  let background = match fill with
    | Pattern t -> generate_background window renderer t
    | _ -> None in
  { check_on;
    check_off;
    radio_on;
    radio_off;
    background }

let create_window  ?x ?y ~w ~h name =
  match Sdl.create_window ?x ?y ~w ~h name
          Sdl.Window.(windowed + resizable + hidden +
                      opengl + allow_highdpi) with
  | Ok w -> printd debug_graphics "SDL Window created"; w
  | Error _ -> let er = Sdl.get_error () in
    print "Error creating SDL Window: %s\n" er;
    if er = "Couldn't find matching GLX visual"
    then print "This may happen if you are running from a Virtual Machine.\n You \
                should try 'export SDL_VIDEO_X11_VISUALID='";
    raise (Sdl_error ("SDL ERROR: " ^ (Sdl.get_error ())))

(* Sdl init. [w,h] is the physical size of the window in pixels. In case of
   High-DPI mode, SDL might actually produce a larger window. We need to correct
   this, because we have our own DPI engine.

   See https://wiki.libsdl.org/SDL_CreateWindow:

   «If the window is created with the SDL_WINDOW_ALLOW_HIGHDPI flag, its size in
   pixels may differ from its size in screen coordinates on platforms with
   high-DPI support (e.g. iOS and macOS). Use SDL_GetWindowSize() to query the
   client area's size in screen coordinates, and SDL_GL_GetDrawableSize() or
   SDL_GetRendererOutputSize() to query the drawable size in pixels.»

*)
(* This function returns a new canvas. A canvas has the physical size in pixels
   of the rendering window, ie after scaling. *)
(* if an Sdl window is provided, we try to use it... *)
let init ?window ?(name="BOGUE Window") ?fill ?x ?y ~w ~h () =
  video_init ();
  if Theme.opengl_multisample
  then begin
    go (Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1);
    go (Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 4);
    if go (Sdl.gl_get_attribute Sdl.Gl.multisamplebuffers) <> 1
    then printd (debug_error + debug_graphics)
        "The opengl driver does not support multisampling"
  end;
  let win = default_lazy window (lazy (create_window ?x ?y ~w ~h name)) in
  do_option !icon (Sdl.set_window_icon win);
  Sdl.set_window_minimum_size win ~w:8 ~h:8;
  let px = Sdl.get_window_pixel_format win in
  printd debug_graphics "Window pixel format = %s" (Sdl.get_pixel_format_name px);
  let renderer = match window with
    | None -> go (Sdl.create_renderer
                    ~flags:Sdl.Renderer.(targettexture + presentvsync) win)
    | Some win -> match Sdl.get_renderer win with
      | Ok w -> printd debug_graphics "Using existing renderer"; w
      | Error _ ->
        go (Sdl.create_renderer ~flags:Sdl.Renderer.targettexture win) in
  let rw, rh = Sdl.gl_get_drawable_size win in
  if window = None && (rw, rh) <> (w,h) then begin
    dpi_xscale := float rw /. float w;
    dpi_yscale := float rh /. float h;
    printd (debug_graphics+debug_warning)
      "This display imposes a hard scaling of (%f,%f)." !dpi_xscale !dpi_yscale;
    set_window_size win ~w ~h
  end;
  let ri = go (Sdl.get_renderer_info renderer) in
  let ww, wh = Sdl.get_window_size win in
  printd debug_graphics "Window size (SDL) = (%u,%u)" ww wh;
  let wx, wy = Sdl.get_window_position win in
  printd debug_graphics "Window position (SDL) = (%d,%d)" wx wy;
  printd debug_graphics "Renderer name = %s" ri.Sdl.ri_name;
  let rw, rh = go(Sdl.get_renderer_output_size renderer) in
  printd debug_graphics "Renderer size = (%u,%u)" rw rh;
  printd debug_graphics "Render target supported: %b" (Sdl.render_target_supported renderer);
  printd debug_graphics "Renderer pixel formats: %s"
    (String.concat ", " (List.map Sdl.get_pixel_format_name ri.Sdl.ri_texture_formats));
  (* go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend); *)

  (* set dummy solid background in case of new window *)
  if window = None then begin
    set_color renderer (opaque red);
    go (Sdl.render_clear renderer)
  end;

  printd debug_graphics "Canvas created";

  let fill = default_lazy fill
      (lazy (fill_of_string renderer Theme.background)) in
  let textures = load_textures win renderer fill in
  { renderer;
    window = win;
    textures;
    fill;
    gl_context = None;
  }

(* rarely used...? *)
let clear_layers layer =
  Chain.iter (fun q ->
      if not (Queue.is_empty q)
      then begin
        printd debug_graphics "Clearing layer";
        Queue.clear q
      end) layer

(* Clear the canvas, using the background color, or the pre-computed
   texture. For re-computing the texture, use [update_background]. *)
let clear_canvas c =
  printd debug_graphics "Clear canvas";
  (* go (Sdl.render_set_clip_rect c.renderer None); I lost many hours due to
     this one. If the above line is active, the render_clear only affects part
     of the renderer, no idea why... What's even more illogical, the SDL doc
     says that render_clear does not take clip_rect into account... *)
  let color = match c.fill with
    | Solid x -> x
    | _ -> opaque grey in
  set_color c.renderer color;
  go (Sdl.render_clear c.renderer);
  (* paste background image *)
  do_option c.textures.background
    (fun tex -> go (Sdl.render_copy c.renderer tex))

let rect_to_layer ?color ?bg canvas layer (x,y) w h =
  let target = create_target canvas.renderer w h in
  let push = push_target canvas.renderer target in
  do_option bg (fun c ->
      set_color canvas.renderer c;
      go(Sdl.render_clear canvas.renderer));
  do_option color (set_color canvas.renderer);
  go (Sdl.render_draw_lines canvas.renderer
        [Sdl.Point.create ~x:0 ~y:0;
         Sdl.Point.create ~x:(w - 1) ~y:0;
         Sdl.Point.create ~x:(w - 1) ~y:(h - 1);
         Sdl.Point.create ~x:0 ~y:(h - 1);
         Sdl.Point.create ~x:0 ~y:0]);
  pop_target canvas.renderer push;
  let dst = Sdl.Rect.create ~x ~y ~w ~h in
  forget_texture target;
  make_blit ~dst canvas layer target


(* Like the hand of a clock. *)
(* if not specified, the thickness is computed to that the full disc of correct
   radius is filled when rotating the hand with 180*radius/100 steps of angle
   2pi/steps. If thickness is specified (and is large), then rotating the ray
   may produce a disc of larger radius *)
let make_ray renderer ~bg ~radius ~width ?thickness x y =
  (* let depth,cmask = mask renderer in *)
  let steps = 180*radius/100 in
  let alpha = pi /. (float steps) in (* step angle = 2 alpha *)
  let beta = alpha +. 1. /. (float radius) /. 2. in
  let w = round ((float radius) *. cos (beta)) in
  (* w is just a silly approximation of the optimal length of the rectangle we use
     to draw the ring. In practice w = radius !! *)
  let h = match thickness with (* thickness of the ray *)
    | None -> round (2. *. (float radius) *. sin (beta))
    (* environ h = 1 + round (2. *. (sin alpha) *. (float radius)) *)
    | Some t -> t in
  let surf = create_surface ~renderer ~color:none w h in
  (* let surf = create_rgb_surface ~w ~h ~depth cmask in *)
  printd debug_graphics "Ring: radius:%d, length of ray:%d, heigth:%d, steps=%d" radius w h steps;
  (* fill_rect surf None none; *)
  let r = Sdl.Rect.create ~x:(w-width) ~y:0 ~w:(width) ~h in
  fill_rect surf (Some r) bg;
  (* let r = Sdl.Rect.create ~x:(radius-1) ~y:0 ~w:1 ~h in *)
  (* fill_rect surf (Some r) (more_transp bg); *)
  let tex = create_texture_from_surface renderer surf in
  free_surface surf;
  let center = Sdl.Point.create ~x:0 ~y:(h/2) in
  (* =center coordinates relative to the dst rect below *)
  let dst = Sdl.Rect.create ~x ~y:(y-h/2) ~w ~h in
  tex, center, dst, steps

(* draw the "ray" (radius) on the renderer *)
let ray renderer ?(bg = opaque black) ~radius ~width ?thickness ~angle x y =
  let tex, center, dst, _ = make_ray renderer ?thickness ~bg ~radius ~width x y in
    go(Sdl.render_copy_ex renderer ~dst tex angle (Some center) Sdl.Flip.none)

let ray_to_layer canvas layer ?(bg = opaque black) ?voffset ~radius ~width ?thickness ~angle x y =
  let tex, center, dst, _ = make_ray canvas.renderer
      ?thickness ~bg ~radius ~width x y in
  let transform = make_transform ~angle ~center () in
  (* { flip = Sdl.Flip.none; angle; center = Some center; alpha = 255 } in *)
  forget_texture tex;
  make_blit ?voffset ~dst ~transform canvas layer tex

let center x0 big_w small_w =
  x0 + (big_w - small_w) / 2

let align align x0 big_w small_w =
  match align with
  | Min -> x0
  | Center -> x0 + (big_w - small_w) / 2
  | Max -> x0 + big_w - small_w

(** copy the texture on the canvas, clipped (or else) in the given
    area *)
let copy_tex ?(overlay = TopRight) renderer tex area x y =
  let w, h = tex_size tex in
  let rect = Sdl.Rect.create ~x ~y ~w ~h in
  let dst = Sdl.intersect_rect rect area in
  do_option dst (fun dst ->
      let src = (let open Sdl in match overlay with
        | Shrink -> Rect.create ~x:0 ~y:0 ~w ~h
        | Clip -> Rect.create ~x:(Rect.x dst - x) ~y:(Rect.y dst - y)
                    ~w:(Rect.w dst) ~h:(Rect.h dst)
        | TopRight -> Rect.create ~x:(w - Rect.w dst)
                        ~y:0 ~w:(Rect.w dst) ~h:(Rect.h dst)
        | Xoffset x0 -> Rect.create ~x:(min x0 (w - Rect.w dst))
                          ~y:0 ~w:(Rect.w dst) ~h:(Rect.h dst)
        ) in
      go (Sdl.render_copy ~src ~dst renderer tex))

(* new version for layers *)
let copy_tex_to_layer ?(overlay = TopRight) ?voffset ?transform
      canvas layer tex area x y =
  let w, h = tex_size tex in
  let rect = Sdl.Rect.create ~x ~y ~w ~h in
  let dst = Sdl.intersect_rect rect area in
  let src = match dst with
    | None -> None
    | Some dst -> Some (let open Sdl in match overlay with
      | Shrink -> Rect.create ~x:0 ~y:0 ~w ~h
      | Clip -> Rect.create ~x:(Rect.x dst - x) ~y:(Rect.y dst - y)
                  ~w:(Rect.w dst) ~h:(Rect.h dst)
      | TopRight -> Rect.create ~x:(w - Rect.w dst)
                      ~y:0 ~w:(Rect.w dst) ~h:(Rect.h dst)
      | Xoffset x0 -> Rect.create ~x:(min x0 (w - Rect.w dst))
                        ~y:0 ~w:(Rect.w dst) ~h:(Rect.h dst)
      ) in
  make_blit ?src ?dst ?voffset ?transform canvas layer tex

(** copy the texture on the canvas, centered in the given area *)
let center_tex ?(horiz=true) ?(verti=true) renderer tex x y w h =
  let rw, rh = tex_size tex in
  (* we center the texture *)
  let x = if horiz then center x w rw else x in
  let y = if verti then center y h rh else y in
  let dst= Sdl.Rect.create ~x ~y ~w:rw ~h:rh in
  go (Sdl.render_copy ~dst renderer tex)

(* new version for layers. If clip is true and the texture is larger than the
   geometry, we do not center, instead we align from the origin. *)
(* TODO use voffset *)
let center_tex_to_layer ?(horiz=Center) ?(verti=true) ?(clip=true)
      canvas layer tex g =
  let tw, th = tex_size tex in
  let w, h = if clip then imin tw g.w, imin th g.h else tw, th in
  let src =
    if not clip || (tw <= g.w && th <=  g.h)
    then None
    else Some (Sdl.Rect.create ~x:0 ~y:0 ~w ~h) in
  (* we center the texture *)
  let x = match horiz with
    | Center -> center g.x g.w w
    | Min -> g.x
    | Max -> g.x + g.w - w in
  let y = if verti then center g.y g.h h else g.y in
  let dst = Sdl.Rect.create ~x ~y ~w ~h in
  make_blit ~voffset:g.voffset ?src ~dst canvas layer tex

let tex_to_layer canvas layer tex g =
  let w, h = tex_size tex in
  let dst = Sdl.Rect.create ~x:g.x ~y:g.y ~w ~h in
  make_blit ~voffset:g.voffset ~dst canvas layer tex



(********************************************************************************)
(* Drawing primitives. The goal is to provide basic drawing primitives to BOGUE,
   but also to have fun pushing ocaml's limits to find high-quality
   algorithms. For more serious (and faster) graphics, use bogue-cairo instead!
   https://github.com/sanette/bogue-cairo *)

let normsq (x,y) =
  x*x + y*y

(* Euclidian norm. One could also use Stdlib.hypot, but benchmarks say that it
   would be slower, see tests/norm. For default ocamlopt compilation, hypot
   (float x) (float y) takes 40% more time than our norm. *)
let norm (x,y) =
  sqrt(float (x*x + y*y))

(* Euclidian distance *)
let dist (x,y) (x0,y0) =
  norm (x-x0, y-y0)

(* The type for fast plot of points of the same color *)
type point_buffer =
  { ba : (int32, Bigarray.int32_elt) Sdl.bigarray;
    mutable index : int;
    len : int }

(* Create buffer for n pixels *)
let create_buffer n =
  let ba = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (2*n) in
  { ba; index = 0; len = 2*n }

(* Yes, it is unsafe, and can crash, but that's not "too bad" because
   [render_buffer] will tell you if the index is out of bounds. *)
let unsafe_add_point_to_buffer buffer x y =
  Bigarray.Array1.unsafe_set buffer.ba buffer.index (Int32.of_int x);
  buffer.index <- buffer.index + 1;
  Bigarray.Array1.unsafe_set buffer.ba buffer.index (Int32.of_int y);
  buffer.index <- buffer.index + 1 [@@inline]

let add_point_to_buffer buffer x y =
  if buffer.index + 1 < buffer.len
  then unsafe_add_point_to_buffer buffer x y
  else failwith "Buffer index ouf of bounds"

let render_buffer buffer renderer =
  Sdl.render_draw_points_ba renderer
    (Bigarray.Array1.sub buffer.ba 0 buffer.index) |> go


(************************************************************************************)
(* CIRCLE                                                                           *)
(*           |                                          y
             |                \3 | 2/                   ^
             |              4  \ | /  1                 |
             |              ----------------            +---> x
             |              5  / | \  8
             |                /6 | 7\

                            octants 1,3,5,7 are closed (include boundary. eg: (1)
                            y >= 0
                            x >= 0
                            y <= x)
                            octants 2;4;6;8 are open (exclude boundary).

                            But in fact the center (0,0) should always be excluded,
                            otherwise it will be
                            repeated 4 times.

                            (flip upside-down for SDL - we don't care for a circle)
*)



(* Draw a circle on the renderer. Most of the time you want to
   [set_render_draw_blend_mode renderer Blend.mode_blend] before calling this
   function.  *)
(* cf experiments in circle.ml *)
(* Nowadays a circle with only one-pixel width is not so useful. See
   [annulus].*)
let circle renderer (r,g,b,a0) x0 y0 radius =

  if radius = 0 then begin
    go (Sdl.set_render_draw_color renderer r g b a0);
    go (Sdl.render_draw_point renderer x0 y0)
  end

  else
    let alpha0 = float a0 in

    let rec loop (x,y) e =
      if x >= y then begin
        let x',e' = if e > 0. (* blending on the left *)
          then x-1, e -. (float (2*x - 1))
          else (* blending on the right *)
            x+1, e +. (float (2*x + 1)) in
        let alpha = abs_float e' /. (abs_float e +. abs_float e') in
        let alpha' = 1. -. alpha in

        (* ESSAI: ça fait des cercles plus 'fins'...*)
        (* let alpha = alpha *. alpha in *)
        (* let alpha' = alpha' *. alpha' in *)

        let a = round (alpha *. alpha0) in
        let a' = round (alpha' *. alpha0) in

        (* color A: *) (* TODO regrouper avec color A ci-dessous *)
        go (Sdl.set_render_draw_color renderer r g b a);
        let bf = create_buffer 8 in
        unsafe_add_point_to_buffer bf (x0+x) (y0+y); (* octant 1 *)
        unsafe_add_point_to_buffer bf (x0-x) (y0-y); (* octant 5 *)
        unsafe_add_point_to_buffer bf (x0-y) (y0+x); (* octant 3 *)
        unsafe_add_point_to_buffer bf (x0+y) (y0-x); (* octant 7 *)

        (* now we need to be careful avoiding repeated/missing pixels along the
           diagonal x=y *)
        if y <> 0 && y <> x then begin
          unsafe_add_point_to_buffer bf (x0+y) (y0+x); (* octant 2 *)
          unsafe_add_point_to_buffer bf (x0-x) (y0+y); (* octant 4 *)
          unsafe_add_point_to_buffer bf (x0-y) (y0-x); (* octant 6 *)
          unsafe_add_point_to_buffer bf (x0+x) (y0-y); (* octant 8 *)
        end;
        render_buffer bf renderer;

        go (Sdl.set_render_draw_color renderer r g b a');
        let bf = create_buffer 8 in
        if x' >= y then begin
          (* color A': *)
          unsafe_add_point_to_buffer bf (x0+x') (y0+y); (* octant 1 *)
          unsafe_add_point_to_buffer bf (x0-x') (y0-y); (* octant 5 *)
          unsafe_add_point_to_buffer bf (x0-y) (y0+x'); (* octant 3 *)
          unsafe_add_point_to_buffer bf (x0+y) (y0-x'); (* octant 7 *)
        end;

        (* now we need to be careful avoiding repeated/missing pixels along the
           diagonal x=y *)
        if y <> 0 && y <> x' then begin
          unsafe_add_point_to_buffer bf (x0+y) (y0+x'); (* octant 2 *)
          unsafe_add_point_to_buffer bf (x0-x') (y0+y); (* octant 4 *)
          unsafe_add_point_to_buffer bf (x0-y) (y0-x'); (* octant 6 *)
          unsafe_add_point_to_buffer bf (x0+x') (y0-y); (* octant 8 *)
        end;
        render_buffer bf renderer;

        (* now what is the next point ? *)
        let e1 = e +. float (2*y + 1) in
        let e2 = e +. float (2*(y-x+1)) in
        if abs_float e1 < abs_float e2
        then loop (x,y+1) e1
        else loop (x-1,y+1) e2
      end
    in
    loop (radius,0) 0.

let buffer_draw_hline buffer x0 x1 y =
  (* Because of SDL bug, we cannot use Sdl.render_draw_line right now (SDL 2.0.10)
     see https://discourse.libsdl.org/t/sdl-renderdrawline-endpoint-inconsistency/22065/8 *)
  assert (x0 <= x1);
  for x = x0 to x1 do
    unsafe_add_point_to_buffer buffer x y
  done

let buffer_draw_vline buffer x y0 y1 =
  (* Because of SDL bug, we cannot use Sdl.render_draw_line right now (SDL 2.0.10)
     see https://discourse.libsdl.org/t/sdl-renderdrawline-endpoint-inconsistency/22065/8 *)
  assert (y0 <= y1);
  for y = y0 to y1 do
    unsafe_add_point_to_buffer buffer x y
  done

(* draw a filled annulus between radius1 and radius2 (inclusive) *)
let annulus renderer (r,g,b,a0) xc yc ~radius1 ~radius2 =
  let alpha0 = float a0 in
  (* TODO if radius1 = radius2 appeler circle *)
  let radius1,radius2 = if radius1 <= radius2
    then imax 0 radius1, radius2
    else imax 0 radius2, radius1 in

  if !debug then assert (radius2 >= 0);

  (* A rough estimate of the necessary buffer size in pixels. We take the square
     of size 2*radius2 + 2 minus the square of diagonal 2*radius1 - 2. *)
  let n = 4*radius2*radius2 - 2*radius1*radius1 + 8*(radius1+radius2+1)  in
  let buffer = create_buffer n in

  (* plot will be called with various colors, we cannot use the global buffer *)
  let plot x y =
    if (x,y) = (0,0)
    then go (Sdl.render_draw_point renderer xc yc)
    else let b = create_buffer 8 in begin
        unsafe_add_point_to_buffer b (xc+x) (yc+y); (* octant 1 *)
        unsafe_add_point_to_buffer b (xc-x) (yc-y); (* octant 5 *)
        unsafe_add_point_to_buffer b (xc-y) (yc+x); (* octant 3 *)
        unsafe_add_point_to_buffer b (xc+y) (yc-x); (* octant 7 *)
        (* Now we draw "plot y x" but careful about boundaries: *)
        if y <> 0 && y <> x then begin
          unsafe_add_point_to_buffer b (xc+y) (yc+x); (* octant 2 *)
          unsafe_add_point_to_buffer b (xc-x) (yc+y); (* octant 4 *)
          unsafe_add_point_to_buffer b (xc-y) (yc-x); (* octant 6 *)
          unsafe_add_point_to_buffer b (xc+x) (yc-y); (* octant 8 *)
        end;
        render_buffer b renderer
      end in

  (* line will always be called with alpha a0, we can use a global buffer *)
  let line x0 x3 y =
    (* the center is treated separately *)
    let x0 = if (x0,y) = (0,0) then begin
        unsafe_add_point_to_buffer buffer xc yc;
        1
      end else x0 in
    (* SDL doesn't want to draw a line of 1 pixel length... *)
    if x0 = x3 then plot x0 y (* one could inline this and use buffer *)
    else if x0 < x3 then begin
      buffer_draw_hline buffer (xc+x0) (xc+x3) (yc+y); (* 1 *)
      buffer_draw_hline buffer (xc-x3) (xc-x0) (yc-y); (* 5 *)
      buffer_draw_vline buffer (xc-y) (yc+x0) (yc+x3); (* 3 *)
      buffer_draw_vline buffer (xc+y) (yc-x3) (yc-x0); (* 7 *)
      if y <> 0
      then
        let x0 = if y=x0 then x0+1 else x0 in
        if x0 = x3 then begin
          unsafe_add_point_to_buffer buffer (xc+y) (yc+x0); (* octant 2 *)
          unsafe_add_point_to_buffer buffer (xc-x0) (yc+y); (* octant 4 *)
          unsafe_add_point_to_buffer buffer (xc-y) (yc-x0); (* octant 6 *)
          unsafe_add_point_to_buffer buffer (xc+x0) (yc-y) (* octant 8 *)
        end
        else begin
          (* 2,4,6,8 *)
          buffer_draw_vline buffer (xc+y) (yc+x0) (yc+x3);
          buffer_draw_hline buffer (xc-x3) (xc-x0) (yc+y);
          buffer_draw_vline buffer (xc-y) (yc-x3) (yc-x0);
          buffer_draw_hline buffer (xc+x0) (xc+x3) (yc-y)
        end
    end in

  let rec loop x1 e1 x2 e2 y =
    if y<=x2+1 then begin
      (* we need also the case y=x2+1 because if e2<0 we blend on the right, and
         the point (x2+1,y) will be inside the diagonal => must be drawn *)
      let x0 = (* where to start the horizontal line *)
        if x1<y (* first circle finished *)
        (* TODO here do we need also y = x1+1 ? *)
        then y (* we start on the diagonal (x0=y) *)
        else
          begin (* first radius *)
            if e1 >= 0. (* blending on the left *)
            (* the case e1=0 is only useful when radius1=0 (filled
                      circle) *)
            then let e1' = e1 -. (float (2*x1 - 1)) in
              let alpha1' =  abs_float e1 /. (abs_float e1 +. abs_float e1') in
              let a1' = round (alpha0 *. alpha1') in
              go (Sdl.set_render_draw_color renderer r g b a1');
              if x1 > y then plot (x1-1) y;
              x1
            else let e1' = e1 +. (float (2*x1 + 1)) in
              let alpha1 = abs_float e1' /. (abs_float e1 +. abs_float e1') in
              let a1 = round (alpha0 *. alpha1) in
              go (Sdl.set_render_draw_color renderer r g b a1);
              plot x1 y;
              x1+1
          end in
      let x3 = (* where to finish the horizontal line *)
        if e2 > 0.
        then let e2' = e2 -. (float (2*x2 - 1)) in
          let alpha2 =  abs_float e2' /. (abs_float e2 +. abs_float e2') in
          let a2 = round (alpha0 *. alpha2) in
          go (Sdl.set_render_draw_color renderer r g b a2);
          if x2 >= y then plot x2 y;
          x2-1
        else (* blending on the right *)
          let e2' = e2 +. (float (2*x2 + 1)) in
          let alpha2' = abs_float e2 /. (abs_float e2 +. abs_float e2') in
          let a2' = round (alpha0 *. alpha2') in
          go (Sdl.set_render_draw_color renderer r g b a2');
          plot (x2+1) y;
          x2 in
      (* draw horizontal line from (xc+x0,yc+y) to (xc+x3,yc+y) *)
      (* WARNING SDL doesn't want to draw a line of 1 pixel length... *)
      (* if x0 < x3 then begin *)
      (*     go (Sdl.set_render_draw_color renderer r g b a0); *)
      (*     go (Sdl.render_draw_line renderer (xc+x0) (yc+y) (xc+x3) (yc+y)); *)
      (*   end *)
      (* else if x0=x3 then begin *)
      (*     go (Sdl.set_render_draw_color renderer r g b a0); *)
      (*     go (Sdl.render_draw_point renderer (xc+x0) (yc+y)); *)
      (*   end; *)
      (* note that if y=x2+1 then x0>x3 and no line will be drawn, as
         required *)
      go (Sdl.set_render_draw_color renderer r g b a0);
      line x0 x3 y;

      (* now what are the next points? *)
      let x1', e1' =
        if x1 < y then x1, 0.
        else
          let ea = e1 +. float (2*y + 1) in
          let eb = e1 +. float (2*(y-x1+1)) in
          if abs_float ea < abs_float eb
          then x1, ea
          else x1-1, eb in
      let x2', e2' =
        let ea = e2 +. float (2*y + 1) in
        let eb = e2 +. float (2*(y-x2+1)) in
        if abs_float ea < abs_float eb
        then x2, ea
        else x2-1, eb in
      loop x1' e1' x2' e2' (y+1)
    end
  in
  loop radius1 0. radius2 0. 0;
  render_buffer buffer renderer

(* in this version, we can choose which one of the 8 octants to draw. But recall
   that some of them are "closed" (odd numbers) and the other are "open" (even
   numbers), and the "open" ones are of course slightly smaller *)
(* the octants parameter is an 8bits integer corresponding to the 8 octants *)
(* WARNING: if radius1 = 0, (camembert) the center will always be drawn *)
(* Of course, the antialias=false version could be written separately to be much
   faster *)
let annulus_octants renderer (r,g,b,a0) ?(antialias=true) ?(octants=255)
    xc yc radius1 radius2 =

  let alpha0 = float a0 in
  (* TODO if radius1 = radius2 appeler circle *)
  let radius1, radius2 = if radius1 <= radius2
    then radius1, radius2
    else radius2, radius1 in

  if !debug then assert (radius2 >= 0);

  (* A rough estimate of the necessary buffer size in pixels *)
  let n_octants = octants land 1 + (octants lsr 1) land 1 +
                  (octants lsr 2) land 1 + (octants lsr 3) land 1 +
                  (octants lsr 4) land 1 + (octants lsr 5) land 1 +
                  (octants lsr 6) land 1 + (octants lsr 7) land 1 in
  let r1 = round (float radius1 /. 1.42) in
  let n = 4*(radius2 - r1)*(radius2 + r1 + 1)/n_octants + 1 in
  let buffer = create_buffer n in

  let noalpha a = if a = 0 then 0 else a0 in

  let plot x y =
    if (x,y) = (0,0) then go (Sdl.render_draw_point renderer xc yc)
    else let b = create_buffer 8 in begin
      if octants land 1 = 1 then (* octant 1 *)
        unsafe_add_point_to_buffer b (xc+x) (yc+y);
      if octants land 16 = 16 then (* octant 5 *)
        unsafe_add_point_to_buffer b (xc-x) (yc-y);
      if octants land 4 = 4 then (* octant 3 *)
        unsafe_add_point_to_buffer b (xc-y) (yc+x);
      if octants land 64 = 64 then (* octant 7 *)
        unsafe_add_point_to_buffer b (xc+y) (yc-x);
      if y <> 0 && y <> x then begin (* same as "plot y x" *)
        if octants land 2 = 2 then (* octant 2 *)
          unsafe_add_point_to_buffer b (xc+y) (yc+x);
        if octants land 8 = 8 then (* octant 4 *)
          unsafe_add_point_to_buffer b (xc-x) (yc+y);
        if octants land 32 = 32 then (* octant 6 *)
          unsafe_add_point_to_buffer b (xc-y) (yc-x);
        if octants land 128 = 128 then (* octant 8 *)
          unsafe_add_point_to_buffer b (xc+x) (yc-y);
      end;
      render_buffer b renderer
    end in

  (* Draw a line of the main color: with alpha=a0 *)
  let line x0 x3 y =
    (* the center is treated separately *)
    let x0 = if (x0,y) = (0,0) then begin
        unsafe_add_point_to_buffer buffer xc yc;
        1
      end else x0 in
    (* SDL doesn't want to draw a line of 1 pixel length... *)
    if x0 = x3 then plot x0 y
    else if x0 < x3 then begin
      if octants land 1 = 1 then
        buffer_draw_hline buffer (xc+x0) (xc+x3) (yc+y); (* 1 *)
      if octants land 16 = 16 then
        buffer_draw_hline buffer (xc-x3) (xc-x0) (yc-y); (* 5 *)
      if octants land 4 = 4 then
        buffer_draw_vline buffer (xc-y) (yc+x0) (yc+x3); (* 3 *)
      if octants land 64 = 64 then
        buffer_draw_vline buffer (xc+y) (yc-x3) (yc-x0); (* 7 *)
      if y <> 0
      then
        let x0 = if y=x0 then x0+1 else x0 in
        if x0 = x3 then begin
          if octants land 2 = 2 then (* octant 2 *)
            unsafe_add_point_to_buffer buffer (xc+y) (yc+x0);
          if octants land 8 = 8 then (* octant 4 *)
            unsafe_add_point_to_buffer buffer (xc-x0) (yc+y);
          if octants land 32 = 32 then (* octant 6 *)
            unsafe_add_point_to_buffer buffer (xc-y) (yc-x0);
          if octants land 128 = 128 then  (* octant 8 *)
            unsafe_add_point_to_buffer buffer (xc+x0) (yc-y)
        end
        else begin
          (* 2,4,6,8 *)
          if octants land 2 = 2 then
            buffer_draw_vline buffer (xc+y) (yc+x0) (yc+x3);
          if octants land 8 = 8 then
            buffer_draw_hline buffer (xc-x3) (xc-x0) (yc+y);
          if octants land 32 = 32 then
            buffer_draw_vline buffer (xc-y) (yc-x3) (yc-x0);
          if octants land 128 = 128 then
            buffer_draw_hline buffer (xc+x0) (xc+x3) (yc-y)
        end
    end in

  let rec loop x1 e1 x2 e2 y =
    if y<=x2+1 then begin
      let x0 = (* where to start the horizontal line *)
        if x1<y (* first circle finished *)
        then y (* we start on the diagonal (x0=y) *)
        else
          begin (* first radius *)
            if e1 >= 0. (* blending on the left *)
            (* the case e1=0 is only useful when radius1=0 (filled
                      circle) *)
            then let e1' = e1 -. (float (2*x1 - 1)) in
              let alpha1' =  abs_float e1 /. (abs_float e1 +. abs_float e1') in
              let a1' = round (alpha0 *. alpha1') in
              let a1' = if antialias then a1' else noalpha a1' in
              go (Sdl.set_render_draw_color renderer r g b a1');
              if x1 > y then plot (x1-1) y;
              x1
            else let e1' = e1 +. (float (2*x1 + 1)) in
              let alpha1 = abs_float e1' /. (abs_float e1 +. abs_float e1') in
              let a1 = round (alpha0 *. alpha1) in
              let a1 = if antialias then a1 else noalpha a1 in
              go (Sdl.set_render_draw_color renderer r g b a1);
              plot x1 y;
              x1+1
          end in
      let x3 = (* where to finish the horizontal line *)
        if e2 > 0.
        then let e2' = e2 -. (float (2*x2 - 1)) in
          let alpha2 =  abs_float e2' /. (abs_float e2 +. abs_float e2') in
          let a2 = round (alpha0 *. alpha2) in
          let a2 = if antialias then a2 else noalpha a2 in
          go (Sdl.set_render_draw_color renderer r g b a2);
          if x2 >= y then plot x2 y;
          x2-1
        else (* blending on the right *)
          let e2' = e2 +. (float (2*x2 + 1)) in
          let alpha2' = abs_float e2 /. (abs_float e2 +. abs_float e2') in
          let a2' = round (alpha0 *. alpha2') in
          let a2' = if antialias then a2' else noalpha a2' in
          go (Sdl.set_render_draw_color renderer r g b a2');
          plot (x2+1) y;
          x2 in
      (* draw horizontal line from (xc+x0,yc+y) to (xc+x3,yc+y) *)
      (* WARNING SDL doesn't want to draw a line of 1 pixel length... *)
      (* if x0 < x3 then begin *)
      (*     go (Sdl.set_render_draw_color renderer r g b a0); *)
      (*     go (Sdl.render_draw_line renderer (xc+x0) (yc+y) (xc+x3) (yc+y)); *)
      (*   end *)
      (* else if x0=x3 then begin *)
      (*     go (Sdl.set_render_draw_color renderer r g b a0); *)
      (*     go (Sdl.render_draw_point renderer (xc+x0) (yc+y)); *)
      (*   end; *)
      go (Sdl.set_render_draw_color renderer r g b a0);
      line x0 x3 y;

      (* now what are the next points? *)
      let x1', e1' =
        if x1 < y then x1, 0.
        else
          let ea = e1 +. float (2*y + 1) in
          let eb = e1 +. float (2*(y-x1+1)) in
          if abs_float ea < abs_float eb
          then x1, ea
          else x1-1, eb in
      let x2', e2' =
        let ea = e2 +. float (2*y + 1) in
        let eb = e2 +. float (2*(y-x2+1)) in
        if abs_float ea < abs_float eb
        then x2, ea
        else x2-1, eb in
      loop x1' e1' x2' e2' (y+1)
    end
  in
  loop radius1 0. radius2 0. 0;
  render_buffer buffer renderer

let circle ?(thick=1) renderer ~color ~radius ~x ~y =
  if radius >= 0
  then if thick > 0
    then if thick = 1 then circle renderer color x y radius
      else
        let radius1 = radius - thick + 1 in
        let radius2 = radius in
        annulus renderer color x y ~radius1 ~radius2
    else printd (debug_graphics + debug_user)
        "Circle with non-positive thickness %i gives the empty set..." thick
  else printd (debug_graphics + debug_user)
      "Circle with negative radius %i gives the empty set..." radius


(* we draw a filled circle by calling annulus with radius1 = 0. Not optimal
   (could reduce the number of lines drawn) but not too far. *)
let disc renderer ~color ~x0 ~y0 ~radius =
  annulus renderer color x0 y0 ~radius1:0 ~radius2:radius

(* draw a ring (=annulus) on a new transparent texture *)
(* and returns the texture. *)
(* radius is the exterior radius. Total size is 2*radius+2 *)
let ring_tex renderer ?(color = opaque grey) ~radius ~width x y =
  (* diameter = 2*radius+1 and we add 1 for antialiasing *)
  let w = imax (x+radius+2) (2*radius+2) |> imax (y+radius+2) in
  let target = create_target renderer w w in
  let push = push_target  ~bg:(set_alpha 0 white) renderer target in
  annulus renderer color x y ~radius1:(radius-width+1) ~radius2:radius;
  pop_target renderer push;
  target


(*******************************************************************************)
(* RECTANGLE *)

(* a simple rectangle with uniform thickness inside (w,h) *)
let rectangle ?(thick=1) renderer ~color ~w ~h ~x ~y =
  if thick <= 0 then printd draw_error "rectangle thickness must be positive"
  else if thick = 1 then draw_rect ~color renderer (x,y) w h
  else begin
      let bg = color in
      let width_up, width_down, width_left, width_right =
        if h < w
        then if thick <= h/2
             then thick, thick, thick, thick
             else h/2, h - h/2, 0,0
        else if thick <= w/2
        then thick, thick, thick, thick
        else 0,0, w/2, w - w/2 in
      box renderer ~bg x y w width_up; (* top *)
      box renderer ~bg x (y+h-width_down) w width_down; (* bottom *)
      box renderer ~bg x (y+width_down) width_left
        (h-width_down-width_up); (* left *)
      box renderer ~bg (x+w-width_right) (y+width_down) width_right
        (h-width_down-width_up); (* right *)
    end

(* Draw rounded box of size (w,h) *)
(*
       <> radius+1   <>
         ___________ _
 (5/6) /              \  (7/8)
       |
                      |
       |
                      |
 (3/4) \_ ___________ /  (1/2)
              lw

   *)
(* the global size is (w,h). thick is the width of the line, which is drawn
   _inside_ (w,h) *)
(* there are sanity checks and clipping, so all values of radius and thick
   should give reasonable output *)
(* note that if thick >= radius, the inner rectangle is NOT rounded. We could
   have done the other way, dunno what's the best. But it insures that the line
   thickness (computed in the normal direction) is always constant. *)
(* NICE FEATURE: can use thick<0. Then the inner radius is the given radius, and
   the outer radius will be greater. Of course, in this case, the full size
   exceeds the box (w,h) *)
(* TODO implement different line thicknesses for the 4 sides. This implies
   replacing the circle quadrants by ellipse quadrants *)
let rounded_box renderer color ?(antialias=true) ~w ~h ~radius ~thick x0 y0 =
  if thick <> 0 then begin
    let bg = color in
    let radius = imax 0 (imin radius (imin (w/2 - 2) (h/2 -2))) in
    let width = imin thick radius in
    (* we don't allow radius+1 so that the center of the circle is not drawn by
       annulus_octants *)
    let x = x0 + radius + 1 in
    let y = y0 + radius + 1 in
    let lw = w - 2*radius - 1 in
    let lh = h - 2*radius - 1 in
    box renderer ~bg (x-1) y0 lw width; (* top *)
    box renderer ~bg x (y0+h-width) lw width; (* bottom *)
    box renderer ~bg x0 y width lh; (* left *)
    box renderer ~bg (x0+w-width) (y-1) width lh; (* right *)
    if thick > radius (* need to fill more inside *)
    then rectangle renderer ~color:bg ~thick:(thick - radius) ~x:(x-1) ~y:(y-1)
        ~w:(lw+1) ~h:(lh+1);

    (* draw corners *)
    let x1 = x0+w-radius-1
    and y1 = y0+h-radius-1 in
    let radius0, radius =
      if width > 0
      then (radius - width + 1, radius)
      else (radius + 1, radius - width) in
    annulus_octants renderer ~antialias color (* top left *)
      ~octants:(16+32) (x-1) (y-1) radius0 radius;
    annulus_octants renderer ~antialias color (* top right *)
      ~octants:(64+128) x1 (y-1) radius0 radius;
    annulus_octants renderer ~antialias color (* bottom right *)
      ~octants:(1+2) x1 y1 radius0 radius;
    annulus_octants renderer ~antialias color (* bottom left *)
      ~octants:(4+8) (x-1) y1 radius0 radius;

    (* draw the four centers *)
    if thick > radius then begin
      let bf = create_buffer 4 in
      unsafe_add_point_to_buffer bf (x-1) (y-1);
      unsafe_add_point_to_buffer bf (x1) (y-1);
      unsafe_add_point_to_buffer bf (x1) (y1);
      unsafe_add_point_to_buffer bf (x-1) (y1);
      render_buffer bf renderer
    end
  end

let filled_rounded_box renderer color ?(antialias=true) ~w ~h ~radius x0 y0 =
  let thick = imax w h in
  (* of course this is too much, but rounded_box will correct this
     automatically *)
  rounded_box renderer color ~antialias ~w ~h ~radius ~thick x0 y0


(*********************************************************************************)
(* GRADIENT *)

(* Draw gradient on the renderer. *)
(* vertical gradient with n colors -- hinted version only *)
(* Of course it would be much better to do it directly in opengl *)
(* angle is in degrees *)
(* ici pour faire le gradient, on utilise le "linear filtering" de opengl quand
   on scale deux pixels.  Cependant il y a des effets de bord. Pour deux pixels,
   il semble qu'il faille supprimer les 2 1/4 de chaque côté. Pour n pixels, je
   ne sais pas...J'imagine que c'est toujours 1/2 pixel de chaque coté ? *)
let gradientv3 renderer ?angle colors =
  printd debug_graphics "rendering Gradient";
  let w,h = go(Sdl.get_renderer_output_size renderer) in
  let n = List.length colors in
  if n <> 0 then begin
      let small =
        if (Sdl.(set_hint Hint.render_scale_quality "1"))
        then begin
            (* create an n pixels texture *)
            let small = create_target renderer 1 n in
            let push = push_target ~clear:false renderer small in

            (* draw the n points *)
            go (Sdl.set_render_target renderer (Some small));
            go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_none);
            List.iteri (fun i c ->
                set_color renderer c;
                go (Sdl.render_draw_point renderer 0 i)) colors;
            pop_target renderer push;
            go (Sdl.set_texture_blend_mode small Sdl.Blend.mode_none);
            small
          end
        else (* Cannot set SDL_HINT_RENDER_SCALE_QUALITY *)
          failwith "todo"
      in

      begin
        let h' = if n > 1 then (n*h)/(n-1)
                 (* we need a larger box to remove 1/2 pixels at the top and
                    bottom *)
                 else h in
        match angle with
        | None ->
           let dh = if n > 1 then h/(n-1) else 0 in
           let dst = Sdl.Rect.create ~x:0 ~y:(-dh/2) ~w ~h:h' in
           go(Sdl.render_copy renderer ~dst small)
        | Some t ->
           (* we compute the size of the virtual box before cropping due to
            rotation. This virtual box is the smallest rectangle which, when
            rotated by theta, contains the upright box (w,h).*)
           let theta = pi *. t /. 180. in
           let c,s = cos theta, sin theta in
           let vh = int_of_float (abs_float (float w *. s) +.
                                    abs_float (float h *. c) ) in
           let vw = int_of_float (abs_float (float w *. c) +.
                                    abs_float (float h *. s) ) in
           (* Then we need to * enlarge the height of the VB in order to remove
              the 1/2 pixels at * the top and bottom.  *)
           let vh = if n > 1 then ((vh * n)/(n-1)) else vh in

           (* copy the small onto the target texture *)
           let flip = Sdl.Flip.none in
           let dw = vw - w
           and dh = vh - h in
           let center = Sdl.Point.create ~x:(vw/2) ~y:(vh/2) in
           let dst = Sdl.Rect.create ~x:(-dw/2) ~y:(-dh/2) ~w:(vw) ~h:(vh) in
           go(Sdl.render_copy_ex renderer ~dst small t (Some center) flip)
      end;

      forget_texture small
    end
  else ()

(* top right corner for a box *)
let corner_gradient2 renderer c1 c2 =
    let w,h = go(Sdl.get_renderer_output_size renderer) in
  let small =
    if (Sdl.(set_hint Hint.render_scale_quality "1"))
    then begin
        (* create an 2x2 texture *)
        let small = create_target renderer 2 2 in
        let push = push_target ~clear:false renderer small in

        (* draw the point *)
        go (Sdl.set_render_target renderer (Some small));
        go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_none);
        set_color renderer c2;
        go (Sdl.render_clear renderer);
        set_color renderer c1;
        (* we draw a point in the bottom-left corner *)
        go (Sdl.render_draw_point renderer 0 1);
        pop_target renderer push;
        go (Sdl.set_texture_blend_mode small Sdl.Blend.mode_none);
        small
      end
      else (* Cannot set SDL_HINT_RENDER_SCALE_QUALITY *)
        failwith "todo"
  in
  (* we double the size of the destination blit to remove the 1/2 pixel boundary
     effects: *)
  let dst = Sdl.Rect.create ~x:(-w/2) ~y:(-h/2) ~w:(2*w) ~h:(2*h) in
  go (Sdl.render_copy ~dst renderer small);
  forget_texture small

(* create a gradient texture. Use pop=false only for optimization when using
   several push in a row, only one pop is usually needed. But well, this
   optimization is really nothing... (pop_target is so fast) *)
let gradient_texture renderer ~w ~h ?angle ?(pop=true) colors =
  let target = create_target renderer w h in
  let p = push_target ~clear:false renderer target in
  gradientv3 renderer ?angle colors;
  if pop then pop_target renderer p;
  target


(* blits a "shadow" (to the layer) all around the dst rectangle. *)
(* Uses gradient. The patching of the corners is not perfect... *)
(* TODO: this simple technique does not work for round corners *)
(* but it's very fast *)
(* Note: shadows look better if the box has a white (or light) background *)
(* Warning: the 'radius' here corresponds to 'width' in Style module (+ theme
   scaling) *)
let box_shadow canvas layer ?(radius = Theme.scale_int 8) ?(color = pale_grey)
      ?(size=Theme.scale_int 2) ?(offset=scale_pos (3,5))
      ?voffset ?(fill = true) dst  =
  (* size = 0 means that the complete shadow has the same size as the box -- and
     hence cannot be seen if offset=(0,0). If size>0 then the shadow is larger
     than the box by 'size' pixels in each of the 4 directions. 'size' should be
     less than radius, otherwise there will be a gap between the box and the
     shadow. *)
  let ox,oy = offset in
  let x,y = Sdl.Rect.(x dst, y dst) in
  let w,h = Sdl.Rect.(w dst, h dst) in
  let x = x + ox + radius - size in
  let y = y + oy + radius - size in
  (* We define now the size of the inner rectangle on which the shadow is
     fitted. TODO why don't we just take the original (w,h) ? *)
  let w = w - 2*radius + 2*size in
  let h = h - 2*radius + 2*size in

  if h <= 0 || w <= 0 then
    begin
      printd draw_error "Box too small for the requested Shadow.";
      []
    end else
    begin
      let tcolor = set_alpha 0 color in
      let scolor = set_alpha 200 color in

      (* create the textures *)
      (* TODO: pre-compute and store this!  On the other hand, the advantage of
     doing this here is that the shadow will always adapt to the box, when the
     latter is animated or transformed. *)
      let corner = create_target canvas.renderer radius radius in
      let p = push_target ~clear:false canvas.renderer corner in
      corner_gradient2 canvas.renderer scolor tcolor;
      let horiz = gradient_texture ~pop:false canvas.renderer ~w ~h:radius
                    [scolor; tcolor] in
      (*   create_target canvas.renderer w radius in
       * let _ = push_target ~clear:false canvas.renderer horiz in
       * gradientv3 canvas.renderer [scolor; tcolor]; *)
      let vert = gradient_texture ~pop:false canvas.renderer ~w:radius ~h
                   ~angle:90. [scolor; tcolor] in
      (* create_target canvas.renderer radius h in
       * let _ = push_target ~clear:false canvas.renderer vert in
       * gradientv3 canvas.renderer ~angle:90. [scolor; tcolor]; *)
      pop_target canvas.renderer p;

      (* create the blits *)
      (* it would be --slightly-- faster to pre-rotate the textures instead of
         doing this for each blit *)
      let bottom =
        let dst = Sdl.Rect.create ~x ~y:(y+h) ~w ~h:radius in
        make_blit ?voffset ~dst canvas layer horiz in
      let top =
        let dst = Sdl.Rect.create ~x ~y:(y-radius) ~w ~h:radius in
        let transform = make_transform ~flip:Sdl.Flip.vertical () in
        make_blit ?voffset ~dst ~transform canvas layer horiz in
      let left =
        let dst = Sdl.Rect.create ~x:(x-radius) ~y ~w:radius ~h in
        make_blit ?voffset ~dst canvas layer vert in
      let right =
        let dst = Sdl.Rect.create ~x:(x+w) ~y ~w:radius ~h in
        let transform = make_transform ~flip:Sdl.Flip.horizontal () in
        make_blit ?voffset ~dst ~transform canvas layer vert in

      let top_right =
        let dst = Sdl.Rect.create ~x:(x+w) ~y:(y-radius) ~w:radius ~h:radius in
        make_blit ?voffset ~dst canvas layer corner in
      let top_left =
        let dst = Sdl.Rect.create ~x:(x-radius) ~y:(y-radius)
                    ~w:radius ~h:radius in
        let transform = make_transform ~flip:Sdl.Flip.horizontal () in
        make_blit ?voffset ~dst ~transform canvas layer corner in
      let bottom_left =
        let dst = Sdl.Rect.create ~x:(x-radius) ~y:(y+h) ~w:radius ~h:radius in
        let transform = make_transform
                          ~flip:Sdl.Flip.(horizontal + vertical) () in
        make_blit ?voffset ~dst ~transform canvas layer corner in
      let bottom_right =
        let dst = Sdl.Rect.create ~x:(x+w) ~y:(y+h) ~w:radius ~h:radius in
        let transform = make_transform ~flip:Sdl.Flip.vertical () in
        make_blit ?voffset ~dst ~transform canvas layer corner in

      (* we fill also the inside rectangle, otherwise it looks bad if applied to
         a transparent box (but who wants to add shadow to a transparent box?),
         and also if the offset is larger than the radius.  *)

      List.iter forget_texture [horiz; vert; corner];

      if fill
      then let inside = box_to_layer ?voffset canvas layer ~bg:scolor x y w h in
           [inside; bottom; top; left; right;
            bottom_right; bottom_left; top_left; top_right]
      else [bottom; top; left; right;
            bottom_right; bottom_left; top_left; top_right]
    end

(*********************************************************************************)
(* LINES *)



(* TODO use [box] instead? *)
let make_hline ?(thick=1) renderer ~color ~x0 ~x1 ~y =
  let x0, x1 = if x0 < x1 then x0, x1 else x1, x0 in
  let y = y - thick/2 in
  let w, h = x1 - x0, thick in
  let tex = texture ~color renderer ~w ~h in
  let rect = Sdl.Rect.create ~x:x0 ~y ~w ~h in
  tex, rect

let make_vline ?(thick=1) renderer ~color ~x ~y0 ~y1 =
  let y0, y1 = if y0 < y1 then y0, y1 else y1, y0 in
  let x = x - thick/2 in
  let h, w = y1 - y0, thick in
  let tex = texture ~color renderer ~w ~h in
  let rect = Sdl.Rect.create ~x ~y:y0 ~w ~h in
  tex, rect

(* Draw line. Somewhat brutal algorithm: we draw a horizontal line and rotate
   it. It is actually quite fast but there is no antialiasing. *)
let line ?(thick=1) renderer ~color ~x0 ~y0 ~x1 ~y1 =
  if y0 = y1
  then let tex, dst = make_hline ~thick renderer ~color ~x0 ~x1 ~y:y0 in
    begin
      go (Sdl.render_copy ~dst renderer tex);
      forget_texture tex
    end
  else if x0 = x1
  then let tex, dst = make_vline ~thick renderer ~color ~x:x0 ~y0 ~y1 in
    begin
      go (Sdl.render_copy ~dst renderer tex);
      forget_texture tex
    end
  else let w = round (dist (x0,y0) (x1,y1)) in
    let tex = texture ~color renderer ~w ~h:thick in
    let center = Sdl.Point.create ~x:0 ~y:(thick/2) in
    (* = center coordinates relative to the dst rect below *)
    let dst = Sdl.Rect.create ~x:x0 ~y:(y0 - thick/2) ~w ~h:thick in
    let angle = 180. *. atan2 (float (y1 - y0)) (float (x1 - x0)) /. pi in
    go (Sdl.render_copy_ex renderer ~dst tex angle (Some center) Sdl.Flip.none);
    forget_texture tex


(* Bresenham-type algorithm for fast line rasterization;
   here only the case of a subdiagonal parameterized by x:

o---___
       ---___
             ---o
*)
let simple_line_1 renderer ~x0 ~y0 ~x1 ~y1 =
  let x0, x1 = if x0 <= x1 then x0, x1 else x1, x0 in
  let y0, y1 = if y0 <= y1 then y0, y1 else y1, y0 in
  let dx = x1 - x0 in
  let dy = y1 - y0 in
  assert (dy <= dx);
  let bf = create_buffer dx in
  let rec loop x y f =
    unsafe_add_point_to_buffer bf x y;
    if x <> x1 then let z = dy - f in
      if 2*z <= dx then loop (x+1) y (-z) else loop (x+1) (y+1) (dx-z) in
  loop x0 y0 0;
  render_buffer bf renderer





(** intersection of rectangles; None means no clipping = the whole texture *)
(* if the intersection is empty, we return a rect with zero area. This can be
   tested with Sdl.rect_empty *)
let intersect_rect r1o r2o =
  match r1o, r2o with
  | None, None -> None
  | Some r1, None -> Some r1
  | None, Some r2 -> Some r2
  | Some r1, Some r2 -> let r = Sdl.intersect_rect r1 r2 in
    if r = None then (printd debug_graphics "Empty intersect"; (* DEBUG *)
                      Some (Sdl.Rect.create ~x:0 ~y:0 ~w:0 ~h:0))
    else r

(* see alpha_mult_tex below *)
let alpha_mult_surface surf =
  let blend_mode =  go (Sdl.get_surface_blend_mode surf) in
  let r,g,b = go (Sdl.get_surface_color_mod surf) in
  let w,h = Sdl.get_surface_size surf in
  let dst = create_surface_like surf ~w ~h in
  go (Sdl.set_surface_color_mod surf 0 0 0);
  go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
  go (Sdl.blit_surface ~src:surf None ~dst None);
  go (Sdl.set_surface_color_mod surf r g b);
  go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_add);
  go (Sdl.blit_surface ~src:surf None ~dst None);
  go (Sdl.set_surface_blend_mode surf blend_mode);
  dst

(* Texture manipulations *)

(* multiply the color components by the alpha component *)
(* not used *)
(* see also
https://developer.nvidia.com/content/alpha-blending-pre-or-not-pre
*)
let alpha_mult_tex renderer tex =
  (* 1. we make a copy of the texture and keep only the alpha channel, setting
     colors to zero *)
  let w,h = tex_size tex in
  let target = create_target renderer w h in
    go (Sdl.set_texture_color_mod tex 0 0 0);
  go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_none);
  let p = push_target ~clear:false renderer target in
  go (Sdl.render_copy renderer tex);
  go (Sdl.set_texture_color_mod tex 255 255 255);

  (* 2. we 'add' the tex on the previously extracted alpha channel. Indeed
     'adding' on a texture with only zeros as colors (black) will simply
     multiply the color by its alpha value. *)
  go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_add);
  go (Sdl.render_copy renderer tex);
  pop_target renderer p;
  target

(* f is a function with values in [0,1] *)
let convolution renderer ?(emboss=false) ?(bg = opaque grey) f radius texture =
  let pf, _, (w,h) = go (Sdl.query_texture texture) in
  (*  let buffer =  go(Sdl.create_texture renderer pf Sdl.Texture.access_target ~w ~h) in *)
  let target =  create_texture renderer pf Sdl.Texture.access_target ~w ~h in
  let push = push_target renderer target in
  (* the issue is that the additive blend mode does not add the alphas...*)
  (* go (Sdl.set_texture_blend_mode buffer Sdl.Blend.mode_add); *)

  set_color renderer bg; (* TODO only works with a solid black background... *)
  go (Sdl.render_clear renderer);

  if emboss then begin
    (* copy the center image multiplied by f(0,0) onto the target, so we get the
       alpha information *)
    go (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
    let a = round (255. *. (f 0 0)) in
    go (Sdl.set_texture_color_mod texture a a a);
    let dst = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in
    go (Sdl.render_copy ~dst renderer texture);
  end;

  (* clear the buffer *)
  (* go (Sdl.set_render_target renderer (Some buffer)); *)
  (* go (Sdl.set_render_draw_color renderer 0 0 0 0); *)
  (* go (Sdl.render_clear renderer); *)

  (* now we 'add' the neighbouring images *)
  go (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_add);
  (* go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_blend); *)
  let dx, dy = if emboss then radius, radius else 0,0 in
  for x = -radius to radius do
    for y = -radius to radius do
      (*      if x <> 0 || y <> 0 (* the center image is already copied *)
              then *) begin
      let dst = Sdl.Rect.create ~x:(x+dx) ~y:(y+dy) ~w ~h in
      let a = round (255. *. (f (x) (y))) in
      (*        go (Sdl.set_render_target renderer (Some buffer));*)
      (* go (Sdl.set_render_draw_color renderer 255 255 255 a); *)
      (* go (Sdl.render_clear renderer); *)
      go (Sdl.set_texture_color_mod texture a a a);
      (* go (Sdl.set_texture_alpha_mod texture a); *)
      go (Sdl.render_copy ~dst renderer texture);
      (* go (Sdl.set_texture_blend_mode buffer Sdl.Blend.mode_mod); *)
      (* go (Sdl.set_render_target renderer (Some target)); *)
      (* go (Sdl.set_texture_blend_mode buffer Sdl.Blend.mode_none); *)
      (* go (Sdl.render_copy ~dst renderer buffer); *)
    end
    done
  done;
  pop_target renderer push;
  go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_blend);
  target

(* f is a function with values in [0,1] *)
let convolution_emboss renderer ?(bg = opaque grey) f radius texture =
  let pf, _, (w,h) = go (Sdl.query_texture texture) in
  let target =  create_texture renderer pf Sdl.Texture.access_target ~w:(w+2*radius) ~h:(h+2*radius) in
  let push = push_target renderer target in
  set_color renderer bg;
  go (Sdl.render_clear renderer);

  (* copy the center image multiplied by f(0,0) onto the target, so we get the
      alpha information *)
  go (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend); (* or blend *)
  let a = round (255. *. (f 0 0)) in
  go (Sdl.set_texture_color_mod texture a a a);
  let dst = Sdl.Rect.create ~x:radius ~y:radius ~w ~h in
  go (Sdl.render_copy ~dst renderer texture);

  (* now we 'add' the neighbouring images *)
  go (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_add);
  for x = 0 to 2*radius do
    for y = 0 to 2*radius do
      if x <> radius || y <> radius (* the center image is already copied *)
      then begin
        let dst = Sdl.Rect.create ~x ~y ~w ~h in
        let a = round (255. *. (f (x-radius) (y-radius))) in
        go (Sdl.set_texture_color_mod texture a a a);
        go (Sdl.render_copy ~dst renderer texture);

      end
    done
  done;
  pop_target renderer push;
  go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_blend);
  target

(* use with radius = 1 *)
let one_pixel_blur _ _ = 0.111111


(* gaussian *)
let gaussian ~variance t =
  1. /. (variance *. sqrt(2. *. pi)) *. exp (-. t *. t /. 2. /. (variance *. variance))

let gaussian_blur ~radius x y =
  let variance = (float radius) /. 2. in
  let gx =  gaussian ~variance (float x) in
  let gy =  gaussian ~variance (float y) in
  gx *. gy


(* logical AND blending for surfaces. *)
(* not used yet *)
let mask_surface ~mask surface =
  let sm = Sdl.get_surface_size mask in
  let s = Sdl.get_surface_size surface in

  (* first some tests *)
  let mask, surface, to_free =
    if s <> sm then begin
        printd draw_error
          "The surface and the mask should have same size. We crop";
        let wm,hm = sm and w,h = s in
        let w' = min wm w and h' = min hm h in
        let mask' = create_surface_like surface ~w:w' ~h:h' in
        let surface' = create_surface_like surface ~w:w' ~h:h' in
        let rect = Sdl.get_clip_rect surface' in
        go(Sdl.blit_surface ~src:mask (Some rect) ~dst:mask' None);
        go(Sdl.blit_surface ~src:surface (Some rect) ~dst:surface' None);
        mask', surface', [mask'; surface']
      end
    else let formatm = Sdl.get_surface_format_enum mask in
         let format = Sdl.get_surface_format_enum surface in
         if formatm <> formatm then begin
             printd debug_warning "The surface and the mask should have same pixel format. We convert";
             let mask' = go(Sdl.convert_surface_format mask format) in
             mask', surface, [mask']
           end
         else mask, surface, [] in

  (* then the blending *)
  (* let mlm = Sdl.must_lock mask in *)
  (* if mlm then  *)
  go(Sdl.lock_surface mask);
  (* let ml = Sdl.must_lock surface in *)
  (* if ml then  *)
  go(Sdl.lock_surface surface);
  let pixels = Sdl.get_surface_pixels surface Bigarray.int8_unsigned in
  let pixelsm = Sdl.get_surface_pixels mask Bigarray.int8_unsigned in
  let open Bigarray.Array1 in
  let n = dim pixels in
  let result = create Bigarray.int Bigarray.c_layout n in
  for i = 0 to n - 1 do
    let p = unsafe_get pixels i in
    let pm = unsafe_get pixelsm i in
    unsafe_set result i (p land pm);
  done;
  (* if mlm then  *)
  Sdl.unlock_surface mask;
  (* if ml then  *)
  Sdl.unlock_surface surface;
  let res = create_surface_from ~like:surface result in
  List.iter free_surface to_free;
  res
(* recall in the end the bigarray is part of the surface structure, it is
   not copied *)

(* Warning: this is supposed to be a slow operation *)
(* for faster access, keep the pixels/surface *)
(* not used*)
let get_texture_pixels renderer texture =
  let format,_,(w,h) = go(Sdl.query_texture texture) in
  let bpp,_,_,_,_ = go(Sdl.pixel_format_enum_to_masks format) in
  let tex_bytes_per_pixel = if bpp = 32 then 4 else if bpp <= 8 then 1 else 2 in
  let open Bigarray in
  printd debug_graphics "Texture BBP=%u" tex_bytes_per_pixel;
  (* faster with int16 or 32 ? *)
  let pixels = Array1.create int8_unsigned c_layout
      (w * h * tex_bytes_per_pixel) in
  let pitch = w * tex_bytes_per_pixel in
  let target = create_target ~format renderer w h in
  let push = push_target renderer target in
  go(Sdl.render_copy renderer texture);
  (* Note: there was a bug in some versions of SDL; the RenderRead pixels were
     upside-down.  We would have to use: go(Sdl.render_copy_ex renderer texture
     0. None Sdl.Flip.vertical); *)
  go(Sdl.render_read_pixels renderer None (Some format) pixels pitch);
  pop_target renderer push;
  pixels, pitch, go(Sdl.pixel_format_enum_to_masks format)

(* logical AND blending for textures. *)
(* TODO: faster: let the mask be a surface *)
(* TODO: use get_texture_pixels *)
(* TODO check the new blend modes at
   https://hg.libsdl.org/SDL/rev/180e8906dc3c *)
let land_texture renderer mask texture =
  let format,_,(w,h) = go(Sdl.query_texture texture) in
  let formatm,_,(wm,hm) = go(Sdl.query_texture mask) in
  if formatm <> format
  then printd draw_error "Mask and texture must have same format. \
                          Expect garbage.";
  let bpp,_,_,_,_ = go(Sdl.pixel_format_enum_to_masks format) in
  let tex_bytes_per_pixel = if bpp = 32 then 4 else if bpp <= 8 then 1 else 2 in
  let w' = min wm w and h' = min hm h in
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
  let open Bigarray in
  printd debug_graphics "Texture BPP=%u" tex_bytes_per_pixel;
  let pixels = Array1.create int8_unsigned c_layout
                 (w' * h' * tex_bytes_per_pixel) in
  let pixelsm = Array1.create int8_unsigned c_layout
                  (w' * h' * tex_bytes_per_pixel) in
  (*Array1.fill pixelsm 0;*) (* DEBUG *)
  (*Array1.fill pixels 0;*) (* DEBUG *)
  let pitch = w' * tex_bytes_per_pixel in
  let target = create_target ~format renderer w' h' in
  let push = push_target renderer target in

  let t = Unix.gettimeofday () in
  go (Sdl.render_copy ~src:rect renderer texture);
  go (Sdl.render_read_pixels renderer (Some rect) (Some format) pixels pitch);
  go (Sdl.render_clear renderer);
  go (Sdl.render_copy ~src:rect renderer mask);
  go (Sdl.render_read_pixels renderer (Some rect) (Some format) pixelsm pitch);
  pop_target renderer push;
  printd debug_graphics "READ pixels time: %f" (Unix.gettimeofday () -. t);

  let t = Unix.gettimeofday () in
  let n = Array1.dim pixels in
  for i = 0 to n - 1 do
    let p = Array1.unsafe_get pixels i in
    let pm = Array1.unsafe_get pixelsm i in
    Array1.unsafe_set pixels i (p land pm);
  done;
  printd debug_graphics "LAND Loop time: %f" (Unix.gettimeofday () -. t);
  go (Sdl.update_texture target (Some rect) pixels pitch);
  target


(* alpha=0 ==> transparent, 1 ==> opaque *)
(* https://wiki.libsdl.org/SDL_BlendMode *)
(* SDL_BLENDMODE_NONE no blending dstRGBA = srcRGBA *)
(* SDL_BLENDMODE_BLEND alpha blending dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA)); dstA = srcA + (dstA * (1-srcA)) *)
(* SDL_BLENDMODE_ADD additive blending dstRGB = (srcRGB * srcA) + dstRGB; dstA = dstA *)
(* SDL_BLENDMODE_MOD color modulate dstRGB = srcRGB * dstRGB; dstA = dstA *)

(* WANRNIG: the RGB encoding is NOT linear wrt light intensity. See
   https://en.wikipedia.org/wiki/SRGB
   https://photosounder.com/michel_rouzic/#srgb

When blending, we should not just ADD values. Not sure what SDL does for this.

*)


(* Remark: blending half transparent blue (srcA=0.5) onto full transparent red
   (dstA=0) gives 0.5blue+0.5red, alpha=0.5: the hidden red reappears!
https://stackoverflow.com/questions/45781683/how-to-get-correct-sourceover-alpha-compositing-in-sdl-with-opengl
*)

(* multiply the alpha of the texture by the alpha of the mask *)
(* and set color to white when alpha = 0 (because of Remark above) *)
(* SDL RenderRead pixels WARNING: "This is a very slow operation, and should not
   be used frequently."  *)
(* The result is a target texture with Sdl.Pixel.format_argb8888 *)
let mask_texture ~mask renderer texture =
  let w,h = tex_size texture in
  let wm,hm = tex_size mask in
  let w' = imin wm w and h' = imin hm h in
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
  let open Bigarray in
  let tex_bytes_per_pixel = 4 in
  let pixels = Array1.create int8_unsigned c_layout
      (w' * h' * tex_bytes_per_pixel) in
  let pixelsm = Array1.create int8_unsigned c_layout
      (w' * h' * tex_bytes_per_pixel) in
  let pitch = w' * tex_bytes_per_pixel in
  let format = Sdl.Pixel.format_argb8888 in
  let target = create_target ~format renderer w' h' in
  let push = push_target renderer target in

  let t = Unix.gettimeofday () in
  go(Sdl.render_copy ~src:rect renderer texture);
  go(Sdl.render_read_pixels renderer (Some rect) (Some format) pixels pitch);
  go(Sdl.render_clear renderer);
  go(Sdl.render_copy ~src:rect renderer mask);
  go(Sdl.render_read_pixels renderer (Some rect) (Some format) pixelsm pitch);
  pop_target renderer push;
  printd debug_graphics "READ pixels Loop time: %f" (Unix.gettimeofday () -. t);
  (* The block above is indeed VERY slow: takes more than TWICE the time of the
     for-loop below!  Up to 0.04sec for a full-screen texture 3000x2000. We
     should find another way.  *)

  let t = Unix.gettimeofday () in
  let n = Array1.dim pixels / tex_bytes_per_pixel in
  (* amask=0xff000000 *)
  for i = 0 to n - 1 do
    let a = Array1.unsafe_get pixels (4*i+3) in
    let am = Array1.unsafe_get pixelsm (4*i+3) in
    let alpha = (a * am) / 255 in
    Array1.unsafe_set pixels (4*i+3) alpha;
    if alpha = 0 then begin (* we set the color to white *)
      Array1.unsafe_set pixels (4*i) 255;
      Array1.unsafe_set pixels (4*i+1) 255;
      Array1.unsafe_set pixels (4*i+2) 255
    end;
  done;
  printd debug_graphics "MASK Loop time: %f" (Unix.gettimeofday () -. t);
  go(Sdl.update_texture target (Some rect) pixels pitch);
  target

(* This "fast" version is not equivalent to [mask_texture]: here the alpha
   component of the original texture is lost: we only keep the alpha component
   of the mask. The mask should be black and contain only alpha. This operation
   will merge the Alpha from the mask with the RGB from the texture. *)
let fast_mask_texture renderer ~mask texture =
  let w,h = tex_size texture in
  let wm,hm = tex_size mask in
  let w' = imin wm w and h' = imin hm h in
  let rect = Sdl.Rect.create ~x:0 ~y:0 ~w:w' ~h:h' in
  let target = create_target renderer w' h' in
  let push = push_target ~clear:true ~bg:none renderer target in
  go (Sdl.set_texture_blend_mode mask Sdl.Blend.mode_none);
  go (Sdl.render_copy ~src:rect renderer mask);
  go (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_add);
  go (Sdl.render_copy ~src:rect renderer texture);
  pop_target renderer push;
  target

(* cheap blur by zooming *) (* not used yet *)
let blur_texture renderer tex scale =
  let w,h = tex_size tex in
  let w',h' = imax 1 (w/scale), imax 1 (h/scale) in
  ignore (Sdl.(set_hint Hint.render_scale_quality "1"));
  let small = create_target renderer w' h' in
  let p = push_target ~clear:false renderer small in
  go (Sdl.set_texture_blend_mode tex Sdl.Blend.mode_none);
  go (Sdl.render_copy renderer tex);
  let final = create_target renderer w h in
  let _ = push_target ~clear:false renderer final in
  go (Sdl.set_texture_blend_mode small Sdl.Blend.mode_none);
  go (Sdl.render_copy renderer small);
  pop_target renderer p;
  forget_texture small;
  final

(* More ideas:
   https://software.intel.com/en-us/blogs/2014/07/15/an-investigation-of-fast-real-time-gpu-based-image-blur-algorithms
   *)
