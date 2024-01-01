(* SDL Area Widget *)
(* This file is part of BOGUE *)

module Box = B_box
module Var = B_var
module Draw = B_draw
module Flow = B_flow
module Time = B_time
module Trigger = B_trigger
module Theme = B_theme
module Mouse = B_mouse

open B_utils
open Tsdl

type draw_element = {
  id : int;
  name : string;
  mutable disable : bool;
  f : Tsdl.Sdl.renderer -> unit
}

type t = {
  box : Box.t;
  (* TODO: in fact one could use 2 textures: one for the Box, one for the Area:
     because the Box contains a background, and it's not always necessary to
     clear the background each time we want to clear the Area... *)
  sheet : (draw_element Flow.t) Var.t;
  (* A sheet should be a data structure that is very fast to append AND to
     iterate, AND whose iteration can be split. Queues would be perfect for the
     first two. We implemented Flow for this purpose.  WARNING: commands in the
     queue should NOT modify the sheet itself. For instance [clear] should not
     be used in the sheet.  *)
  mutable update : bool;
  (* if [update] is false, we just draw the box texture without applying the
     [sheet] *)
  timeout : int;
  cache : (Draw.texture option) Var.t;
  mutable pos: (int * int) option;
  (* For convenience, the layout position will be stored here *)
}

let new_id = fresh_int ()

let create ~width ~height ?style ?(timeout = 50) () =
  { box = Box.create ~width ~height ?style ();
    sheet = Var.create (Flow.create ());
    update = true;
    timeout;
    cache = Var.create None;
    pos = None
  }

let sprint el =
  Printf.sprintf "%u%s" el.id
    (if el.name = "" then "" else Printf.sprintf " (%s)" el.name)

(* Note: it would be super smart to reinstall the commands that created the
   cache when we clear the cache... TODO?*)
let clear_cache area =
  match Var.get area.cache with
  | None -> ()
  | Some tex -> begin
      Draw.forget_texture tex;
      Var.set area.cache None
    end

let unload area =
  Box.unload area.box;
  clear_cache area

(* force the area to be redrawn, without clearing the cache. *)
let update area =
  area.update <- true;
  Var.with_protect area.sheet Flow.rewind

(* not for sheet *)
let clear area =
  Var.set area.sheet (Flow.create ());
  clear_cache area;
  update area

(* not for sheet *)
let free area =
  Box.free area.box;
  clear area

(* Add the element to the sheet *)
let add_element area el =
  Var.with_protect area.sheet (fun q ->
      printd debug_custom "Adding element %s to the SDL Area." (sprint el);
      Flow.add el q;
      Flow.rewind q; (* we do this here just to avoid calling [update] *));
  area.update <- true

(* Add a drawing function to the sheet and return the corresponding element. The
   function should be fast, otherwise it will block the UI when the sheet is
   executed.  *)
let add_get area ?(name = "") ?(disable = false) f =
  let el = { id = new_id (); name; disable; f} in
  add_element area el;
  el

(* Just add, don't return the element *)
let add area ?name f =
  add_get area ?name f
  |> ignore

(* Clear the sheet before this point and save the drawing into the
   cache. Currently, the user is responsible for saving the commands that were
   used to create the cache if necessary.  *)
let cache area renderer =
  match Var.get area.box.render with
  | None -> failwith "Sdl_area texture was not created."
  | Some tex ->
    let cache_tex = match Var.get area.cache with
      | None ->
        printd debug_graphics "Creating cache for Sdl_area.";
        let w,h = Draw.tex_size tex in
        let t = Draw.create_target renderer w h in
        Var.set area.cache (Some t);
        t
      | Some t -> t in
    Flow.forget (Var.unsafe_get area.sheet);
    (* : this is dangerous since we are modifying the sheet in-place, and this
       will be execured while itering it (in the display section)... However
       looking at what Flow.rewind does, it looks ok: the next element should
       still be accessible.  *)
    let save_target = Draw.push_target renderer cache_tex in
    go (Sdl.set_texture_blend_mode tex Sdl.Blend.mode_none);
    go (Sdl.render_copy renderer tex);
    Draw.pop_target renderer save_target

let cache area =
  add area ~name:"cache" (cache area)

(* Remove the element from the sheet. OK to be slow. *)
let remove_element area element =
  update area;
  let@ q = Var.with_protect area.sheet in
  try Flow.remove_first_match (fun el -> el.id = element.id) q
  with Not_found ->
    printd debug_error "Element %s not found in SDL Area" (sprint element)

let has_element area element =
  let@ q = Var.with_protect area.sheet in
  Flow.rewind q;
  Flow.exists (fun el -> el.id = element.id) q

let disable element =
  element.disable <- true

let enable element =
  element.disable <- false

let size area =
  Box.size area.box

let resize size area =
  update area;
  Box.resize size area.box

(* size in physical pixels *)
let drawing_size area =
  match Var.get area.box.render with
  | Some t -> Draw.tex_size t
  | None ->
    (* HACK: TODO put this "video_init" in a proper "init" function. It has to
       be done before the user runs the main board and open window. However it
       should (maybe) not be called in the (stupid?) case where no video is
       required, see example00. Well, actually, video_init is currently called
       later anyways, and this doesn't seem to prevent example00 to run on
       computers without display... *)
    if !Theme.scale = 0. then Draw.video_init ();
    Box.size area.box
            |> Draw.to_pixels

(* position in physical pixels with respect to the area *)
let pointer_pos area ev =
  let x0, y0 = default_lazy area.pos
      (lazy (printd (debug_error + debug_user)
               "Cannot find pointer position within the Sdl_area because it is \
                not displayed yet."; (0,0))) in
  let x, y = Mouse.pointer_physical_pos ev in
  x-x0, y-y0

let to_pixels = Draw.to_pixels

let set_rgb area rgb =
  add area (fun renderer -> Draw.(set_color renderer (opaque rgb)))

(* Convenient shortcuts to some Draw functions. Downside: they cannot adapt
   easily to resizing the area. See example 49. *)

let draw_circle area ~color ~thick ~radius (x, y) =
  add area (Draw.circle ~color ~thick ~radius ~x ~y)

let fill_circle area ~color ~radius (x, y) =
add area (Draw.disc ~color ~x0:x ~y0:y ~radius)

let draw_rectangle area ~color ~thick ~w ~h (x, y) =
  add area (Draw.rectangle ~color ~w ~h ~thick ~x ~y)

let fill_rectangle area ~color ~w ~h (x, y) =
  add area (fun renderer -> Draw.box renderer ~bg:color x y w h)

let draw_line area ~color ~thick (x0, y0) (x1, y1) =
  if thick = 1
  then add area (fun renderer ->
      Draw.set_color renderer color;
      go (Tsdl.Sdl.render_draw_line renderer x0 y0 x1 y1))
  else add area (Draw.line ~color ~thick ~x0 ~y0 ~x1 ~y1)

(* Direct access to the texture *)

let get_texture area =
  Var.get area.box.render

let set_texture area texture =
  Var.set area.box.render (Some texture);
  area.update <- false

(************* display ***********)

let display wid canvas layer area g =
  area.pos <- Some (g.Draw.x, g.Draw.y);
  if area.update then Box.unload_texture area.box;
  let blits = Box.display canvas layer area.box g in
  if not area.update && Flow.end_reached (Var.get area.sheet) then blits
  else (* Now we draw directly on the Box texture *)
    let () = printd debug_graphics "Rendering SDL Area of length %u."
        (Flow.length (Var.get area.sheet)) in
    let renderer = canvas.renderer in
    let tex = match Var.get area.box.render with
      | Some t -> t
      | None -> failwith "The Sdl_area texture should have been create by Box \
                          already." in
    let save_target = Draw.push_target ~clear:false canvas.renderer tex in
    do_option (Var.get area.cache) (fun t ->
        printd debug_graphics "Using SDL Area cache.";
        go (Sdl.set_texture_blend_mode t Sdl.Blend.mode_none);
        go (Sdl.render_copy canvas.renderer t));

    (* Executing the drawing functions cannot be done in a separate Thread
       because it uses directly the SDL Renderer API. Hence we have a basic
       timeout mechanism in order to be nice to the rest of the GUI. *)
    (* TODO Currently this mechanism does not work well (user events are
       blocked) because we need to change the way events are consumed in the
       main loop. *)
    let t0 = Time.now () in
    Var.protect_fn area.sheet (fun q ->
        Tsdl.Sdl.(set_render_draw_blend_mode renderer Blend.mode_blend) |> go;
        Flow.iter_until (fun el ->
            if not el.disable then begin
              printd debug_graphics "Executing SDL_Area element %s." (sprint el);
              el.f renderer;
              Time.now () - t0 > area.timeout
            end
            else false)
          q;
        if not (Flow.end_reached q) then begin
          printd (debug_board + debug_warning)
            "The rest of the SDL Area will be rendered later.";
          Trigger.push_redraw wid
        end);
    Draw.pop_target canvas.renderer save_target;
    area.update <- false;
    blits
