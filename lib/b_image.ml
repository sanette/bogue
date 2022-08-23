(* A simple image display widget *)
(* This file is part of BOGUE *)

(* WARNING: this is (currently) the only widget that can be "copied", cf
   [Widget.image_copy]: the same Image.t can be displayed by several
   Widget.t. *)


module Var = B_var
module Draw = B_draw
open B_utils
open Tsdl

type resize =  (* not implemented *)
  | Crop of int (* cut the image at origin x *)
  | Fit (* fit in given area *)
  | KeepRatio (* keep aspect ratio and fit inside area *)
  | Expand (* expand if too small. Do not shrink *)
  | Shrink (* shrink if too big. Do not expand *)
  | Size of int (* make it this size *)

type source =
  | File of string
  | Image of t

and t =
  { source : source Var.t;
    mutable angle : float;
    width : int; (* width of the area *)
    height : int; (* height of the area *)
    xsize : resize; (* NOT used anymore. control the width of the image within the area *)
    ysize : resize; (* NOT used anymore. control the height of the image  .. *)
    xpos : Draw.align; (* NOT used anymore. horizontal centering *)
    ypos : Draw.align; (* NOT used anymore. vertical ... *)
    background : Draw.color option;
    render : (Draw.texture option) Var.t;
    (* The [render] field will contain the unrotated, unscaled rendered
       image. *)
  }

let size img =
  img.width, img.height

let resize _size _i =
  () (* TODO *)

(* use noscale = true to keep original pixel size. *)
(* TODO: noscale=true is not completely accurate because this leads to
 * performing scale (size / scale), thus we loose some units due to integer
   rounding. To be exact, we should keep a flag "original size" and modify the
   blit to use exact size *)
let create ?width ?height ?(noscale = false) ?bg ?(angle=0.) file =
  let width, height = match width, height with
    | Some w, Some h -> (w,h)
    | _ -> begin let (w0,h0) = Draw.image_size file in
               match width, height with
               | None, Some h -> (w0 * h) / h0, h
               | Some w, None -> w, (h0 * w) / w0
               | _ -> w0, h0
           end in
  let width, height = if noscale
                      then Draw.unscale_size (width, height)
                      else width, height in
  { source = Var.create (File file);
    angle;
    xpos = Draw.Center; (* TODO, make this changeable *)
    ypos = Draw.Center; (* idem *)
    width;
    height;
    xsize = KeepRatio; (* idem *)
    ysize = KeepRatio; (* idem *)
    background = bg; (* idem *)
    render = Var.create None;
  }

(* NOTE (SDL_image >= 2.0.2) is able to directly load SVG. HOWEVER, it currently
   it doesn't scale the image, so it's not recommended. *)
let create_from_svg ?width ?height ?bg file =
  create ?width ?height ?bg (Draw.convert_svg ?w:width ?h:height file)

let copy ?(rotate = 0.) img =
  let angle = img.angle +. rotate in
  { img with
    source = Var.create (Image img);
    angle;
    render = Var.create None
  }

let unload img =
  match Var.get img.render with
  | None -> ()
  | Some tex -> begin
      Draw.forget_texture tex;
      Var.set img.render None
    end


(* TODO *)
let free = unload

let rec get_file img =
  match Var.get img.source with
  | File f -> f
  | Image im -> get_file im

(************* display ***********)

(* Retrieve (and compute if necessary) the rotated texture. *)
let rec get_texture renderer img =
  let tex = match Var.get img.render with
    | Some tex -> tex
    | None ->  match Var.get img.source with
      | File file -> Draw.load_image renderer file
      | Image im -> get_texture renderer im
  in
  if img.angle = 0. then tex
  else begin
    let rotated = Draw.copy_rotate_texture renderer img.angle tex in
    Draw.forget_texture tex;
    rotated
  end

let display canvas layer img g =
  let open Draw in
  let tex = match Var.get img.render with
    | Some t -> t
    | None ->
      let tex = get_texture canvas.renderer img in
      (* printd debug_io "Image: Loading image file %s" file; *)
      (* let surf = sdl_image_load file in *)
      (* let box = create_surface ~like:surf ~color:img.background g.w g.h in *)
      (* let sw,sh = Sdl.get_surface_size surf in *)
      (* let bw, bh = match img.xsize, img.ysize with *)
      (*   | Fit, Fit -> g.w, g.h *)
      (*   | KeepRatio, KeepRatio -> let ratio = float sh /. float sw in *)
      (*     if ratio *. (float g.w) <= float g.h then (g.w, round (float g.w *. ratio)) *)
      (*     else (round (float g.h /. ratio), g.h) *)
      (*   | _ -> failwith "resizing not implemented" in *)
      (* let x = align img.xpos 0 g.w bw in *)
      (* let y = align img.ypos 0 g.h bh in *)
      (* let r1 = Sdl.get_clip_rect surf in *)
      (* let r2 = Sdl.Rect.create ~x ~y ~w:bw ~h:bh in *)
      (* go (Sdl.blit_scaled ~src:surf r1 ~dst:box (Some r2)); *)
      (* let tex = create_texture_from_surface canvas.renderer box in *)
      (* Var.set img.render (Some tex); *)
      (* tex *)

      let tex = match img.background with
        | Some bg -> (* we blend the image on the bkg *)
          let w, h = tex_size tex in
          let target = create_target canvas.renderer w h in
          let push = push_target ~clear:true ~bg canvas.renderer target in
          go (Sdl.set_texture_blend_mode tex Sdl.Blend.mode_blend);
          go (Sdl.render_copy canvas.renderer tex);
          pop_target canvas.renderer push;
          forget_texture tex;
          target
        | None -> tex in
      Var.set img.render (Some tex);
      tex


  (* it is better to render first the image at full resolution and then
     scale it, in case we later use some zoom animation. If one has a zoom
     from 0 to 1, then the first time the image will be rendered, the
     required size would be zero. So we have to be careful not to render at
     this size... *)
  in
  let dst = geom_to_rect g in
  [make_blit ~dst ~voffset:g.voffset canvas layer tex]
