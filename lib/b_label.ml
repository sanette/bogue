(** a simple text display in one line *)
open B_utils;;
open Tsdl_ttf;;
module Theme = B_theme
module Var = B_var
module Draw = B_draw
  
type font =
  | File of string
  | Font of Ttf.font;;

type t =
  { text : string Var.t;
    render : (Draw.texture option) Var.t;
    font : font Var.t;
    style : Ttf.Style.t;
    size : int; (* font size *)
    fg : (Draw.color option) Var.t; (* foreground color *)
  };;

let create ?(size = Theme.label_font_size) ?(font = File Theme.label_font) 
    ?(style = Ttf.Style.normal) ?fg text =
  Draw.ttf_init (); (* we init here so that one can get the size of the widget *)
  { text = Var.create text;
    render = Var.create None;
    font = Var.create font;
    style;
    size;
    fg = Var.create fg};;

(* see https://lab.artlung.com/font-awesome-sample/*)
let icon ?size ?fg name =
  create ?size ?fg ~font:(File Theme.fa_font) (Theme.fa_symbol name);;
  
let unload l =
  match Var.get l.render with
  | None -> ()
  | Some tex -> begin
      Draw.forget_texture tex;
      Var.set l.render None
    end;;

(* TODO *)
let free = unload;;
(* TODO free font ? *)

let text l = Var.get l.text;;

let set l text =
  if Var.get l.text <> text
  then begin
    Var.set l.text text;
    let texo = Var.get l.render in
    Var.set l.render None;
    do_option texo Draw.forget_texture
  end;;

let set_fg_color l color =
  Var.set l.fg (Some color);;

(************* display ***********)

(* let default_size = (128,32);;*)
(* "/home/san/public_html/7h09/sites/all/themes/drupal_7h09/css/museo.ttf";; *)


(* physical size *)
let physical_size_text font text = 
  (* Attention, SDL_ttf n'est peut-être pas encore initialisé... *)
  go (Ttf.size_utf8 font text);;

(* not used: *)
let size_text_init font text =
   if not (Ttf.was_init ()) 
   then (go (Ttf.init ());
         printd debug_graphics "SDL TTF initialized";
         Draw.at_cleanup (fun () ->
             printd debug_graphics "Quitting SDL TTF";
             Ttf.quit ()));
   go (Ttf.size_utf8 font text);;

(* not used: *)
(* after benchmarking, this function is faster than size_text_init, and almost
   as fast as size_text, after TTF initialization. Thus, this could be a good
   replacement of size_text, and then we don't care to check that TTF is
   initialized before calling this. *)
let size_text_exn font text =
  try go (Ttf.size_utf8 font text) with
  | Failure _ -> physical_size_text font text
  | e -> raise e;;


let render_text_surf ?fg font style text =
  let text = if text = "" then " " else text in
  printd debug_graphics "render_text:%s" text;
  let color = Draw.create_color (default fg (10,11,12,255)) in
  Draw.ttf_set_font_style font style;
  Draw.ttf_render font text color;;

let render_text renderer ?fg font style text =
  let surf = render_text_surf ?fg font style text in
  printd debug_graphics "convert to texture";
  let tex = Draw.create_texture_from_surface renderer surf in
  Draw.free_surface surf;
  tex;;


(* open font with specified size. Here this is the true size, it will not be
   scaled. *)
(* This can be used by all widgets requiring a font. *)
let get_font_var v size =
  match Var.get v with
    | Font f -> f
    | File file -> let f = Draw.open_font file size in
      Var.set v (Font f); f;;

let font l = get_font_var l.font (Theme.scale_int l.size);;

let physical_size l =
  match Var.get l.render with
    | Some tex -> Draw.tex_size tex
    | None -> physical_size_text (font l) (text l);;

(* a first order approximation of the "logical" size is obtained by dividing by
   the scale; this is not ideal because the final physical scale of the layout
   will be calculated by multiplying this by the scale, resulting in a +/- 1
   pixel error *)
let size l =
  physical_size l |> Draw.unscale_size;;

let display canvas layer l g =
  let tex = match Var.get l.render with
    | Some t -> t
    | None ->
      let fg = default (Var.get l.fg) Draw.(opaque label_color) in
      let tex = render_text canvas.Draw.renderer (font l) l.style (text l) ~fg in
      Var.set l.render (Some tex); tex in
  [Draw.center_tex_to_layer canvas layer tex g];;
