(* This file is part of BOGUE, by San Vu Ngoc *)

open Str
open B_utils
open Tsdl
open Tsdl_ttf
module Draw = B_draw
module Label = B_label
module RGBA = B_rgba
module Theme = B_theme
module Var = B_var

(* TODO?: use TTF_RenderUTF8_Blended_Wrapped? *)
(* cf SDL_ttf.h *)

(* TODO horiz. align text (Min, Center or Max) *)

type entity =
  | Word of string
  | Space
  | Color of Draw.color
  | Style of Ttf.Style.t
  (* remark: Styles are cumulative. Thus in [ Style bold; Word "Bla"; Style
     italic; Word "Foo"], "Foo" is bold and italic. Use Style normal to cancel a
     style. *)

type words = entity list

let example : words = let open Ttf.Style in
  [ Color RGBA.blue; Word "Hello";
    Space; Word "I"; Space; Word "am"; Space;
    Style bold; Word "bold"; Color !RGBA.text_color;
    Style normal; Space; Word "and"; Space;
    Style italic; Word "italic." ]

type t = {
  paragraphs : (words list) Var.t;
  render : (Draw.texture option) Var.t;
  font : (Label.font) Var.t;
  size: int; (* FONT size *)
  mutable w: int option; (* width of the widget in pixels. Currently it is
                            not used, since rendering is done with the
                            geometry of the housing layout. *)
  mutable h: int option; (* height in pixels. See above. *)
}

let default_font () = Label.File !Theme.text_font

let unload td =
  match Var.get td.render with
  | None -> ()
  | Some tex -> begin
      Draw.forget_texture tex;
      Var.set td.render None
    end

(* TODO *)
let free = unload
(* TODO free font ? *)

(* Determine the style at the end of the entity list, assuming that the initial
   style is normal. *)
(* TODO: be careful when using this, maybe the initial normal style is a wrong
   assumption. This should be ok if the user may only concatenate entity lists,
   not split them. *)
let last_style words =
  List.fold_left (fun style entity -> match entity with
      | Style s when s = Ttf.Style.normal -> s (* note that normal == 0 *)
      | Style s -> Ttf.Style.(s + style)
      | _ -> style) Ttf.Style.normal words

let set_style style words =
  let last = last_style words in
  let new_words =
    if style = Ttf.Style.normal
    (* we remove all style declaration *)
    then List.filter (function
        | Style _ -> false
        | _ -> true) words
    else let w = List.filter (function
        (* we remove the normal (incompatible with style) and style (redundant
           with style) declarations *)
        | Style s when s = style -> false
        | Style s when s = Ttf.Style.normal -> false
        | _ -> true) words in
      (Style style) :: w in
  List.rev (Style last :: (List.rev new_words))


let bold = set_style Ttf.Style.bold
let italic = set_style Ttf.Style.italic
let normal = set_style Ttf.Style.normal
let underline = set_style Ttf.Style.underline
let strikethrough = set_style Ttf.Style.strikethrough

(* Convert tabs '\t' in a string to the required number of spaces *)
let tab_to_space ?(sep = 8) s =
  let l = String.length s in
  let b = Buffer.create l in
  let rec loop i j =
    if i < l then
      let c = s.[i] in
      let n = if c = '\t'
        then (let n = sep*(j/sep) + sep - j in
              let spaces = String.make n ' ' in
              Buffer.add_string b spaces;
              n)
        else (Buffer.add_char b c;
              1) in
      loop (i+1) (j+n)
  in
  loop 0 0;
  Buffer.contents b

(* Change the content of the text on the fly *)
let update ?w ?h t paragraphs =
  Var.update t.render (fun texo ->
  do_option texo Draw.forget_texture;
  Var.set t.paragraphs paragraphs;
  t.w <- w;
  t.h <- h;
  None)

let split_line line =
  full_split (regexp " ") line
  |> List.map (function
      | Text w -> Word w
      | Delim _ -> Space)

let para = split_line
(* Example: *)
(* Text_display.[para "Hello"; para "World"] *)

(* [raw] is used if you don't want to break spaces. This is (currently) the only
   way to have spaces underlined *)
let raw s = [Word s]

let append w1 w2 : words =
  List.append w1 w2

let ( ++ ) = append

let create ?(size = Theme.text_font_size) ?w ?h ?(font = default_font ())
    paragraphs =
  Draw.ttf_init ();
  { paragraphs = Var.create (List.rev ([Style Ttf.Style.normal] :: (List.rev paragraphs)));
    (* : we add normal style at the end *)
    render = Var.create None;
    font = Var.create font;
    size; w; h}

(* Convert a full text including '\n' into paragraphs *)
let paragraphs_of_string text =
  split (regexp "\n") text
  |> List.map split_line

(* Convert each line into a paragraph *)
let paragraphs_of_lines lines =
  List.map split_line lines

let create_from_string ?(size = Theme.text_font_size) ?w ?h
    ?(font = default_font ()) text =
  let paragraphs = paragraphs_of_string text in
  create ~size ?w ?h ~font paragraphs

let create_from_lines ?(size = Theme.text_font_size) ?w ?h
    ?(font = default_font ()) lines =
  let paragraphs = paragraphs_of_lines lines in
  create ~size ?w ?h ~font paragraphs

(* Basic html parser *)

(* List of accepted html tags *)
let htmltags =
  [ "<b>"; "</b>";
    "<em>"; "</em>";
    "<u>"; "</u>";
    "<strong>"; "</strong>";
    "<p>"; "</p>"; "<br>";
    "<font[ \t\n]+color=\"[^\"]+\">"; "</font>" ]
  |> String.concat "\\|"
  |> regexp

let delims = regexp "[ \n]+"

let style_from_stack stack =
  List.fold_left (Ttf.Style.(+)) Ttf.Style.normal stack

(* add a style declaration to a list of words in reverse order *)
let add_style line style =
  let line = match line with
    | (Style _) :: rest -> rest
    (* if the line 'last' element (remember it's reverse order) is a Style we
       may remove it. *)
    | _ -> line in
  (Style style)::line

(* what to do when encountering a new style tag (style should be a primitive
   one, ie a power of 2) *)
let apply_style line stylestack style =
  let stk = style :: stylestack in
  let line = if List.mem style stylestack
    then line
    else add_style line (style_from_stack stk) in
  stk, line

(* what to do when encountering a closing tag *)
let close_style line stylestack style =
  let stk = try list_remove_first (fun x -> x = style) stylestack
    with Not_found ->
      printd debug_warning "Bad HTML: closing tag without opening first.";
      stylestack in
  let line = add_style line (style_from_stack stk) in
  stk, line

let color_from_html = RGBA.find_color

let color_from_tag s =
  let s = global_replace delims " " s in
  if string_match (regexp "<font color=\"\\([^\"]+\\)\">") s 0 then
    begin
      let c = matched_group 1 s in
      color_from_html c
    end
  else begin
    printd debug_error "Cannot recognize an HTML color tag in [%s]" s;
    RGBA.grey
  end

(* TODO? write a truly recursive fn instead of manually handling stacks? but
   then we should eliminate redundant information like (bold (bold (bold
   aaa))). *)
let paragraphs_of_html src =
  let def_color = Color !RGBA.text_color in
  let colorstack = Stack.create () in
  Stack.push def_color colorstack;
  let rec loop stylestack paras line = function
    | [] -> List.rev ((List.rev line)::paras)
    | x::rest -> match x with
      | Text s -> loop stylestack paras ((Word s)::line) rest
      | Delim "<p>" when paras = [] && line = [] ->
        loop stylestack [] [] rest
      | Delim "<p>" when line = [] -> loop stylestack paras [] rest
      | Delim "<p>" -> loop stylestack ((List.rev line)::paras) [] rest
      | Delim "<br>" -> loop stylestack ((List.rev line)::paras) [] rest
      | Delim "</p>" -> loop stylestack ([]::(List.rev line)::paras) [] rest
      | Delim s when String.trim s = ""
        (* TODO handle more spaces in case of <pre> tag *)
        -> loop stylestack paras (Space::line) rest
      | Delim d ->
        let stk, line = match String.lowercase_ascii d with
          | "<b>"
          | "<strong>" -> apply_style line stylestack Ttf.Style.bold
          | "<em>" -> apply_style line stylestack Ttf.Style.italic
          | "<u>" -> apply_style line stylestack Ttf.Style.underline
          | "</b>"
          | "</strong>" -> close_style line stylestack Ttf.Style.bold
          | "</em>" -> close_style line stylestack Ttf.Style.italic
          | "</u>" -> close_style line stylestack Ttf.Style.underline
          | "</font>" ->
            let _w = default (Stack.pop_opt colorstack) def_color in
            let c = default (Stack.top_opt colorstack) def_color in
            stylestack, c::line
          | s when String.length s >= 5 && String.sub s 0 5 = "<font" ->
            let c = color_from_tag s in
            Stack.push (Color c) colorstack;
            stylestack, ((Color c)::line)
          | _ ->
            printd debug_error "html tag %s not implemented" d;
            stylestack, ((Word d)::line) in
        loop stk paras line rest in
  let list = full_split htmltags src
    |> List.map (function
        | Text t -> full_split delims t
        | Delim d -> [Delim d])
    |> List.flatten in
  loop [] [] [] list

(* *** *)

(* example
   let s = "Voici la <u>liste</u> <font color=\"red\">et l'<u><b>autre liste</b></u> et <font\ncolor=\"#12C\">le bleu</font> retour rouge</font> fin.";;

*)
let create_from_html ?(size = Theme.text_font_size) ?w ?h
    ?(font = default_font ()) html =
  let paragraphs = paragraphs_of_html html in
  create ~size ?w ?h ~font paragraphs

let create_verbatim ?(size = Theme.text_font_size)
    ?(font = Label.File Theme.mono_font) text =
  Draw.ttf_init ();
  let font = match font with
    | Label.Font f -> f
    | Label.File f -> Draw.open_font f (Theme.scale_int size) in
  let lines = List.map tab_to_space (split (regexp "\n") text) in
  let w = list_max compare (List.map (fun s -> fst (Label.physical_size_text font s)) lines) in
  let w = map_option w Theme.unscale_int in
  let h = Some ((List.length lines) * (Ttf.font_line_skip font)) in
  let h = map_option h Theme.unscale_int in
  (* print "SIZE = (%d,%d)" (default w 0) (default h 0); *)
  let paragraphs = List.map (fun p -> [Word p]) lines in
  create ~size ?w ?h ~font:(Label.Font font) paragraphs

let update_verbatim_old t text =
  let size = t.size in
  let font = Var.get t.font in
  let dummy = create_verbatim ~size ~font text in
  let paragraphs = Var.get dummy.paragraphs in
  print "New SIZE %d,%d" (default dummy.w 0) (default dummy.h 0);
  update ?w:dummy.w ?h:dummy.h t paragraphs

let replace ~by:t old =
  let paragraphs = Var.get t.paragraphs in
  update ?w:t.w ?h:t.h old paragraphs

let update_verbatim t text =
  let size = t.size in
  let font = Var.get t.font in
  let dummy = create_verbatim ~size ~font text in
  (* print "New SIZE %d,%d" (default dummy.w 0) (default dummy.h 0); *)
  replace ~by:dummy t

let unsplit_old words =
  let rec loop list acc =
    match list with
      | [] -> acc
      | w::rest -> loop rest (if acc = "" then w else acc ^ " " ^ w) in
  loop words ""

let unsplit_words words =
  List.map (function
      | Word w -> w
      | Space -> " "
      | _ -> "") words
  |> String.concat ""

let unsplit pars = String.concat "\n" (List.map unsplit_words pars)

let paragraphs td = Var.get td.paragraphs

let text td = unsplit (Var.get td.paragraphs)

let default_size = (256,128)

let resize (w,h) td =
  unload td;
  td.w <- Some w;
  td.h <- Some h

(************* display ***********)

let render_word ?fg font word =
  printd debug_graphics "render word:%s" word;
  let color = Draw.create_color (default fg (10,11,12,255)) in
  let surf = Draw.ttf_render font word color in
  go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
  surf

let word_size font word =
  Tsdl_ttf.Ttf.size_utf8 font word
  |> Result.to_option

let get_font td = Label.get_font_var td.font (Theme.scale_int td.size)

(* We implement a wrapping engine *)

type attr = {
  style : Ttf.Style.t;
  color : RGBA.t }

(* A chunk is a piece of text that can be rendered by one call to
   Ttf.render_utf8_blended. *)
type chunk = {
  geom : Draw.geometry;
  attr : attr;
  text: string }

(* An mbox (LaTeX terminology) is a group of chunks (usually drawn on the same
   line) that cannot be split into several lines. *)
type mbox = chunk list

let add_mbox mbox list =
  if mbox = [] then list else mbox :: list

(* Compute the bounding box of a list of chunks. Another option would be to add
   a geometry field for the mbox type. *)
let bbox chunks =
  let x,y,w,h = match chunks with
    | [] -> printd debug_error "Empty mbox (should not happen)";
      0,0,0,0
    | c0::rest ->
      let g0 = c0.geom in
      let (xmin,xmax, ymin,ymax) = List.fold_left
          (fun (xmin,xmax, ymin,ymax) { geom; attr=_; text=_ } ->
             (imin xmin geom.x),
             (imax xmax (geom.w + geom.x)),
             (imin ymin geom.y),
             (imax ymax (geom.h + geom.y))) (g0.x, g0.x + g0.w,
                                             g0.y, g0.y + g0.h) rest
      in xmin, ymin, xmax-xmin, ymax-ymin in
  Draw.make_geom ~x ~y ~w ~h ()

(* Relative translation of an mbox *)
let rel_move_mbox (dx, dy) mbox =
  List.map (fun chunk ->
      let geom = { chunk.geom with x = chunk.geom.x + dx;
                                   y = chunk.geom.y + dy } in
      {chunk with geom}) mbox

(* Absolute translation (not used) *)
let move_mbox (x,y) mbox =
  let g = bbox mbox in
  let dx = x - g.x in
  let dy = y - g.y in
  rel_move_mbox (dx, dy) mbox

(* Compute the positions of the words to render, without rendering. More
   precisely, [compute_boxes] returns a list of chunks.
   + Rendering is stopped beyond the height [gh].
   + Text wrapping is performed so that no text should exceed the width [gw],
     except if there is no space where we can split.
   + [style] and [color] are the default attributes for starting the rendering,
     but they can be changed with [Style] and [Color] entities. *)
(* TODO: implement optional text justify/center/right align. *)
let compute_boxes ~dx ~dy (gw, gh) font style color paragraphs =

  let lineskip = Ttf.font_line_skip font in
  let space = fst (Label.physical_size_text font " ") in
  let attr = {style; color} in

  let rec loop acc mbox list attr dx dy =
    if dy > gh then acc
    else match list with
      | [] -> add_mbox mbox acc;
      | []::rest -> (* New line *)
        loop acc mbox rest attr 0 (dy + lineskip)
      | [e]::rest when e <> Space ->
        (* Last item of paragraph is not a space, we add a final space for
           proper wrapping. *)
        loop acc mbox ([e; Space]::rest) attr dx dy
      | (entity::rest_line)::rest ->
        match entity with
        | Word text ->
          Draw.ttf_set_font_style font attr.style;
          let tw,th = Label.physical_size_text font text in
          let geom = Draw.make_geom ~x:dx ~y:dy ~w:tw ~h:th () in
          loop acc ({geom; attr; text}::mbox) (rest_line::rest) attr (dx + tw) dy
        | Space ->
          let g = bbox mbox in
          let space = if Ttf.Style.(test attr.style italic)
            then round (float space *. 0.6) else space in
          if g.x <> 0 && dx >= gw
          then begin (* We go to new line and register the mbox. *)
            let mbox = rel_move_mbox (-g.x, lineskip) mbox in
            loop (mbox::acc) [] (rest_line::rest) attr (g.w + space) (dy + lineskip)
          end else (* We register the mbox and continue. *)
            loop (mbox::acc) [] (rest_line::rest) attr (dx + space) dy
        (* TODO Space should be rendered in case of underline or
           strikethrough. But not when we break at the end of the line, of
           course. *)
        | Style s ->
          let new_style = if s =  Ttf.Style.normal
            then s else Ttf.Style.(s + attr.style) in
          loop acc mbox (rest_line::rest) {attr with style = new_style} dx dy
        | Color c ->
          let attr = {attr with color = c} in
          loop acc mbox (rest_line::rest) attr dx dy
  in
  loop [] [] paragraphs attr dx dy

(* Return the size of the widget even if it is not rendered yet. *)
let size td =
  match Var.get td.render with
  | Some tex -> Draw.tex_size tex |> Draw.unscale_size
  | None ->
    let w,_ = default_size in
    match td.w, td.h with
    | None, None -> default_size
    | Some w, None ->
      let font = get_font td in
      let fg = ref !RGBA.text_color in
      let style = Ttf.get_font_style font in
      let boxes =  compute_boxes ~dx:0 ~dy:0 (Theme.scale_int w, 65536)
          font style !fg (paragraphs td) in
      let g = bbox (List.flatten boxes) in
      printd debug_graphics "Computed Text_display geometry = %s" (Draw.pr_geometry g);
      (w, Theme.unscale_int g.h)
    | None, Some h -> (w, h)
    | Some w, Some h -> (w, h)

let render_chunk target_surf font last_color {geom; attr={style; color}; text} =
  Draw.ttf_set_font_style font style;
  (* print "Render chunk [%s] at %s" text (Draw.pr_geometry geom); *)
  let src = render_word ~fg:color font text in
  let dst_rect = Draw.geom_to_rect geom in
  go (Sdl.blit_surface ~src:src None ~dst:target_surf (Some dst_rect));
  last_color := color;
  Draw.free_surface src

let render_mbox target_surf font fg mbox =
  List.iter (render_chunk target_surf font fg) mbox

let render_boxes target_surf font fg boxes =
  List.iter (render_mbox target_surf font fg)  boxes

let display canvas layer td g =
  let open Draw in
  match Var.update_get td.render (function
      | Some t -> Some t
      | None -> begin
          let font = get_font td in
          let fg = ref !RGBA.text_color in
          let style = Ttf.get_font_style font in
          let w = match td.w with None -> g.w | Some w -> Theme.scale_int w in
          let h = match td.h with None -> g.h | Some h -> Theme.scale_int h in
          let w = imin g.w w in
          let h = imin g.h h in
          (* print "[%s] gw=%i gh=%i    w=%i h=%i" (text td) g.w g.h w h; *)
          let target_surf = create_surface ~renderer:canvas.renderer g.w g.h in
          (* on pourrait aussi mettre w h et modifier dst ci-dessous pour avoir
             une surface plus petite *)
          compute_boxes ~dx:0 ~dy:0 (w,h) font style !fg (paragraphs td)
          |> render_boxes target_surf font fg;
          let tex = create_texture_from_surface canvas.renderer target_surf in
          free_surface target_surf;
          RGBA.set_text_color !fg;
          Some tex
        end) with
  | Some tex ->
    let dst = geom_to_rect g in
    [make_blit ~voffset:g.voffset ~dst canvas layer tex]
  | None -> failwith "Text_display.display error" (* should not happen *)
