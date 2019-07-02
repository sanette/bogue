open Str
open B_utils
open Tsdl
open Tsdl_ttf
module Theme = B_theme
module Var = B_var
module Draw = B_draw
module Label = B_label
  
(* TODO: use TTF_RenderUTF8_Blended_Wrapped *)
(* cf SDL_ttf.h *)

(* TODO: use Tsdl_extra.TTF.style *)

type entity = 
  | Word of string
  | Space
  | Style of Ttf.Style.t
  (* remark: Styles are cumulative. Thus in [ Style bold; Word "Bla"; Style
     italic; Word "Foo"], "Foo" is bold and italic. Use Style normal to cancel a
     style. *)

type words = entity list;;

let example : words = let open Ttf.Style in
  [ Word "Hello"; Space; Word "I"; Space; Word "am"; Space; Style bold; Word "bold"; Style normal; Space; Word "and"; Space; Style italic; Word "italic." ];;

type t =
    { paragraphs : (words list) Var.t;
      render : (Draw.texture option) Var.t;
      font : (Label.font) Var.t;
      size: int; (* FONT size *)
      mutable w: int option; (* width of the widget in pixels *)
      mutable h: int option; (* height in pixels *)
    };;

let default_font = Label.File Theme.text_font;;

let unload td =
    match Var.get td.render with
  | None -> ()
  | Some tex -> begin
      Draw.forget_texture tex;
      Var.set td.render None
    end;;

(* TODO *)
let free = unload;;
(* TODO free font ? *)

(* determine the style at the end of the entity list, asuming that the initial
   style is normal *)
(* TODO: be careful when using this, maybe the initial normal style is a wrong
   assumption. This should be ok if the user may only concatenate entity lists,
   not split them. *)
let last_style words =
List.fold_left (fun style entity -> match entity with
      | Style s when s = Ttf.Style.normal -> s
      | Style s -> Ttf.Style.(s + style)
      | _ -> style) Ttf.Style.normal words;;

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
  List.rev (Style last :: (List.rev new_words));;


let bold = set_style Ttf.Style.bold;;

let italic = set_style Ttf.Style.italic;;

let normal = set_style Ttf.Style.normal;;

let underline = set_style Ttf.Style.underline;;

let strikethrough = set_style Ttf.Style.strikethrough;;

(** convert tabs '\t' in a string to the required number of spaces *)
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
  Buffer.contents b;;

(** change the content of the text on the fly *)
let update ?w ?h t paragraphs =
  Var.protect t.render;
  let texo = Var.get t.render in
  Var.unsafe_set t.render None;
  do_option texo Draw.forget_texture;
  Var.set t.paragraphs paragraphs;
  t.w <- w;
  t.h <- h;
  Var.release t.render;;

let split_line line =
  full_split (regexp " ") line
  |> List.map (function
      | Text w -> Word w
      | Delim _ -> Space);;
    
let para = split_line;;

(* raw is used if you don't want to break spaces. This is (currently) the only
   way to have spaces underlined *)
let raw s = [Word s];;

let append w1 w2 : words =
  List.append w1 w2;; 

let ( ++ ) = append;;

(* This is a shorthand which allows the notation: *)
(* Text_display.(page [para "Hello"; para "World"]) *)
(* instead of: *)
(* let open Text_display in *)
(*   [para "Hello"; para "World"]  *)
let page list : words list = list

let create ?(size = Theme.text_font_size) ?w ?h ?(font = default_font) paragraphs =
  Draw.ttf_init ();
  { paragraphs = Var.create (List.rev ([Style Ttf.Style.normal] :: (List.rev paragraphs))); (* we add normal style at the end *)
    render = Var.create None;
    font = Var.create font;
    size; w; h};;

(* convert a full text including '\n' into paragraphs *)
let paragraphs_of_string text =
  split (regexp "\n") text
  |> List.map split_line;;

(* convert each line into a paragraph *)
let paragraphs_of_lines lines =
  List.map split_line lines;;

let create_from_string ?(size = Theme.text_font_size) ?w ?h ?(font = default_font) text =
  let paragraphs = paragraphs_of_string text in
  create ~size ?w ?h ~font paragraphs;;


let create_from_lines ?(size = Theme.text_font_size) ?w ?h ?(font = default_font) lines =
  let paragraphs = paragraphs_of_lines lines in
  create ~size ?w ?h ~font paragraphs;;

let create_verbatim ?(size = Theme.text_font_size) ?(font = Label.File Theme.mono_font) text =
  Draw.ttf_init ();
  let font = match font with
    | Label.Font f -> f
    | Label.File f -> Draw.open_font f (Theme.scale_int size) in
  let lines = List.map tab_to_space (split (regexp "\n") text) in
  let w = list_max compare (List.map (fun s -> fst (Label.physical_size_text font s)) lines) in
  let w = map_option w Theme.unscale_int in
  let h = Some ((List.length lines) * (Ttf.font_line_skip font)) in
  let h = map_option h Theme.unscale_int in
  print_endline (Printf.sprintf "SIZE = (%d,%d)" (default w 0) (default h 0));
  let paragraphs = List.map (fun p -> [Word p]) lines in
  create ~size ?w ?h ~font:(Label.Font font) paragraphs;;

let update_verbatim t text =
  let size = t.size in
  let font = Var.get t.font in
  let dummy = create_verbatim ~size ~font text in
  let paragraphs = Var.get dummy.paragraphs in
  print_endline (Printf.sprintf "New SIZE %d,%d" (default dummy.w 0) (default dummy.h 0));
  update ?w:dummy.w ?h:dummy.h t paragraphs;;

let unsplit_old words =
  let rec loop list acc =
    match list with
      | [] -> acc
      | w::rest -> loop rest (if acc = "" then w else acc ^ " " ^ w) in
  loop words "";;

let unsplit_words words = 
  List.map (function 
      | Word w -> w
      | Space -> " "
      | _ -> "") words
  |> String.concat "";;

let unsplit pars = String.concat "\n" (List.map unsplit_words pars);;

let paragraphs td = Var.get td.paragraphs;;

let text td = unsplit (Var.get td.paragraphs);;

let default_size = (256,128);;

let size td =
  let w,h = default_size in
  (default td.w w),
  (default td.h h);;

(************* display ***********)


let render_word ?fg font word =
  printd debug_graphics "render word:%s" word;
  let color = Draw.create_color (default fg (10,11,12,255)) in
  let surf = Draw.ttf_render font word color in
  go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
  surf;;

let get_font td = Label.get_font_var td.font (Theme.scale_int td.size);;

let display canvas layer td g =
  let open Draw in
  Var.protect td.render;
  let tex = match Var.get td.render with
    | Some t -> t
    | None ->
      begin
        let font = get_font td in
        let fg = opaque text_color in
        let lineskip = Ttf.font_line_skip font in
        let space = fst (Label.physical_size_text font " ") in (* idem *)
        let target_surf = create_surface ~renderer:canvas.renderer g.w g.h in

        let rec loop list dx dy =
          if dy > g.h then ()
          else match list with
            | [] -> ();
            | []::rest -> loop rest 0 (dy + lineskip)
            | (entity::rest_line)::rest ->
              match entity with
              | Word w ->
                let surf = render_word ~fg font w in
                let rect = Sdl.get_clip_rect surf in
                let tw,th = Sdl.Rect.(w rect, h rect) in
                if dx <> 0 && dx+tw >= g.w then begin
                  free_surface surf;
                  (* this word will hence be rendered twice. This could be
                     optimized of course. *)
                  loop list 0 (dy + lineskip);
                end
                (* =we go to new line *)
                else (go (Sdl.blit_surface ~src:surf (Some rect) ~dst:target_surf
                            (Some (Sdl.Rect.create ~x:dx ~y:dy ~w:tw ~h:th)));
                      free_surface surf;
                      loop (rest_line::rest) (dx + tw) dy)
              | Space -> 
                let space = if Ttf.Style.(test (Ttf.get_font_style font) italic)
                  then (round (float space *. 0.6)) else space in
                loop (rest_line::rest) (dx + space) dy
              (* TODO Space should be rendered in case of underline or
                 strikethrough. But not when we break at the end of the line, of
                 course *)
              | Style s -> 
                let current_style = Ttf.get_font_style font in
                let new_style = if s =  Ttf.Style.normal 
                  then s else Ttf.Style.(s + current_style) in
                ttf_set_font_style font new_style;
                loop (rest_line::rest) dx dy
        in
        loop (paragraphs td) 0 0;
        let tex = create_texture_from_surface canvas.renderer target_surf in
        free_surface target_surf;
        Var.unsafe_set td.render (Some tex); tex
      end
  in
  Var.release td.render;
  let dst = geom_to_rect g in
  [make_blit ~voffset:g.voffset ~dst canvas layer tex];;
