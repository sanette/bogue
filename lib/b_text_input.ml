(** One-line text-editor widget *)

(* bug bizarre: après execution de l'exemple 12 (et lorsqu'on joue un peu avec),
   emacs freeze complètement (obligé de tuer) dès qu'on sélectionne une
   région *)

(* cf:
   https://wiki.libsdl.org/Tutorials/TextInput#CandidateList
*)
open Tsdl
open B_utils
module Utf8 = B_utf8
module Theme = B_theme
module Var = B_var
module Trigger =  B_trigger
module Draw = B_draw
module Label = B_label
module Mouse = B_mouse
  
type selection =
  | Empty
  | Start of int (* ne sert pas à grand chose *)
  | Active of (int * int);; (* n1 should be <= n2 *)

type filter = string -> bool;;

type t =
  { keys : (string list) Var.t;
    (* = each string is just a letter (cf utf8 encoding problems...) *)
    cursor : (Draw.texture option) Var.t; (* make this a global variable? *)
    cursor_font : (Label.font) Var.t; (* make this a Theme variable? *)
    cursor_pos : int Var.t;
    cursor_char : string;
    render : (Draw.texture option) Var.t;
    offset : int Var.t;
    (* = this is the x-offset of the section to be rendered on
       screen, with respect to the whole text if it was rendered on
       a full line *)
    font : (Label.font) Var.t;
    size : int; (* font size *)
    active : bool Var.t;
    room_x : int Var.t; (* physical x *) (* TODO this is a hack to access room geometry (we need this to treat click events). do better? *)
    selection : selection Var.t;
    max_size : int; (* max number of letters *)
    prompt : string; (* text to display when there is no user input *)
    filter : filter; (* which letters are accepted *)
  };;

let no_filter _ = true;;
let uint_filter s = List.mem s ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"];;

let and_filter f1 f2 = function
    s -> (f1 s) && (f2 s);;

let default_font = Label.File Theme.text_font;;

let create ?(max_size = 2048) ?(prompt = "Enter text")
      ?(size = Theme.text_font_size)
      ?(filter = no_filter) ?(font = default_font) text =
  Draw.ttf_init ();
  let keys = Utf8.split text in
  { keys = Var.create keys;
    cursor = Var.create None;
    cursor_font = Var.create (Label.File Theme.fa_font);
    cursor_pos = Var.create 0;
    cursor_char = Theme.fa_symbol "tint";
    render = Var.create None;
    offset = Var.create 0;
    font = Var.create font;
    size;
    active = Var.create false;
    room_x = Var.create 0;
    selection = Var.create Empty;
    max_size;
    prompt;
    filter;
  };;

let unload ti =
  begin
    match Var.get ti.render with
  | None -> ()
  | Some tex -> begin
      Var.set ti.render None;
      Draw.forget_texture tex
    end
  end;
  match Var.get ti.cursor with
  | None -> ()
  | Some tex -> begin
      Var.set ti.cursor None;
      Draw.forget_texture tex
    end;;

(* TODO *)
let free = unload;;
(* TODO free font ? *)


let text ti = String.concat "" (Var.get ti.keys);;

let is_active ti = Var.get ti.active;;

let clear ti =
  let texo = Var.get ti.render in
  Var.set ti.render None;
  do_option texo Draw.forget_texture;;

let stop ti =
  printd debug_event "Stopping text input";
  if Sdl.is_text_input_active () then Sdl.stop_text_input ();
  clear ti;
  Var.set ti.active false;;

(* because there is a length test, it should be placed ad the end of
   all modifications of ti *)
let set ti keys =
  if keys <> Var.get ti.keys
  then begin
    let keys = if List.length keys > ti.max_size
      then (printd debug_memory "Warning: text_input was truncated because it should not exceed %u symbols" ti.max_size;
            Var.set ti.cursor_pos (min (Var.get ti.cursor_pos) ti.max_size);
            stop ti;
            let head, _ = split_list keys ti.max_size in head)
      else keys in
    Var.set ti.keys keys;
    clear ti
  end;;

let kill_selection ti =
  match Var.get ti.selection with
  | Active (n1,n2) -> let head1, tail1 = split_list (Var.get ti.keys) n1 in
    let _, tail2 = split_list tail1 (n2-n1) in
    Var.set ti.cursor_pos n1;
    Var.set ti.selection Empty;
    set ti (List.flatten [head1; tail2]);
  | _ -> ();;

(* better to inline ? *)
let unselect ti =
  printd debug_board "Removing selection";
  Var.set ti.selection Empty;;

let select_all ti =
  printd debug_board "Select all text";
  let l = List.length (Var.get ti.keys) in
  Var.set ti.selection (Active (0,l));
  clear ti;;

(* insert a list of letters *)
let insert_list ti list =
  kill_selection ti;
  let x = Var.get ti.cursor_pos in
  let head, tail = split_list (Var.get ti.keys) x in
  Var.set ti.cursor_pos (x + (List.length list));
  set ti (List.flatten [head; list; tail]);;

(** insert a letter *)
let insert ti s =
  insert_list ti [s];;

(** insert a whole string *)
let insert_text ti text =
  let list = Utf8.split text in
  insert_list ti list;;

let seps = [" "; ";"; "."; ","; "/"; ":"; "\\n"; "\\t"; "\\j"; "?"; "!"];;

(* find a word containg the cursor position *)
let find_word ti =
  let n = Var.get ti.cursor_pos in
  let daeh, tail = split_list_rev (Var.get ti.keys) n in
  let rec find_sep ~complement list pos =
    match list with
    | [] -> pos
    | key::rest -> if (not complement && List.mem key seps) || (complement && not (List.mem key seps))
      then pos
      else find_sep ~complement rest (pos + 1) in
  if tail = [] then (printd debug_board "No word found: we are at the end";
                     Empty)
  else let cursor_key = List.hd tail in
    let complement = List.mem cursor_key seps in
    (* if 'complement' is true, then we are on a separator, so we must
       find a "word of separators" *)
    let left = find_sep ~complement daeh 0 in
    let right = find_sep ~complement tail 0 in
    printd debug_board "Word found (%d,%d)" left right;
    Active (n-left, n+right);;

let select_word ti =
  let sel = find_word ti
  in Var.set ti.selection sel;
  clear ti;;

let ctrl_pressed () =
  let m = Sdl.get_mod_state () in
  m = Sdl.Kmod.ctrl
  || m = Sdl.Kmod.lctrl
  || m = Sdl.Kmod.rctrl;;

let start_selection ti =
  let n = Var.get ti.cursor_pos in
  printd debug_board "Starting text selection at %d" n;
  Var.set ti.selection (Start n);;

(* start selection on pressing SHIFT *)
let shift_check_sel ti =
  if Trigger.shift_pressed () then
    (if Var.get ti.selection = Empty then start_selection ti)
  else unselect ti;;

let backspace ti =
  if Var.get ti.selection <> Empty then kill_selection ti
  else let x = Var.get ti.cursor_pos in
    if x > 0 then
      let head, tail = split_list (Var.get ti.keys) (x-1) in
      let tail' = match tail with
        | [] -> printd debug_error "This should not happen in backspace"; []
        | _::rest -> rest in
      Var.set ti.cursor_pos (x-1);
      set ti (List.flatten [head; tail']);;

(* move cursor to the left *)
let left ti =
  shift_check_sel ti;
  let x = Var.get ti.cursor_pos in
  clear ti;
  Var.set ti.cursor_pos (max 0 (x-1));;

(* move cursor to the right *)
let right ti =
  shift_check_sel ti;
  let x = Var.get ti.cursor_pos in
  clear ti;
  Var.set ti.cursor_pos (min (List.length (Var.get ti.keys)) (x+1));;

(* move to beginning of line *)
let home ti =
  shift_check_sel ti;
  clear ti;
  Var.set ti.cursor_pos 0;;

(* move to end of line *)
let last ti =
  shift_check_sel ti;
  clear ti;
  Var.set ti.cursor_pos (List.length (Var.get ti.keys));;

(*** input ***)

let activate ?(check=true) ti ev =
  if (not check) || Sdl.Event.(get ev mouse_button_state) = Sdl.pressed
 (* = DIRTY trick, see bogue.ml *)
  then begin
    printd debug_event "Activating text_input";
    Sdl.start_text_input ();
    Var.set ti.active true;
    clear ti;
  end;;

(** validate selection from starting point to current cursor_pos *)
let make_selection ti =
  match Var.get ti.selection with
    | Empty -> ()
    | Start n0 ->
        let n = Var.get ti.cursor_pos in
        if n <> n0 then (printd debug_board "Make selection [%d,%d]" n0 n;
                         Var.set ti.selection (Active (min n0 n, max n0 n)))
        else (Var.set ti.selection Empty)
    | Active _ -> Var.set ti.selection Empty;;

(*** clipboard ***)

(* retrieve the string corresponding to the selection *)
let selection_text ti =
  match Var.get ti.selection with
    | Active (n1,n2) -> let _, tail = split_list (Var.get ti.keys) n1 in
                        let head, _ = split_list tail (n2-n1) in
                        String.concat "" head
    | _ -> "";;

(* copy to clipboard *)
let copy ti =
  let text = selection_text ti in
  if text <> "" then begin
    printd debug_memory "Copy to clipboard: [%s]" text;
    go (Sdl.set_clipboard_text text)
  end;;

(* copy and kill *)
let kill ti =
  copy ti;
  kill_selection ti;;

(* paste from clipboard *)
let paste ti =
  if Sdl.has_clipboard_text () then
    let text = go (Sdl.get_clipboard_text ()) in
    insert_text ti text;;


(* keyboard events *)


(* treat the text events *)
(* DOC: *)
(* SDL_Scancode values are used to represent the physical location of a keyboard
   key on the keyboard. *)
(* SDL_Keycode values are mapped to the current layout of the keyboard and
   correlate to an SDL_Scancode *)
let receive_key ti ev =
  if is_active ti then let
    (* in principe, if not active, text-input events are already disabled, but
       one could still receive keyboard input events. This is why we have to
       double check here *)
    open Sdl.Event in
    match Trigger.event_kind ev with
    | `Text_input -> (* a letter is recognized *)
      let s = get ev text_input_text in
      if ti.filter s then insert ti s
    | `Text_editing -> print_endline "Text composing mode"
    (* TODO:
       Update the composition text.
       Update the cursor position.
       Update the selection length (if any). *)
    | `Key_down -> (match get ev keyboard_keycode with
        | c when c = Sdl.K.backspace -> backspace ti
        | c when c = Sdl.K.left -> left ti
        | c when c = Sdl.K.right -> right ti
        | c when c = Sdl.K.up -> home ti
        | c when c = Sdl.K.home -> home ti
        | c when c = Sdl.K.down -> last ti
        | c when c = Sdl.K.kend -> last ti
        | c when c = Sdl.K.return -> stop ti
        | c when c = Sdl.K.a && ctrl_pressed () -> select_all ti
        | c when c = Sdl.K.c && ctrl_pressed () -> copy ti (* : desactivate this for debugging the emacs problem *)
        | c when c = Sdl.K.x && ctrl_pressed () -> kill ti
        | c when c = Sdl.K.v && ctrl_pressed () -> paste ti
        | c -> (printd debug_event "==> Key down event discarded.";
                printd debug_event "Key=[%s], mod=%u, Keycode:%u" (Sdl.get_key_name c) (Sdl.get_mod_state ()) c)
      )
    | `Key_up -> (match get ev keyboard_keycode with
        | c when c = Sdl.K.lshift -> make_selection ti
        | c when c = Sdl.K.rshift -> make_selection ti
        | c -> (printd debug_event "==> Key up event discarded.";
                printd debug_event "Key=[%s], mod=%u, Keycode:%u" (Sdl.get_key_name c) (Sdl.get_mod_state ()) c)
      )
    | _ -> printd debug_warning "Warning: Event should not happen here"
;;

(************* display ***********)

(* In the "display" function, the input text is drawn on the "surf" surface.
   Then surf is copied onto a larger surface, "box", to accommodate for
   underline. Then, box is clipped to its visible part (due to scrolling if the
   text exceed the size of the widget) into the "visible" surface.  The visible
   surface gives the final widget texture.  Nothing can be drawn outside the
   "visible" surface.  box, and visible have same height. The cursor is a
   separate texture (it's difficult to pre-blend everything due to SDL current
   limitations on blend modes. Maybe soon we will have
   https://wiki.libsdl.org/SDL_ComposeCustomBlendMode). *)

(*
                                                     ^
                                                     | bottom_margin (to center
<---------left_margin------------>                   v text vertically)
   (put in the blit geom)         |---visible---------------------------|
               --------------box--|-------------------------------------|-----
               |       -----------|-----surf-------------------------   |    |
               |       |          |                                 |        |
               |<----->|BLA BLA                                     |<------>|
               |cursor ---------------------------------------------- cursor |
               |width/2 _______  <--- underline      ^                width  |
               |         ^       <--- cursor         | bottom_margin         |
               |                                     v                       |
               -------------------|-------------------------------------|-----

The "cursor_xpos" is computed wrt the origin of the surface "surf"

*)


let memo = ref true;; (* use more memory (memoization) for speeding up display *)
let default_size = (128,32);;
let left_margin = 2;; (* in logical pixels *)
let bottom_margin = 5;; (* used for underline *)
(* TODO replace bottom_margin by cursor height (to compute) *)
(* let cursor_width = 10;;
 * let cursor_height = 9;; *)
(* let cursor_thickness = 2;; *)
  
let font ti = Label.get_font_var ti.font (Theme.scale_int ti.size);;

(* we cannot use Sdl.color type here if we want to memoize, since colors are
   typically recreated by Sdl.Color.create... *)
let render_key font key color =
  let color = Draw.create_color color in
  let surf = Draw.ttf_render font key color in
  incr Draw.ttf_surfaces_in_memory;
  go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
  (* If we use blend, the semitransparent pixels will acquire the color of the
     surface on which we blit (usually black, even if its alpha=0), which is not
     good because that's not the final blitting. *)
  go (Sdl.set_surface_rle surf true);
  (* "If RLE is enabled, color key and alpha blending blits are much faster, but
     the surface must be locked before directly accessing the pixels." (SDL
     doc) *)
  surf;;

(* memoize. Warning: do NOT free the resulting surfaces !! *)
(* NOTE: it seems that this memoing does not really improve speed, but at least
   it does not degrade speed... *)
(* Warning: the arguments should not be mutable, otherwise memo is likely to
   fail (equality problem). For instance, do not use Sdl.Color type instead of
   (r,g,b,a) *)

let render_key, render_key_cleanup =
  if !memo then let f,table = memo3 render_key in
    let cleanup () =
      printd debug_graphics "Cleaning up %u SDL_TTF surfaces..."
        (Hashtbl.length table);
      Hashtbl.iter (fun _ surf ->
          Draw.free_surface surf;
          decr Draw.ttf_surfaces_in_memory) table;
      Hashtbl.clear table in
    f, cleanup
  else render_key, fun () -> ();;

Draw.at_cleanup render_key_cleanup;;

(** return size of rendered text. It seems that Sdl.TTF.size_utf8 does not always
    give the exact same result as size of blended-rendered surface. Warning:
    thus, should use this only on single letters ! *)
(* from http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf_frame.html : *)
(* Kerning is the process of spacing adjacent characters apart depending on the
   actual two adjacent characters. This allows some characters to be closer to
   each other than others. When kerning is not used, such as when using the
   glyph metrics advance value, the characters will be spaced out at a constant
   size that accomodates all pairs of adjacent characters. This would be the
   maximum space between characters needed. There's currently no method to
   retrieve the kerning for a pair of characters from SDL_ttf, However correct
   kerning will be applied when a string of text is rendered instead of
   individual glyphs. *)
let text_dims font text =
  if text = "" then (print_endline "ERROR: text empty !"; 0,0) (* OK ? or use 1,1 ?? *)
  else let w,h = (* if !memo *)
  (* (\* if !memo, this is (maybe ?) faster to get surface_size than calling *)
  (*    TTF.size_utf8. BUT this will save another surface (with color 0,0,0,0) i *)
  (*    the table... *\) *)
  (*   then let surf = render_key font text (0,0,0,0) in *)
  (*     (\* : no need to free in case of memo *\) *)
  (*     Sdl.get_surface_size surf *)
  (*   else *) Label.physical_size_text font text
    in
    printd debug_graphics "Size of '%s' = (%d,%d)." text w h;
    w,h;;

(* we use all-purpose memo to memoize the kerning values. One could do
   something more optimized, of course. *)
let text_dims = memo2 text_dims;;

(* initial size of the widget *)
(* not scaled, in order to conform to all widgets size functions *)
let size ti =
  let w,h =
    match Var.get ti.render with
    | Some tex -> Draw.tex_size tex
    | None -> text_dims (font ti) (ti.prompt) in
  let w,h = Draw.unscale_size (w,h) in
  (w + 2*left_margin (* this should probably be left_margin + cursor_width/2 *),
   h + 2*bottom_margin);;
(* The bottom margin is also added at the top, in order to keep the text
   vertically centered. *) 

let text_width font s =
  let w,_ = text_dims font s in w;;

(** return the cursor position with respect to the total text surface *)
(* warning: this is already scaled... (physical pixels) *)
let cursor_xpos ?n ti =
  let n = match n with
    | None -> Var.get ti.cursor_pos
    | Some n -> n in
  let head, _ = split_list (Var.get ti.keys) n in
  List.fold_left (fun s key -> s + text_width (font ti) key) 0  head;;

(** return cursor_pos corresponding to the x position *)
let x_to_cursor_old ti x0 =
  let room_x = Var.get ti.room_x in
  let x0 = x0 - room_x - (Theme.scale_int (left_margin - 2)) + (Var.get ti.offset) in
  (* TODO optimize !!! *)
  let l = List.length (Var.get ti.keys) in
  let rec loop n =
    if n >= l then l
    else let cx = cursor_xpos ~n ti in
      if cx >= x0 then n else loop (n+1)
      (* TODO: test if n is larger than max size ? *)
  in loop 0;;

(** Return cursor_pos corresponding to the x position *)
let x_to_cursor ti x0 =
  let room_x = Var.get ti.room_x in
  let char_offset = ti.size/3 in
  (* TODO, this should be roughly the half size of a char *)
  let x0 = x0 - room_x - (Theme.scale_int (left_margin + char_offset))
           + (Var.get ti.offset) in
  let rec loop list cx n =
    match list with
    | [] -> n
    | key::rest ->
      if cx >= x0 then n else
        let advance, _ = text_dims (font ti) key in
        loop rest (cx + advance) (n+1) in
  loop (Var.get ti.keys) 0 0;;


(* Recall that none of the functions that are called by threads should call
   video functions directly. *)

(** treat the click event to position the cursor, once the widget is active *)
let click_cursor ti ev =
  printd debug_event "Click cursor";
  let x0u, _ = Mouse.pointer_pos ev in
  let x0 = Theme.scale_int x0u in (* on pourrait éviter de faire unscale-scale *)
  Var.set ti.cursor_pos (x_to_cursor ti x0);
  clear ti;;

(* This should be called on mouse_button_down *)
let button_down ti ev =
  if is_active ti then
    (click_cursor ti ev;
     start_selection ti);;

(* This should be called on mouse_button_up *)
let click ti ev =
  if is_active ti then
    (
      if Trigger.was_double_click () then select_word ti
      else begin
        if Sdl.Event.(get ev mouse_button_state) = Sdl.pressed
        (* = DIRTY trick, see bogue.ml *) then click_cursor ti ev;
        ignore (make_selection ti)
      end
    )
  else activate ti ev;;

let tab ti ev =
  if Sdl.Event.(get ev keyboard_keycode) = Sdl.K.tab then begin
    activate ~check:false ti ev;
    select_all ti
  end;;

(* we ask for redraw when mouse moves when button is pressed *)
let mouse_select ti ev=
  printd debug_event "Mouse selection";
  click_cursor ti ev;
  clear ti;;

(* render letter by letter so that x position is precise *)
let draw_keys ?fg font keys =
  let color = if keys = [] then Draw.(transp faint_color) (* inutile ? *)
    else default fg (10,11,12,255) in
  printd debug_graphics "Renders keys";
  let rec loop keys surfs w h =
    match keys with
    | [] -> surfs, w, h
    | key::rest -> let surf = render_key font key color in
      let dw,h = Sdl.get_surface_size surf in
      loop rest ((surf, w) :: surfs) (w+dw) h in
  let keys = if keys = [] then [" "] else keys in
  let surfs, tw, h = loop keys [] 0 0 in
  let surf, _ = List.hd surfs in
  printd debug_graphics "Create total surface";
  let total_surf = Draw.create_surface ~like:surf tw h in
  printd debug_graphics "Blit the letters on the surface";
  let rec draw_loop = function
    | [] -> ()
    | (surf, w) :: rest ->
      let dst_rect = Sdl.Rect.create ~x:w ~y:0 ~w:0 ~h:0 in
      go (Sdl.blit_surface ~src:surf None ~dst:total_surf (Some dst_rect));
      (* no free in case of memo: *)
      if not !memo then Draw.free_surface surf;
      draw_loop rest in
  draw_loop surfs;
  total_surf;;

(* REMARK: instead of blitting surfaces, one could also use textures and SDL
   RenderTarget ? *)
let display canvas layer ti g = (* TODO mettre un lock global ? *)

  let cursor = match Var.get ti.cursor with
    | Some s -> s
    | None ->
      let csize = 2*(Theme.scale_int ti.size)/3 in
      let cfont = Label.get_font_var ti.cursor_font csize in
      let s = draw_keys cfont [ti.cursor_char] ~fg:Draw.(opaque cursor_color) in
      (* TODO use render_key, it should be faster *)
      let tex = Draw.create_texture_from_surface canvas.Draw.renderer s in
      Var.set ti.cursor (Some tex);
      Draw.free_surface s;
      tex
  in
  let cw, _ = Draw.tex_size cursor in
  let tex = match Var.get ti.render with
    | Some t -> t
    | None ->
      let start_time = Unix.gettimeofday () in (* =for debug only *)
      let keys = Var.get ti.keys in
      let fg = if keys <> [] then Draw.(opaque text_color) else
          (* if is_active ti then Draw.(opaque pale_grey) else *) Draw.(opaque faint_color) in
      let keys = if keys = [] && not (is_active ti) then [ti.prompt] else keys in
      let surf = draw_keys (font ti) keys ~fg in
      (* TODO: draw only the relevent text, not everything. *)
      let tw,th = Sdl.get_surface_size surf in
      (* we need to make a slightly larger surface in order to have room for
         underline and cursor *)
      let box = Draw.create_surface ~like:surf
          (tw + cw + cw/2) (th + Theme.scale_int bottom_margin) in 
      go (Sdl.set_surface_blend_mode box Sdl.Blend.mode_none);

      (* draw text on the larger surface, at (0,0) (upper-left corner)
         preserving transparency information *)
      let rect = Draw.rect_translate (Sdl.get_clip_rect surf) (cw/2, 0) in
      go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_none);
      go (Sdl.blit_surface ~src:surf None ~dst:box (Some rect));

      (* draw selection background: this will erase the corresponding text... *)
      (match Var.get ti.selection with
       | Active (n1,n2) ->
         let x1 = cursor_xpos ~n:n1 ti in
         let x2 = cursor_xpos ~n:n2 ti in
         let sel_rect = Sdl.Rect.create ~x:x1 ~y:0 ~w:(x2-x1) ~h:th in
         let sel_rect_cw = Draw.rect_translate sel_rect (cw/2, 0) in
         Draw.fill_rect box (Some sel_rect_cw) Draw.(opaque sel_bg_color);
         (* now we reblit the text on the selection rectangle, this time with
            blending *)
         let sel =  draw_keys (font ti) keys ~fg:Draw.(opaque sel_fg_color) in
         (* TODO: draw only the relevent text, not everything. *)
         go (Sdl.set_surface_blend_mode sel Sdl.Blend.mode_blend);
         go (Sdl.blit_surface ~src:sel (Some sel_rect) ~dst:box (Some sel_rect_cw))
       | Start n1 ->
         let x1 = cursor_xpos ~n:n1 ti in
         let n2 = Var.get ti.cursor_pos in
         let x2 = cursor_xpos ~n:n2 ti in
         let sel_rect = Sdl.Rect.create ~x:(min x1 x2) ~y:0
             ~w:(abs (x2-x1)) ~h:th in
         let sel_rect_cw =  Draw.rect_translate sel_rect (cw/2, 0) in
         (* TODO regrouper avec ci-dessus ? *)
         Draw.fill_rect box (Some sel_rect_cw) Draw.(opaque grey);
         (* now we blend the text on the selection rectangle *)
         go (Sdl.set_surface_blend_mode surf Sdl.Blend.mode_blend);
         go (Sdl.blit_surface ~src:surf (Some sel_rect) ~dst:box (Some sel_rect_cw))
       | _ -> ());

      Draw.free_surface surf;
      if Var.get ti.active then begin
        (* draw underline *)
        let thick = Theme.scale_int 1 in
        let hline = Sdl.Rect.create ~x:(cw/2) ~y:(th (*+ bmargin - thick*)) ~w:tw ~h:thick in
        (* Sdl.fill_rect : If the color value contains an alpha
           component then the destination is simply filled with that
           alpha information, no blending takes place. *)
        Draw.fill_rect box (Some hline) Draw.(transp grey);

        (* move the offset to have the cursor in the visible area *)
        let cx = cursor_xpos ti in
        let offset = Var.get ti.offset in
        let offset = if cx <= offset+cw then max 0 (cx-cw)
          else if cx - offset >= g.Draw.w - cw - cw/2
          then min tw (cx - g.Draw.w + cw + cw/2)
          else offset in
        Var.set ti.offset offset
      end;
      (* we extract the visible part and save it as a texture, with all
         transparency info (no blending) *)
      (* note: if we don't clip to the visible part, it is easy to reach the max
         allowed texure width = 4096 *)
      let bw, bh = Sdl.get_surface_size box in
      let offset = Var.get ti.offset in
      let rect_b = Sdl.Rect.create ~x:offset ~y:0 ~w:(min g.Draw.w (bw - offset)) ~h:bh in
      let visible = Draw.create_surface ~like:box ~color:Draw.none (Sdl.Rect.w rect_b) bh in
      (* this surface (converted to texture) will be *blended* on the canvas *)
      go (Sdl.blit_surface ~src:box (Some rect_b) ~dst:visible None);
      let tex = Draw.create_texture_from_surface canvas.Draw.renderer visible in
      Draw.free_surface box;
      Draw.free_surface visible;
      Var.set ti.render (Some tex);
      printd debug_graphics "Time for creating texture = %f s" (Unix.gettimeofday () -.  start_time);
      tex
  in

  (* finally we copy onto the canvas *)
  let open Draw in
  let area = geom_to_rect g in
  Sdl.set_text_input_rect (Some area);
  Var.set ti.room_x g.x;
  let text_blit = copy_tex_to_layer ~overlay:(Draw.Xoffset 0) ~voffset:g.voffset
      canvas layer tex area (g.x + (Theme.scale_int left_margin))
      (g.y + (Theme.scale_int bottom_margin)) in
  (* we could instead have used a box surface of larger size, including margins,
     and use tex_to_layer instead of copy_tex_to_layer *)

  if is_active ti
  then   (* (re...)compute cursor position *)
    (* The cursor is an additional blit. We don't pre-blend the two textures
       (text+cursor) into a single blit, because the SDL current blend modes
       don't allow this... 
       http://www.adriancourreges.com/blog/2017/05/09/beware-of-transparent-pixels/
    *)
    let _,bh = tex_size tex in
    let voff = Theme.scale_int 4 in
    let cursor_g = { g with x = g.x + Theme.scale_int left_margin +
                                cursor_xpos ti - Var.get ti.offset;
                            y = g.y + bh - voff } in
    [text_blit; tex_to_layer canvas layer cursor cursor_g]
  else [text_blit];;
