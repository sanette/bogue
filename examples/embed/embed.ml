(* testing the "one_step" mode *)

(* This file is part of Bogue documentation. *)
(* http://sanette.github.io/bogue/Principles.html *)

(* press TAB to show GUI *)

open Tsdl
open Bogue
open Utils
module W = Widget
module L = Layout

type tmprect = { rect : Sdl.rect;
                 color : int * int * int;
                 created : int } 

let duration = 1000 (* lifetime of a rectangle *)
let width = 400 (* width of window in Bogue units. *)
let height = 400 (* height of window in Bogue units. *)
let size = 50 (* max size of rectangle in Bogue units. *)
let n = 60 (* number of rectangles *)
let margin = 10
           
let bg = (180,180,180,255)

let get_alpha r =
  let t = Time.now () - r.created in
  if t > duration then 0
  else
    let _ = print_int t in
    let _ = print_newline() in
    let a = 255. *. (sin (3.14159 *. (float t) /. (float duration)))
            |> round in
    imax a 0

let new_rect () =
  let size, width, height =
    Theme.(scale_int size, scale_int width, scale_int height) in
  let w = Random.int size in
  let h = Random.int size in
  let x = Random.int (width - w) in
  let y = Random.int (height - h) in
  let color = Random.(int 185, int 185, int 185) in
  let rect = Sdl.Rect.create ~x ~y ~w ~h in
  let created = Time.now () in
  { rect; color; created }

let draw_rect renderer tr =
  let alpha = get_alpha tr in
  let r,g,b = tr.color in
  Draw.set_color renderer (r,g,b,alpha);
  go (Sdl.render_fill_rect renderer (Some tr.rect))

let make_board () =
  let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in
  let label = W.label ~size:40 "Hello !" in
  let background = L.color_bg (Draw.(transp (find_color "olivedrab"))) in
  let w = width - 2*margin in
  let layout = L.tower ~margins:margin [L.resident ~background ~w input;
                                        L.resident ~w ~h:200 label] in

  let before_display () =
    let text = W.get_text input in
    W.set_text label ("Hello " ^ text ^ "!") in

  Bogue.make [] [layout], before_display
  
let main () =
  Sys.catch_break true;
  go(Sdl.init Sdl.Init.video);
  let w,h = Theme.(scale_int width, scale_int height) in
  let win = go(Sdl.create_window ~w ~h
                 "Test Window" Sdl.Window.windowed) in
  let renderer = go(Sdl.create_renderer win) in
  (* very important: set blend mode: *)
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  Draw.set_color renderer bg;
  go(Sdl.render_clear renderer);
  Random.self_init ();

  let rec rect_loop i new_list = function
    | [] -> if i < n
            then let r = new_rect () in
                 draw_rect renderer r;
                 List.rev (r::new_list)
            else List.rev new_list
    | r :: rest ->
       let r' = 
         let a = get_alpha r in
         if a <> 0 then r else new_rect () in
       draw_rect renderer r';
       rect_loop (i+1) (r'::new_list) rest
  in

  let show_gui = ref false in
  let board, before_display = make_board () in
  Bogue.make_sdl_windows ~windows:[win] board;
  let start_fps, fps = Time.adaptive_fps 60 in

  let rec mainloop e list =
    if not !show_gui && Sdl.poll_event (Some e)
    then begin
        match Trigger.event_kind e with
        | `Key_up when Sdl.Event.(get e keyboard_keycode) = Sdl.K.tab ->
           show_gui := not !show_gui
        | `Key_up when Sdl.Event.(get e keyboard_keycode) = Sdl.K.escape ->
           raise Sys.Break
        | _ -> ()
      end;
    Draw.set_color renderer bg;
    go(Sdl.render_clear renderer);
    let new_list = rect_loop 0 [] list in

    if !show_gui
    then begin
        (*List.iter Window.to_refresh board.Bogue.windows;*)
        Bogue.refresh_custom_windows board;
        try if not (Bogue.one_step ~before_display true (start_fps, fps) board)
                   (* one_step returns true if fps was executed *)
            then fps () with
        | Bogue.Exit -> show_gui := false
        | e -> raise e
      end
    else fps ();
    Sdl.render_present renderer;
    mainloop e new_list in

  let e = Sdl.Event.create () in
  start_fps ();
  let () = try mainloop e [] with
           | Sys.Break -> print_endline "Stop"
           | e -> raise e in

  Sdl.destroy_window win;
  Draw.quit ()

let () =
  print_endline "Press TAB to show GUI. Then press ESC to quit GUI.";
  main ()
