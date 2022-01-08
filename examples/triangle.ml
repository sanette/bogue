(* draw filled equilateral triangles using the Renderer API *)
(* of course it's much easier to use OPENGL directly! *)

open Tsdl;;

let pi = 4. *. atan 1.

let rad angle =
  pi *. angle /. 180.

let round x =
  int_of_float (Float.round x)

let go : 'a Tsdl.Sdl.result -> 'a = function
  | Error _ -> failwith ("SDL ERROR: " ^ (Sdl.get_error ()))
  | Ok r -> r

(* this will draw a filled equilateral triangle of given side, angle and
   color. One vertex of the triangle is at the center of the texture. Another
   vertex is at the "angle" direction.  *)
let triangle renderer side angle color =
  let flip = Sdl.Flip.none in

  (* size of the rectangle containing the horizontal triangle *)
  let w = side in
  let h = round (float side *. sqrt 3. /. 2.) in

  (* create a mask texture for cutting operations *)
  let format = Sdl.Pixel.format_argb8888 in
  let access = Sdl.Texture.access_target in
  let mask = go (Sdl.create_texture renderer format access ~w ~h) in
  go (Sdl.set_texture_blend_mode mask Sdl.Blend.mode_none);

  (* create a large target texture that will contain the rotated triangle with
     one vertex at the center of the texture. The size could be made smaller
     depending on angle, of course. *)
  let target = go (Sdl.create_texture renderer format access ~w:(2*side) ~h:(2*side)) in
  go (Sdl.set_texture_blend_mode target Sdl.Blend.mode_blend);

  (* draw rotated rectangle that will contain the triangle: *)
  (* 1. fill the mask with color *)
  go (Sdl.set_render_target renderer (Some mask));
  let r,g,b,a = color in
  go (Sdl.set_render_draw_color renderer r g b a);
  go (Sdl.render_clear renderer);

  (* 2. clear the texture with 0 *)
  go (Sdl.set_render_target renderer (Some target));
  go (Sdl.set_render_draw_color renderer 0 0 0 0);
  go (Sdl.render_clear renderer);

  (* 3. rotate and copy the mask onto the target texture *)
  let dst = Sdl.Rect.create ~x:side ~y:side ~w ~h in
  let center = Sdl.Point.create ~x:0 ~y:0 in
  go(Sdl.render_copy_ex renderer ~dst mask angle (Some center) flip);

  (* clear the mask with 0 *)
  go (Sdl.set_render_target renderer (Some mask));
  go (Sdl.set_render_draw_color renderer 0 0 0 0);
  go (Sdl.render_clear renderer);

  (* cut top-left corner *)
  go (Sdl.set_render_target renderer (Some target));
  go(Sdl.render_copy_ex renderer ~dst mask (angle +. 60.) (Some center) flip);

  (* cut the top-right corner *)
  let x = round (float side *. cos (rad angle)) in
  let y = round (float side *. sin (rad angle)) in
  let dst = Sdl.Rect.create ~x:(side+x-w) ~y:(side+y) ~w ~h in
  let center = Sdl.Point.create ~x:w ~y:0 in
  go(Sdl.render_copy_ex renderer ~dst mask (angle -. 60.) (Some center) flip);

  (* we don't need the mask anymore *)
  Sdl.destroy_texture mask;

  (* reset rendering target to default *)
  go (Sdl.set_render_target renderer None);

  (* we return the target texture *)
  target



let main () =
  let w = 800 and h = 800 in
  go (Sdl.init Sdl.Init.video);
  let win = go (Sdl.create_window ~w ~h "Test Window" Sdl.Window.windowed) in
  let renderer = go (Sdl.create_renderer ~flags:Sdl.Renderer.targettexture win) in

  Random.self_init ();
  let max_side = min w h / 5 in

  (* we draw random triangles *)
  for j = 0 to 1000 do
    go(Sdl.render_clear renderer);
    for i = 0 to 50 do
      let x = Random.int w in
      let y = Random.int h in
      let side = Random.int max_side + 1 in
      let angle = Random.float 360. in
      let color = Random.(int 255, int 255, int 255, int 255) in
      let trg = triangle renderer side angle color in
      let _,_,(w,h) = go (Sdl.query_texture trg) in
      let dst = Sdl.Rect.create ~x ~y ~w ~h in
      go(Sdl.render_copy ~dst renderer trg);
      Sdl.destroy_texture trg;
    done;

    (* display the result and pause *)
    Sdl.render_present renderer;
    Sdl.delay 16l;
  done;

  Sdl.delay 10000l;

  (* close *)
  Sdl.destroy_window win;
  Sdl.quit ()


let () = main ()
