(* This file is part of BOGUE, by San Vu Ngoc *)

(* This module is still experimental *)

(* We create a Box widget whose image will be a snapshot of a given room. *)
(* The texture is initialized at the startup event, or upon user call. *)

(* In order to achieve this, we hijack the Layout.display function, which uses
   blits in layers, and render the layers to a target texture. Hence the
   assumption is that the layers blits are empty. This means that creating
   snapshots should NOT be done in a separate thread: the risk is to corrupt
   layers in case it is called at the same time as the Bogue.render function. *)

(* Just in case, we have added a mutex to the current_layer. but still... *)

(* When we create the widget, the size of the box may not be the same as the
   size of the widget at the time it receives the startup event... *)

(* TODO: we have trouble rendering correctly when the room background has alpha
   channel... ? use the new https://wiki.libsdl.org/SDL_ComposeCustomBlendMode??
   *)

(* Warning: a new texture is created at every update, because size might
   change. TODO add an option in case we want to be fast and reuse the same
   texture? *)
open Tsdl
open B_utils
module Layout = B_layout
module Widget = B_widget
module Var = B_var
module Trigger =  B_trigger
module Draw = B_draw
module Box = B_box
module Style = B_style
module Sync = B_sync

let update widget room =
  let renderer = Layout.renderer room in
  let w,h = Layout.get_physical_size room in
  let x,y = Layout.(getx room, gety room) in
  let target = Draw.create_target renderer w h in
  printd debug_graphics "Rendering snapshot...";
  Var.protect Draw.current_layer;
  (* Now we assume that the blits are empty... TODO check this and issue an
     error otherwise *)
  Layout.display ~pos0:(-x,-y) room;
  let save = Draw.push_target renderer target in
  Draw.render_all_layers (Layout.get_layer room);
  Var.release Draw.current_layer;
  Draw.pop_target renderer save;
  printd debug_graphics "...rendering snapshot done.";

  (* essai blur *)
 (* let target = Draw.blur_texture renderer target 16 in *)

  let box = Widget.get_box widget in
  do_option (Var.get box.Box.render) Draw.forget_texture;
  Var.set box.Box.render (Some target)

let create ?border room =
  let w,h = Layout.get_size room in
  let style = Style.create ?border () in
  let box = Widget.box ~w ~h ~style () in
  (* let c = Widget.connect_main box box (fun w _ _ -> update w room) *)
  (*           [Trigger.startup] in *)
  (* Widget.add_connection box c; *)
  Sync.push (fun () -> update box room);
  box

(* should be called after graphics init *)
let surface room =
  let w,h = Layout.get_size room in
  let box = Widget.box ~w ~h () in
  update box room;
  let box = Widget.get_box box in
  let renderer = Layout.renderer room in
  match Var.get box.Box.render with
  | None -> printd (debug_graphics+debug_error) "Cannot create surface because Box has no texture (yet)."; None
  | Some tex -> Some (Draw.surface_from_texture renderer tex)

(* should be called after graphics init *)
let to_cursor ?hot_x ?hot_y room =
  map_option (surface room) (fun surf ->
      let w,h = Sdl.get_surface_size surf in
      let hot_x = default hot_x (w/2) in
      let hot_y = default hot_y (h/2) in
      go (Sdl.create_color_cursor surf ~hot_x ~hot_y))
