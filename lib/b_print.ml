open Printf
module L = B_layout
module Widget = B_widget
module Utils = B_utils
module Chain = B_chain
module Draw = B_draw
module Label = B_label
module Style = B_style

let kind = Widget.string_of_kind

let widget w =
  let open Widget in
  sprintf "wid: #%u, kind: %s" w.wid (kind w.kind)

let color (r,g,b,a) =
  sprintf "(r=%u, g=%u, b=%u, a=%u)" r g b a

let geometry g =
  let open L in
  sprintf "(x=%d, y=%d, w=%d, h=%d, vo=%d)"  g.x g.y g.w g.h g.voffset

let content c =
  match c with
  | L.Rooms list -> sprintf "Rooms (%u)" (List.length list) (* do not call recursively all rooms, because we have a circular type *)
  | L.Resident w -> sprintf "Resident (%s)" (widget w)

let house h =
  sprintf "%s" (L.sprint_id h)

let canvas c =
  sprintf "Window id=%u" (Draw.window_id c)

let option f o =
  match o with
  | Some x -> "Some " ^ (f x)
  | None -> "None"

let bool b =
  sprintf "%b" b

let anim _ =
  "Anim";; (* TODO*)

let style_bg _ = "" (* TODO *)
let style_border _ = "" (* TODO *)
let style_shadow _ = "" (* TODO *)

let style s =
  let open Style in
  sprintf "Style {background=%s; border=%s; shadow=%s}"
    (style_bg s.background) (style_border s.border) (style_shadow s.shadow)

let background = function
  | L.Style s -> style s
  | L.Box _ -> "Box"

let layer_depth l =
  if Chain.is_empty l then "None"
  else string_of_int (Chain.depth l)

let rec layout_up ?(indent = "") r =
let list = [
  sprintf "\n%s┏━ Layout %s\n" indent (L.sprint_id r);
  sprintf "%s┃ layer: %s\n" indent (layer_depth r.L.layer);
  sprintf "%s┃ current geometry: %s\n" indent (geometry r.L.current_geom);
  sprintf "%s┃ background: %s\n" indent (option background r.L.background);
  sprintf "%s┃ content: %s\n" indent (content r.L.content);
  sprintf "%s┃ canvas: %s\n" indent (option canvas r.L.canvas);
  sprintf "%s┃ has anim: %s\n" indent (bool (L.has_anim r));
  sprintf "%s┃ clip: %s\n" indent (bool (r.L.clip));
  sprintf "%s┃ keyboard_focus: %s\n" indent (option bool r.L.keyboard_focus);
  sprintf "%s┃ house: %s\n" indent (option (layout_up ~indent:(indent^"┃  ")) r.L.house);
  sprintf "%s┗━" indent ] in
  String.concat "" list

(* Layout to string. We print less if debug=true *)
let rec layout_down ?(indent = "") r =
  let buf = Buffer.create 256 in
  let add s = ksprintf (Buffer.add_string buf) s in
  let addd s = ksprintf
      (fun s -> if !Utils.debug then Buffer.add_string buf s) s in
  add  "\n%s┏━ Layout %s\n" indent (L.sprint_id r);
  addd   "%s┃ layer: %d\n" indent (Chain.depth (r.L.layer));
  add    "%s┃ current geometry: %s\n" indent (geometry r.L.current_geom);
  addd   "%s┃ background: %s\n" indent (option background r.L.background);
  addd   "%s┃ canvas: %s\n" indent (option canvas r.L.canvas);
  addd   "%s┃ anim: %s\n" indent (bool (L.has_anim r));
  addd   "%s┃ clip: %s\n" indent (bool (r.L.clip));
  addd   "%s┃ keyboard_focus: %s\n" indent (option bool r.L.keyboard_focus);
  add    "%s┃ house: %s\n" indent (option house r.L.house);
  add    "%s┃ content: %s\n" indent (full_content ~indent:(indent^"┃  ") r.L.content);
  add    "%s┗━" indent;
  Buffer.contents buf

and full_content ?(indent = "") = function
  | L.Resident w -> sprintf "Resident (%s)" (widget w)
  | L.Rooms list -> "Rooms:" ^ (String.concat "," (List.map (layout_down ~indent) list))

(* Toplevel printers: #install_printer pp_widget;; #install_printer pp_layout;; *)

let pp_widget f w = Format.pp_print_string f (widget w)
let pp_layout f x = Format.pp_print_string f (layout_down x)


(* Print error messages *)

let dump r =
  let file, ch = Filename.open_temp_file "bogue" ".dump" in
  print_endline ("Saving dump to " ^ file);
  output_string ch (layout_down r);
  close_out ch

let layout_error_kf room msg =
  Printf.kfprintf (fun ch -> flush ch) stderr msg;
  dump (L.top_house room)

let layout_error room msg =
  output_string stderr msg;
  dump (L.top_house room)
