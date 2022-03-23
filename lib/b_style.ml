(* This file is part of BOGUE *)
(* Style is used to describe background, border, shadow in an immutable way. Box
   uses this, Layout uses this in addition to a Box field for caching the
   computation.  *)

module Draw = B_draw
module Image = B_image
module Theme = B_theme
open B_utils

type line_style =
  | Solid
  | Dotted of (int * int)
  (* TODO implement "line_with_label" ----------like this----------- *)

type line = {
  color : Draw.color;
  width : int;
  style : line_style (* TODO implement this *)
}

(* TODO implement those fantastic old-style boxes with pattern lines ;) *)
type border = {
  up : line;
  down : line;
  left : line;
  right : line;
  radius : int option (* int should be enough since 0 means None *)
}

type gradient =
  { colors : Draw.color list; angle : float }

type background =
  | Image of Image.t (* pattern image *)
  | Solid of Draw.color
  | Gradient of gradient

type shadow = {
    size: int; (* should the shadow be larger than the box? *)
    offset: int * int;
    radius: int option; (* corner radius *)
    width: int (* the width of the gradient *)
  }

type t = {
  background : background option;
  border : border option;
  shadow : shadow option
}

let create ?background ?border ?shadow () =
  { background; border; shadow }

let of_bg background = create ~background ()

let of_border border = create ~border ()

let of_shadow shadow = create ~shadow ()

let empty = create ()

let with_bg background t =
  { t with background = Some background }

let with_shadow shadow t =
  { t with shadow = Some shadow }

let with_border border t =
  { t with border = Some border}

let without_bg t =
  { t with background = None}

let without_shadow t =
  { t with shadow = None }

let without_border t =
  { t with border = None }

let get_bg t =
  t.background

let get_border t =
  t.border

let get_shadow t =
  t.shadow

let unload t =
  do_option t.background @@ function
  | Image img -> Image.unload img
  | Solid _ -> ()
  | Gradient _ -> ()

let image_bg image =
  Image image

let color_bg color =
  Solid color

let opaque_bg rgb =
  color_bg (Draw.opaque rgb)

let theme_bg = opaque_bg (Draw.find_color Theme.bg_color)

let get_color = function
  | Solid c -> c
  | _ -> printd debug_error "Cannot get color from non Solid background";
    Draw.none

let gradient ?(angle = 0.) colors =
  Gradient { colors; angle }

let hgradient colors =
  Gradient { colors; angle = 180. }

let vgradient colors =
  Gradient { colors; angle = 0. }

let mk_line ?(color = Draw.(opaque black)) ?(width = 1)
    ?(style : line_style = Solid) () =
  { color; width; style }

let mk_radius radius =
  if radius = Some 0 then None else radius

let mk_border ?radius line =
  { up = line; down = line; left = line; right = line;
    radius = mk_radius radius }

let mk_shadow ?(offset = (1,3)) ?(size = 3) ?(width = 6) ?radius () : shadow =
  { size;
    offset;
    radius = mk_radius radius;
    width
  }
