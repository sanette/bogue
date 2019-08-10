module Draw = B_draw
module Image = B_image
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
  radius : int option
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
let color_bg color =
  Solid color

let get_color = function
  | Solid c -> c
  | _ -> printd debug_error "Cannot get color from non Solid background";
    Draw.none;;

let gradient ?(angle = 0.) colors =
  Gradient { colors; angle }

let hgradient colors =
  Gradient { colors; angle = 180. }

let vgradient colors =
  Gradient { colors; angle = 0. }

let line ?(color = Draw.(opaque black)) ?(width = 1)
    ?(style : line_style = Solid) () =
  { color; width; style };;

let border ?radius line =
  { up=line; down=line; left=line; right=line; radius };;

let shadow ?(offset = (1,3)) ?(size = 3) ?(width = 6) ?radius () : shadow =
  { size;
    offset;
    radius;
    width
  }
