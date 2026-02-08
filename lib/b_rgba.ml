open B_utils
module RGB = B_rgb
module Theme = B_theme

type t = int * int * int * int

include Rgba_names

(* alpha=0 means totally transparent, alpha=255 means totally opaque *)
let add_alpha alpha (r,g,b) : t =
  (r,g,b,alpha)

let set_alpha alpha (r,g,b,_) : t =
  (r,g,b,alpha)

let opaque = add_alpha 255
let transp = add_alpha 127

let grey = opaque RGB.grey
let pale_grey = opaque RGB.pale_grey
let dark_grey = opaque RGB.dark_grey

let none = (0,0,0,0)

let of_int32 i =
  (i lsr 24) land 255, (i lsr 16) land 255, (i lsr 8) land 255, i land 255

let of_int16 i =
  let a = i land 15 in
  let r,g,b = RGB.of_int12 (i lsr 4) in
  r,g,b, (a * 255 / 15)

(* Convert a string of the form "grey" or "#FE01BC" or "#AABBCCDD" or "#ABC"
   or "#ABCD" to a color code (r,g,b,a) *)
let find_color c =
  let l = String.length c in
  if l <> 0 && c.[0] = '#' then
    match l, RGB.int_of_hex c with
    | 9, Some i -> of_int32 i
    | 5, Some i -> of_int16 i
    | 7, Some i -> opaque (RGB.of_int24 i)
    | 4, Some i -> opaque (RGB.of_int12 i)
    | _ ->
      printd debug_error "Cannot extract color code from '%s'" c;
      grey
  else
    try List.assoc c RGB.colors |> opaque
    with
    | Not_found ->
      printd debug_error "Color '%s' unknown" c;
      grey

let scrollbar_color = add_alpha 20 RGB.blue
let bg_color = opaque RGB.bg_color
let box_bg_color = opaque RGB.box_bg_color
let cursor_color = opaque RGB.cursor_color
let disabled_bg_color = opaque RGB.disabled_bg_color
let disabled_fg_color = opaque RGB.disabled_fg_color
let faint_color = opaque RGB.faint_color
let text_color = ref (opaque !RGB.text_color)
let sel_bg_color = opaque RGB.sel_bg_color
let sel_fg_color = opaque RGB.sel_fg_color
let label_color = opaque RGB.label_color
let menu_hl_color = opaque RGB.menu_hl_color
let menu_bg_color = opaque RGB.menu_bg_color

(* TODO put in VAR: *)
let set_text_color c =
  text_color := c

let random_color () : t =
let r () = Random.int 256 in
(r(), r(), r(), r())

let darker (r,g,b,a) : t =
(3*r/4, 3*g/4, 3*b/4, a)

let component_lighter x =
  min 255 ((4*x)/3 + 80)

let lighter (r,g,b,a) : t =
(component_lighter r, component_lighter g, component_lighter b, a)

let median (r1,g1,b1,a1) (r2,g2,b2,a2) : t =
(r1+r2)/2, (g1+g2)/2, (b1+b2)/2, (a1+a2)/2
