open B_utils
module Theme = B_theme

type t = int * int * int

(* http://www.rapidtables.com/web/color/html-color-codes.htm *)
include Rgb_names

let grey = (100,100,100)
let pale_grey = (150,150,150)
let dark_grey = (75,75,75)

let custom =
  [ "grey", grey;
    "pale_grey", pale_grey;
    "dark_grey", dark_grey;
  ]

let table = [
  "aliceblue", aliceblue;
  "antiquewhite", antiquewhite;
  "aqua", aqua;
  "aquamarine", aquamarine;
  "azure", azure;
  "beige", beige;
  "bisque", bisque;
  "black", black;
  "blanchedalmond", blanchedalmond;
  "blue", blue;
  "blueviolet", blueviolet;
  "brown", brown;
  "burlywood", burlywood;
  "cadetblue", cadetblue;
  "chartreuse", chartreuse;
  "chocolate", chocolate;
  "coral", coral;
  "cornflowerblue", cornflowerblue;
  "cornsilk", cornsilk;
  "crimson", crimson;
  "cyan", cyan;
  "darkblue", darkblue;
  "darkcyan", darkcyan;
  "darkgray", darkgray;
  "darkgreen", darkgreen;
  "darkkhaki", darkkhaki;
  "darkmagenta", darkmagenta;
  "darkolivegreen", darkolivegreen;
  "darkorange", darkorange;
  "darkorchid", darkorchid;
  "darkred", darkred;
  "darksalmon", darksalmon;
  "darkseagreen", darkseagreen;
  "darkslateblue", darkslateblue;
  "darkslategray", darkslategray;
  "darkturquoise", darkturquoise;
  "darkviolet", darkviolet;
  "deeppink", deeppink;
  "deepskyblue", deepskyblue;
  "dimgray", dimgray;
  "dodgerblue", dodgerblue;
  "firebrick", firebrick;
  "floralwhite", floralwhite;
  "forestgreen", forestgreen;
  "fuchsia", fuchsia;
  "gainsboro", gainsboro;
  "ghostwhite", ghostwhite;
  "gold", gold;
  "goldenrod", goldenrod;
  "gray", gray;
  "green", green;
  "greenyellow", greenyellow;
  "honeydew", honeydew;
  "hotpink", hotpink;
  "indianred", indianred;
  "indigo", indigo;
  "ivory", ivory;
  "khaki", khaki;
  "lavender", lavender;
  "lavenderblush", lavenderblush;
  "lawngreen", lawngreen;
  "lemonchiffon", lemonchiffon;
  "lightblue", lightblue;
  "lightcoral", lightcoral;
  "lightcyan", lightcyan;
  "lightgoldenrodyellow", lightgoldenrodyellow;
  "lightgray", lightgray;
  "lightgreen", lightgreen;
  "lightpink", lightpink;
  "lightsalmon", lightsalmon;
  "lightseagreen", lightseagreen;
  "lightskyblue", lightskyblue;
  "lightslategray", lightslategray;
  "lightsteelblue", lightsteelblue;
  "lightyellow", lightyellow;
  "lime", lime;
  "limegreen", limegreen;
  "linen", linen;
  "magenta", magenta;
  "maroon", maroon;
  "mediumaquamarine", mediumaquamarine;
  "mediumblue", mediumblue;
  "mediumorchid", mediumorchid;
  "mediumpurple", mediumpurple;
  "mediumseagreen", mediumseagreen;
  "mediumslateblue", mediumslateblue;
  "mediumspringgreen", mediumspringgreen;
  "mediumturquoise", mediumturquoise;
  "mediumvioletred", mediumvioletred;
  "midnightblue", midnightblue;
  "mintcream", mintcream;
  "mistyrose", mistyrose;
  "moccasin", moccasin;
  "navajowhite", navajowhite;
  "navy", navy;
  "oldlace", oldlace;
  "olive", olive;
  "olivedrab", olivedrab;
  "orange", orange;
  "orangered", orangered;
  "orchid", orchid;
  "palegoldenrod", palegoldenrod;
  "palegreen", palegreen;
  "paleturquoise", paleturquoise;
  "palevioletred", palevioletred;
  "papayawhip", papayawhip;
  "peachpuff", peachpuff;
  "peru", peru;
  "pink", pink;
  "plum", plum;
  "powderblue", powderblue;
  "purple", purple;
  "red", red;
  "rosybrown", rosybrown;
  "royalblue", royalblue;
  "saddlebrown", saddlebrown;
  "salmon", salmon;
  "sandybrown", sandybrown;
  "seagreen", seagreen;
  "seashell", seashell;
  "sienna", sienna;
  "silver", silver;
  "skyblue", skyblue;
  "slateblue", slateblue;
  "slategray", slategray;
  "snow", snow;
  "springgreen", springgreen;
  "steelblue", steelblue;
  "tan", tan;
  "teal", teal;
  "thistle", thistle;
  "tomato", tomato;
  "turquoise", turquoise;
  "violet", violet;
  "wheat", wheat;
  "white", white;
  "whitesmoke", whitesmoke;
  "yellow", yellow;
  "yellowgreen", yellowgreen;
]

let colors = List.rev_append custom table

(* example c="#FFF" -> 0xFFF*)
let int_of_hex c =
  try
    Some (int_of_string ("0x" ^ (String.sub c 1 (String.length c - 1))))
  with
  | Failure _ -> (* int_of_string *)
    printd debug_error "Cannot extract hex integer from '0x%s'" c;
    None
  | e -> raise e

let of_int24 i =
  (i lsr 16) land 255, (i lsr 8) land 255, i land 255

let of_int12 i =
let r,g,b = (i lsr 8) land 15, (i lsr 4) land 15, i land 15 in
(r * 255 / 15), (g *255 / 15), (b * 255 / 15)

(* Convert a string of the form "grey" or "#FE01BC" or "#ABC" to a color code
   (r,g,b) *)
let find_color c =
  let l = String.length c in
  if l <> 0 && c.[0] = '#' then
    match l, int_of_hex c with
    | 7, Some i -> of_int24 i
    | 4, Some i -> of_int12 i
    | _ ->
      printd debug_error "Cannot extract color code from '%s'" c;
      grey
  else
    try List.assoc c colors
    with
    | Not_found ->
      printd debug_error "Color '%s' unknown" c;
      grey

let bg_color = find_color Theme.bg_color
let box_bg_color = find_color Theme.box_bg_color
let cursor_color = find_color Theme.cursor_color
let disabled_bg_color = find_color Theme.disabled_bg
let disabled_fg_color = find_color Theme.disabled_fg
let faint_color = find_color Theme.faint_color
let text_color = ref (find_color Theme.text_color)
let sel_bg_color = find_color Theme.sel_bg_color
let sel_fg_color = find_color Theme.sel_fg_color
let label_color = find_color Theme.label_color
let menu_hl_color = find_color Theme.menu_hl_color
let menu_bg_color = find_color Theme.menu_bg_color

(* TODO put in VAR: *)
let set_text_color c =
  text_color := c

(* non linear increase of color *)
(* f(x) = a - exp(-bx), f(0)=0.1, f(1)=1  => a = 1.1, b = - ln 0.1 = 2.3... *)
let incr_color x = min 255 (round (255. *. (1.1 -. exp (-.2.3 *. float x /. 255.))))

let pale (r,g,b) = (incr_color r, incr_color g, incr_color b)
