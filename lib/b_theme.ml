(* b_theme.ml : theme variables *)
(* This file is part of BOGUE. San Vu Ngoc, 2019 *)
open B_utils
module Utf8 = B_utf8
  
(*
   
Format of the $HOME/.config/bogue/bogue.conf file ($HOME/.bogue.conf is also ok):
## BOGUE version 0.1
    
# This is a comment
# The version string is compulsory
# it must not contain space
    
# syntax VAR = value
# the spaces around "=" are required
    
SCALE = 2.15
DIR = /home/john/.config/bogue/themes
    
# (note: there are no quotes around string values)
    
*)

let this_version = "20200630"  (* see VERSION file *)
  
let default_vars = [
  (* Debug: *)
  "DEBUG", "false";
  (* The main themes dir: usually $HOME/.config/bogue/themes *)
  "DIR", "";
  (* The chosen theme: *)
  "THEME", "default"; (* It must be a subdirectory of DIR: *)
  (* The window background image: *)
  "BACKGROUND", "file:background.png"; (* if is starts with "/" it is an absolute path. Otherwise, it is a file path inside THEME. *)
  (* This background color should be clearly visible over the BACKGROUND *)
  "BG_COLOR", "lightsteelblue";
  (* Color for active or inactive button *)
  "BUTTON_COLOR_ON", "darkturquoise";
  "BUTTON_COLOR_OFF", "steelblue";
  (* The "checked" image: either image or fa icon, eg. "fa:check-square-o" *)
  "CHECK_ON", "check_on.png"; 
  (* The "unchecked" image: (eg: "fa:square-o") *)
  "CHECK_OFF", "check_off.png";
  (* The cursor color for text input: *)
  "CURSOR_COLOR", "#2a7da2"; (* a color identifier. Either a name like "black" or a RGB code as "#FE01BC" *)
  (* Color for unimportant things that should not be so visible *)
  "FAINT_COLOR", "gainsboro"; (* idem *)
  (* The color for standard text display: *)
  "TEXT_COLOR", "black"; (* idem *)
  (* Text section background and foreground colors: *)
  "SEL_BG_COLOR", "slategray";
  "SEL_FG_COLOR", "white";
  (* The color for text labels: *)
  "LABEL_COLOR", "black"; (* idem *)
  (* The color for highlighting selected menu entries: *)
  "MENU_HL_COLOR", "#8099a2"; (* idem *)
  (* The background color for unselected menu entries: *)
  "MENU_BG_COLOR", "#BEBEBE"; (* idem *)
  (* The font size for text labels: *)
  "LABEL_FONT_SIZE", "14";
  (* The font for text labels: *)
  "LABEL_FONT", "Ubuntu-R.ttf"; (* path for a TTF font *)
  (* The font for standard text: *)
  "TEXT_FONT", "Ubuntu-R.ttf"; (* idem *)
  (* The font size for standard text: *)
  "TEXT_FONT_SIZE", "14";
   (* The font size for small text like tooltips: *)
  "SMALL_FONT_SIZE", "10";
  (* Monospaced font: *)
  "MONO_FONT", "UbuntuMono-R.ttf";
  (* Room margin: *)
  "ROOM_MARGIN", "10";
  (* Font awesome dir, in the theme/common directory *)
  "FA_DIR", "font-awesome-4.6.3";
  (* The global scale to be applied to all graphical elements. Setting
     SCALE=2. is useful for HIDPI screens. Here, the scale should be transparent
     to the user, and be applied only at the last moment, when dealing directly
     with rendering functions, or when creating blits. It might be a good idea
     to have a different scale per window, in case of multiple monitors. SCALE=0
     will try to autodetect. *)
  "SCALE", "0"; ];;


let id x = x;;

let sub_file = Filename.concat

(* some global environment variables *)
let home = Sys.getenv "HOME";;

let conf = try Sys.getenv "XDG_CONFIG_HOME" with
  | Not_found -> sub_file home ".config"
  | e -> raise e;;

let skip_comment buffer =
  let rec loop () =
    let (* comment *) _ = Scanf.bscanf buffer " #%s@\n" id in
    (* printd debug_io "Comment ignored: \"%s\"" comment; *)
    loop () in
  try loop () with
  | End_of_file
  | Scanf.Scan_failure _ -> ()
  | e -> print_endline "SCAN ERROR"; raise e;;

(* Load variables from config file. Returns an association list. Most recent
   entries are put first, and hence will be selected first by List.assoc.*)
let load_vars config_file =
  let buffer = Scanf.Scanning.from_file config_file in
  let version = try
      Scanf.bscanf buffer "## BOGUE version %s " id
    with e -> raise e in
  printd debug_io "Reading config file [%s]. BOGUE Version [%s]"
    config_file version;
  let rec loop list =
    skip_comment buffer;
    try
      let varname, value = Scanf.bscanf buffer " %s = %s" (fun a b -> a,b) in
      printd debug_io "Reading %s=%s" varname value;
      if varname = "DEBUG"
      then debug := (String.lowercase_ascii value = "true");
      loop ((varname, value) :: list)
    with
    | End_of_file -> Scanf.Scanning.close_in buffer; list
    | e -> raise e
  in
  loop [];;


(* TODO move this in an "init" function (and hence theme vars must be mutable)
   *)
let user_vars =
  let config_file = let r = sub_file home ".bogue.conf" in
    if Sys.file_exists r then r
    else  
      let d = sub_file conf "bogue/bogue.conf" in
      if Sys.file_exists d then d
      else "" in
  try load_vars config_file with
  | _ (* e *) -> printd debug_error "Error loading config file %s. Using defaults" config_file;
    default_vars;; (*raise e;;*)

let user_vars = ref user_vars;;

(* Checks the (first) THEME entry in user's vars, and then loads & inserts the
   theme variables *)
let load_theme_vars dir vars =
  let rec loop newv = function
    | [] -> printd debug_io "No theme specified"; newv 
    | (name, value)::rest ->
       if name = "THEME"
       then
         let theme_file = sub_file value "bogue.conf" in
         let theme_vars = load_vars (sub_file dir theme_file) in
         (name, value) :: (List.rev_append newv (List.append theme_vars rest))
       else loop ((name, value)::newv) rest
  in
  loop [] vars;;
    
let get_var s =
  try let v = List.assoc s !user_vars in
      printd debug_warning "Using %s=%s" s v;
      v
  with
  | Not_found -> begin
      try let v = List.assoc s default_vars in
          printd debug_warning "User variable '%s' not found in config. Using default '%s'" s v; v
      with
      | Not_found ->
         printd debug_error "Variable '%s' not found. Prepare for a crash." s;
         ""
      | e -> raise e;
    end
  | e -> raise e;;
 
let get_int ?(default = 0) s =
  let v = get_var s in
  try int_of_string v with
  | Failure _ -> (* "int_of_string" *)
    printd debug_error "Expected an integer for '%s', got '%s' instead. Using default=%d." s v default;
    default
  | e -> raise e;;

let get_float ?(default = 0.) s =
  let v = get_var s in
  try float_of_string v with
  | Failure _ -> (* "float_of_string" *)
    printd debug_error "Expected a float for '%s', got '%s' instead. Using default=%f." s v default;
    default
  | e -> raise e;;
  
let get_bool s =
  let b = get_var s in
  String.lowercase_ascii b = "true" || b = "1";;

debug := get_bool "DEBUG";;

(* we try to locate the theme dir *)
(* by default it is in .config/bogue/themes. For a first run, it probably
   doesn't exist, so we copy it from the system lib dir *)
let dir =
  let dir = get_var "DIR" in
  if Sys.file_exists dir && Sys.is_directory dir
  then dir
    (* TODO maintenant aller chercher dans  `opam config var prefix`/bogue/themes *)
  else let config = sub_file conf "bogue/themes" in
    if Sys.file_exists config && Sys.is_directory config
    then if dir = ""
      then begin
        printd debug_warning "Using %s as bogue themes directory" config;
        config
      end else begin
        printd debug_error "Bogue themes directory %s does not exist. Using %s instead" dir config;
        config
      end
    else try
        (* TODO use `opam config var prefix` instead of ocamlfind to remove
           ocamlfind dependency. *)
        let system = Unix.open_process_in "ocamlfind query bogue" in
        let res = input_line system in
        match Unix.close_process_in system with
        | Unix.WEXITED 0 ->
          let sp = Printf.sprintf in
          let config = sub_file conf "bogue" in begin
            let tar = sp "%s/../../share/bogue/themes.tgz" res in
            printd debug_io "tar -C %s -x -v -f %s" config tar;
            if Sys.command (sp "mkdir -p %s" config) = 0
            && Sys.command (sp "cp -r %s %s" tar config) = 0
            && Sys.command (sp "tar -C %s -x -v -f %s" config tar) = 0
            then (Sys.remove tar; sub_file config "themes")
            else begin
              printd debug_error "(FATAL) Cannot create user configuration file.";
              raise Not_found
            end
          end
        | _ -> printd debug_error "(FATAL) Cannot find a usable bogue configuration directory";
          raise Not_found
      with
      | End_of_file ->
        printd debug_error "(FATAL) Bogue configuration directory %s does not exist, and system-wide config cannot be found." dir;
        raise Not_found
      | e -> raise e;;

(* Add variables from theme config file (if specified in the user config file)
   *)
user_vars := load_theme_vars dir !user_vars;;
let current = sub_file dir (get_var "THEME");;
print_endline (Printf.sprintf
                 "Loading BOGUE %s with config dir %s " this_version current);;
let common = sub_file dir "common";;
let fonts_dir = sub_file common "fonts";;

let default_dpi = 110;;

(* try to obtain the monitor's DPI on linux systems. Does not work with multiple
   monitors *)
let get_dpi () =
  try
    let proc = Unix.open_process_in
        "xdpyinfo | grep resolution | awk '{print $2}'" in
    let res = input_line proc in
    match Unix.close_process_in proc with
    | Unix.WEXITED 0 ->
      let i = String.index res 'x' in
      let dpi =int_of_string (String.sub res 0 i) in
      printd debug_graphics "Detected DPI=%u" dpi;
      Some dpi
    | _ -> printd debug_warning
             "Cannot get monitor's DPI from [%s]. Using 110." res;
      None
  with
  | _ -> printd debug_warning
           "Cannot get monitor's DPI from xdpyinfo. Using 110.";
    None;;

(* A file starting with "/" is considered a global path, otherwise it will be
   searched in the current theme directory. *)
let get_path file =
  if file = "" then failwith "Filename empty";
  if file.[0] = '/' then file
  else sub_file current file;;

let get_fa_or_path s =
  if startswith s "fa:" then s else get_path s;;

(* Font names not starting with "/" are searched first in the theme directory,
   then in bogue's common fonts_dir, then in the system's fonts. *)
let get_font_path name =
  if name = "" then failwith "Empty fontname";
  if name.[0] = '/' then name
  else let check_file file fail =
         if Sys.file_exists file then file else fail () in
    (* stupid construction, I know hehe) *)
    check_file name (fun () ->
        check_file (sub_file fonts_dir name) (fun () ->
            let fclist = Unix.open_process_in
                (Printf.sprintf "fc-list : file | grep %s" name) in
            let res = input_line fclist in
            match Unix.close_process_in fclist with
            | Unix.WEXITED 0 ->
              String.sub res 0 (String.rindex res ':')
            | _ -> printd debug_error "Cannot find font %s" name; name
          )
      )
      
let background = get_var "BACKGROUND";;
let bg_color = get_var "BG_COLOR";;
let button_color_off = get_var "BUTTON_COLOR_OFF";;
let button_color_on = get_var "BUTTON_COLOR_ON";;
let check_on = get_fa_or_path (get_var "CHECK_ON");;
let check_off = get_fa_or_path (get_var "CHECK_OFF");;
let cursor_color = get_var "CURSOR_COLOR";;
let faint_color = get_var "FAINT_COLOR";;
let text_color = get_var "TEXT_COLOR";;
let sel_bg_color = get_var "SEL_BG_COLOR";;
let sel_fg_color = get_var "SEL_FG_COLOR";;
let label_color = get_var "LABEL_COLOR";;
let menu_hl_color = get_var "MENU_HL_COLOR";;
let menu_bg_color = get_var "MENU_BG_COLOR";;
let label_font_size = get_int ~default:14 "LABEL_FONT_SIZE";;
let label_font = get_font_path (get_var "LABEL_FONT");;
let text_font = get_font_path (get_var "TEXT_FONT");;
let text_font_size = get_int ~default:14 "TEXT_FONT_SIZE";;
let small_font_size = get_int ~default:10 "SMALL_FONT_SIZE";;
let mono_font = get_font_path (get_var "MONO_FONT");;
let room_margin = get_int ~default:10 "ROOM_MARGIN";;
let fa_dir = sub_file common (get_var "FA_DIR");;
let fa_font = sub_file fa_dir "fonts/fontawesome-webfont.ttf";;
let scale = let s = get_float ~default:0. "SCALE" in
  if s > 0. then s
  else (* choose a reasonable scale. Probably not OK in case of multiple monitors. *)
    let dpi = default (get_dpi ()) default_dpi in
    let s = if dpi <= 110 then 1. else (float dpi /. 110.) in
    printd debug_warning "Using SCALE=%f" s;
    s;;


let fa_font_size = 18;;
                        
(** some standard (?) UTF8 symbols *)
let symbols = [
"check_empty", "\239\130\150";
"check", "\239\129\134";
];;

let scale_int i =
  round (scale *. float i);;

let unscale_int i =
  round (float i /. scale);;

let unscale_f x =
  x /. scale;;

let scale_from_float x =
  round (scale *. x)

let unscale_to_float i =
  scale *. (float i)
  
(** font awesome variables *)
let load_fa_variables () =
  let file = sub_file fa_dir "less/variables.less" in
  let buffer = Scanf.Scanning.from_file file in
  let rec loop list =
    try 
      let key =  Scanf.bscanf buffer "@fa-var-%s@: " (fun x -> x) in
      let value = Scanf.bscanf buffer "\"\\%s@\"" (fun x -> x) in
      let int_value = int_of_string ("0x" ^ value) in
      let ucode = Bytes.to_string (Utf8.uencode int_value) in
      loop ((key, ucode) :: list)
    with 
    | End_of_file -> 
      Scanf.Scanning.close_in buffer;
      list
    | Scanf.Scan_failure _ -> 
      ignore (Scanf.bscanf buffer "%s@\n" (fun x -> x));
      loop list
    | e -> raise e
  in
  loop [];;

let fa_symbols = load_fa_variables ();;
(* http://fortawesome.github.io/Font-Awesome/cheatsheet/ *)
(* http://bluejamesbond.github.io/CharacterMap/ *)
let fa_symbol name =
  try
    List.assoc name fa_symbols
  with Not_found ->
    printd debug_error "FA symbol '%s' was not found. Using 'question' instead" name;
    List.assoc "question" fa_symbols;;

let load_colors () =
  let file = sub_file common "colors/liste.txt" in
  let buffer = Scanf.Scanning.from_file file in
  let rec loop list =
    try
      let name,_,r,g,b = Scanf.bscanf buffer "%s #%x rgb(%u,%u,%u)\n" (fun n h r g b -> n,h,r,g,b) in
      printd debug_io "Reading color [%s]=(%u,%u,%u)" name r g b;
      loop ( (name,(r,g,b))::list )
    with
    | End_of_file -> 
      Scanf.Scanning.close_in buffer;
      list
    | Scanf.Scan_failure _ -> 
      let fail = Scanf.bscanf buffer "%s@\n" (fun x -> x) in
      printd (debug_error+debug_io) "Fail to read [%s]" fail;
      loop list
    | e -> raise e
  in
  loop [];; 

let color_names = load_colors ();;
(* http://www.rapidtables.com/web/color/html-color-codes.htm *)




(* some unused functions, just for me... *)
let print_bin c =
  let rec loop code list =
    if code = 0 then list
    else loop (code lsr 1) ((string_of_int (code land 1)) :: list)
  in
  if c = 0 then "0" else String.concat "" (loop c []);;
