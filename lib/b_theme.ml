(* b_theme.ml : theme variables *)
(* This file is part of BOGUE.  *)
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

let this_version = "20230310"  (* see VERSION file *)
(* Versions are compared using usual (lexicographic) string ordering. *)

let default_vars = [
    (* Debug: *)
    "DEBUG", "false";
    (* Whether debug information should be logged to a file *)
    "LOG_TO_FILE", "false";
    (* The main themes dir: usually $HOME/.config/bogue/themes *)
    "DIR", "";
    (* The chosen theme: *)
    "THEME", "default"; (* It must be a subdirectory of DIR: *)
    (* The window background image (eg: "file:background.png") or color: *)
    "BACKGROUND", "color:white"; (* if is starts with "/" it is an absolute path. Otherwise, it is a file path inside THEME. *)
    (* This background color should be clearly visible over the BACKGROUND *)
    "BG_COLOR", "lightsteelblue";
    (* Color for active or inactive button *)
    "BUTTON_COLOR_ON", "darkturquoise";
    "BUTTON_COLOR_OFF", "steelblue";
    (* The "checked" image: either image (eg. "check_on.png") or fa icon,
       eg. "fa:check-square-o" *)
    "CHECK_ON", "fa:check-square-o";
    (* The "unchecked" image: (eg: "check_off.png") *)
    "CHECK_OFF", "fa:square-o";
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
    (* Font awesome dir, in the theme/common directory: *)
    "FA_DIR", "font-awesome-4.6.3";
    (* The global scale to be applied to all graphical elements. Setting
     SCALE=2. is useful for HIDPI screens. Here, the scale should be transparent
     to the user, and be applied only at the last moment, when dealing directly
     with rendering functions, or when creating blits. It might be a good idea
     to have a different scale per window, in case of multiple monitors. SCALE=0
       will try to autodetect: *)
    "INT_SCALE", "false";
    "SCALE", "0";
    "OPENGL_MULTISAMPLE", "false";
    (* https://wiki.libsdl.org/SDL_GLattr#multisample *)
  ]

let id x = x

let (//) = Filename.concat

(* Some global environment variables *)
module User_dirs = Directories.User_dirs ()
module Project_dirs = Directories.Project_dirs (struct
  let qualifier = "org"
  let organization = "sanette"
  let application = "bogue"
end
)

let home = match User_dirs.home_dir with
  | None -> failwith "can't compute home directory path"
  | Some home -> home

let conf = match Project_dirs.config_dir with
  | None -> failwith "can't compute configuration directory path"
  | Some config_dir -> config_dir

let skip_comment buffer =
  let rec loop () =
    let (* comment *) _ = Scanf.bscanf buffer " #%s@\n" id in
    (* printd debug_io "Comment ignored: \"%s\"" comment; *)
    loop () in
  try loop () with
  | End_of_file
  | Scanf.Scan_failure _ -> ()
  | e -> print_endline "SCAN ERROR"; raise e

(* Load variables from config file. Returns an association list. Most recent
   entries are put first (ie. the order is the reverse of the order of the lines
   in the file), and hence will be selected first by List.assoc.*)
let load_vars config_file =
  let buffer = Scanf.Scanning.from_file config_file in
  let version = try
      Scanf.bscanf buffer "## BOGUE version %s " id
    with e -> raise e in
  printd debug_io "Reading config file [%s]. BOGUE Version [%s]"
    config_file version;
  if version > this_version
  then printd debug_warning
      "The version indicated in the config file (%s) is more recent than your \
       this version of Bogue (%s)" version this_version;
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
  loop []


(* TODO move this in an "init" function (and hence theme vars must be mutable)
*)

let conf_file = "bogue.conf"

(* We load all config files, in case of conflict, the first ones take
   precedence. The theme config file will be loaded afterwards, see below. *)
let all_conf_files () = [
  conf_file;
  home // ("." ^ conf_file);
  conf // conf_file
]

let user_vars =
  List.map (fun file ->
      if Sys.file_exists file then
        try load_vars file with
        | _ (* TODO check correct exception *) ->
          printd (debug_error + debug_io) "Error loading config file %s." file; []
      else (printd debug_io "Config file %s not found." file; []))
    (all_conf_files ())
  |> List.flatten

let user_vars = ref user_vars

let get_var s =
  let v = try Sys.getenv ("BOGUE_" ^ s) with
    | Not_found ->
      try List.assoc s !user_vars with
      | Not_found -> begin
          printd debug_warning
            "User variable '%s' not found in config." s;
          try List.assoc s default_vars with
          | Not_found ->
            printd debug_error "Variable '%s' not found. Prepare for a crash." s;
            ""
        end in
  printd debug_warning "Using %s=%s" s v;
  v

let get_int ?(default = 0) s =
  let v = get_var s in
  try int_of_string v with
  | Failure _ -> (* "int_of_string" *)
    printd debug_error
      "Expected an integer for '%s', got '%s' instead. Using default=%d."
      s v default;
    default
  | e -> raise e

let get_float ?(default = 0.) s =
  let v = get_var s in
  try float_of_string v with
  | Failure _ -> (* "float_of_string" *)
    printd debug_error "Expected a float for '%s', got '%s' instead. Using default=%f." s v default;
    default
  | e -> raise e

let get_bool s =
  let b = get_var s in
  String.lowercase_ascii b = "true" || b = "1"

let rev_insert_theme_vars theme_path vars rest =
  let theme_vars = load_vars theme_path in
  List.rev_append vars (List.append theme_vars rest)

(* Checks the (first) THEME entry in user's vars, and then loads & inserts the
   theme variables. If there is no THEME entry, we add ("THEME", "default"). *)
let load_theme_vars dir vars =
  let rec loop newv = function
    | [] ->
      let value = get_var "THEME" in (* should be "default" *)
      printd debug_io "No theme specified, using default=%s" value;
      loop newv ["THEME", value]
    | (name, value)::rest ->
       if name = "THEME"
       then
         let theme_path = dir // value // conf_file in
         (name, value) :: (rev_insert_theme_vars theme_path newv rest)
       else loop ((name, value)::newv) rest
  in
  loop [] vars

let () =
  debug := get_bool "DEBUG";
  if get_bool "LOG_TO_FILE"
  then begin
    let log_file = Filename.temp_file "bogue" ".log" in
    Printf.printf "Bogue - Logging to file %s\n" log_file;
    log_channel := open_out log_file
  end

(* TODO this won't work with native Windows; use ocaml-crunch? *)
let download_conf () =
  let open Printf in
  let rescue = "bogue_conf.tgz" in
  let cwd = Sys.getcwd () in
  let tmpdir = Filename.temp_file "bogue" "" in
  Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o777;
  Sys.chdir tmpdir;
  if Sys.command (sprintf "wget https://raw.githubusercontent.com/sanette/bogue/master/%s" rescue) <> 0
  then failwith "Cannot download rescue config. Aborting."
  else if Sys.command (sprintf "tar xvfz %s" rescue) <> 0
  then failwith "Cannot extract rescue config. Aborting."
  else if Sys.command (sprintf "mkdir -p %s/bogue" conf) <> 0
  then failwith "Cannot create config dir. Aborting."
  else if Sys.command (sprintf "cp -r bogue/* %s/bogue/" conf) <> 0
  then failwith "Cannot copy config files. Aborting"
  else print "Minimal config downloaded to %s/bogue." conf;
  Sys.chdir cwd;
  sprintf "%s/bogue/themes" conf

(* Look for a share dir for [prog] either of the form "prefix/share/prog" or
   "prefix/share" that contains the given [file]. This is a utility for other
   programs using Bogue, not for Bogue itself. *)
let find_share prog file =
  let cwd = Sys.getcwd () in
  let queue = Queue.create () in
  let bin_dir = Filename.dirname Sys.executable_name in
  let prefix_dir = Sys.chdir bin_dir; Sys.chdir ".."; Sys.getcwd () in
  let b = Filename.basename bin_dir in
  if b = "bin"
  then Queue.add (prefix_dir // "share" // prog) queue;
  if Filename.basename b = "bin"
  then Queue.add (prefix_dir // "share") queue;
  let () =
    try let system = Unix.open_process_in "opam var share" in
        let res = input_line system in
        (* We could also use [sprintf "opam var %s:share" prog] *)
      Queue.add (res // prog) queue
    with _ -> () in
  Sys.chdir cwd;
  match Queue.fold (fun list path ->
      if Sys.file_exists (path // file)
      then List.cons path list else list) [] queue with
  | [] -> printd debug_error "Cannot find share directory for %s/%s! bin_dir=%s, prefix_dir=%s"
            prog file bin_dir prefix_dir; None
  | path :: _ -> Some path

let app_share_dir = match Filename.basename Sys.executable_name with
  | "ocaml" | "utop" | "utop.exe" -> None
  | app -> printd debug_io "Executable=%s" app; find_share app "."

(* We try to locate the theme dir. *)
(* We first check [conf]/bogue/themes, then `opam var share`/bogue/themes

   WARNING, when installing from a tmp dir and running `dune build @install
   @runtest` (which is what the ocaml-ci does when submitting a new opam
   version), then `ocamlfind query bogue` will return
   "tmp/bogue-20220115/_build/install/default/lib/bogue", which is good (the
   assets are installed in ../../share/), while `opam var share` returns
   "/home/essai/.opam/4.08.1/share" which is not good because the assets are not
   installed there yet. This is a problem only for dune testing. Once `opam
   install .` is done, it should work.  *)
let dir =
  let dir = get_var "DIR" in
  if Sys.file_exists dir && Sys.is_directory dir
  then dir
  else let config = conf // "bogue" // "themes" in
    if Sys.file_exists config && Sys.is_directory config
    then if dir = ""
      then begin
        printd debug_warning "Using %s as bogue themes directory" config;
        config
      end else begin
        printd debug_error
          "Bogue themes directory %s does not exist. Using %s instead" dir config;
        config
      end
    else try
        let system = Unix.open_process_in "ocamlfind query bogue" in
        let res = input_line system in
        match Unix.close_process_in system with
        | Unix.WEXITED 0 ->
          let res = Filename.(dirname @@ dirname res) in
          let dir = Printf.sprintf "%s/share/bogue/themes" res in
          if Sys.file_exists dir && Sys.is_directory dir
          then dir else begin
            printd debug_error "Cannot find themes directory";
            raise Not_found
          end
        | _ ->
          printd debug_error
            "Cannot find a usable bogue configuration directory";
          raise Not_found
      with
      | Not_found
      | End_of_file ->
        printd debug_error
          "(FATAL) Bogue configuration directory %s does not exist, and \
           system-wide config cannot be found." dir;
        print_endline "Cannot find bogue lib directory";
        print_endline "Trying to download a minimal Bogue config...";
        download_conf ()
      | e -> raise e

(* Add variables from theme config file (if specified in the user config file)
*)
let () = user_vars := load_theme_vars dir !user_vars

let current = dir // (get_var "THEME")

let () =  print_endline
    (Printf.sprintf "Loading Bogue %s with config dir %s" this_version current)

let common = dir // "common"
let fonts_dir = common // "fonts"

let load_path = List.fold_left (fun l -> function
    | Some p -> p::l
    | None -> l) [] [app_share_dir; Some ""; Some current; Some common]

(* A file starting with "/" is considered a global path. If the file starts with
   "%", that char is replaced by the shared bogue dir/. Otherwise it will be
   searched in the [path] (by default = [load_path]) *)
let get_path ?(path = load_path) file =
  if file = "" then invalid_arg "[Theme.get_path]: filename empty";
  if file.[0] = '/' then file
  else if file.[0] = '%'
  then let file = String.sub file 1 (String.length file - 1) in
    ((Filename.dirname dir) // file)
  else let rec loop = function
      | [] ->
        printd (debug_error + debug_io)
          "File %S not found in path: %s (current dir=%s)"
          file (String.concat ":" path) (Sys.getcwd ());
        file
      | dir::rest ->
        let p = dir // file in
        if Sys.file_exists p then p else loop rest in
    loop path

let get_fa_or_path s =
  if startswith s "fa:" then s else get_path s

(* Font names not starting with "/" are searched first in the theme directory,
   then in bogue's common fonts_dir, then in the system's fonts. *)
let get_font_path name =
  if name = "" then failwith "Empty fontname";
  if name.[0] = '/' then name
  else let check_file file fail =
         if Sys.file_exists file then file else fail () in
    (* stupid construction, I know hehe) *)
    check_file name (fun () ->
        check_file (fonts_dir // name) (fun () ->
            let fclist = Unix.open_process_in
                (Printf.sprintf "fc-list : file | grep %s" name) in
            try
              let res = input_line fclist in
              match Unix.close_process_in fclist with
              | Unix.WEXITED 0 ->
                String.sub res 0 (String.rindex res ':')
              | _ -> printd debug_error "Cannot find font %s" name; name
            with End_of_file -> printd debug_error "Cannot find font %s" name; name
          )
      )

let background = get_var "BACKGROUND"
let bg_color = get_var "BG_COLOR"
let button_color_off = get_var "BUTTON_COLOR_OFF"
let button_color_on = get_var "BUTTON_COLOR_ON"
let check_on = get_fa_or_path (get_var "CHECK_ON")
let check_off = get_fa_or_path (get_var "CHECK_OFF")
let cursor_color = get_var "CURSOR_COLOR"
let faint_color = get_var "FAINT_COLOR"
let text_color = get_var "TEXT_COLOR"
let sel_bg_color = get_var "SEL_BG_COLOR"
let sel_fg_color = get_var "SEL_FG_COLOR"
let label_color = get_var "LABEL_COLOR"
let menu_hl_color = get_var "MENU_HL_COLOR"
let menu_bg_color = get_var "MENU_BG_COLOR"
let label_font_size = get_int ~default:14 "LABEL_FONT_SIZE"
let label_font = ref (get_font_path (get_var "LABEL_FONT"))
let text_font = ref (get_font_path (get_var "TEXT_FONT"))
let text_font_size = get_int ~default:14 "TEXT_FONT_SIZE"
let small_font_size = get_int ~default:10 "SMALL_FONT_SIZE"
let mono_font = get_font_path (get_var "MONO_FONT")
let room_margin = get_int ~default:10 "ROOM_MARGIN"
let fa_dir = common // (get_var "FA_DIR")
let fa_font = fa_dir // "fonts/fontawesome-webfont.ttf"
let integer_scale = ref (get_bool "INT_SCALE")
let scale = ref (get_float ~default:0. "SCALE")
let opengl_multisample = get_bool "OPENGL_MULTISAMPLE"
let fa_font_size = 18

(** some standard (?) UTF8 symbols *)
let symbols = [
"check_empty", "\239\130\150";
"check", "\239\129\134";
]

let set var x = var := x

let set_text_font f = text_font := get_font_path f
let set_label_font f = label_font := get_font_path f

let set_scale s =
  let s = if !integer_scale then Float.floor s else s in
  scale := s

let set_integer_scale b =
  integer_scale := b;
  if b then set_scale !scale

let scale_int i =
  round (!scale *. float i)

let unscale_int i =
  round (float i /. !scale)

let unscale_f x =
  x /. !scale

let scale_from_float x =
  round (!scale *. x)

let unscale_to_float i =
  !scale *. (float i)

(** font awesome variables *)
let load_fa_variables () =
  let file = fa_dir // "less" // "variables.less" in
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
  loop []

let fa_symbols = load_fa_variables ()
(* http://fortawesome.github.io/Font-Awesome/cheatsheet/ *)
(* http://bluejamesbond.github.io/CharacterMap/ *)
let fa_symbol name =
  try
    List.assoc name fa_symbols
  with Not_found ->
    printd debug_error "FA symbol '%s' was not found. Using 'question' instead" name;
    List.assoc "question" fa_symbols

let load_colors () =
  let file = common // "colors" // "liste.txt" in
  printd debug_io "Reading color names from [%s]." file;
  let buffer = Scanf.Scanning.from_file file in
  let rec loop list =
    try
      let name,_,r,g,b = Scanf.bscanf buffer "%s #%x rgb(%u,%u,%u)\n" (fun n h r g b -> n,h,r,g,b) in
      (* printd debug_io "Reading color [%s]=(%u,%u,%u)" name r g b; *)
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
  loop []

let color_names = load_colors ()
(* http://www.rapidtables.com/web/color/html-color-codes.htm *)


(* some unused functions, just for me... *)
let print_bin c =
  let rec loop code list =
    if code = 0 then list
    else loop (code lsr 1) ((string_of_int (code land 1)) :: list)
  in
  if c = 0 then "0" else String.concat "" (loop c [])
