(* This file is part of BOGUE, by San Vu Ngoc *)

(* Internationalization module *)

(* The rules of the game is to load only the necessary languages, and only on
   demand. This means (I think) that data cannot be saved in .ml files. We need
   to dynamically load external files.

   Of course, given that the current set of i18n files is so small, and so is
   the current set of translated phrases, it would be a very minimal burden to
   load them all into the library... But well, a game without rules is not a
   game, right? On a more serious side, if a user of the library wants to build
   an app with thousands of words to translate -- without any IA -- then it
   should scale well.
*)

open Printf
open B_utils
module Theme = B_theme
module Var = B_var

type locale = { language : string; country : string option }

(* From the system (OS) we extract a list of locales, for instance via the $LANG
   variable (but the SDL function get_preferred_locales is more cross-platform,
   see this PR https://github.com/dbuenzli/tsdl/pull/111). This list is ranked
   by order of preference; hence when translating a phrase, we first look for
   the translation into the language of the first locale; if it cannot be found,
   we turn to the next locale, and so on. *)

(* We offer two translation systems. The first one is a set of predefined lazy
   variables, like I18n.File.cancel which (when forced) will return "Annuler" in
   French. *)

(* The second system is a gettext type of lookup, where we need to know the
   exact English phrase (string) --- like "cancel". It performs a Hashtbl
   search, so it is slightly slower than lazy variables once they are cached. *)

(* The translations are organized by language, of course, but also by
   "context". Each Bogue module has its own context; for instance one can
   translate differently "save as" as "enregistrer sous" or "enregistrer comme"
   depending on the context. *)

(* Translation files are ".conf" files located in the share/locales dir. *)

(* Context are modules and are initialized by the [make_context] function (which
   uses first-class modules). *)

let languages = Var.create (Hashtbl.create 15)
(* [languages] contains the "db" tables, each for one language. Each [db] table
   contains "ctxb" context tables. *)

let common_context = "Common context"
let ctx_tag = "__CONTEXT"

let get_ctxb db context =
  let@ db = Var.with_protect db in
  match Hashtbl.find_opt db context with
  | Some table -> table
  | None ->
    printd debug_user "Creating I18n context [%s]" context;
    let table = Hashtbl.create 50 in
    Hashtbl.add db context table;
    table

let get_country_extension locale =
  match locale.country with
  | Some cc -> "_" ^ cc
  | None -> ""

let filename locale =
  sprintf "locale_%s%s.conf" locale.language
     (get_country_extension locale)

let load_locale locale =
  let db = Var.create (Hashtbl.create 15) in
  let ctxb = get_ctxb db common_context in
  let file = filename locale
             |> Filename.concat "%locales"
             |> Theme.get_path in
  let () = if Sys.file_exists file
    then try printd debug_io "Loading [%s]..." file;
        let alist = Theme.load_vars file in
        let rec loop table = function
          | [] -> ()
          | (key, t) :: rest ->
            if key = ctx_tag then loop (get_ctxb db t) rest
            else begin
              Hashtbl.replace table key t;
              loop table rest
            end in
        loop ctxb alist
      with _ -> printd (debug_error + debug_io) "Cannot parse locale file [%s]" file
    else printd (debug_io + debug_warning)
        "Locale file [%s] does not exist. Maybe you could contribute to it?" file
  in
  db

let get_db locale =
  let@ languages = Var.with_protect languages in
  match Hashtbl.find_opt languages locale with
  | Some table -> table
  | None ->
    let db = load_locale locale in
    Hashtbl.add languages locale db; db

(* taken from ocaml 4.10 *)
let rec find_map f = function
  | [] -> None
  | x :: l ->
    begin match f x with
      | Some _ as result -> result
      | None -> find_map f l
    end

let text_from_context context text db =
  let ctxb = get_ctxb db context in
  Hashtbl.find_opt ctxb text

(* If not found in context, lookup all contexts. *)
let text_from_locale ~context text locale =
  let db = get_db locale in
  match text_from_context context text db with
  | None ->
    let@ db = Var.with_protect db in
    let ctxlist = Hashtbl.fold (fun _ctxname ctxb list -> ctxb :: list) db [] in
    find_map (fun ctxb -> Hashtbl.find_opt ctxb text) ctxlist
  | a -> a

let gettext_opt ~context locales text =
  printd debug_custom "Looking for a translation for: %s" text;
  find_map (text_from_locale ~context text) locales

(* If not found we return the original text *)
let gettext ?fallback ~context locales text =
  let fallback = default fallback text in
  gettext_opt ~context locales text
  |>  Option.value ~default:fallback (* TODO log error *)

let test_gettext () =
  let fr = { language = "fr(test)"; country = Some "FR" } in
  let de = { language = "de(test)"; country = None} in
  let french = get_db fr in
  let context = "Test" in
  let test_context = get_ctxb french context in
  List.iter2 (Hashtbl.add test_context)
    ["open"; "close"; "save"; "save as"]
    ["ouvrir(test)"; "fermer"; "enregistrer"; "enregistrer sous"];
  assert (gettext_opt ~context [de; fr] "save as" = Some "enregistrer sous");
  assert (gettext_opt ~context [fr; de] "open" = Some "ouvrir(test)");
  assert (gettext_opt ~context [fr; de] "openn" = None)

let add_translation ~context locale text translation =
  let table = get_ctxb (get_db locale) context in
  Hashtbl.replace table text translation

let save_locale_alist ?domain locale alist =
  let share = match domain with
    | None -> "."
    | Some domain ->
      match Theme.find_share domain "." with
      | None ->
      print_endline "Cannot find share directory!";
      "."
      | Some path -> path in
  let file = Filename.concat share (filename locale) in
  let outch = try open_out file
    with _ -> begin
        printd (debug_error + debug_user) "Cannot open file [%s] for write access." file;
        stdout
      end in
  try
    output_string outch (sprintf "## BOGUE version %s\n\n" Theme.this_version);
    List.iter (fun (text, translation) ->
        output_string outch (sprintf "%s = %s\n" text translation)) alist;
    close_out outch;
    printd (debug_io + debug_user) "Wrote [%s]" file
  with _ -> begin
      printd (debug_io + debug_error + debug_user)
        "Error while writing to file [%s.]" file;
      close_out outch
    end

let ctxb_to_alist table context =
  Hashtbl.fold (fun k t list -> (k, t) :: list) table [ctx_tag, context]
  |> List.sort (fun a1 a2 -> Stdlib.compare (fst a1) (fst a2))

let db_to_alist db =
  Hashtbl.fold (fun context table list -> List.append list (ctxb_to_alist table context))
    db []

(* This saves all the bogue's locales (all contexts) plus the additional
   bindings added by [add_translation]. *)
let save_locale ?domain locale =
  let@ db = Var.with_protect (get_db locale) in
  db_to_alist db
  |> save_locale_alist ?domain locale

(* Now some functions for detecting the user's locales *)

let parse_locale s =
  if s = "" then None
  else let ll_CC = List.hd (String.split_on_char '.' s) in
    if ll_CC = "" then begin
       printd debug_error "Malformed language variable [%s]." s; None
    end
    else match String.split_on_char '_' ll_CC with
      | [language] -> Some { language; country = None}
      | [ll; cc] -> Some { language = String.lowercase_ascii ll;
                           country = Some (String.uppercase_ascii cc) }
      | _ ->  printd debug_error "Malformed language variable [%s]." s; None

let test_parse_lang () =
  assert (parse_locale "C" = Some { language = "C"; country = None });
  assert (parse_locale "fr_FR" = Some { language = "fr"; country = Some "FR" });
  assert (parse_locale "fr_FR.UTF-8" = Some { language = "fr"; country = Some "FR" })

let get_lang () =
  (* https://www.gnu.org/savannah-checkouts/gnu/gettext/manual/gettext.html#Locale-Environment-Variables *)
  let lc = (try Sys.getenv "LC_ALL" with
      | Not_found -> try Sys.getenv "LANG" with
        | Not_found -> "")
           |> parse_locale in
  match lc with
  | Some { language = "C"; _ } -> [{ language = "C"; country = None}]
  | _ ->
    let list = String.split_on_char ':'
        (try Sys.getenv "LANGUAGE" with Not_found -> "") in
    List.append (List.map parse_locale list) [lc]
    |> List.filter_map (fun x -> x)

(* Example:
# get_lang ();;
- : (string * string option) list = [("fr", None); ("fr", Some "FR")]
*)

let get_preferred_locales () =
  (* TODO use SDL version of this function *)
  get_lang ()

let locales = lazy (get_preferred_locales ())
let tf = Lazy.force
let get_locales () = tf locales
let str_fmt = Scanf.format_from_string "%s" "%s"
let int_fmt = Scanf.format_from_string "%i" "%i"
let uint_fmt = Scanf.format_from_string "%u" "%u"
let uint2_fmt = Scanf.format_from_string "%u%u" "%u%u"

(* We need to help the type checker here, in case these formats are not used
   elsewhere in this file: *)
let _ = sprintf int_fmt 0
let _ = sprintf uint_fmt 0
let _ = sprintf uint2_fmt 0 0
let _ = sprintf str_fmt ""

module type ContextInit = sig
  val gettext : string -> string
  val gettext_opt : string -> string option
  val tt : string -> string lazy_t
  val tf : 'a Lazy.t -> 'a
  val t_uint : string -> (int -> string) lazy_t
  val t_int : string -> (int -> string) lazy_t
  val t_str : string -> (string -> string) lazy_t
  val t_uint2 : string -> (int -> int -> string) lazy_t
  val add_translation : locale -> string -> string -> unit
end

(* Warning: we don't check whether a context with the same name already
   exist. If this happens, both contexts will share the same database. *)
let make_context name =
  let module C = struct
    let context = name
    let gettext text = gettext ~context (tf locales) text
    let gettext_opt text = gettext_opt ~context (tf locales) text
    let tt text = lazy (gettext text)
    let tf = tf
    let t_str s = lazy (sprintf (Scanf.format_from_string (gettext s) str_fmt))
    let t_int s = lazy (sprintf (Scanf.format_from_string (gettext s) int_fmt))
    let t_uint s = lazy (sprintf (Scanf.format_from_string (gettext s) uint_fmt))
    let t_uint2 s = lazy (sprintf (Scanf.format_from_string (gettext s) uint2_fmt))
    let add_translation = add_translation ~context
  end in
  (module C : ContextInit)

(* Now the predefined contexts *)

module File = struct

  include (val (make_context "File") : ContextInit)

  let cancel = tt "Cancel"
  let close = tt "Close"
  let continue = tt "Continue"
  let enter_path = tt "Enter path"
  let modified = tt "Modified"
  let name = tt "Name"
  let no_selection = tt "No selection"
  let one_dir_selected = tt "1 directory selected"
  let one_file_selected = tt "1 file selected"
  let open_ = tt "Open"
  let open_dir = tt "Open dir"
  let save = tt "Save"
  let save_as = tt "Save as"
  let select = tt "Select"
  let select_directory = tt "Select directory"
  let select_dirs = tt "Select dirs"
  let select_file = tt "Select file"
  let select_files = tt "Select files"
  let size = tt "Size"
  let x_dirs_selected = t_uint "%u directories selected"
  let x_files_selected = t_uint "%u files selected"
  let x_files_x_dirs_selected = t_uint2 "%u files and %u dirs selected"

  let test () =
    let o = Lazy.force open_ in
    print_endline o

end

module Menu = struct

  include (val (make_context "Menu") : ContextInit)

  let copy = tt "Copy"
  let edit = tt "Edit"
  let open_ = tt "Open"
  let paste = tt "Paste"
  let save = tt "Save"
  let save_as = tt "Save as"
end

module Popup = struct

  include (val (make_context "Popup") : ContextInit)

  let cancel = tt "Cancel"
  let close = tt "Close"
  let no = tt "No"
  let yes = tt "Yes"

end

module Text_input = struct

  include (val (make_context "Text_input") : ContextInit)

  let prompt = tt "Enter text"

end
