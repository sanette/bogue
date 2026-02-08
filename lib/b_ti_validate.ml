(* This file is part of BOGUE, by San Vu Ngoc *)

(* Validator mechanism for text_inputs *)

open B_utils
module Draw = B_draw
module Label = B_label
module Layout = B_layout
module RGB = B_rgb
module RGBA = B_rgba
module Style = B_style
module Text_input = B_text_input
module Trigger = B_trigger
module Widget = B_widget

type status = bool option
    (* true = good text, false = bad text, none = undecided yet *)
type validator = string -> status * string option

type t = {
  ti : Widget.t;
  ok: Widget.t;
  validator : validator;
  status : status ref;
  layout : Layout.t
}

let get_text_input t = t.ti
let get_status t = !(t.status)
let get_layout t = t.layout
let get_text t = Widget.get_text t.ti

let fa_warning = "warning"
let fa_ok = "check"
let warning_color = RGBA.crimson
let ok_color = RGBA.limegreen

let regexp_validator ?(strict = false) regexp =
  let r = Str.regexp regexp in
  fun s ->
    if strict then
      match Str.string_match r s 0 with
      | true when Str.matched_string s = s ->
        Some true, None
      | true -> Some false, Some (Str.matched_string s)
      | false -> Some false, Some ""
    else
      match Str.string_partial_match r s 0 with
      | true when Str.matched_string s = s ->
        if Str.string_match r s 0
        then Some true, None
        else None, None
      | true -> Some false, None
      | false -> Some false, None

(* Example:

   let f = regexp_validator ~strict:true {|[A-Za-z]+|};;

*)



let set_ok ok =
  let l = Widget.get_label ok in
  Label.set_fg_color l ok_color;
  Label.set_icon l fa_ok

let set_neutral ok =
  let l = Widget.get_label ok in
  Label.set_fg_color l RGBA.faint_color;
  Label.set_icon l fa_ok

let set_warning ok =
  let l = Widget.get_label ok in
  Label.set_fg_color l warning_color;
  Label.set_icon l fa_warning

let connect_ok validator old_status default ti ok _ =
  let status, s = validator (Widget.get_text ti) in
  let () = match status with
    | None when !old_status <> None ->
      set_neutral ok
    | Some true when !old_status <> Some true ->
      set_ok ok
    | Some false when !old_status <> Some false ->
      set_warning ok
    | _ -> () in
  old_status := status;
  do_option s (fun s ->
      Widget.set_text ti (if s="" then default else s))

let make validator ?(bg = RGB.bg_color) ?prompt ?size text : t =
  if fst (validator text) = Some false
  then printd (debug_error + debug_user)
      "The default text [%s] for the text_input validator is invalid. Please \
       change it." text;
  let ti = Widget.text_input ~text ?prompt ?size () in
  let ok = Widget.icon ?size ~fg:RGBA.faint_color fa_ok in
  let old_status = ref None in

  let c = Widget.connect_main ti ok (connect_ok validator old_status text)
      (List.append Text_input.triggers Trigger.buttons_down) in
  Widget.add_connection ti c;
  let background = Layout.style_bg
      Style.(of_bg (opaque_bg bg)
             |> with_border (mk_border ~radius:12 (mk_line ()))) in
  { ti; ok; status = old_status; validator;
    layout =  Layout.(flat ~hmargin:6 ~vmargin:2 ~sep:2 ~background
                        ~resize:Resize.Linear ~align:Draw.Center
                        [resident ok; resident ti])}

let of_regexp ?strict regexp =
  make (regexp_validator ?strict regexp)

module Email = struct

  (* Validating emails (with hints) following

     https://html.spec.whatwg.org/multipage/input.html#email-state-(type=email)

     which is a simplification of the strict standards.
  *)

  (* let let_dig = *)
  (*   let r = Str.regexp {|[A-Za-z0-9]*|} in *)
  (*   fun s -> Str.string_match r s 0 && *)
  (*            Str.matched_string s = s *)

  (* let atext = *)
  (*   let r = Str.regexp "[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]" in *)
  (*   fun s -> Str.string_match r s 0 && *)
  (*            Str.matched_string s = s *)

  let first_part =
    let r = Str.regexp "[A-Za-z0-9!#$%&'*+/=?^_`{|}~.-]+" in
    fun s -> Str.string_match r s 0 &&
             Str.matched_string s = s

  let label =
    let r = Str.regexp {|[A-Za-z0-9]\([A-Za-z0-9-]*[A-Za-z0-9]\)?|} in
    fun s -> String.length s <= 63 && Str.string_match r s 0 &&
             Str.matched_string s = s

  let domain s =
    let labels = String.split_on_char '.' s in
    labels <> []
    && List.fold_left (fun bool lb -> bool && label lb) true labels

  let is_valid s =
    match String.split_on_char '@' s with
    | [first; dom] ->
      first_part first && domain dom
    | _ -> false

  let valid_emails = [
    "a@b"; "a@1"; "0@0"; "user@localhost"; "foo@bar";
    ".a@b";"a.@b"; "a..b@c"; "...@a";
    "a+b@c"; "a!#$%&'*+/=?^_{}~@b";
    "a@b-c"; "a@bc-1";
    "a@b.c"; "a@b.c.d";
    "a!a.a-a+a@b-b.b-b.b"
  ]

  let invalid_emails = [
    "a@-bc"; "a@bc-";
    "a@b..c"; "a@b_c"; "a@b$c";
    "a@b@c"; "abc"; "@b"; "a@";
    "@"; ".@."; "a@.";
    "a@zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
  ]
  let test () =
    List.iter (fun s -> assert (is_valid s)) valid_emails;
    List.iter (fun s -> assert (not (is_valid s))) invalid_emails

  (* TODO: show hints as tooltips or popup when clicking on the validator
     icon. *)
  let hint s =
    let b = Buffer.create 256 in
    let add = Buffer.add_string b in
    let () = if is_valid s then add "The email address is valid.\n"
      else match String.split_on_char '@' s with
        | [first; dom] ->
          if not (first_part first)
          then add "The first part of the email address contains an illegal character.\n";
          if not (domain dom)
          then add "The domain part of the email is invalid.\n"
        | [_] -> add "Email address should contain one '@' character.\n"
        | _ -> add "Email address should contain only one '@' character.\n"
    in
    Buffer.contents b

  let validator s =
    let ok = match String.split_on_char '@' s with
      | [first; dom] when first_part first ->
        if dom = "" then None else begin match domain dom with
          | true -> Some true
          | false when dom.[String.length dom - 1] = '.' &&
                       domain (String.sub dom 0 (String.length dom - 1)) ->
            None
          | _ -> Some false
        end
      | [first] -> if first = "" || first_part first then None else Some false
      | _ -> Some false in
    ok, None
end
