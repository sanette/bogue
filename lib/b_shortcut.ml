(* Keyboard shortcuts for the main loop *)
open Tsdl

module IntPairs =
struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c
end

module PairsMap = Map.Make(IntPairs)

type 'a action = 'a -> unit

type 'a t = ('a action) PairsMap.t

(* Bind a new action to the keycode. If the keycode was already present, the
   previous action is disregarded. *)
let add_map map (keycode, keymod, action) =
  PairsMap.add (keycode, keymod) action map

let remove map pair = PairsMap.remove pair map

(* Some seemingly inocuous modifiers may be pressed, for instance Num_lock, so
   we need to check against the "useful" modifiers.  See
   https://stackoverflow.com/questions/48706762/sdl-2-how-to-check-for-no-modifiers-keyboard-input
   *)
let mod_none = Sdl.Kmod.(ctrl lor shift lor alt lor gui)

(* Return the action bound to the keycode, or None. *)
let find map pair =
  match PairsMap.find_opt pair map with
  | Some r -> Some r
  | None -> let c, m = pair in
    PairsMap.find_opt (c, m land mod_none) map

(* Add new entries from a list of triples (keycode, keymod, action) *)
let add_list map alist =
  List.fold_left add_map map alist

(* Create a Shortcut map for a list of triples (keycode, keymod, action). *)
let create alist : 'a t =
  add_list PairsMap.empty alist

let empty () = create []

(* add a binding to a keycode without modifier *)
let add (keycode, action) map =
  add_map map (keycode, Sdl.Kmod.none, action)

let add_ctrl (keycode, action) map =
  let alist = [keycode, Sdl.Kmod.lctrl, action;
               keycode, Sdl.Kmod.rctrl, action] in
  add_list map alist

let add_ctrl_shift (keycode, action) map =
  let alist = [keycode, Sdl.Kmod.lctrl lor Sdl.Kmod.lshift, action;
               keycode, Sdl.Kmod.lctrl lor Sdl.Kmod.rshift, action;
               keycode, Sdl.Kmod.rctrl lor Sdl.Kmod.rshift, action;
               keycode, Sdl.Kmod.rctrl lor Sdl.Kmod.lshift, action] in
  add_list map alist
