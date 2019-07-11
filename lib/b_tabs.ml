(** group layouts into tabs *)
open B_utils;;
(* open Types;; *)
module Layout = B_layout
module Var = B_var
module Trigger =  B_trigger
module Draw = B_draw
module Label = B_label
module Button = B_button
module Style = B_style
module Box = B_box
               
(* module MakeTabs ( L:WL ) = *)
(* struct *)
type tab = {
  title : Label.t;
  room : Layout.t;
}

type t = {
  tabs : tab array; (* TODO use chain.ml instead of array ? *)
  active : int Var.t; (* index of active tab, first is 0 *)
  circular : bool;
  (*    layout : L.t; *)  (* TODO: the layout to display, containing the labels and the
                             current tab-room *)
}

(* let create_one title room =
 *   { title; room };;
 * 
 * (\** create tabs from a assoc list ("title"; layout) *\)
 * let create ?(circular = true) list =
 *   let l = List.map (fun (title, layout) -> create_one title layout) list in
 *   let tabs = Array.of_list l in
 *   { tabs;
 *     active = Var.create 0;
 *     circular
 *   } *)

  (** how many tabs ? *)
let length t =
  Array.length t.tabs;;

(* activate next tab *)
let next t =
  let i = Var.get t.active in
  let i = if t.circular then (i + 1) mod (length t)
    else min (i + 1) (length t - 1) in
  Var.set t.active i;;

(* activate previous tab *)
let prev t =
  let i = Var.get t.active in
  let i = if t.circular then (i - 1) mod (length t)
    else max (i - 1) 0 in
  Var.set t.active i;;

(* activate tab i *)
let activate t i =
    Var.set t.active i;;


(* display *)

(* end *)

(* new attempt *)
(* warning: not thread safe ? we modify the dest_room *)
module W = B_widget;;

let bg_on = Style.gradient Draw.[opaque Button.button_color_off; opaque Button.button_color_off; opaque Button.button_color_on];;
  (* Style.gradient Draw.[opaque white;opaque pale_grey;opaque pale_grey;opaque pale_grey];; *)
let bg_off = Style.Solid Draw.(opaque Button.button_color_off);;
(* Style.gradient Draw.[opaque pale_grey;opaque grey;opaque grey;opaque grey];; *)

(* TODO en fait il faudrait plutôt attacher tous les rooms dans le layout (en
   mode "superposition"), et mettre en show=true celui qu'on veut. ça
   permettrait de les nettoyer correctement tous à l'extinction. *)
let create_one ?slide title room dest_room =
  let l = W.create_empty (W.Button (Button.create ~bg_on ~bg_off title)) in
  (* the first action sets the button to 'pressed' when we click (button_down)
     on it. Below we will add another action to reset all other buttons to 'not
     pressed' *)
  let onpress w _ _ =
    let b = W.get_button w in
    (* we do nothing if the tab is already selected: *)
    (* if not (let present_tab = let open Layout in *)
    (*          match dest_room.content with *)
    (*          | Rooms [r] -> r *)
    (*          | _ -> failwith "Tab layout should contain a [room]" in *)
    (*         Layout.(equal room present_tab)) *)
    if not (Button.is_pressed b)
    then begin
      Button.press b;
      Layout.set_rooms dest_room [room];
      do_option slide (fun from -> Layout.slide_in ~from ~dst:dest_room room);
      W.update w; (* or refresh only layout ? *)
    end
 in
 let c = W.connect_main l l onpress Trigger.buttons_down in
  W.add_connection l c;
  l;;

(** create tabs from a assoc list ("title"; layout) *)
(* TODO, return a function that can be called to activate tab #i *)
let create (*?(circular = true)*) ?slide ?(adjust = Layout.Fit) ?(expand = true)
    ?canvas ?(name="tabs") list =
  if list = [] then failwith "Cannot create empty tabs"
  else begin
    let _, first_room = List.hd list in
    let dest_room = Layout.flat ~sep:0 ~adjust ?canvas [first_room] in
    begin (* we fix initial geometry *)
      let open Layout in
      let maxw, maxh =
        List.fold_left (fun m (_,r) -> max m (getx r + width r)) 0 list,
        List.fold_left (fun m (_,r) -> max m (gety r + height r)) 0 list in
      match adjust with
      | Nothing -> (* we fix the size to the maximum of all tabs *)
        set_width dest_room maxw;
        set_height dest_room maxh
      | Width -> 
        set_height dest_room maxh
      | Height -> 
        set_width dest_room maxw
      | Fit -> ()
    end;
    let labels = List.map (fun (title, layout) ->
        create_one ?slide title layout dest_room) list in
    let reset_other_labels w _ _ =
      List.iter (fun b ->
          if not (W.equal w b) then Button.reset (W.get_button b)) labels;
      (* + refresh ? *) in
    List.iter (fun l ->
        let c =  W.connect_main l l reset_other_labels Trigger.buttons_down in
        W.add_connection l c) labels;
    (* we activate the first label (TODO: choose which one) *)
    let first_l = List.hd labels in
    Button.press (W.get_button first_l);

    let menu = Layout.flat_of_w ?canvas ~sep:0 labels in
    if expand (* we set the same width to all labels... do better? *)
    then begin
      let w = Layout.width dest_room in (* TODO utiliser relayout *)
      Layout.set_width menu w;
      List.iter (fun r -> Layout.set_width r (w/3)) (Layout.get_rooms menu)
      (* TODO ajuster le dernier w - n(w/3)... *)
    end;
    Layout.reflat ~margins:0 menu;

    (* on ajoute une ligne ?? *)
    (* let hline = Layout.empty ~w:(Layout.width menu) ~h:1 ~background:(Layout.color_bg Draw.(opaque dark_grey)) () in *)
    Layout.tower ~name ~sep:0 ~adjust ?canvas [menu; (*hline;*) dest_room]
  end;;

(* TODO mutually recursive modules in separate files
http://www.davehking.com/2011/05/23/mutually-recursive-modules-in-ocaml-and-why-you-might-care.html
*)
