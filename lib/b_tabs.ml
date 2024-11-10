(** group layouts into tabs *)
open B_utils
module Layout = B_layout
module Trigger =  B_trigger
module Draw = B_draw
module Button = B_button
module Style = B_style
module Theme = B_theme
module W = B_widget

let bg_on = Style.gradient Draw.[opaque Button.color_off;
                                 opaque Button.color_off; opaque Button.color_on]
(* Style.gradient Draw.[opaque white;opaque pale_grey;opaque pale_grey;opaque pale_grey];; *)
let bg_off = Style.Solid Draw.(opaque Button.color_off)
(* Style.gradient Draw.[opaque pale_grey;opaque grey;opaque grey;opaque grey];; *)

(* On attache tous les rooms dans le layout (en mode "superposition"), et on met
   en show=true celui qu'on veut. Ça permet de les nettoyer correctement tous, à
   l'extinction.  Cependant, ce n'est pas optimisé au cas où il y a des millions
   de tabs (peu probable ...) car la boucle de display va les passer tous en
   revue pour trouver celui à montrer. Au départ on ne mettait dans le layout
   que le tab actif; l'inconvénient était que tous les autres n'étaient pas
   "visibles" par bogue, et donc n'étaient pas nettoyés en quittant. D'autre
   part, ça forçait à "modifier" le contenu du layout à chaque changement de tab
   actif, ce qui n'est pas très joli. *)
let create_one ?slide title room dest_room =
  (* The button is neither normal Trigger nor Switch since once selected it
     cannot be unselected by clicking on it. *)
  let l = W.create_empty
      (W.Button (Button.create ~bg_on ~bg_off Button.Trigger title)) in
  (* the first action sets the button to 'pressed' when we click (button_down)
     on it. Below we will add another action to reset all other buttons to 'not
     pressed' *)
  let onpress w _ _ =
    let b = W.get_button w in
    if not (Button.is_pressed b) && not (Layout.is_shown room)
    then begin
        Button.press b;
        Layout.iter_rooms (fun l -> Layout.set_show l false) dest_room;
        Layout.set_show room true;

        do_option slide (fun from -> Layout.slide_in ~from ~dst:dest_room room);
        W.update w; (* or refresh only layout ? *)
      end
  in
  let c = W.connect_main l l onpress Trigger.(E.key_down :: buttons_down) in
  W.add_connection l c;
  l

(** create tabs from a assoc list ("title"; layout) *)
(* TODO, return a function that can be called to activate tab #i *)
let create (*?(circular = true)*) ?slide ?(adjust = Layout.Fit) ?(expand = true)
    ?canvas ?(name="tabs") list =
  if list = [] then failwith "Cannot create empty tabs"
  else begin
    let _, first_room = List.hd list in
    let all_rooms = List.map snd list in
    List.iter (fun l -> Layout.set_show l false) all_rooms;
    Layout.set_show first_room true;
    let dest_room = Layout.superpose ?canvas all_rooms in
    let labels = List.map (fun (title, layout) ->
        create_one ?slide title layout dest_room) list in

    let reset_other_labels w _ _ =
      List.iter (fun b ->
          if not (W.equal w b) then Button.reset (W.get_button b)) labels;
      (* + refresh ? *) in

    List.iter (fun l ->
        let c =  W.connect_main l l reset_other_labels
            Trigger.(E.key_down :: buttons_down) in
        W.add_connection l c) labels;

    (* We activate the first label (TODO: choose which one) *)
    let first_l = List.hd labels in
    Button.press (W.get_button first_l);

    let menu = Layout.flat_of_w ~name:(name ^ ".menu") ?canvas ~sep:0 labels in
    if expand then Layout.(set_width menu (width dest_room));
    let tabs = Layout.tower ~name ~sep:0 ~adjust ?canvas [menu; dest_room]
    in if expand then
      menu.resize <- (fun (w, _) ->
          Layout.Resize.set_width menu (w - 2 * Theme.room_margin))
    else Layout.disable_resize menu;
    tabs

  end
