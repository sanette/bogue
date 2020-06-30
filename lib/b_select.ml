(* Select list *)
(* based on the Menu2 module *)
(* This is a simple select list with no submenus *)

(* TODO highlight the selected entry on OPENING the menu. (standard behaviour,
   cf: https://www.w3schools.com/tags/tryit.asp?filename=tryhtml_select ).

   But with the current implementation, this is not so obvious. We probably have
   to modify Menu. 

   Example: 28
*)

(* TODO: scroll when navigating with keyboard *)

open B_utils
module Layout = B_layout
module Widget = B_widget
module Var = B_var            
module Label = B_label
module Menu = B_menu
module Theme = B_theme
module Draw = B_draw
module Sync = B_sync
module Print = B_print

let pre = if !debug
  then fun s -> print_endline ("[Select2] " ^ s) (* for local debugging *)
  else nop

(* We will create a menu with a unique entry, being a submenu. This function
   returns the submenu. *)
let get_submenu menu =
  let open Menu.Engine in
  pre (Printf.sprintf "#entries=%u" (List.length menu.entries));
  match menu.entries with
  | [entry] -> begin
      match entry.kind with
      | Menu sub -> Some sub
      | Action _ -> None end
  | _ -> None

(* We construct a simple Menu2 with a custom Layout for the main entry, and
   automatically generated labels for the menu entries. Using a custom layout
   makes it easier to modify its text, but in principle we could also use the
   automatically generated layout and recover its widget resident. *)

let create ?dst ?(name = "select") ?(action = fun _ -> ()) ?fg
    entries selected =

  let selected = Var.create selected in
  let action = Var.create action in

  (* let background = Layout.Solid(Draw.(transp white)) in *)
  let selected_widget = Widget.label ?fg entries.(Var.get selected) in
  let selected_layout = Layout.flat_of_w ~name (* ~background *)
      ~sep:0 [selected_widget] in
  let selected_label = Widget.get_label selected_widget in

  let entries = Array.to_list entries |>
                List.mapi (fun i s ->
                    let action () =
                      Label.set selected_label entries.(i);
                      Var.set selected i;
                      Var.get action i in
                    Menu.{ label = Text s;
                           content = Action action }) in

  let entry = Menu.{ label = Layout selected_layout;
                     content = Tower entries } in

  (* First pass just to obtain the menu width. This could probably be done more
     economically, but well... *)
  let menu = Menu.(raw_engine (Flat [entry])) in
  let submenu = match get_submenu menu with
    | Some s -> s
    | None -> failwith "Menu should have a unique submenu" in

  (* The main entry should have the width of the menu *)
  let w = Layout.width (Menu.layout_of_menu submenu) in
  let menu_layout = Menu.layout_of_menu menu in
  (* Here the structure of menu_layout is
     [ menu_layout =
      [ formatted_label =
        [ selected_layout = [label "banana"]] [ caret-down ]; 
      ]
     ]
  *)
  pre (Print.layout_down menu_layout);
  List.iter (fun l -> Layout.set_width l (w - Menu.suffix_width))
    [menu_layout;
     Layout.get_rooms menu_layout |> List.hd;
     selected_layout;
     Layout.get_rooms selected_layout |> List.hd;
    ];

  (* Now the principal pass with the corrected entry. *)
  let menu = Menu.(raw_engine (Flat [entry])) in
  let menu_layout = Menu.layout_of_menu menu in
  let submenu = match get_submenu menu with
    | Some s -> s
    | None -> failwith "Menu should have a unique submenu" in

  let tmp_dst = default dst
      (* Just a horizontal line *)
      (let line = Widget.empty ~w ~h:1 () in
       (* let background = Layout.Solid(Draw.(transp grey)) in *)
       (* DEBUG *)
       Layout.flat_of_w ~sep:0 (* ~background *) [line]) in

  Menu.Engine.init ~dst:tmp_dst menu;
  Layout.set_height tmp_dst (Layout.height menu_layout);

  if dst = None then begin
    (* We need to relocate to the top layout *)    
    (fun () ->
       pre "RELOCATE!";
       let room = Menu.layout_of_menu submenu in
       let screen = Layout.get_rooms tmp_dst |>
                    List.rev |>
                    List.hd in
       (* We move the menu layout to the top layout; if the menu is too
          big, we add a scrollbar. Note that, currently, this has the
          effect of hiding the shadow. TODO: correct this...*)
       let new_room = Layout.relocate ~scroll:true
           ~dst:(Layout.top_house tmp_dst) room in
       (* We expand the screen to full size: *)
       let screen = Layout.relocate ~scroll:false
           ~dst:(Layout.top_house tmp_dst) screen in
       Layout.maximize screen;
       if not Layout.(new_room == room)
       then begin
         Menu.set_layout submenu new_room;
         Layout.hide ~duration:0 new_room
       end;
       (* pre (Print.layout_down screen);
        * pre (Print.layout_down new_room); *)
       (* pre (Print.layout_down (Layout.top_house screen)); *)

       (* TODO si on créé plusieurs select dans la même page, le deuxième
            va être tracé sur un layer + élevé que celui du screen du
            premier select... et donc il va PAS être caché correctement par
            ce screen: bref le premier menu ne va pas se fermer quuand on
            clique sur le deuxième... *)
    )
    |> Sync.push;
  end;

  tmp_dst;;

(* TODO faire un clip si trop grand *)
