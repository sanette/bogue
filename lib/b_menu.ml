(** a generic menu layout with submenus *)
(* can be used with entries (layouts) at arbitrary locations *)
(* VERSION2 *)


open B_utils
open Tsdl
module Layout = B_layout
module Widget = B_widget
module Avar = B_avar
module Chain = B_chain
module Timeout = B_timeout
module Trigger =  B_trigger
module Draw = B_draw
module Label = B_label
module Button = B_button
module Popup = B_popup
module Style = B_style

let pre = if !debug
  then fun s -> print_endline ("[Menu2] " ^ s) (* for local debugging *)
  else nop

module Engine = struct

  (* A menu is a usual birectional tree, where each node is either terminal (a
     leaf) and corresponds to a menu item with a action, or a submenu. However,
     we don't really have to optimize functions for arbitrary trees, because it
     will always be a very small tree (not deep).

     The top of this tree is of type 'menu' and is the only one with a 'None'
     parent_entry. *)
  
  type action = unit -> unit

  and entry_type =
    | Menu of menu
    | Action of action

  and entry = {
      kind : entry_type;
      enabled: bool;
      mutable selected: bool; (* equivalent to highlighted *)
      layout : Layout.t; (* how to display the entry label *)
      (* Note: a Separator should be an empty Layout *)
      parent_menu : menu 
    }

  and menu = {
      pos :  (int * int) option;
      (* Relative position wrt the parent_entry *)
      mutable active : bool;
      (* 'active' implies that the menu is shown. But a menu can be shown
         without being active. Active implies that submenu will open on
         mouse_over, and keyboard is active. *)
      mutable always_shown : bool;
      (* If a menu is shown, it must be either 'active', or 'always_shown'. *)
      (* some menus (typically a menu bar, for instance) are always shown, but
       not necessary always 'active' in the sense above. *)
      mutable entries : entry list;
      mutable room : Layout.t; (* the layout that contains all menu entries *)
      mutable parent_entry : entry option
      (* the entry to which this menu is attached, or None if this is the top
         menu. *)
    }

  let separator = Action (fun () ->
                      pre "This action should not be launched.")

  (* 1. Functions for gearing menus interaction *)
  (* ------------------------------------------ *)

  let duration = 200
  (* Duration of animations in ms. *)

  (* The 'screen' layout is used for grabbing mouse even outside of the menus
     themselves. Used for closing menus when clicking outside. *)
  let screen_enable screen =
    pre "ENABLE";
    Layout.set_show screen true
    
  let screen_disable screen =
    pre "DISABLE";
    Layout.set_show screen false

  let entry_is_open entry =
    match entry.kind with
    | Action _ -> false
    | Menu menu -> menu.active || menu.always_shown

  (* TODO don't change bg in case of custom layout?*)
  let set_entry_bg ?bg entry =
    if entry.enabled then Layout.set_background entry.layout bg

  (* the entry below mouse should always be highlighted. But we also highlight
     the parent of each open menu. *)
  let highlight_entry ?(bg=Layout.Solid Draw.(opaque menu_hl_color)) entry =
    set_entry_bg ~bg entry;
    entry.selected <- true

  let reset_entry ?(bg=Layout.Solid Draw.(opaque menu_bg_color)) entry =
    set_entry_bg ~bg entry;
    entry.selected <- false
    
  (* Iter menu downwards *)
  let rec iter f menu =
    f menu;
    List.iter (fun entry -> match entry.kind with
                            | Action _ -> ()
                            | Menu submenu -> iter f submenu) menu.entries

  (* not used *)
  let add_submenus_to_dst_old ~dst menu =
    let f menu =
      Layout.add_room ~dst menu.room;
      if not menu.active && not menu.always_shown
      then Layout.set_show menu.room false

    in
    List.iter (fun entry -> match entry.kind with
                            | Action _ -> ()
                            | Menu submenu -> iter f submenu) menu.entries

  (* Inserts all layouts inside 'dst' at the proper position.  Should be done
     only once, otherwise the 'repeated widgets' error will appear. *)
  let add_menu_to_dst ~dst menu =
    let f menu =
      Layout.add_room ~dst menu.room;
      do_option menu.pos (fun (dx, dy) ->
          let x, y = match menu.parent_entry with
            | None -> 0, 0
            | Some entry ->
              let m = entry.parent_menu.room in
              let x0, y0 = Layout.(getx m, gety m) in
              let dx0, dy0 = Layout.(getx entry.layout, gety entry.layout) in
              x0+dx0, y0+dy0 in
          Layout.setx menu.room (x+dx);
          Layout.sety menu.room (y+dy));

      if not menu.active && not menu.always_shown
      then Layout.set_show menu.room false
    in
    iter f menu

  let add_menu_to_layer menu layer =
    let f menu =
      Layout.global_set_layer menu.room layer in
    iter f menu

  (* Return the top menu *)
  let rec top menu =
    pre "TOP";
    match menu.parent_entry with
    | None -> menu
    | Some entry -> top entry.parent_menu

  let is_top menu =
    menu.parent_entry = None

  (* Search menu entries for selected entry *)
  let selected_entry menu =
    list_findi (fun a -> a.selected) menu.entries 
      
  (* Search the top tree for the first (which should be unique) entry of Action
     kind which is 'selected'. Is there a simpler way to loop? *)
  let selected_action_entry menu =
    let rec menuloop menu =
      let check entry =
        if entry.selected then
          match entry.kind with
          | Action _ -> Some entry
          | Menu menu -> menuloop menu
        else None in
      let rec entriesloop = function
        | [] -> None
        | e::rest -> match check e with
                     | Some e' -> Some e'
                     | None -> entriesloop rest in
      entriesloop menu.entries in
    menuloop (top menu)

  (* use this for opening menus, not for closing *)
  let new_timeout, clear_timeout =
    let t = ref None in
    (* there is only one global timeout variable because we assume only one user
       can use only one menu at a time... *)
    (function action ->
       do_option !t Timeout.cancel;
       t := Some (Timeout.add 150 action)),
    
    (function () ->
       do_option !t Timeout.cancel)
    
  let show screen menu =
    screen_enable screen;
    Layout.show ~duration menu.room;
    (* Layout.rec_set_show true menu.room; *)
    Layout.fade_in ~duration menu.room
    
  let activate ?(timeout = false) screen menu =
    if menu.active then ()
    else begin
        if not menu.always_shown
        then if timeout
             then new_timeout (fun () -> show screen menu)
             else show screen menu;
        menu.active <- true
      end

  let close ?(timeout = false) screen menu =
    pre "CLOSE";
    (* If the parent of this menu is the top menu, this should mean that we have
       no other open menus. We can disable the screen. *)
    do_option menu.parent_entry
      (fun e ->
        if is_top e.parent_menu then screen_disable screen;
        reset_entry e
      );
    if not menu.always_shown && menu.active then
      begin
        menu.active <- false;
        clear_timeout ();
        let action () = 
          Layout.hide ~duration ~towards:Avar.Top menu.room;
          (* il y peut y avoir des bugs qd on ouvre/ferme vite. *)
          Layout.fade_out ~duration menu.room in
        if timeout
        then ignore (Timeout.add 150 action) (* put 1000 for easy debugging *)
        else action ()
      end

  (* We could make it more efficient and stop going down a branch as soon as a
     node is aleady closed. But a Menu tree is never very long, it's probably
     not worth. *)
  let rec close_children ?(timeout = false) screen menu =
    pre (Printf.sprintf "CLOSE_CHILDREN with %i ENTRIES"
                     (List.length menu.entries));
    List.iter (fun entry ->
        match entry.kind with
        | Action _ -> ()
        | Menu m -> begin
            close_children ~timeout screen m;
            close ~timeout screen m
          end
      ) menu.entries

  (* Close all closable menus, and un-activate the top menu *)
  let close_tree screen menu =
    pre "CLOSE_TREE";
    let t = top menu in
    close_children screen t;
    t.active <- false

  let close_entry ~timeout screen entry =
    match entry.kind with
    | Action _ -> ()
    | Menu m ->
       close ~timeout screen m;
       close_children ~timeout screen m
    
  (* Close the other menus at the same level *)
  let close_others ?(timeout = false) screen entry =
    let menu = entry.parent_menu in
    let other_entries = List.filter
                          (fun e -> not Layout.(e.layout == entry.layout))
                          menu.entries in
    pre (Printf.sprintf "OTHER ENTRIES = %i" (List.length other_entries));
    List.iter (close_entry ~timeout screen) other_entries

  let run_action screen entry =
    match entry.kind with
    | Menu _ -> printd debug_error "Cannot run action on a Menu entry"
    | Action action ->
      let bg = Layout.Solid Draw.(opaque Button.color_on) in
      reset_entry ~bg entry;
      action ();
      (* We use a Timeout to make the colored entry visible longer. Warning: it
         is possible that the menu state be scrambled if the user is fast enough
         to do things in the Timeout delay...*)
      ignore (Timeout.add 100 (fun () ->
          reset_entry entry; (* reset usual background *)
          close_tree screen entry.parent_menu))
    
  (* Ask the board to set keyboard (and hence mouse) focus on the entry. *)
  let set_keyboard_focus entry_layout =
    let filter = Layout.get_rooms entry_layout
                 |> List.rev
                 |> List.hd in
    if !debug then assert (filter.Layout.name = Some "filter");
    Layout.claim_keyboard_focus filter
    
  (* 2. Functions for reacting to events *)
  (* ----------------------------------- *)
    
  (* The behaviour we code here is more or less the same as QT/KDE apps. It's
     not exactly the same as GTK apps. *)
      
  (* button_down can open/close menus. It also toggles the 'active' state of the
     parent menu, which is reponsible for opening submenus on mouse over or not,
     and works only if the parent menu is 'always_shown'. *)
  let button_down screen entry =
    pre "BUTTON_DOWN";
    if entry.enabled then begin
      match entry.kind with
      | Menu menu -> if menu.active
        then begin
          close_children screen entry.parent_menu;
          highlight_entry entry;
          (* because closing menu will also reset the parent
             entry. We don't want this here since the mouse is
             over. *)
          if entry.parent_menu.always_shown
          then entry.parent_menu.active <- false
        end else begin
          pre (B_print.layout_down entry.layout);
          set_keyboard_focus entry.layout;
          activate screen menu;
          activate screen entry.parent_menu
        end
      | Action _ -> () (* actions are executed on button_up *)
    end

  let button_up screen entry =
    pre "BUTTON_UP";
    (* the entry here is maybe the wrong one, because it is the one that has
       'focus' in the sense of main.ml, not necessarily the highlighted entry,
       due to 'drag' mechanism: if the user clicked on some entry, and then
       moved to another without letting the button up.  So we switch:*)
    let entry = default (selected_action_entry entry.parent_menu) entry in
    if entry.enabled then begin
      match entry.kind with
      | Menu _ -> () (* menus are open/closed on button_down or mouse_over *)
      | Action _ -> run_action screen entry 
    end
              
  (* mouse_enter (and mouse_motion?). mouse_motion will be useful only when we
     add keyboard support. WARNING: when touching a new entry, both mouse_enter
     and button_down are triggered... so the menu opens and then quickly
     closes...(not anymore, why?) *)
  let mouse_over screen entry =
    pre "MOUSE_OVER";
    if entry.enabled && not entry.selected then begin
      highlight_entry entry;
      close_others ~timeout:true screen entry;
      set_keyboard_focus entry.layout; (* mettre dans le timeout *)
      match entry.kind with
      | Menu menu ->
        if (not menu.active) && entry.parent_menu.active
        then begin
          activate ~timeout:true screen menu;
        end
      | Action _ -> ()
    end
    
  let mouse_leave entry =
    pre "MOUSE_LEAVE";
    if entry.enabled then begin
      if not (entry_is_open entry) then reset_entry entry;
      if entry.parent_menu.active
      then match entry.kind with
        | Menu _ -> ()
        (* if menu.active then close screen menu *)
        | Action _ -> ()
    end

  (* Keyboard navigation. The main entry keeps keyboard_focus while navigating
     its submenus. *)
  (* TODO here we use up/down as if the menu were vertical. What about if the
     menu is horizontal, or even custom?? *)
  let key_down screen entry keycode =
    pre "KEY_DOWN";
    if keycode = Sdl.K.escape then close_tree screen entry.parent_menu
    else if entry.enabled then 
      if keycode = Sdl.K.return then begin
        match entry.kind with
        | Menu menu -> 
          (* 1/ouvrir 2/selectionner premier *)
          if menu.active
          then set_keyboard_focus (List.hd menu.entries).layout
          (* vérifier liste non vide ? *)
          else activate screen menu          
        | Action _ -> run_action screen entry
      end else
      if keycode = Sdl.K.up || keycode = Sdl.K.down then
        match selected_entry entry.parent_menu with
        | None -> printd debug_error "Cannot find selected entry in menu!"
        | Some (_,i0) ->
          pre (string_of_int i0);
          let n = List.length entry.parent_menu.entries in
          let rec loop i (* search enabled entry upwards *) =
            let i = (if keycode = Sdl.K.up
                     then (i-1+n)
                     else i+1) mod n  in
            let new_entry = List.nth entry.parent_menu.entries i in
            if new_entry.enabled then new_entry
            else if i = i0 then entry
            else loop i in
          let new_entry = loop i0 in
          set_keyboard_focus new_entry.layout

  (* 3. Creation of widgets and connections. *)
  (* --------------------------------------- *)
    
  (* First we must coat all entry layouts using the Popup module, in order to
     get the correct mouse focus. This means that menus will be drawn on a
     separate layer. The coat has a widget (either Empty of Box) that will
     handle the connections. *)

  let connect_entry screen layer entry =
    (* 'layer' is the coating layer *)
    let coat = Popup.filter_screen ~keyboard_focus:false ~layer entry.layout in
    (* We need a coat to get mouse focus on the whole length of the menu entry,
       not only on the area of the text itself (label). *)
    Layout.add_room ~dst:entry.layout coat;
    (* we don't use Popup.add_screen to avoid creating too many layers. *)
    let widget = Layout.widget coat in
    Widget.set_cursor widget
      (Some (go (Draw.create_system_cursor Sdl.System_cursor.hand)));

    let action _ _ _ = button_down screen entry in
    let c = Widget.connect_main widget widget action Trigger.buttons_down in
    Widget.add_connection widget c;

    let action _ _ _ = button_up screen entry in
    let c = Widget.connect_main widget widget action Trigger.buttons_up in
    Widget.add_connection widget c;

    let action _ _ _ = mouse_over screen entry in
    let c = Widget.connect_main widget widget action
        [(* Trigger.E.mouse_motion; *) Trigger.mouse_enter] in
    (* Warning do NOT add finger_motion, it will interfere with finger_down.
       TODO finger doesn't work well yet. *)
    Widget.add_connection widget c;

    let action _ _ _ = mouse_leave entry in
    let c = Widget.connect_main widget widget action [Trigger.mouse_leave] in
    Widget.add_connection widget c;

    let action _ _ ev = key_down screen entry
        Sdl.Event.(get ev keyboard_keycode) in
    let c = Widget.connect_main widget widget action [Trigger.E.key_down] in
    Widget.add_connection widget c
    
  let rec connect_loop screen layer menu =
    List.iter (fun entry ->
        if Layout.get_rooms entry.layout <> []
        then connect_entry screen layer entry;
        (* :we don't connect the separators *)
        match entry.kind with
        | Menu submenu -> connect_loop screen layer submenu
        | Action _ -> ()
      ) menu.entries

  (* Init, attach the menu to a destination layout. *)

  let init ~dst t =
  let dst_layer = Chain.last (Layout.get_layer dst) in
  let entry_layer = Popup.new_layer_above dst_layer in
  add_menu_to_layer t entry_layer;
  let coating_layer = Popup.new_layer_above entry_layer in
  
  (* the screen is used to grab all mouse focus outside of the submenus while
     they are open *)
  let screen = Popup.filter_screen ~layer:entry_layer
      (* ~color:Draw.(more_transp (transp green)) *) (* DEBUG*) dst in
  (* Le screen couvre tout ce qui est actuellement tracé, y compris le menu,
     mais les connexions pour les entrées de menu sont sur le coating_layer, qui
     est encore au dessus, donc ça fonctionne. TODO ça serait plus logique que
     le screen soit entre dst_layer et entry_layer. Ou alors le mettre AVANT les
     entries pour qu'il soit recouvert par elles (c'est le contraire
     actuellement). ATTENTION si un deuxième menu est construit après, il sera
     affiché AU DESSUS de ce screen... *)
  (* TODO one could reserve a special layer for some usual menu types, like menu
     bar on the main layout, and make sure this layer is always above anything
     else. OU ALORS: définir le screen de façon dynamique quand on clique. *)
  connect_loop screen coating_layer t;
  add_menu_to_dst ~dst t;
  
  screen_disable screen;
  Layout.add_room ~dst screen;
  
  let w = Layout.widget screen in
  Widget.on_click ~click:(fun _ -> pre "CLICK SCREEN";
      close_tree screen t
      (* screen_disable screen *)) w;

end

(* Now we can make a friendly API for creating elements of the menu type. *)
(* ---------------------------------------------------------------------- *)
              
(* example:
   let file = Tower [{label = (Text "open"); content = (Action open_in)};
   etc...] in
   let edit = ... in
   Flat [     
   {label = (Text "File"); content = (Menu file)}; 
   {label = (Text "Edit"); content = (Menu edit)};
   etc... ]
*)

type t = Engine.menu
          
type action = unit -> unit
            
type label =
  | Text of string
  | Layout of Layout.t
(* The user (programmer) can either define the menu entry by a text -- like
   'File', etc. or directly by an arbitrary layout -- useful for game menus, for
   instance. In the latter case, the layout content is not altered to ensure
   that its features, whether it is part of a menu or not, are not
   altered. However, we cannot preserve its house, because usually the menu is
   relocated into the main window-layout. One can 'kind-of' preserve the house
   by letting it be the 'dst' parameter. But warning, in all cases, the layout
   will be encapsulated into a screen, so the 'dst' will not remain its "direct
   house". *)
            
type entry = {
  label : label;
  content : content }
(* TODO: add "hover" field to execute an action on hovering the entry (useful
   for games). Mieux: ajouter "connection" field? *)

(* the content type mixes two different things: Actions and submenus. Not clean
   from the point of view of the library programmer (me), but (I think) simpler
   from the 'public' viewpoint. Thus, before working with this, we convert into
   the Engine types. *)
and content =
  | Action of action
  | Flat of entry list
  | Tower of entry list
  | Custom of entry list
  | Separator

let separator = { label = Text "Dummy separator label"; content = Separator }

let text_margin = 5
                
(* Text to Layout. w and h are only used for text. maybe remove *)
let format_label ?w ?h = function
  | Text s ->
    let res = Layout.resident ?w ?h (Widget.label s) in
    (* : here we cannot use a resident as is because we will need to add another
       room later. we need to wrap it: *)
    let background = Layout.Solid Draw.(opaque menu_bg_color) in
    Layout.flat ~name:"menu entry label"
      ~margins:text_margin ~background [res]
  | Layout l ->
    let name = "formatted label" in
    if !debug then assert (l.Layout.name <> Some name);
    (* this function should be applied only ONCE to the label *)
    Layout.superpose ~name [l] (* We preserve the (x,y) position. *)
     

(* Warning, does not check whether there is already an icon... *)
let add_icon_suffix ?(icon = "caret-right") layout =
  (* the icon used to indicate submenus *)
  let submenu_indicator = Layout.resident ~name:icon (Widget.icon icon) in
  Layout.add_room ~dst:layout ~valign:Draw.Center ~halign:Draw.Max
    submenu_indicator

(* really private, hackish, function...to call only after connections/filters
   have been added.  It relies on the fact that the icon should be the 2nd-to
   last room of the list (the last one being the filter). Does not raise
   anything in case of error. *)
let remove_icon_suffix ?(icon = "caret-right") layout =
  try begin
    Layout.iter_rooms (fun l -> pre (Layout.sprint_id l)) layout;
    match List.rev (Layout.get_rooms layout) with
    | []
    | [_] -> ()
    | filter::(this::others) -> assert (default this.Layout.name "" = icon);
      Layout.set_rooms layout (List.rev (filter::others))
  end with
  | e -> printd debug_error "Menu: Cannot remove icon suffix";
    raise e

let suffix_width = 10 (* TODO compute this *)
                          
module Tmp = struct
  (* We temporarily convert to a more programmer-friendly type, before
     converting to Engine.menu.  This type also carry more information
     (eg. suffix)that can be modified for a customizable menu. *)

  (* position of the submenu wrt the parent label *)
  type position =
    | Below
    | RightOf

  type menukind =
    | Flat
    | Tower
    | Custom
    
  type menu =
    { entries : tentry list;
      kind : menukind
    }

  and tcontent =
    | Action of action
    | Menu of menu
    | Separator

  and tentry = {
    label : label; (* ignored in case of Separator *)
    content : tcontent;
    mutable formatted : bool;
    mutable suffix : position option } (* TODO add keyboard shortcuts *)
             
  let get_layout entry =
    if !debug then assert entry.formatted;
    match entry.label with
    | Text _ -> failwith "get_layout should be called only when the Layout is \
                          generated. BUG."
    | Layout l -> l

  let compute_suffix entry =
    do_option entry.suffix (fun p ->
        let icon = match p with
          | Below -> "caret-down"
          | RightOf -> "caret-right" 
        in
        match entry.content with
        | Menu _ -> add_icon_suffix ~icon (get_layout entry)
        | _ -> ()
      )

  let next_submenu_position_old = function
    | Flat -> pre "BELOW"; Some Below
    | Tower -> pre "RIGHTOF"; Some RightOf
    | Custom -> pre "NONE"; None

  (* Return a copy of the tree with all Text labels replaced by Layouts *)
  let rec compute_layouts entry =
    let layout =
      if entry.formatted
      then get_layout entry
      else match entry.content with
        | Separator->
          let background = Layout.Solid Draw.(opaque grey) in
          Layout.empty ~background ~w:10 ~h:1 ()
        | Menu _ 
        | Action _ ->  format_label entry.label
    in
    if not entry.formatted && entry.suffix <> None
    then Layout.(set_width layout (width layout + suffix_width));
    (* we make some room for adding the suffix later *)

    let content = match entry.content with
      | Action _ -> entry.content
      | Menu menu -> let entries =
                       List.map compute_layouts menu.entries in
        Menu {menu with entries}
      | Separator -> Separator
    in
    { label = Layout layout;
      content;
      formatted = true;
      suffix = entry.suffix}

                
  let menu_formatter = function 
    | Flat -> (fun list ->
        let background = Layout.Solid Draw.(opaque menu_bg_color) in
        let shadow = Style.shadow ~offset:(1,1) ~size:1 () in
        Layout.flat ~margins:0 ~background ~shadow list)
    | Tower -> (fun list ->
        let shadow = Style.shadow ~offset:(1,1) ~size:1 () in
        let background = Layout.Solid Draw.(opaque menu_bg_color) in
        let l = Layout.tower ~margins:0 ~sep:0 ~background ~shadow list in
        Layout.expand_width l; l)
    | Custom  -> (fun list -> Layout.superpose list)
                 
  (* Return (x,y) option, the coordinates where the submenu should be placed
     when positioned in the same layout as the parent layout. *)
  let submenu_pos parent position = 
    let w, h = Layout.get_size parent in 
    map_option position
      (function | Below -> (0, h)
                | RightOf -> (w, 0))
    
  let get_entries = function
    | Menu menu -> menu.entries
    | _ -> pre "get_entries should be called only with Menu."; []

  (* Compute the room containing the menu. *)
  let compute_room menu =
    let layouts = List.map get_layout menu.entries in
    let room = menu_formatter menu.kind layouts in
    room

  (* Convert an entry to an Engine.entry. Warning, this is not an obvious
     function, because Engine.entry is bidirectional, and hence cannot be
     created by a simple recursive loop. We need to use mutability: some fields
     are filled in later. *)
  (* This should be called on a well prepared entry tree where all labels are
     layouts. *)
  (* 'position' indicates where to put the submenu in case entry has a
     submenu. *)
  let rec entry_to_engine parent_menu entry =
    let layout = get_layout entry in
      (* We add the suffixes, except for the first entry, which is dummy, see
         create_engine below.  *)
      if not (Engine.is_top parent_menu) then compute_suffix entry;
    let kind = match entry.content with
      | Action a -> Engine.Action a
      | Separator -> Engine.separator
      | Menu menu ->
        let room = compute_room menu in
        let pos = submenu_pos layout entry.suffix in
        let engine_menu = Engine.{
            pos;
            active = false;
            always_shown = false;
            entries = []; (* will be inserted later *)
            room;
            parent_entry = None} in
        Engine.Menu engine_menu in
    let engine_entry = Engine.{kind;
                               enabled = entry.content <> Separator;
                               selected = false;
                               layout;
                               parent_menu} in
    (* second pass to recursively insert the entries field *)
    let _ = match engine_entry.Engine.kind with
      | Engine.Action _ -> ()
      | Engine.Menu menu ->
        menu.Engine.parent_entry <- Some engine_entry;
        let entries = List.map (entry_to_engine menu)
            (get_entries entry.content) in
        menu.Engine.entries <- entries;
    in
    engine_entry
    
  (* Create an Engine.menu from a content *)
  let create_engine = function
    | Action _ -> failwith "Cannot create a menu from an Action content."
    | content ->
       let dummy_parent = Layout.empty ~name:"dummy parent" ~w:0 ~h:0 () in
       let entry = compute_layouts {label = Layout dummy_parent;
                                    content;
                                    formatted = true;
                                    suffix = None} in
       let parent_menu = Engine.{pos = None; active = true; always_shown = true;
                                 entries = []; room = dummy_parent;
                                 parent_entry = None} in
       let eentry = entry_to_engine parent_menu entry in
       let open Engine in
       let menu = match eentry.kind with
         | Action _ -> failwith "An Action should not show up here. BUG."
         | Menu menu -> menu in
       menu.Engine.always_shown <- true;
       menu.Engine.parent_entry <- None; (* remove the dummy parent *)
       menu

         (* TO BE CONTINUED... *)

       
end


let next_entry_position = function
  | Custom _
  | Separator
  | Action _ -> None
  | Tower _ -> Some Tmp.RightOf
  | Flat _ -> Some Tmp.Below
              
(* Convert to the Tmp type, guessing a standard suffix *)
let rec content_to_tmp position = function
  | Action a -> Tmp.Action a
  | Flat list ->
    let entries = List.map (entry_to_tmp position) list in
    Tmp.(Menu {entries; kind = Flat})
  | Tower list ->
    let entries = List.map (entry_to_tmp position) list in
    Tmp.(Menu {entries; kind = Tower})
  | Custom list ->
    let entries = List.map (entry_to_tmp position) list in
    Tmp.(Menu {entries; kind = Custom})
  | Separator -> Tmp.Separator

and entry_to_tmp position entry =
  let next_position = next_entry_position entry.content in
  { Tmp.label = entry.label;
    Tmp.content = content_to_tmp next_position entry.content;
    Tmp.formatted = false;
    Tmp.suffix = position
  }
                       
let layout_of_menu menu : Layout.t =
  menu.Engine.room    

let set_layout menu room =
  menu.Engine.room <- room
  
let raw_engine content =
  let position = next_entry_position content in
  let tcontent = content_to_tmp position content in
  Tmp.create_engine tcontent

let make_engine ~dst content =  
  let t = raw_engine content in
  Engine.init ~dst t;
  t
  
(* Create a generic menu layout and insert it into the dst layout. *)
(* let create ~dst content =
 *   let t = make_engine ~dst content in
 *   layout_of_menu t *)
let create = make_engine

(* Specific "menu bar" creation *)
let bar ~dst entries =
  let content = Flat entries in
  let t = make_engine ~dst content in
  let room = layout_of_menu t in
  Layout.(set_width room (width dst));
  (* expand first entry (menu bar) to the whole dst width. *)
  Layout.set_shadow room None;
  (* for a menu bar, we usually don't want indicator icons *)
  List.iter (fun entry ->
      let open Engine in
      match entry.kind with
      | Menu _ -> remove_icon_suffix ~icon:"caret-down" entry.layout
      | Action _ -> ())
    t.Engine.entries

