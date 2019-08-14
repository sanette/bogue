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
      layout : Layout.t;
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
      room : Layout.t; (* the layout that contains all entries *)
      mutable parent_entry : entry option
      (* the entry to which this menu is attached, or None if this is the top
         menu. *)
    }

  let separator = Action (fun () ->
                      print_endline "This action should not be launched.")

  (* 1. Functions for gearing menus interaction *)
  (* ------------------------------------------ *)

  (* The 'screen' layout is used for grabbing mouse even outside of the menus
     themselves. Used for closing menus when clicking outside. *)

  let duration = 200
  (* Duration of animations in ms. *)
               
  let screen_enable screen =
    print_endline "ENABLE";
    Layout.set_show screen true
    
  let screen_disable screen =
    print_endline "DISABLE";
    Layout.set_show screen false

  let entry_is_open entry =
    match entry.kind with
    | Action _ -> false
    | Menu menu -> menu.active || menu.always_shown
                 
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
  let add_submenus_to_dst ~dst menu =
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
    print_endline "TOP";
    match menu.parent_entry with
    | None -> menu
    | Some entry -> top entry.parent_menu

  let is_top menu =
    menu.parent_entry = None

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
    print_endline "CLOSE";
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
    print_endline (Printf.sprintf "CLOSE_CHILDREN with %i ENTRIES"
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
    print_endline "CLOSE_TREE";
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
    print_endline (Printf.sprintf "OTHER ENTRIES = %i" (List.length other_entries));
    List.iter (close_entry ~timeout screen) other_entries

    
    
  (* 2. Functions for reacting to events *)
  (* ----------------------------------- *)
    
  (* The behaviour we code here is more or less the same as QT/KDE apps. It's
     not exactly the same as GTK apps. *)
      
  (* button_down can open/close menus. It also toggles the 'active' state of the
     parent menu, which is reponsible for opening submenus on mouse over or not,
     and works only if the parent menu is 'always_shown'. *)
  let button_down screen entry =
    print_endline "BUTTON_DOWN";
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
                           activate screen menu;
                           activate screen entry.parent_menu
                         end
        | Action _ -> () (* actions are executed on button_up *)
      end

  let button_up screen entry =
    print_endline "BUTTON_UP";
    (* the entry here is maybe the wrong one, because it is the one that has
       'focus' in the sense of main.ml, not necessarily the highlighted entry,
       due to 'drag' mechanism: if the user clicked on some entry, and then
       moved to another without letting the button up.  So we switch:*)
    let entry = default (selected_action_entry entry.parent_menu) entry in
    if entry.enabled then begin
        match entry.kind with
        | Menu _ -> () (* menus are open/closed on button_down or mouse_over *)
        | Action action -> begin
            let bg=Layout.Solid Draw.(opaque Button.color_on) in
            reset_entry ~bg entry;
            action ();
            (* We use a Timeout to make the colored entry visible
               longer. Warning: it is possible that the menu state be scrambled
               if the user is fast enough to do things in the Timeout delay...*)
            ignore (Timeout.add 100 (fun () -> close_tree screen entry.parent_menu))
          end
      end
              
  (* mouse_enter (and mouse_motion?). mouse_motion will be useful only when we
     add keyboard support. PROBLEM: menu should not open when using
     touch. Because when touching a new entry, both mouse_enter and button_down
     are triggered... so the menu opens and then quickly closes... *)
  let mouse_over screen entry =
    print_endline "MOUSE_OVER";
    if entry.enabled && not entry.selected then begin
        highlight_entry entry;
        close_others ~timeout:true screen entry;
        match entry.kind with
        | Menu menu ->
           if (not menu.active) && entry.parent_menu.active
           then activate ~timeout:true screen menu
        | Action _ -> ()
      end
    
  let mouse_leave entry =
    print_endline "MOUSE_LEAVE";
    if entry.enabled then begin
        if not (entry_is_open entry) then reset_entry entry;
        if entry.parent_menu.active
        then match entry.kind with
             | Menu _ -> ()
                (* if menu.active then close screen menu *)
             | Action _ -> ()
      end

  (* 3. Creation of widgets and connections. *)
  (* --------------------------------------- *)
    
  (* First we must coat all entry layouts using the Popup module, in order to
     get the correct mouse focus. This means that menus will be drawn on a
     separate layer. The coat has a widget (either Empty of Box) that will
     handle the connections. *)

  let connect_entry screen layer entry =
    (* 'layer' is the coating layer *)
    let coat = Popup.filter_screen ~layer entry.layout in
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
    Widget.add_connection widget c
    
  let rec connect_loop screen layer menu =
    List.iter (fun entry ->
        connect_entry screen layer entry;
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
  
  (* the screen is used to grab all mouse focus while the submenus are open *)
  let screen = Popup.filter_screen ~layer:entry_layer
                 (* ~color:Draw.(more_transp (transp green)) *) (* DEBUG*) dst in
  connect_loop screen coating_layer t;
  add_menu_to_dst ~dst t;
  
  screen_disable screen;
  Layout.add_room ~dst screen;
  
  let w = Layout.widget screen in
  Widget.on_click ~click:(fun _ -> print_endline "CLICK SCREEN";
      close_tree screen t
      (* screen_disable screen *)) w;

end

(* Now we can make a friendly API for creating elements of the menu type. *)

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

type entry = {
    label : label;
    content : content }

(* the content type mixes two different things: Actions and submenus. Not clean
   from the point of view of the programmer, but (I think) simpler from the
   public viewpoint. Thus, before working with this, we convert into the Engine
   types. *)
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
     Layout.flat ~margins:text_margin ~background [res]
  | Layout l -> 
     if Layout.has_resident l
     then Layout.flat ~margins:0 [l]
     else l

let add_icon_suffix layout =
  let submenu_icon = "caret-right" in
  (* the icon used to indicate submenus *)
  let submenu_indicator = Layout.resident (Widget.icon submenu_icon) in
  Layout.add_room ~dst:layout ~valign:Draw.Center ~halign:Draw.Max
    submenu_indicator

module Tmp = struct
  (* We temporarily convert to a more programmer-friendly type, before
     converting to Engine.menu.  *)

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
      content : tcontent }
             
  (* position of the submenu wrt the parent label *)
  type position =
    | Below
    | RightOf

  let get_layout entry =
    match entry.label with
    | Text _ -> failwith "get_layout should be called only when the Layout is \
                          generated. BUG."
    | Layout l -> l

  let compute_suffix entry =
    match entry.content with
    | Menu { kind = Tower; _ } -> add_icon_suffix (get_layout entry)
    | _ -> ()
                             
  (* Return a copy of the tree with all Text labels replaced by Layouts *)
  let rec compute_layouts entry =
    let layout = match entry.content with
      | Separator->
         let background = Layout.Solid Draw.(opaque grey) in
         Layout.empty ~background ~w:10 ~h:1 ()
      | Menu _ 
        | Action _ -> format_label entry.label
    in
    let content = match entry.content with
      | Action _ -> entry.content
      | Menu menu -> let entries =
                       List.map compute_layouts menu.entries in
                     Menu {menu with entries}
      | Separator -> Separator
    in
    { label = Layout layout; content }

                
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
    
  let next_submenu_position = function
    | Flat -> print_endline "BELOW"; Some Below
    | Tower -> print_endline "RIGHTOF"; Some RightOf
    | Custom -> print_endline "NONE"; None

  let get_entries = function
    | Menu menu -> menu.entries
    | _ -> print_endline "get_entries should be called only with Menu."; []

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
  let rec entry_to_engine position parent_menu entry =
    let layout = get_layout entry in
    let kind, position = match entry.content with
      | Action a -> Engine.Action a, None
      | Separator -> Engine.separator, None
      | Menu menu ->
         let room = compute_room menu in
         (* Now we add the suffixes: *)
         if not (Engine.is_top parent_menu)
         then List.iter compute_suffix menu.entries;
         let pos = submenu_pos (get_layout entry) position in
         let engine_menu = Engine.{
               pos;
               active = false;
               always_shown = false;
               entries = []; (* will be inserted later *)
               room;
               parent_entry = None} in
         Engine.Menu engine_menu, next_submenu_position menu.kind in
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
         let entries = List.map (entry_to_engine position menu)
                         (get_entries entry.content) in
         menu.Engine.entries <- entries;
    in
    engine_entry
    
  (* Create an Engine.menu from a content *)
  let create_menu = function
    | Action _ -> failwith "Cannot create a menu from an Action content."
    | content ->
       let dummy_parent = Layout.empty ~w:0 ~h:0 () in
       let entry = compute_layouts {label = Layout dummy_parent; content} in
       let parent_menu = Engine.{pos = None; active = true; always_shown = true;
                                 entries = []; room = dummy_parent;
                                 parent_entry = None} in
       let eentry = entry_to_engine None parent_menu entry in
       let open Engine in
       let menu = match eentry.kind with
         | Action _ -> failwith "An Action should not show up here. BUG."
         | Menu menu -> menu in
       menu.Engine.always_shown <- true;
       menu.Engine.parent_entry <- None; (* remove the dummy parent *)
       menu

         (* TO BE CONTINUED... *)

       
end
           
(* Convert to the Tmp type *)
let rec content_to_tmp = function
  | Action a -> Tmp.Action a
  | Flat list ->
     let entries = List.map entry_to_tmp list in
     Tmp.(Menu {entries; kind = Flat})
  | Tower list ->
     let entries = List.map entry_to_tmp list in
     Tmp.(Menu {entries; kind = Tower})
  | Custom list ->
     let entries = List.map entry_to_tmp list in
     Tmp.(Menu {entries; kind = Custom})
  | Separator -> Tmp.Separator
     
and entry_to_tmp entry =
  { Tmp.label = entry.label;
    Tmp.content = content_to_tmp entry.content
  }
                       
let layout_of_menu menu : Layout.t =
  menu.Engine.room    

let create ~dst content =
  let tcontent = content_to_tmp content in
  let t = Tmp.create_menu tcontent in
  Engine.init ~dst t;
  let room = layout_of_menu t in
  let () = match content with
    | Flat _ -> Layout.(set_width room (width dst))
    (* if the first menu is a Flat, we assume we want a menu bar, and hence make
       it fill the whole width. *)
    | _ -> () in
  room
