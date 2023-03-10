(* This file is part of BOGUE, by San Vu Ngoc *)

(* Layout is the main object type. *)

(* a layout is a 'box' which can contain 'sub-boxes'. We use the
   terminology of houses: a house contains several rooms. Each room
   can be viewed as a house which contains other rooms etc. Thus, this
   is a simple graph with variable degree. A leaf (a room which does
   not contain subrooms) is called a resident; it contains a
   Widget. In the whole (connected) tree, the summit is the main
   layout: the only one which does not belong to any house; it is
   called the top_house, and corresponds to a "physical" SDL window.
   The size of the SDL window should always match the size of the
   top_house. *)

(* Warning: a widget should *not* appear twice (or more) inside a
   Layout. Otherwise, the results are not going to be satisfactory: a
   widget is associated to a geometry in a layout. Instead one should
   use two differents widgets with a connection between them to
   synchronize the data *)


open Tsdl
open B_utils
module Widget = B_widget
module Avar = B_avar
module Chain = B_chain
module Theme = B_theme
module Time = B_time
module Var = B_var
module Tvar = B_tvar
module Trigger = B_trigger
module Sync = B_sync
module Draw = B_draw
module Mouse = B_mouse
module Style = B_style
module Box = B_box
module Slider = B_slider
module Selection = B_selection

type background =
  (* TODO instead we should keep track of how the box was
     created... in case we want to recreate (eg. use it for another
     window...?) *)
  | Style of Style.t
  | Box of Box.t

let color_bg color =
  Style (Style.(of_bg (color_bg color)))

let opaque_bg color = color_bg Draw.(opaque color)

let theme_bg = opaque_bg @@ Draw.find_color Theme.bg_color

let style_bg s =
  Style s

let box_bg b =
  Box b

type adjust =
  | Fit
  | Width
  | Height
  | Nothing

type transform = {
    angle : float Avar.t;
    center : (Sdl.point option) Avar.t;
    flip : Sdl.flip Avar.t;
    alpha : float Avar.t
  }

type geometry = {
    x : int Avar.t;
    y : int Avar.t;
    (* The (x,y) coords define the position of the layout wrt its
       container (the house). Origin is top-left. *)
    w : int Avar.t;
    h : int Avar.t;
    voffset : (int Avar.t) Var.t;
    (* The [voffset] is the vertical offset = the y value of where the content of
       the layout will be drawn. It is typically used for scrolling. It is similar
       to the 'y' variable', except that:

       1. the clipping rect (if defined) is *not* translated in case of voffset
       2. the background is not translated either *)
    transform: transform;
  }

type current_geom = {
    x : int;
    y : int;
    w : int;
    h : int;
    voffset : int
  }

(* convert between same type in Draw... *)
let to_draw_geom (g : current_geom) =
  { Draw.x = g.x; Draw.y = g.y; Draw.w = g.w; Draw.h = g.h;
    Draw.voffset = g.voffset }

type room_content =
  | Rooms of room list
  (* In principle, rooms in a house with the same layer should have
     non-intersecting geometries, otherwise it is not clear which one gets the
     mouse focus (this can be violated, eg. with Layout.superpose). Popups are
     drawn on a different layer *)
  | Resident of Widget.t

and room = {
    id : int; (* unique identifier *)
    name : string option;
    (* If needed for debugging, one can give a name to the room. *)
    lock : Mutex.t;
    (* Lock for concurrent access by several threads. *)
    mutable thread_id : int;
    (* Id of the thread holding the lock. TODO use Var.t instead. *)
    adjust : adjust;
    (* should we adjust the size of this room to fit its content? *)
    (* not implemented yet *)
    mutable resize : ((int * int) -> unit);
    (* The [resize] function is called when the house changed size. (int * int)
       is the house size (w,h). *)
    mutable show : bool; (* should we show this room? *)
    mutable hidden : bool;
    (* The [hidden] field is only useful when [t.show = true]. Then [t.hidden =
       true] if the layout is currently not displayed onscreen. (Upon creation,
       all layouts are hidden.)  Only used to correctly detect if animations are
       running. This field is only set by the Layout.display function, it should
       not be modified by user.  Note that t.show has precedence for being
       hidden: it [t.show = false], then t is hidden no matter what [t.hidden]
       says. *)
    mutable geometry : geometry;
    (* [geometry] contains the relative geometry of the room wrt the house. All
       components are dynamic variables, that need to be recomputed at each
       iteration. Note: rooms inside a house must be physically inside the
       geometry of the house. If not, they will not be detected by the mouse,
       for instance. *)
    mutable current_geom : current_geom;
    (* [current_geom] is the current *absolute* geometry. Is updated at each
       display. But because of clip, the actual rendered size can be smaller
       than indicated size. Before the start of the main loop, it is equal to
       the initial values of the geometry field. *)
    (* A special case of current_geom.(x,y) is to specify window position for
       the top layouts. See set_window_pos *)
    mutable clip : bool;
    (* If [clip]=true, the room (and its children) will be clipped inside its
       geometry. This should be set whenever one want to scroll the content of
       the layout inside the layout. This is also used (and set) by hide/show
       animations. TODO replace this by a more flexible 'overflow' specification
     *)
    mutable background : background option;
    mutable shadow : Style.shadow option;
    mask : Sdl.surface option;
    (* If there is a mask, a position (x,y) will be declared inside the layout
       if it corresponds to a mask pixel with alpha value <> 0. A mask will act
       as a clip if it is uniformly white, and the shape is given by nonzero
       alpha values. (TODO) *)
    mutable content : room_content;
    mutable layer : Draw.layer;
    (* [layer] is the particular layer = chain element of this layout. It should
       never be an empty layer (Chain.None), except for the special layout that
       contains all windows. If a room contains other Rooms, its layer should be
       at least as deep as the layers of the Rooms, otherwise the "background"
       might end-up not being at the background... *)
    (* In principle a chain of layers is attached to a window. When creating a
       new window, one has to select a new layer chain (use_new_layer). *)
    mutable canvas : Draw.canvas option;
    (* The canvas contains the "hardware" information to render the room *)
    (* The canvas is not really an intrinsic property of the layout, it is used
       only when rendering is required. It may change "without notice" when a
       layout is copied into another window. It is first initialized by
       [make_window]. *)
    mutable house: room option;
    (* [house] = parent: this is the "room" that contains this room in his
       "Rooms". This field is mutable because of cyclic definition: one cannot
       set the house before defining it... It is our responsibility to make sure
       that the house really corresponds to the parent element, in order to
       avoid cycles etc. *)
    (* cache : Sdlvideo.surface; *) (* ou texture? mettre un cache pour
                                       accélerer l'affichage, plutôt que
                                       d'effacer tout à chaque itération ? *)
    mutable mouse_focus : bool; (* set interactively when has mouse focus *)
    mutable keyboard_focus : bool option;
    (* None = cannot have focus; Some b = has focus or not *)
    (* TODO: should we move the keyboard_focus to the Widget? A layout which
       contains a Rooms list cannot really have keyboard_focus...and in fact it
       will not be detected by 'next_keyboard' *)
    (* TODO : mutable draggable : int option; *) (* None = not draggable; Some
                                                  delay = drag after delay (in ms) *)
    mutable draggable : bool;
    (* TODO keep_focus_on_pressed: bool (default = true) CF. menu2. BUT It's not
       so easy because many layouts can cover a widget. Ideally, this property
       should belong to the widget. *)
    mutable removed : bool;
    (* [removed] is an experimental field: hint that the layout should not be
       used anymore by the board, at least temporarily. Maybe show/hidden could
       be used instead.  *)
  }

type t = room
(* The whole connected component of a layout is a tree, whose vertices (nodes)
   are rooms and leaves are widgets (=Resident). The number of branches (=Rooms
   list) from a vertex is arbitrary. The house field gives the parent of a
   vertex.

   There are several interesting ways of going through a tree:
   - through every vertex
   - only through leaves
   - only leaves at a common level (=same generation number)
   - nearest neighbour (left, right, up, or down) in the planar embedding
*)

(* We use words "room", "layout", and "house" for the same type of object.

   - "layout" will in general refer to the main house, ie containing everything
     that is displayed on the window.
   - "house" in general refers to a parent of some room, ie an object contaning
     sub-rooms.
   - "room" is the generic term for sub objects contained in the general layout.
 *)

exception Fatal_error of (t * string)
exception Found of t

(* [not_specified] is a special value used to indicate that the window position
   should be guessed by the program. *)
let not_specified = Sdl.Window.pos_undefined

let no_clip = ref false
(* The normal behaviour when a non-zero voffset is specified is to clip the
   layout to the original rectangle. This permits the show/hide
   animation. Setting [no_clip = true] can be a good idea for debugging
   graphics. *)

let draw_boxes = Widget.draw_boxes
(* this is only used for debugging. This can slow down rendering quite a bit *)

let equal r1 r2 = r1.id = r2.id
let (==) = equal

let sprint_id r =
  Printf.sprintf "#%u%s" r.id (match r.name with
      | None -> ""
      | Some s -> Printf.sprintf " (%s)" s)

module Hash = struct
  type t = room
  let equal = equal
  let hash room = room.id
end

module WHash = Weak.Make(Hash)

(* [rooms_wtable] this is a weak set of all created rooms, searchable by their
   unique id. It is weak in the sense that rooms can be reclaimed by the GC when
   not anymore in use, and automatically disappear from the set. *)
let rooms_wtable = WHash.create 50


(* [cemetery] is only for debugging: we insert here the room ids we think are
   not used anymore. Then we can check if the GC did remove them from the
   [rooms_wtable]. *)
let cemetery = ref []
let send_to_cemetery room =
  cemetery := room.id :: !cemetery

let rec remove_wtable room =
  if WHash.mem rooms_wtable room
  then begin
      printd debug_memory "Removing room %s from Wtable" (sprint_id room);
      WHash.remove rooms_wtable room;
      if WHash.mem rooms_wtable room
      then begin
          printd debug_error
            "Several instances of room %s are registered in the weak hash table."
            (sprint_id room);
          remove_wtable room;
        (* The hash can host several instances of the room. However this signals
           a bug somewhere. *)
        end;
      send_to_cemetery room;
    end

let clear_wtable () = WHash.clear rooms_wtable

(* let rooms_table : (int, room) Hashtbl.t = Hashtbl.create 50;;*)
(* this is where we store the reverse lookup: room.id ==> room *)
(* of course the problem is to free this when rooms are not used anymore... to
   prevent a memory leak. *)
(* TODO use weak tables (or Ephemerons???) *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/libref/Weak.html *)

(* let of_id id = *)
(*   try Hashtbl.find rooms_table id with *)
(*   | Not_found -> failwith (Printf.sprintf "Cannot find room with id=%d" id);; *)


(* if !debug is true, we replace the background by solid red *)
let delete_background room =
  printd debug_memory "Delete background for room %s" (sprint_id room);
  do_option room.background
    (fun b ->
      let () =
        room.background <-
          if !debug then Some (opaque_bg Draw.red) else None in
      match b with
      | Style s -> Style.unload s
      | Box b -> Box.unload b)

(* this can be used to force recreating the background, for instance after
   changing the size of the room *)
let unload_background room =
  do_option room.background (function
      | Box b -> Box.unload b
      | Style s -> Style.unload s) (* maybe not necessary *)

(* WARNING: in "iter" and in all other search functions below, recall that
   itering though a room is tricky because of mutability and threading. The
   structure of the tree can be changed by another thread while we iter. Most
   dangerous: it can also be changed by the itering itself, hehe. If necessary,
   doing "iter lock room" should minimize the risk (but not 100%: the tree can
   still be modified while we are locking..) *)

(* iter through all the rooms (layouts & widgets) contained in the [room],
   including the initial [room] itself. *)
(* top to bottom *)
let rec iter f room =
  f room;
  match room.content with
  | Resident _ -> ()
  | Rooms list -> List.iter (iter f) list

(* iter through widgets *)
let rec iter_widgets f room =
  match room.content with
  | Resident w -> f w
  | Rooms list -> List.iter (iter_widgets f) list

let map_widgets f room =
  let list = ref [] in
  iter_widgets (fun w -> list := (f w) :: !list) room;
  !list

(* iter the direct children *)
let iter_rooms f house =
  match house.content with
  | Resident _ -> printd (debug_error + debug_board)
                    "Layout %s has no rooms: cannot iter." (sprint_id house)
  | Rooms list -> List.iter f list

(* returns the list of rooms of the layout, or Not_found if there is a
   resident *)
let get_rooms layout =
  match layout.content with
  | Resident _ ->
    printd debug_error
      "[Layout.get_rooms] This layout %s is a leaf, not a node: it does not \
       contain a list of rooms" (sprint_id layout);
    raise Not_found
  | Rooms list -> list

let siblings room =
  match room.house with
  | None ->
     printd debug_error
       "Cannot get siblings of room %s because it does not belong to any \
        house." (sprint_id room);
     []
  | Some house -> get_rooms house

let rec belongs_to ~parent room =
  match room.house with
  | None -> false
  | Some h -> equal h parent || belongs_to ~parent h

(* return the resident widget, or Not_found *)
let widget layout =
  match layout.content with
  | Rooms _ ->
     printd debug_error
       "This room %s is a node, not a leaf: \
        it does not contain a resident widget" (sprint_id layout);
     raise Not_found
  (* or, return the first available widget with next_widget? *)
  | Resident w -> w

let get_resident = widget

(* return the first resident widget with show=true inside the layout, or
   Not_found *)
let rec first_show_widget layout =
  if layout.show
  then match layout.content with
       | Resident w ->
          (printd debug_board "first_show_widget selects %u" (Widget.id w); w)
       | Rooms rooms ->
          let rec loop = function
            | [] -> raise Not_found
            | r::rest -> try first_show_widget r with Not_found -> loop rest in
          loop rooms
  else raise Not_found

(* use this to reset all widget textures (room + all children) for reducing
   memory. The layout can still be used without any impact, the textures will be
   recreated on the fly. If you want to really remove all created textures, you
   have to use delete_backgrounds too; but then the backgrounds will *not* be
   recreated. *)
let unload_widget_textures room =
  unload_background room;
  iter_widgets Widget.unload_texture room

(* same, but for all rooms + widgets *)
let unload_textures room =
  let f r =
    unload_background r;
    match r.content with
    | Resident w -> Widget.unload_texture w
    | _ -> () in
  iter f room

let delete_backgrounds room =
  iter delete_background room

let delete_textures room =
  unload_textures room;
  delete_backgrounds room

let finalize room =
  printd debug_memory "Finalize room %s" (sprint_id room);
  delete_textures room

(* Return the list of all texts contained in the widgets *)
let get_texts room =
  map_widgets Widget.get_text room
  |> List.filter (fun s -> s <> "")

let get_text room =
  get_resident room
  |> Widget.get_text

let set_text room text =
  Widget.set_text (get_resident room) text

(* Pressing the TAB key in the main loop will switch the keyboard focus to
   another room. Here we save the room that had keyboard focus just before
   pressing TAB. This global variable should be thread safe because it is
   modified only by the main loop. Another option could be to store the room_id
   in an event. (?) *)
let keyboard_focus_before_tab : t option ref = ref None

let fresh_id = fresh_int ()

(** make geometry *)
let geometry ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(voffset=0) ?transform () : geometry =
  { x = Avar.var x;
    y = Avar.var y;
    w = Avar.var w;
    h = Avar.var h;
    voffset = Var.create (Avar.var voffset);
    transform = default transform
        { angle = Avar.var 0.;
          center = Avar.var None;
          flip = Avar.var Sdl.Flip.none;
          alpha = Avar.var 1.}
  }

(** list of all integer dynamical variables *)
let get_int_avars room =
let g = room.geometry in [g.x; g.y; g.w; g.h; Var.get g.voffset]

let current_geom ?(x=0) ?(y=0) ?(w=0) ?(h=0) ?(voffset=0) () : current_geom =
  { x; y; w; h; voffset}

(* Transform geometry into current_geom *)
let to_current_geom (g : geometry) : current_geom =
  { x = Avar.get g.x;
    y = Avar.get g.y;
    w = Avar.get g.w;
    h = Avar.get g.h;
    voffset = Avar.get (Var.get g.voffset) }

(* get current layer of layout *)
let get_layer l =
  l.layer

(* [base_layer rooms] returns the deepest layer of the list of rooms, or the
   current layer if the list is empty. *)
let base_layer = function
  | [] -> Draw.get_current_layer ()
  | room::rooms ->
     List.fold_left Chain.min (get_layer room) (List.map get_layer rooms)

(* Create a new room. Rather use the [create] function below. *)
let create_unsafe
    ?name
    ?(set_house = true) ?(adjust = Fit)
    ?(resize = fun _ -> ())
    ?layer
    ?mask ?background ?shadow ?house ?keyboard_focus ?(mouse_focus=false)
    ?(show = true) ?(clip = false) ?(draggable = false) ?canvas
    geometry content =
  let id = fresh_id () in
  let layer = match layer with
    | Some layer -> layer
    | None -> match content with
      | Rooms rooms -> base_layer rooms
      | Resident _ -> Draw.get_current_layer () in
  let room =
    {
      id;
      name;
      lock = Mutex.create ();
      thread_id = Thread.(id (self ()));
      show;
      hidden = true;
      adjust;
      resize;
      geometry;
      current_geom = to_current_geom geometry;
      clip;
      mask;
      background; (* = (Some (Solid Draw.(opaque blue))); (* DEBUG *) *)
      shadow;
      content;
      layer;
      house;
      keyboard_focus;
      mouse_focus;
      canvas;
      draggable;
      removed = false;
    } in
  (* we update the lookup table: *)
  (* remove is in principle not necessary *)
  if !debug
  then if WHash.mem rooms_wtable room
    then (printd debug_error "A room with same id was already in the table !";
          remove_wtable room);
  WHash.add rooms_wtable room;
  (* we update the resident room_id field *)
  (* we update the content's house field *)
  let () = match content with
    | Resident w -> w.Widget.room_id <- Some id
    | Rooms list -> if set_house
      then List.iter (fun r -> r.house <- Some room) list in
  Gc.finalise finalize room;
  (* Should we really do this [finalize]? Because who knows when the Gc will
     destroy the background texture.... maybe too late (after renderer was
     destroyed, and hence the texture pointer could point to a completely
     different texture). In order to prevent this, we call Gc.full_major when
     destroying the renderer. *)
  printd debug_board "Layout %s created." (sprint_id room);
  room

(* The public [create] version. *)
let create = create_unsafe ~set_house:true

(* the dummy room is only used to search the Weak table *)
let dummy_room = create ~name:"dummy" (geometry ()) (Rooms [])

let of_id_unsafe id : room =
  try WHash.find rooms_wtable {dummy_room with id} with
  | Not_found ->
    printd debug_warning "Cannot find room with id=%d" id;
    raise Not_found

(* A detached room is a layout that does not belong to the current layout tree,
   and is not associated to any SDL window (so no canvas field).  *)
let is_detached room =
  room.house = None && room.canvas = None

(* Currently [is_removed] is different from [is_detached]. *)
let is_removed room =
  room.removed

(* Notify the board that the layout cannot have focus (but it can still belong
   to the layout tree). *)
let remove_one room =
  printd debug_board "Removing layout %s from focus" (sprint_id room);
  room.removed <- true

let remove ?(children = false) room =
  if children
  then iter remove_one room
  else remove_one room;
  Trigger.push_remove_layout (room.id)

(* This one is more secure: we check if the layout is not detached. *)
let of_id_opt ?not_found id : room option =
  match (WHash.find_opt rooms_wtable {dummy_room with id}) with
  | None ->
    printd debug_error "Cannot find room with id=%d" id;
    do_option not_found run;
    None
  | Some r as o ->
    if is_detached r
    then (printd debug_error "Trying to access the detached room #%d" id; None)
    else o

(* find the room containing a widget (or None if the widget does not belong to a
   room or if the room has disappeared in the air)*)
let containing_widget w =
  check_option w.Widget.room_id of_id_opt

let of_wid wid =
  let w = Widget.of_id wid in
  containing_widget w

(* only for debugging: *)
(* check if rooms sent to cemetery have effectively been removed by GC *)
let check_cemetery () =
  let check id = try
      let r = of_id_unsafe id in
      printd debug_memory
        "Dead room %s seems to be living. Beware of zombies." (sprint_id r);
      false
    with
    | Not_found ->
       printd debug_memory
         "Dead room #%u was correctly burried by the GC. RIP." id;
       true
  in
  let rec loop list newlist empty = (* easier to use a Queue *)
    match list with
    | [] -> empty, newlist
    | id::rest ->
       if check id
       then loop rest newlist empty
       else loop rest (id :: newlist) false in
  let empty, newlist = loop !cemetery [] true in
  cemetery := newlist;
  empty

(* Kind of recursive Mutex. Bad style? TODO remove this necessity...
   Here we lock the layout to make it available only by the locking
   thread. Hence two consecutive locks by the same thread will not
   block. TODO mutualize with Var?  Probably better to use protect_fn
   anyways. *)
let lock l =
  if Mutex.try_lock l.lock
  then begin
      let id = Thread.(id (self ())) in
      printd debug_thread "Locking room %s for thread #%i." (sprint_id l) id;
      l.thread_id <- id
    end
  else (* then it was already locked *)
    if Thread.(id (self ())) <> l.thread_id (* not same thread, we must wait *)
    then begin
        let id = Thread.(id (self ())) in
        printd debug_thread "Waiting for thread #%i to remove lock for room %s"
          id (sprint_id l);
        Mutex.lock l.lock;
        l.thread_id <- id
      end
    else begin
        printd (debug_thread + debug_error + debug_user)
          "!! Layout %s was locked, but by the same thread: we \
           continue, but this should be corrected."
          (sprint_id l)
      end

let unlock l =
  printd debug_thread "Unlocking layout %s" (sprint_id l);
  if Mutex.try_lock l.lock
  then printd debug_thread " (but layout %s was already unlocked)." (sprint_id l);
  Mutex.unlock l.lock

(* get the renderer of the layout *)
let renderer t = match t.canvas with
  | Some c -> c.Draw.renderer
  | _ -> failwith "Cannot get renderer because no canvas was defined"

(* get the Sdl window of the layout *)
let window_opt t =
  map_option t.canvas (fun c -> c.Draw.window)

let window t =
  match window_opt t with
  | Some w -> w
  | _ -> begin
      printd debug_error
        "Cannot get window for layout %s because no canvas was defined"
        (sprint_id t);
      raise Not_found
    end

(* return the top-level layout *)
(* This is relevent only once the main loop has started. Before this, the
   top_house is not even created, so it will not return what you expect. *)
let rec top_house layout =
  match layout.house with
  | None -> layout
  | Some r -> top_house r

(* see [top_house] *)
let guess_top () =
  try WHash.iter (fun r ->
      if not (is_detached r) then raise (Found r)) rooms_wtable;
  None with
  | Found r -> Some (top_house r)

let is_top layout =
  layout.house = None && layout.canvas <> None

(* Shoud this be a public function? The house it not always the one we
   imagine. For instance [make_clip] will change the room. Maybe we should
   enforce in [make_clip] and others that the original room should not belong to
   a house to start with. *)
let get_house layout =
  layout.house

let get_content layout =
  layout.content

let get_canvas l =
  match l.canvas with
  | Some c -> c
  | None ->
    raise (Fatal_error
             (l, Printf.sprintf "The room #%d is not associated with any canvas"
                l.id))

(* test if layouts share the same layer (= same depth) *)
let same_layer l1 l2 =
  Chain.(get_layer l1 == get_layer l2)

let same_stack l1 l2 =
  Chain.(same_stack (get_layer l1) (get_layer l2))

(* get the layout background *)
let get_background l =
  l.background

(* force compute background at current size. Canvas must be created *)
let compute_background room =
  do_option room.background (
    fun bg ->
      let g = room.current_geom in
      Sdl.log "COMPUTE BG w=%u h=%u" g.w g.h;
      let box = match bg with
        | Style style ->
          let b = Box.(create ~width:g.w ~height:g.h ~style ()) in
          room.background <- (Some (Box b));
          b
        | Box b -> Box.unload b; b in
      ignore (Box.display (get_canvas room) (get_layer room) box
                (Draw.scale_geom (to_draw_geom g))))

(* Change background. *)
(* can be called by a thread *)
(* Remark: one should not set a "Box" background (for safety, because one cannot
   use a background of type Box in case the box already belongs to another
   room...) *)
let set_background l b =
  unload_background l;
  l.background <- b

let set_shadow l s =
  l.shadow <- s

(** get size of layout *)
let get_size l =
  l.current_geom.w, l.current_geom.h

let get_physical_size l =
  get_size l |> Draw.scale_size

(** get width of layout *)
let width l =
  l.current_geom.w

(** get height *)
let height l =
  l.current_geom.h

let resize room =
  do_option (get_house room) (fun house ->
      room.resize (get_size house))

let disable_resize room =
  room.resize <- (fun _ -> ())

let on_resize room f =
  let r = room.resize in
  room.resize <- (fun house_size -> r house_size; f ())

let fix_content house =
  iter_rooms disable_resize house

let resize_content room =
  match room.content with
  | Rooms list -> List.iter resize list
  | Resident w -> Widget.resize w (get_size room)

(* l must be the top house *)
let adjust_window_size l =
  if not (is_top l)
  then printd debug_error
      "[adjust_window_size] should only be called with a top house, what %s is \
       not." (sprint_id l)
  else if l.canvas <> None
  then let w,h = get_physical_size l in
    let win = window l in
    if (w,h) <> Draw.get_window_size win
    then Draw.set_window_size win ~w ~h
    else printd debug_graphics
        "Window for layout %s already has the required size."
        (sprint_id l)

(* Change the size of the room. By default this will cancel the resize function
   of this room. If [set_size] or its derivatives [set_width] and [set_height]
   are used as part of a layout resize function of the same room, this default
   behaviour should be disabled to prevent the resize function to cancel itself:
   use [keep_resize:true]. *)
(* TODO faire un module Resize avec keep_resize=true par défaut. *)
let set_size ?(keep_resize = false) ?(check_window = true)
      ?(update_bg = false) ?w ?h l  =
  let () = match w,h with
    | Some w, Some h ->
       l.current_geom <- { l.current_geom with w; h };
       Avar.set l.geometry.h h;
       Avar.set l.geometry.w w
    | Some w, None ->
       l.current_geom <- { l.current_geom with w };
       Avar.set l.geometry.w w
    | None, Some h ->
       l.current_geom <- { l.current_geom with h };
       Avar.set l.geometry.h h
    | None, None -> () in

  if update_bg && l.canvas <> None then compute_background l;
  (* = ou plutot unload_background ?? *)
  if not keep_resize then disable_resize l;
  if check_window && is_top l then adjust_window_size l;
  resize_content l

let set_height ?keep_resize ?check_window ?update_bg l h =
  set_size ?keep_resize ?check_window ?update_bg ~h l

let set_width ?keep_resize ?check_window ?update_bg l w =
  set_size ?keep_resize ?check_window ?update_bg ~w l

(* The public version of [set_size] *)
let set_size ?keep_resize ?check_window ?update_bg l (w,h) =
  set_size ?keep_resize ?check_window ?update_bg ~w ~h l

(* get voffset *)
let get_voffset l =
  (* l.current_geom.voffset;; *)
  Avar.get (Var.get l.geometry.voffset)

(** get current absolute x position (relative to the top-left corner of the
   window). Not necessarily up-to-date. *)
let xpos l =
  l.current_geom.x

(** get current absolute y position *)
let ypos l =
  l.current_geom.y

(* left absolute coordinate of the layout's house *)
let x_origin l = match l.house with
    | None -> 0
    | Some h -> xpos h

(* top absolute coordinate of the layout's house *)
 let y_origin l = match l.house with
    | None -> 0
    | Some h -> ypos h

(* position of room relative to house *)
let pos_from house room =
  xpos room - xpos house, ypos room - ypos house

(** get current x value.  *)
(* WARNING don't use this inside an animation for x ! It will loop
forever. Instead use Avar.old l.geometry.x *)
let getx l =
  Avar.get l.geometry.x

let get_oldx l =
  Avar.old l.geometry.x

(** get current y value *)
let gety l =
  Avar.get l.geometry.y

let get_oldy l =
  Avar.old l.geometry.y

(* Change x of layout, without adjusting parent house. Warning, by
   default this disables the resize function. *)
(* This is the x coordinate wrt the containing house *)
(* This won't work if there is an animation running (see Avar.set) *)
let setx ?(keep_resize = false) l x =
  let x0 = getx l in
  l.current_geom <- { l.current_geom with x = l.current_geom.x + x - x0 };
  Avar.set l.geometry.x x;
  if not keep_resize then disable_resize l
  (* :TODO à vérifier, cf dans "flat" et "tower" *)

(* Change y of layout, without adjusting parent house. *)
(* see above *)
let sety ?(keep_resize = false) l y =
  let y0 = get_oldy l in
  l.current_geom <- { l.current_geom with y = l.current_geom.y + y - y0 };
  Avar.set l.geometry.y y;
  if not keep_resize then disable_resize l

(* see above *)
(* warning, it the animation is not finished, using Avar.set has almost no
   effect *)
let set_voffset l vo =
  Avar.set (Var.get l.geometry.voffset) vo;
  l.current_geom <-  { l.current_geom with voffset = vo }

(* use this to shift the voffset by a constant amount without stopping an
   animation *)
let shift_voffset l dv =
  Var.update l.geometry.voffset (fun av ->
  if Avar.finished av
  then begin
      let vo = Avar.get av + dv in
      Avar.set av vo;
      l.current_geom <-  { l.current_geom with voffset = vo };
      av
    end
  else Avar.apply (fun y -> y + dv) av)

(* not used... *)
let reset_pos l =
  let w,h = get_size l in
  let g = geometry ~w ~h () in (* or modify l.geometry fields in-place? *)
  l.geometry <- g;
  l.current_geom <- to_current_geom g

(* [get_window_pos] is meaningful only when the window is created, that is after
   calling Bogue.make. The corresponding SDL window is created only after
   Bogue.run. In the meantime, a special use of current_geom is to indicate the
   desired window position within the desktop at startup. *)
let get_window_pos layout =
  let f x = if x = not_specified then None else Some x in
  let x,y = match layout.canvas with
    | None -> xpos layout, ypos layout
    | Some _ -> Draw.get_window_position (window layout) in
  f x, f y

(* see [get_window_pos]. It should be set *after* Bogue.make. Otherwise it has
   possibly no effect, or perhaps causes some glitches. TODO test this more
   thoroughly. *)
let set_window_pos layout (x,y) =
  match layout.canvas with
  | None -> let g = layout.current_geom in
    layout.current_geom <- { g with x; y }
  | Some _ -> Draw.set_window_position (window layout) x y

let get_transform l =
  let t = l.geometry.transform in
  let angle = Avar.get t.angle in
  let center = Avar.get t.center in
  let flip = Avar.get t.flip in
  let alpha = Avar.get t.alpha in
  Draw.make_transform ~angle ?center ~flip ~alpha ()

let get_alpha l =
  Avar.get l.geometry.transform.alpha

let draggable l =
  l.draggable

let set_draggable l =
  l.draggable <- true

let set_clip l =
  l.clip <- true

let unset_clip l =
  l.clip <- false

let set_show l b =
  l.show <- b

let rec_set_show b l =
  let rec loop b l =
    l.show <- b;
    match l.content with
    | Resident _ -> ()
    | Rooms list -> List.iter (loop b) list in
  loop b l

let show_window t =
  set_show (top_house t) true;
  do_option (window_opt t) Sdl.show_window

let hide_window t =
  set_show (top_house t) false;
  do_option (window_opt t) Sdl.hide_window

(** return absolute (x,y) position *)
(* TODO optimize: test if x is up_to_date, then one can use current_geom
   instead? *)
(* of course this test will fail for hidden rooms *)
let compute_pos room =
  let rec loop x0 y0 r =
    let x,y = x0 + (Avar.get r.geometry.x),
              y0 + (Avar.get r.geometry.y) + (Avar.get (Var.get r.geometry.voffset)) in
    match r.house with
      | None -> x,y
      | Some h -> loop x y h in
  loop 0 0 room

(* Get absolute position of the parent house *)
let house_pos room =
  match room.house with
    | None -> 0,0
    | Some h -> compute_pos h;;

(* Not used, just to fix the vocabulary "leaf" *)
let is_leaf room =
  match room.content with
  | Resident _
  | Rooms [] -> true
  | _ -> false

(* Return the first resident *below (or including) room* for which test w =
    true, or None *)
let rec find_resident test room =
  match room.content with
  | Resident w -> if test w then Some room else None
  | Rooms list -> let rec loop = function
    | [] -> None
    | r :: rest -> let f = find_resident test r in
      if f = None then loop rest else f in
    loop list

(* Search through the whole component of the layout (children and parents)
   starting from top house containing room *)
let search room scan =
  if scan room then Some room
  (* =just in case it might speed-up things: room is the "initial guess" *)
  else let f r = if scan r then raise (Found r) in
    try
      iter f (top_house room);
      raise Not_found
    with
    | Found r -> printd debug_warning "Search OK"; Some r
    | Not_found -> printd debug_error "Search produced no result!"; None
    | e -> raise e

(* Find room by id in the connected component of house *)
(* cf Layout.of_id *)
let find_room_old house id =
  printd debug_warning "Search room #%d in %d..." id (house.id);
  let scan r = r.id = id in
  search house scan

(* Find the next room in the same level of the house. In circular mode, after
   the last one comes the first. In non circular mode, if room is the last one,
   we return None. If [only_visible] is true, we skip all rooms that have
   [.show=false] and rooms that belong to a hidden house. *)
let next ?(circular = false) ?(only_visible = true) room =
  match room.house with
  | None -> (* we must be in top_house *)
     None
  | Some h when only_visible && not h.show ->  (* h is hidden *)
     None
  | Some h ->
     let rooms = get_rooms h in (* It should not be empty since room is inside. *)
     let first = List.hd rooms in
     let rec loop list found = match list with
       | [] -> if found then if circular then Some first else None
               else None (* nothing was found, so the [room] itself was hidden. *)
       | a::rest -> if found && (not only_visible || a.show)
                    then Some a
                    else loop rest (found || equal a room) in
     loop rooms false

(* Find the "first" (and deepest) room (leaf) contained in the layout by going
   deep and choosing always the first room of a house *)
(* WARNING a room with empty content is considered a leaf too *)
let rec first_room r =
  printd debug_board "Descending to room %s" (sprint_id r);
  match r.content with
  | Resident _ -> r
  | Rooms [] -> r
  | Rooms (a::_) -> first_room a

(* Find a 'uncle': a room next to some parent room, going up in generation. *)
let rec next_up r =
  check_option r.house (fun h ->
      match next h with
      | None -> next_up h
      | o -> o)

(* find the next leaf (=room containing a widget, or empty) in the whole layout
   (which should be the connected component of [top]. If [room] does not belong
   to this compoment -- which can happen after mutation -- we return the first
   leaf of [top]). *)
(* we first look at the same level, then below, then upstairs. *)
(* repeated calls to this function will visit the whole connected component --
   although this is not the optimal way to visit everything, of course -- and
   start over indefinitely. Thus you should check when the returned room is the
   one you started with... (which means you should start with a leaf !) *)
let next_leaf ~top room =
  if not (top_house room == top)
  then begin
      printd (debug_board + debug_warning + debug_custom)
        "Room %s does not belong the the top house %s. We select the first room \
         of the top house." (sprint_id room) (sprint_id top);
      first_room top
    end
  else match room.content with
       | Rooms []
         | Resident _ -> begin
           match next room with
           | None -> (* last one at this level; we go upstairs *)
              let h = match next_up room with
                | Some r -> r
                | None -> printd debug_board
                            "No next widget was found; we start again from top";
                          top in
              first_room h
           | Some n ->
              (match n.content with
               | Resident _ -> n
               | Rooms _ -> first_room n)
         end
       | Rooms _ -> first_room room

(* Find the next visible room with a widget that can have keyboard_focus *)
(* TODO check example25 *)
let next_keyboard ~top room =
  let rec loop r visited =
    if List.mem r.id visited (* this happens sometimes (in case of mutation) *)
    then (printd debug_custom "Room %s already visited" (sprint_id room);
         None)
    else
      let n = next_leaf ~top r in
      if equal room n
      then (printd (debug_board + debug_custom) "No keyboard_focus found"; None)
      else if n.keyboard_focus <> None && n.show
      then (printd (debug_board + debug_custom) "Found %s" (sprint_id n); Some n)
      else loop n (r.id :: visited) in
  loop room []

(********************)



let remove_canvas room =
  delete_textures room;
  iter (fun r -> r.canvas <- None) room

(* What to do when a layout is not used anymore ? *)
(* First check that it is detached from its house. *)
(* If it has children, they will become orphans  *)
(* (because a room should NOT belong to several different houses). *)
(* The resulting orphans are not freed. They are still available to use in a new
   room (see eg. longlist.ml). Use kill_all if you want to recursively free all
   subrooms *)
(* mask and background are not freed because nothing prevents them from being
   shared with another object (which is maybe not a good idea...) We leave this
   to the GC... (?) *)
(* WARNING: be careful it's quite easy to forget that something else points to
   the layout... or its children. This is easily the case for instance with
   Bogue.board fields like windows_house, mouse_focus, keyboard_focus,
   button_down... Not to mention widgets, which refer indirectly to layouts via
   their id... So it's preferable never to use this... *)
(* not used yet *)


(* When to call this ? *)
(* in particular, when this function is called, the layout l in principle has
   already been removed from rooms_wtable *)
let free l =
  printd debug_memory "Freeing Layout %s" (sprint_id l);
  unload_background l;
  begin match l.content with
  | Resident _ -> ()
  | Rooms list ->
     list_iter list (fun r ->
         do_option r.house (fun h ->
             if equal h l
             then printd debug_warning "Room %s is now orphan" (sprint_id r)))
  end

(* kill functions below are quite dangerous, beware *)

(* use this when the layout + all children is not used anymore *)
(* In fact don't use this, use kill_rooms instead. Because very often a layout
   is created with subrooms that don't all necessarily have a name. Thus, if you
   want to kill a layout, you may forget that its direct house has no name, so
   it will likely stay in the table for ever. It's difficult for the user to
   keep track of this. One could use ocaml's Ephemeron instead ?*)
(* Note that rooms can be reclaimed though their id, for instance via of_wid, or
   even more devily stored in an event... At this point it DOES cause some fatal
   errors that I don't know how to locate... *)
let kill_all_NO room =
  match room.house with
  | Some h -> printd debug_error "Cannot kill layout #%u because it still \
                                  belongs to a house #%u" room.id h.id;
  | None -> (* we defer it to the main loop *)
     Sync.push (fun () ->
         delete_backgrounds room;
         let rec loop r =
           remove_wtable r;
           match r.content with
           | Resident w -> Widget.free w
           | Rooms list -> List.iter loop list
         in
         loop room)

(* kill all rooms (and theirs subrooms) of this house *)
(* defered to the main loop *)
(* don't use this. See WARNING in "kill" above *)
let kill_rooms_NO house =
  match house.content with
  | Resident _ -> printd debug_error "House #%u does not have rooms to kill..." house.id
  | Rooms list ->
     Sync.push (fun () ->
         let rec loop r =
           delete_background r;
           remove_wtable r;
           match r.content with
           | Resident w -> Widget.free w
           | Rooms list -> List.iter loop list
         in
         List.iter loop list)



(**********)

(* Use this to shift all current_geometries before inserting a room inside a
   house. This can be needed because inserting will trigger fit_content which
   uses current_geom *)
let global_translate room dx dy =
  do_option room.house (fun h ->
      printd debug_warning
        "You are translating the current_geom of room #%u which already has a \
         house #%u. This is likely to have no effect, as the current_geom is \
         automatically updated at each display" room.id h.id);
  iter (fun r ->
      r.current_geom <- { r.current_geom with x = r.current_geom.x + dx;
                                              y = r.current_geom.y + dy }) room

(* adjust layout size to the inner content in the same layer (but not to the
   larger layouts, neither to the window) *)
(* TODO: treat margins *)
(* not used yet... *)
let rec fit_content ?(sep = Theme.room_margin/2) l =
  if l.adjust = Nothing || l.clip then ()
  else let w,h = match l.content with
      | Resident widget -> Widget.default_size widget
      (* | Rooms [r] -> r.geometry.w, r.geometry.h *)
      | Rooms list ->
        let x0 = l.current_geom.x in
        let y0 = l.current_geom.y in
        ( List.fold_left (fun m r ->
              if same_layer r l
              then imax m (r.current_geom.x - x0 + r.current_geom.w)
              else m)
              0 list,
          List.fold_left (fun m r ->
              if same_layer r l
              then imax m (r.current_geom.y - y0 + r.current_geom.h)
              else m)
            0 list )
    in
    let g' = match l.adjust with
      | Fit -> { l.current_geom with w = w+sep; h = h+sep };
      | Width -> { l.current_geom with w = w+sep };
      | Height -> { l.current_geom with h = h+sep };
      | Nothing -> failwith "already treated case !" in
    let oldg = l.current_geom in
    if g' <> oldg then begin
      printd debug_graphics "ADJUST %s to New SIZE %d,%d" (sprint_id l) w h;
      set_size l (g'.w, g'.h);
      do_option l.house fit_content  (* we adjust the parent (???) *)
    end

(** return the list of widgets used inside the layout *)
let rec get_widgets layout =
  match layout.content with
    | Rooms h -> List.flatten (List.map get_widgets h)
    | Resident w -> [w]

let has_resident layout =
  match layout.content with
  | Resident _ -> true
  | Rooms _ -> false

let has_keyboard_focus r =
  r.keyboard_focus = Some true

(* Set keyboard_focus to the room and the resident widget, if possible. In debug
   mode, this will draw some shadow around the layout when focused... Warning,
   currently, even if the room doesn't have the keyboard_focus flag, this does
   not prevent the board to register it as keyboard focus... TODO: what to
   do? *)
let set_keyboard_focus r =
  match r.keyboard_focus with
  | Some b ->
     if not b then begin
         printd debug_board "Setting keyboard_focus to room %s" (sprint_id r);
         r.keyboard_focus <- Some true;
         match r.content with
         | Rooms _ -> ()
         | Resident w -> Widget.set_keyboard_focus w
       end
  | None -> printd debug_board
              "Cannot set keyboard_focus to room %s because if was not created \
               with keyboard_focus capability." (sprint_id r)

let rec remove_keyboard_focus r =
  do_option r.keyboard_focus (fun b -> if b then r.keyboard_focus <- Some false);
  match r.content with
  | Rooms list -> List.iter remove_keyboard_focus list
  | Resident w -> Widget.remove_keyboard_focus w

let claim_focus r =
  if has_resident r then Trigger.push_mouse_focus r.id
  else printd (debug_error + debug_board)
         "Cannot claim focus on room %s without resident." (sprint_id r)

let claim_keyboard_focus r =
  if has_resident r then Trigger.push_keyboard_focus r.id
  else printd (debug_error + debug_board)
         "Cannot claim keyboard_focus on room %s without resident." (sprint_id r)

(* Center vertically the rooms of the layout (first generation only) *)
let v_center layout y0 h =
  match layout.content with
  | Resident _ -> ()
  | Rooms rs ->
     list_iter rs
       (fun r -> let h0 = height r in
                 let y = Draw.center y0 h h0 in
                 sety r y)

(** vertical align *)
(* v_center is the same as v_align ~align:Draw.Center *)
let v_align ~align layout y0 h =
  match layout.content with
  | Resident _ -> ()
  | Rooms rs ->
     list_iter rs
       (fun r -> let h0 = height r in
                 let y = Draw.align align y0 h h0 in
                 sety r y)


(** create a room (=layout) with a unique resident (=widget), in the current
   layer (unless specified). No margin possible. *)
(* x and y should be 0 if the room is the main layout *)
(* warning, the widget is always centered *)
(* x,y specification will be overwritten if the room is then included in a flat
   or tower, which is essentially always the case... *)
let resident_with_layer ?layer ?name ?(x = 0) ?(y = 0) ?w ?h ?background
    ?draggable ?canvas ?keyboard_focus widget =
  let (w',h') = Widget.default_size widget in
  let w = default w w' in
  let h = default h h' in
  let keyboard_focus = match keyboard_focus with
    | Some true -> Some false
    | Some false -> None
    | None -> Widget.guess_unset_keyboard_focus widget in
  let geometry = geometry ~x ~y ~w ~h () in
  create ?name ?background ?keyboard_focus ?draggable ?canvas ?layer
    geometry (Resident widget)

let resident ?name ?(x = 0) ?(y = 0) ?w ?h ?background ?draggable
    ?canvas ?keyboard_focus widget =
  resident_with_layer ?name ~x ~y ?w ?h ?background ?draggable
    ?canvas ?keyboard_focus widget

let of_widget = resident

(* Set the given widget as the new resident of the given room. If w,h are not
   specified, the size of the room will be updated by the size of the widget. *)
let change_resident ?w ?h room widget =
  match room.content with
  | Resident resid ->
     printd debug_board "Replacing room %s's widget by widget #%d"
       (sprint_id room) (Widget.id widget);
     let (w',h') = Widget.default_size widget in
     let w = default w w' in
     let h = default h h' in
     room.content <- Resident widget;
     widget.Widget.room_id <- Some room.id;
     room.keyboard_focus <- Widget.guess_unset_keyboard_focus widget;
     resid.Widget.room_id <- None;
     set_size room (w,h)
  | _ -> printd debug_event "[change_resident]: but target room has no resident!"

(* An empty layout can reserve some space without stealing focus (and has no
   keyboard_focus) *)
(* WARNING TODO in the search functions, we have assumed rooms where never
   empty... *)
let empty ?name ?background ~w ~h () =
  let geometry = geometry ~w ~h () in
  create ?name ?background geometry (Rooms [])

(* Simple resize function that scales the room with respect to the given
   original house size (w,h) *)
let scale_resize ?(scale_width=true) ?(scale_height=true)
      ?(scale_x=true) ?(scale_y=true) (w,h) r =
  let x = xpos r in
  let y = ypos r in
  let rw,rh = get_size r in
  let keep_resize = true in
  let resize (hw,hh) =
    let scalex z = z * hw / w in
    let scaley z = z * hh / h in
    if scale_x then setx ~keep_resize r (scalex x);
    if scale_y then sety ~keep_resize r (scaley y);
    if scale_height then set_voffset r (scaley (get_voffset r));
    if scale_height && scale_width then set_size ~keep_resize r (scalex rw, scaley rh)
    else if scale_height then set_height ~keep_resize r (scaley rh)
    else if scale_width then set_width ~keep_resize r (scalex rw) in
  r.resize <- resize

(* convenience function for scaling all rooms by the same factor -- to be use for rooms in the same house *)
let scale_resize_list ?scale_width ?scale_height ?scale_x ?scale_y (w,h) rooms =
  List.iter (scale_resize ?scale_width ?scale_height ?scale_x ?scale_y (w,h))
    rooms

(* Overrides scale_resize to retrieve the house size automatically. Can only be
   applied if the room is already in a house. *)
let scale_resize ?scale_width ?scale_height ?scale_x ?scale_y room =
  match room.house with
  | None -> printd debug_error
              "Cannot compute the resize function of room %s since it does not \
               belong to a house" (sprint_id room)
  | Some h ->
     let s = get_size h in
     scale_resize ?scale_width ?scale_height ?scale_x ?scale_y s room

let auto_scale house =
  let (w,h) = get_size house in
  match house.content with
  | Rooms rooms -> scale_resize_list (w,h) rooms
  | Resident _ -> printd debug_warning "TODO: auto_scale resident"

let resize_follow_house room =
  room.resize <- (fun size ->
    set_size ~keep_resize:true room size)

(* Not very smart, but [resize_fix_x/y] is currently used by Space. Another
   possibility would be to have two resize functions: horizontally and
   vertically *)
let resize_fix_x room =
  let f = room.resize in
  let keep_resize = true in
  room.resize <- (fun size ->
    let x,w = getx room, width room in
    f size;
    setx ~keep_resize room x;
    set_width ~keep_resize room w
  )

let resize_fix_y room =
  let f = room.resize in
  let keep_resize = true in
  room.resize <- (fun size ->
    let y,h = gety room, height room in
    f size;
    sety ~keep_resize room y;
    set_height ~keep_resize room h
  )

(* sets l with the size of the top_house. In principle the (x,y) of the
   top_house should be (0,0), we don't check this here. The (x,y) of l is set to
   (0,0). Should be called dynamically after main loop starts. *)
let maximize l =
  setx l 0;
  sety l 0;
  let w,h = get_size (top_house l) in
  l.current_geom <- { l.current_geom with h; w };
  Avar.set l.geometry.h h;
  Avar.set l.geometry.w w;
  scale_resize_list (w,h) [l];
  resize_content l

(* check if a sublayer is deeper (= below = Chain.<) than the main layer, which
    (in principle) should not happen *)
let check_layers room =
  let rec loop house r =
    match r.content with
    | Resident _ ->
       if Chain.(get_layer house >. get_layer r)
       then printd debug_error
              "The house #%d contains a room #%d with deeper layer! (%d>%d)"
              house.id r.id (Chain.depth (get_layer house))
              (Chain.depth (get_layer r));
    | Rooms h -> List.iter (loop r) h
  in
  loop room room

(** Set the canvas of the layout. Warning! we assume that if a room has a
    canvas, all smaller rooms already have the same canvas... *)
(* warning: this is also used when we change the layer, but the window stays the
   same *)
(* let rec set_canvas canvas room = *)
(*   if Draw.canvas_equal room.canvas canvas *)
(*   then () *)
(*   else begin *)
(*     printd debug_warning "Changing room canvas"; *)
(*     room.canvas <- canvas; *)
(*     match room.content with *)
(*       | Rooms list -> List.iter (set_canvas canvas) list *)
(*       | Resident _ -> () *)
(*   end;; *)
let set_canvas canvas room =
  room.canvas <- Some canvas;
  if !debug then check_layers room

(** Set the canvas for layout and all children *)
let global_set_canvas room canvas =
  iter (fun r -> r.canvas <- Some canvas) room

let check_layer_error room house =
  if not (Chain.same_stack room.layer house.layer)
  then printd debug_error
         "The replacement room %s belongs to a separate set of layers disjoint \
          from the house %s (or one of them has empty layer). Beware that it \
          will probably never be displayed"
         (sprint_id room) (sprint_id house)

(* Move the room layer into the stack of the dst layer (this actually makes a
   copy of the layer into a dst stack, without destroying the initial layer,
   which may be shared by other rooms). Warning: this does not check the blit
   contents of the layers; it's supposed to be done before the blits are
   computed (when all layers should contain empty queues). The new copy get an
   empty queue anyways.*)
let move_to_stack ~dst room =
  if not (same_stack room dst)
  then begin
    let dst_layer = get_layer dst in
      printd debug_board "Moving layer of %s into that of %s (stack:%u)"
        (sprint_id room) (sprint_id dst) (Chain.get_stack_id dst_layer);
      room.layer <- Chain.copy_into ~dst:dst_layer (get_layer room);
      Chain.replace room.layer (Draw.new_layer ())
    end

(* Move the room layer and the layers of all inhabitants to the dst layer *)
let move_all_to_stack ~dst room =
  iter (move_to_stack ~dst) room

(* move all layers contained in [room] into the same stack as the [room]
   layer. *)
let unify_layer_stack room =
  move_all_to_stack ~dst:room room

let set_new_stack win =
  if is_top win then begin
      printd debug_board "Creating a new stack for window layout %s."
        (sprint_id win);
      win.layer <- Chain.copy_into ~dst:None (get_layer win);
      Chain.replace win.layer (Draw.new_layer ())
    end
  else printd (debug_board + debug_error)
         "Creating a new stack is only allowed for top layouts (windows), not \
          for %s" (sprint_id win)

let move_to_new_stack room =
  set_new_stack room;
  unify_layer_stack room

(* specialized [create] version for creating the list of all windows (= top
   layouts) *)
let create_win_house windows =
  (* We make sure each window's layer belong to a different stack. (If not, we
     create new stacks.) *)
  let rec loop layer_ids = function
    | [] -> ()
    | win::rest ->
      let id = Chain.get_stack_id (get_layer win) in
      let id =
        if List.mem id layer_ids then
          begin
            set_new_stack win;
            Chain.get_stack_id (get_layer win)
          end else id in
      unify_layer_stack win;
      loop (id::layer_ids) rest in
  loop [] windows;
  let layer = None in
  create_unsafe ~set_house:false ~name:"windows_house" ~layer
    (geometry ()) (Rooms windows)

(* use this only if you know what you are doing... *)
(* remember that a room with no house will be considered a "top layout" *)
(* see WARNING of the "kill" fn. If the detached room is still pointed to by the
   board (eg. mouse_focus...=> the mouse will not find what you expect) *)
(* not used *)
let detach_rooms layout =
  match layout.content with
  | Resident _ ->
     printd debug_error "No rooms to detach from layout %s" (sprint_id layout)
  | Rooms rooms ->
     list_iter rooms (fun r ->
         if r.house <> None then
           (r.house <- None;
            r.canvas <- None;
            printd debug_warning "Room %s was detached from House %s..."
              (sprint_id r) (sprint_id layout)))

(* Detach a room from its house. See detach_rooms. Warning, the textures are not
   freed. *)
let detach room =
  match room.house with
  | None -> printd debug_error "Cannot detach because room %s has no house"
              (sprint_id room)
  | Some h ->
    room.house <- None;
    room.canvas <- None;
    remove ~children:true room;
    let rooms = List.filter (fun r -> not (r == room)) (get_rooms h) in
    h.content <- Rooms rooms;
    printd debug_warning "Room %s was detached from House %s."
      (sprint_id room) (sprint_id h)

(* Sets the required fields for [room] to be a room of [dst], but does not
   install it within the rooms list. This has to be done separately. See for
   instance [add_room] or [replace_room]. *)
let attach ~dst room =
  room.house <- Some dst;
  move_to_stack ~dst room;
  (* if there is a canvas in layout, we copy it to all rooms *)
  do_option dst.canvas (global_set_canvas room)

(* Check if [room] can be added to the content of [dst]. If [already = true] we
   accept to add a room in a house that already contains it... (this is used
   when we want to re-order rooms). If [loop_error = false] it doesn't raise an
   error when room = dst, we just return false. *)

let ok_to_add_room ?(already = false) ?(loop_error = true) ~dst room =
  if equal room dst
  then begin
    printd debug_error "Cannot add room %s to itself!" (sprint_id room);
    if loop_error then invalid_arg "[Layout.ok_to_add_room] room %s"
        (sprint_id room)
    else false
    (* equivalent to (not ((not loop_error) || raise ...)) *)
  end else match room.house with
    | Some h when equal h dst ->
      printd ((if already then debug_warning else debug_error) + debug_board)
        "Room %s already belongs to %s." (sprint_id room) (sprint_id dst);
      already
    | Some h ->
      printd (debug_error + debug_board)
        "Room %s should not be added to %s because it belongs to another house \
         (%s). We do it anyway." (sprint_id room) (sprint_id dst) (sprint_id h);
      (* This is actually quite bad error because then the other house will be
         garbage collected (finalized), its rooms will be GCed too... *)
      true
    | None -> true

(* Modify the layout content by setting new rooms *)
(* Old ones are *not* freed, but they are *detached* from house, and their
   textures are "unloaded". *)
(* Note that setting rooms that are already there is legal (can be used to
   change the order). Then they are not detached, of course. *)
(* With sync=false, this is highly non thread safe. Locking layout is not enough
   (or, we should lock all layouts in the main loop too... Therefore, it is
   better to set sync=true, which delays the execution to Sync (the main loop
   Queue) *)
(* mutualize with [add_room, replace_room]?*)
let set_rooms layout rooms =
  match layout.content with
  | Resident _ ->
    printd debug_error
      "Cannot transform a leaf (Resident #%u) to a node (Rooms) because the \
       resident widget would be lost" layout.id
  | Rooms old_rooms ->
    list_iter rooms (fun r ->
        if mem equal r old_rooms
        then printd (debug_board + debug_warning)
            "Trying to insert a room (%s) that is already there (%s). We \
             leave it there, no problem."
            (sprint_id r) (sprint_id layout)
        else if ok_to_add_room ~dst:layout r
        then attach ~dst:layout r);

    (* Now we rescan the list to detach unused rooms. *)
    list_iter old_rooms (fun r ->
        if not (mem equal r rooms) then begin
          detach r;
          unload_textures r
        end);
    (* detach_rooms layout; *) (* we don't detach because some orphans may
                                  want to survive longer than you
                                  think... see WARNING in 'kill' *)
    layout.content <- Rooms rooms
(*fit_content layout*)

let set_rooms layout ?(sync=true) rooms =
  (if sync then Sync.push else run) (fun () -> set_rooms layout rooms)

(* like set_rooms but in addition the old ones are killed *)
let replace_rooms_NO layout rooms =
  kill_rooms_NO layout;
  set_rooms layout rooms

(* copy the 'relocatable content' of src into dst.  Of course, this should be
   avoided when writing in functional style, but can be handy sometimes *)
(* Warning: size will change, and this is not transmitted to the parent house *)
(* Warning: the old content is not freed from memory *)
(* TODO: move everything to Sync (not only set_rooms) ? *)
let copy ~src ~dst =
  let dx = dst.current_geom.x - src.current_geom.x in
  let dy = dst.current_geom.y - src.current_geom.y in
  global_translate src dx dy;
  dst.geometry <- { dst.geometry with w = src.geometry.w; h = src.geometry.h };
  let w,h = get_size src in
  dst.current_geom <- { dst.current_geom with w; h };
  dst.clip <- src.clip;
  dst.background <- src.background;
  begin match src.content with
    | Resident r as c -> r.Widget.room_id <- Some dst.id; dst.content <- c
    | Rooms rooms -> set_rooms dst rooms
  end;
  dst.keyboard_focus <- src.keyboard_focus;
  dst.draggable <- src.draggable

(* Add a room to the dst layout content (END of the list). This does *not*
   enlarge the containing house. The resize function of the room is cancelled. *)
(* This is used to add a pop-up *)
(* warning: the room should NOT already belong to some house. *)
(* TODO: write a "remove_room" function *)
let add_room ?valign ?halign ~dst room =
  if ok_to_add_room ~dst room then begin
    check_layer_error room dst;
    let rooms = get_rooms dst in
    (* we cannot add room to layout which already contains a Resident *)
    (* We now check oversize. But this should not happen. The user should not
       rely on this. If the added room is too large, beware that nothing outside
       of the geometry of the destination room will never have mouse focus
       (mouse focus is detected per house, and THEN into the children rooms. *)
    let wmax = (getx room) + (width room) in
    if wmax > width dst
    then (printd debug_error "The attached Room #%u is too wide" room.id;
          (*set_width dst wmax*));
    let hmax = (gety room) + (height room) in
    if hmax > height dst
    then (printd debug_error "The attached Room #%u is too tall" room.id;
          (*set_height dst hmax*));

    let x = default_lazy (map_option halign (fun a ->
        Draw.align a 0 (width dst) (width room)))
        (lazy (getx room)) in
    let y = default_lazy (map_option valign (fun a ->
        Draw.align a 0 (height dst) (height room)))
        (lazy (gety room)) in
    setx room x;
    sety room y;
    attach ~dst room;

    dst.content <- Rooms (List.rev (room :: (List.rev rooms)))
    (*  fit_content dst;; *)
  end

let set_layer ?(debug = !debug) room layer =
  room.layer <- layer;
  if debug then check_layers room

(* TODO: do some "move layer" or translate layer instead *)
let global_set_layer room layer =
  iter (fun r -> set_layer ~debug:false r layer) room

(** construct a horizontal house from a list of rooms *)
(* sep = horizontal space between two rooms *)
(* hmargin = horizontal margin (left and right). *)
(* vmargin = vertical margin (top and bottom). *)
(* if margins is set, then sep, hmargin and vmargin are all set to this value *)
(* WARNING: resulting layout has position (0,0). *)
let flat ?name ?(sep = Theme.room_margin / 2) ?(adjust=Fit)
      ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin)
      ?margins ?align ?background ?shadow ?canvas ?(scale_content=true) rooms =
  (* List.iter (set_canvas canvas) rooms; *)
  let sep, hmargin, vmargin = match margins with
    | Some m -> m,m,m
    | None -> sep, hmargin, vmargin in
  let rec loop list x y =
    match list with
    | [] -> (x - sep + hmargin, y)
    | r::rest ->
       setx r x;
       sety r vmargin;
       loop rest (x + sep + (width r)) (max y ((height r) + 2*vmargin)) in
  let w,h = loop rooms hmargin vmargin in
  let layout = create ?name ?background ?shadow
                 (geometry ~w ~h ()) ~adjust (Rooms rooms) ?canvas in
  do_option align (fun align -> v_align ~align layout vmargin (h-2*vmargin));
  (* Now that the geometry is finalized, we may compute the resize function for
     each room: *)
  if scale_content then scale_resize_list (w,h) rooms
  else List.iter disable_resize rooms;
  layout

let hbox = flat

(* Construct a flat directly from a list of widgets that we convert to
   Residents. By default it uses smaller margins than [flat]. *)
let flat_of_w ?name ?(sep = Theme.room_margin) ?h ?align ?background ?widget_bg
    ?canvas ?scale_content widgets =
  let rooms =
    List.map (fun wg ->
        let name = map_option name (fun s -> "Resident of [" ^ s ^ "]") in
        resident ?name ?h ~x:0 ~y:sep ?background:widget_bg ?canvas wg) widgets
  in
  flat ?name ~margins:sep ?align ?background ?canvas ?scale_content rooms

let h_center layout x0 w =
  match layout.content with
  | Resident _ -> ()
  | Rooms rs ->
     list_iter rs
       (fun r -> let w0 = width r in
                 let x = Draw.center x0 w w0 in
                 setx r x)

let h_align ~align layout x0 w =
  match layout.content with
  | Resident _ -> ()
  | Rooms rs ->
     list_iter rs
       (fun r -> let w0 = width r in
                 let x = Draw.align align x0 w w0 in
                 setx r x)

(* Create a tower from a list of rooms *)
(* sep = vertical space between two rooms *)
(* hmargin = horizontal margin (left and right). *)
(* vmargin = vertical margin (top and bottom). *)
let tower ?name ?(sep = Theme.room_margin/2) ?margins
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin)
    ?align ?(adjust = Fit) ?background ?shadow ?canvas
    ?clip ?(scale_content=true) rooms =
  (* List.iter (set_canvas canvas) rooms; TODO *)
  let sep, hmargin, vmargin = match margins with
    | Some m -> m,m,m
    | None -> sep, hmargin, vmargin in
  let rec loop list x y =
    match list with
    | [] -> (x, y - sep + vmargin)
    | r::rest ->
      setx r hmargin;
      sety r y;
      loop rest (max x ((width r) + 2*hmargin)) (y + sep + (height r)) in
  let w,h = loop rooms hmargin vmargin in
  let layout = create ~adjust ?name ?background ?shadow ?clip
      (geometry ~w ~h ()) (Rooms rooms) ?canvas in
  do_option align (fun align -> h_align ~align layout hmargin (w-2*hmargin));
  if clip = None && scale_content then scale_resize_list (w,h) rooms else fix_content layout;
  (* TODO ce n'est pas la peine de scaler la largeur si elle ne dépasse pas le
     layout. Voir par exemple la demo/demo *)
  layout

(* Construct a tower directly from a list of widgets that we convert to
   Residents. *)
let tower_of_w ?name ?(sep = Theme.room_margin) ?w ?align ?background ?widget_bg
      ?canvas ?scale_content widgets =
  let rooms =
    List.map (fun wg ->
        let name = map_option name (fun s -> "Resident of [" ^ s ^ "]") in
        resident ?name ?w ~x:sep ~y:0 ?background:widget_bg ?canvas wg) widgets
  in
  tower ?name ~margins:sep ?align ?background ?canvas ?scale_content rooms

(* compute the x,y,w,h that contains all rooms in the list *)
let bounding_geometry = function
  | [] -> printd debug_warning "Trying to find bounding_geometry of empty list";
    0,0,0,0
  | rooms ->
    let rec loop xmin ymin xmax ymax = function
      | [] -> (xmin, ymin, xmax - xmin + 1, ymax - ymin + 1)
      | room :: rest ->
        let x,y = getx room, gety room in
        loop
          (imin xmin x)
          (imin ymin y)
          (imax xmax (width room + x - 1))
          (imax ymax (height room + y - 1))
          rest in
    loop max_int max_int 0 0 rooms

module Grid = struct

  (* return a Selection.t corresponding to the vertical projections of the
     bounding boxes of the rooms in the house. *)
  let detect_rows ?(overlap = 1) house =
    let vranges =
      match house.content with
      | Resident _ -> [(0, height house)]
      | Rooms rooms -> List.map (fun r ->
          let y = gety r in
          (y, y + height r - overlap)) rooms in
    Selection.of_list vranges

  (* same for horizontal projections *)
  let detect_columns ?(overlap = 1) house =
    let hranges =
      match house.content with
      | Resident _ -> [(0, width house)]
      | Rooms rooms -> List.map (fun r ->
          let x = getx r in
          (x, x + width r - overlap)) rooms in
    Selection.of_list hranges

end

(* Superpose a list of rooms without changing their relative (x,y) positions.
   Unless specified by ~w ~h, the resulting layout has the *size* of the total
   bounding box of all rooms. Its (x,y) *position* is such that, when displayed
   at this position, all rooms should be located at the positions they
   claimed. *)
(* TODO it seems that only the first one gets focus... *)
let superpose ?w ?h ?name ?background ?canvas ?(center=false)
    ?(scale_content=true) rooms =
  let x,y,bw,bh = bounding_geometry rooms in
  (* We translate the rooms: *)
  List.iter (fun r ->
      setx r (getx r - x);
      sety r (gety r - y)) rooms;
  let w = default w bw in
  let h = default h bh in
  if center then List.iter (fun r ->
      setx r (Draw.center (getx r) w (width r));
      sety r (Draw.center (gety r) h (height r))) rooms;
  let geometry = geometry ~x ~y ~w ~h () in
  if scale_content then scale_resize_list (w,h) rooms
  else List.iter disable_resize rooms;
  create ?name ?background ?canvas geometry (Rooms rooms)

(** save the layout_id in the user event *)
(* not used anymore *)
let save_to_event_OLD event room =
  Sdl.Event.(set event Trigger.room_id room.id)
(* TODO: since we only use one global event for mouse_enter or mouse_leave, it
   would be more efficient to store directly the room in a global variable,
   rather that storing the id, and then painfully search the room corresponding
   to id... *)

(** ask all the subwidgets to update themselves. *)
(* in fact, this just send the redraw_event, which ask for redrawing the whole
   window. Thus, it would be enough to ask this to only one widget of the layout
   (since all widgets must be in the same window) *)
let rec ask_update room =
  match room.content with
  | Resident w -> Widget.update w
  | Rooms list -> List.iter ask_update list

(** animations: *)
(* animations with Anim are deprecated, use Avar instead *)

let animate_x room x =
  let g = room.geometry in
  Avar.stop g.x;
  room.geometry <- { g with x }

let animate_y room y =
  let g = room.geometry in
  Avar.stop g.y;
  room.geometry <- { g with y }

let animate_w room w =
  let g = room.geometry in
  Avar.stop g.w;
  room.geometry <- { g with w }

let animate_h room h =
  let g = room.geometry in
  Avar.stop g.h;
  room.geometry <- { g with h }

let animate_voffset room voffset =
  let g = room.geometry in
  let avar = Var.get g.voffset in
  let is_current = Avar.started avar && not (Avar.finished avar) in
  Avar.stop avar;
  Var.set g.voffset voffset;
  (* if the animation was already running we need to start immediately,
     otherwise the value that we set here will be valid only for the next
     iteration, which may cause non-immediate transitions: useful ???*)
  if is_current then ignore (get_voffset room)

let animate_alpha room alpha =
  let g = room.geometry in
  Avar.stop g.transform.alpha;
  room.geometry <- { g with transform = { g.transform with alpha }};
  ask_update room

let animate_angle room angle =
  let g = room.geometry in
  Avar.stop g.transform.angle;
  room.geometry <- { g with transform = { g.transform with angle }}

let stop_pos room =
  printd debug_graphics "Stop position animation for layout %s." (sprint_id room);
  let g = room.geometry in
  Avar.stop g.x;
  Avar.stop g.y

(** get desired room (relative) geometry after applying animation *)
let geom r =
  let g = r.geometry in
  to_current_geom g (* the calculation is there *)


(** some predefined animations: *)
(* warning, these animations can be set on-the-fly, so be careful with other
   existing animations *)
let default_duration = 300

(* add a show animation (vertical sliding) to the room; however: *)
(* 1. if the room is already animated, we replace the old animation by the show,
   and the duration is reduced proportionally to the current voffset of the old
   animation *)
(* 2. if the room is shown and without animation, we do nothing *)
let show ?(duration=default_duration) ?from room =
  if room.show && Avar.finished (Var.get room.geometry.voffset)
  then if !debug then begin assert (not room.removed);
      printd (debug_board + debug_warning)
        "Room %s is already shown, we don't run the show animation"
        (sprint_id room)
    end else ()
  (* it is ok to show a room that currently is performing a hide animation. *)
  else begin
    let clip = ref false in
    let init () =
      clip := room.clip;
      set_clip room
    in
    (* it is important to do this AFTER the ending() of the previous
       animation. *)
    let h = height room in
    if not room.show && (get_voffset room <> -h)
    then (printd debug_warning
            "Using a 'show' animation on a room that was not previously in a \
             'hidden' state. Forcing voffset to %d." (-h);
          set_voffset room (-h));
    let h, duration = match from with
      | None ->
        let current_vo = get_voffset room in
        let d' = abs ((current_vo * duration) / h) in
        current_vo, d'
      | Some Avar.Bottom -> h, duration
      | Some Avar.Top -> -h, duration
      | Some _ -> printd debug_board "Layout.show direction not implemented";
        h, duration in
    let ending () =
      printd debug_board "End of show for %s" (sprint_id room);
      room.clip <- !clip in
    let voffset = Avar.show ~init ~ending ~duration h 0 in
    animate_voffset room voffset;
    rec_set_show true room;
    room.removed <- false
  end

(* add a hide animation to the room *)
let hide ?(duration=default_duration) ?(towards = Avar.Bottom) room =
  if (not room.show) (*&& Avar.finished (Var.get room.geometry.voffset)*) then ()
  else begin
      let clip = ref false in
      let init () =
        clip := room.clip;
        set_clip room
      in
      let h = height room in
      let current_vo = get_voffset room in
      let d' = abs ((h + current_vo)*duration) / (abs h+1) in (* DEBUG *)
      let vo = match towards with
        | Avar.Bottom -> h
        | Avar.Top -> -h
        | _ -> printd debug_board "Layout.show direction not implemented"; h in
      let ending _ =
        printd debug_board "End of hide";
        room.clip <- !clip;
        rec_set_show false room;
        remove room in
      (* WARNING: if the room contains subrooms with animations, they will remain
       forever because a layout with show=false is not displayed and hence not
       updated: the anim is not removed. Even more so with Avar. Thus compute
       has_anim during display ? *)
      let voffset = Avar.show ~init ~ending ~duration:d' current_vo vo in
      animate_voffset room voffset
    end

(** scrolling to a particular vertical position y, for a prescribed duration. *)
(* y should be between 0 and the total height *)
(* Warning: in principle, room.house.clip should be set to true for this *)
let scroll_to ?(duration=1000) (*?(weight=0.5)*) y room =
  match room.house with
  | None -> printd debug_error "Cannot scroll the top layout (need a house)"
  | Some house ->
    let current_vo = get_voffset house in
    let y' = max 0 (min y (height room - height house)) in
    let duration = duration * (abs(current_vo + y') + 1) / (abs (current_vo + y) + 1) in
    printd debug_graphics "Scroll room#%u in house #%u, from %d to %d, duration=%d"
      room.id house.id current_vo (-y') duration;
    let voffset = Avar.fromto ~duration current_vo (-y') in
    (* TODO: make a special animation when reaching bounds *)
    animate_voffset house voffset

(** relative scrolling *)
(* the specifications are: the scrolling must be continuous (no jump), and n
   calls to (scroll dy) should end up in the same position as (scroll (n*dy)),
   even if they are triggered before the previous animation is not finished. In
   case of rapid mouse wheel events, it is not obvious to have the scrolling
   look smooth. Thus we add the following spec: the scroll curve should be
   epsilon-close to the h-translated starways curve that would be obtained with
   immediate jumps, where epsilon is the jump height (50 for scroll wheel) and h
   is a small time amout, that we choose to be 2*dt here (it should be less than
   duration, otherwise the second part of the animation g2 is never executed) *)
let scroll_delay = ref 0.5
let scroll_old2 ?(duration=default_duration) dy room =
  do_option room.house (fun house ->
      let current_vo = get_voffset house in
      let avar = Var.get house.geometry.voffset in
      let jump_vo = Avar.final_value avar in
      let final_vo = - (max 0 (min (dy - jump_vo) (height room - height house))) in
      let duration = duration * (abs(final_vo - current_vo) + 1) / (abs (jump_vo - dy - current_vo) + 1) in
      let dt = Avar.progress avar in
      printd debug_graphics "Scroll room #%u: dy=%d, current_vo=%d, jump_vo=%d, final_vo=%d, duration=%d, progress=%f"
        room.id dy current_vo jump_vo final_vo duration dt;
      let voffset = if current_vo = jump_vo || duration = 0
        then (scroll_delay := 0.5;
              Avar.fromto ~duration current_vo final_vo)
        else (* the previous animation was not finished, we need to catch up *)
          let dv = final_vo - jump_vo in (* warning, opposite sign to dy *)
          let h = if dt < !scroll_delay
            then scroll_delay := max 0.3 !scroll_delay /. 1.5
            else if dt > 1.5 *. !scroll_delay
            then scroll_delay := min 0.5 (1.5 *. !scroll_delay);
            !scroll_delay in
          let slope = (float (jump_vo - current_vo)) *. (1. -. h) /.
                      (h *. (float (final_vo - jump_vo))) in
          (* rem we may have (slope < 0.) in case of changing direction *)
          let g1 = Avar.affine (float current_vo) (float jump_vo) in
          let g2 u = Avar.initial_slope ~slope:(max 1. (abs_float slope)) u
                     |> Avar.affine (float jump_vo) (float final_vo) in

          let update _ u = Avar.concat ~weight:h g1 g2 u
                           |> round in
          printd debug_graphics "Scroll: dv=%d, h=%f slope=%f" dv h slope;
          Avar.create ~duration ~update current_vo in
      animate_voffset house voffset)

let last_time = ref (Time.now ());; (* TODO replace this by the time of the Avar *)
let scroll ?(duration=default_duration) dy room =
  do_option room.house (fun house ->
      let current_vo = get_voffset house in
      let avar = Var.get house.geometry.voffset in
      let jump_vo = Avar.final_value avar in (* à vérifier *)
      let final_vo = - (max 0 (min (dy - jump_vo) (height room - height house))) in
      let elapsed = Time.(now () - !last_time) in
      let duration =
        if (elapsed = 0)
        || (elapsed > 300)
        || current_vo = final_vo
        then duration
        else let speed = (float (-dy)) /. (float elapsed) in
          (* = the speed that user expects to have, because she's rolling the
             mouse wheel *this* fast. In pixels per ms. Now we need to adjust
             the duration so that the expected final value is indeed reached at
             that speed. *)
          let final = current_vo + (round (speed *. (float duration))) in
          if final = current_vo then duration
          else abs (duration * (final_vo - current_vo) / (final - current_vo)) in
      printd debug_graphics
        "Scroll room #%u: current:%d, final_vo=%d, duration=%d"
        room.id current_vo final_vo duration;
      let voffset = Avar.fromto ~duration current_vo final_vo in
      last_time := Time.now ();
      animate_voffset house voffset)

let scroll_old ?(duration=300) dy room =
  do_option room.house (fun house ->
      Avar.finish (Var.get house.geometry.voffset); (* TODO: do something smoother *)
      let previous_vo = get_voffset house in
      printd debug_graphics "Scroll room #%u, dy=%d, previous_vo=%d" room.id dy previous_vo;
      scroll_to ~duration (dy - previous_vo) room)

(* find a parent whose house has the 'clip' property *)
let rec find_clip_house room =
  match room.house with
  | None -> None
  | Some h ->
    if h.clip then Some room
    else find_clip_house h

(** add fade_in transform to the existing animation of the room *)
let fade_in ?duration ?(from_alpha = 0.) ?(to_alpha = 1.) room =
  let alpha = Avar.fade_in ?duration ~from_alpha ~to_alpha () in
  animate_alpha room alpha

(** add fade_out transform to the existing animation of the room *)
(* WARNING: fading out to alpha=0 results in a completely transparent room, but
   the room is *still there*. (it's not "hidden"). Which means it can still get
   mouse focus. If you want to hide it, then use hide=true *)
let fade_out ?duration ?from_alpha ?(to_alpha = 0.) ?(hide=false) room =
  let from_alpha = default_lazy from_alpha (lazy (get_alpha room)) in
  let ending _ =
    printd debug_board "End of complete fade_out => hiding room";
    remove room;
    (* One could also use: Trigger.push_from_id Trigger.E.mouse_motion room.id; *)
    (* This forces the board to recompute the new mouse focus. *)
    rec_set_show false room in
  let ending = if hide then Some ending else None in
  let alpha = Avar.fade_out ?duration ?ending ~from_alpha ~to_alpha () in
  animate_alpha room alpha

(* angle in degree *)
(* WARNING: it's not a global rotation. All widgets in the room will rotate
   separately about their own center *)
(* If you want to rotate a complete layout, use a Snapshot *)
let rotate ?duration ?(from_angle = 0.) ~angle room =
  let angle = Avar.fromto_float ?duration from_angle (from_angle +. angle) in
  animate_angle room angle

(* Zoom works for Resident, but for a general Rooms it will not work *)
(* moreover, only few resident widgets will be ok. (image, box...) *)
(* In order to zoom a general layout, use a Snapshot *)
(* TODO: add zoom center *)
let zoom_x ?duration ~from_factor ~to_factor room =
  let w0 = round (float (width room) *. from_factor) in
  let w1 = round (float (width room) *. to_factor) in
  let w = Avar.fromto ?duration w0 w1 in
  printd debug_graphics "ZOOM width from %d to %d" w0 w1; (* DEBUG *)
  animate_w room w

let zoom_y ?duration ~from_factor ~to_factor room =
  let h0 = round (float (height room) *. from_factor) in
  let h1 = round (float (height room) *. to_factor) in
  let h = Avar.fromto ?duration h0 h1 in
  printd debug_graphics "ZOOM height from %d to %d" h0 h1; (* DEBUG *)
  animate_h room h

let zoom ?duration ~from_factor ~to_factor room =
  zoom_x ?duration ~from_factor ~to_factor room;
  zoom_y ?duration ~from_factor ~to_factor room

(** oscillate (for fun) *)
let oscillate ?(duration = 10000) ?(frequency=5.) amplitude room =
  let x = Avar.oscillate ~duration ~frequency amplitude (getx room)in
  animate_x room x

(** add a slide_in animation to the room *)
let slide_in ?duration ?from ?dst room =
  let dst = default dst room in
  let x,y = Avar.slide_in ?from ?duration ~size:(get_size dst)
              ~pos:(getx room, gety room) () in
  animate_x room x;
  animate_y room y

(** translation animation *)
let slide_to ?(duration=default_duration) room (x0,y0) =
  let x1 = getx room in
  let y1 = gety room in
  let x = Avar.fromto ~duration x1 x0 in
  let y = Avar.fromto ~duration y1 y0 in
  animate_x room x;
  animate_y room y

(** follow mouse animation. *)
(* Note that the window is not available before running the layout... *)
let mouse_motion_x ?dx ?modifier room =
  let x0 = ref 0 in (* we store here the dist between mouse and room *)
  let init () =
    x0 := default_lazy dx
        (lazy (fst (Mouse.window_pos (window room)) - xpos room)) in
  let update _ _ =
    let x = fst (Mouse.window_pos (window room)) - (x_origin room) - !x0 in
    match modifier with
    | None -> x
    | Some f -> x + f x in
  Avar.create ~duration:(-1) ~update ~init 0

let mouse_motion_y ?dy ?modifier room =
  let y0 = ref 0 in
  let init () =
    y0 := default_lazy dy
        (lazy (snd (Mouse.window_pos (window room)) - ypos room)) in
  let update _ _ =
    let y = snd (Mouse.window_pos (window room)) - (y_origin room) - !y0 in
    match modifier with
    | None -> y
    | Some f -> y + f y in
  Avar.create ~duration:(-1) ~update ~init 0

(* set the origin of the room at the mouse position - (dx,dy) *)
(* if dx or dy is not specified, the default is the current distance between
   room and mouse *)
(* modifierx and modifiery are executed continuously during the animation, and
   return an integer offset, which can typically be used to obtain a "magnetic"
   effect. See examples/displays *)
(* TODO merge into unique function "action" *)
let follow_mouse ?dx ?dy ?modifierx ?modifiery room =
  let x = mouse_motion_x ?dx ?modifier:modifierx room in
  let y = mouse_motion_y ?dy ?modifier:modifiery room in
  animate_x room x;
  animate_y room y

(* Clip a room inside a smaller container and make it scrollable, and optionally
   add a scrollbar widget (which should appear only when necessary). Currently
   only vertical scrolling is implemented.  *)
let make_clip
      ?w ?(scrollbar = true) ?(scrollbar_inside = false)
      ?(scrollbar_width = 10) ~h room =
  (* iter_rooms disable_resize room; *)
  let name = (default room.name "") ^ ":clip" in
  if w <> None
  then printd debug_error "Horizontal scrolling is not implemented yet";
  let w = default w (width room) in
  let y0 = gety room in
  sety room 0;
  let active_bg = Widget.empty ~w:(width room) ~h:(height room) () in
  (* We add an invisible box to make the whole area selectable by the mouse
     focus. Otherwise, only the parts of the room that contain a widget will
     react to the mouse wheel event. Of course, if the room was full of widgets,
     this is superfluous... *)
  let layer = get_layer room in
  let container = tower ~margins:0 ~clip:true
      [superpose [resident_with_layer ~layer active_bg; room]] in
  (* The container should be a room with a unique subroom (and the active
     background); the subroom can then be scrolled with respect to the container
  *)
  set_size container (w,h);

  let result =
    if scrollbar
    then begin
      (* We first initialize the bar layout with a dummy widget, so that the
         var is able to use it. This is only useful if the height of the
         container is modified after creation, for instance when the user
         resizes the window. *)
      let bar = resident_with_layer ~layer
          ~background:(color_bg Draw.(lighter scrollbar_color))
          (Widget.empty ~w:10 ~h:10 ()) in
      (* The scrollbar is a slider. Its Tvar takes the voffset value into the
         slider value, between 0 and (height room - height container). 0
         corresponds to the bottom position of the slider, so this means the
         *largest* scroll (voffset is the most negative). *)
      let var = Tvar.create container.geometry.voffset
          ~t_from:(fun vo ->
              let dh = height room - height bar in
              if dh <= 0 then 0 (* then the bar should be hidden *)
              else dh + Avar.get vo)
          ~t_to:(fun v ->
              let dh = height room - height bar in
              let v = imin v dh |> imax 0 in
              Avar.var (height bar - height room + v)) in
      let wsli = Widget.slider ~kind:Slider.Vertical ~length:h
          ~thickness:scrollbar_width
          ~tick_size:(h * h / (height room))
          ~var (imax 0 (height room - h)) in
      change_resident bar wsli;
      if h >= (height room) then hide ~duration:0 bar;
      let r = if scrollbar_inside
        then (setx bar (w - width bar);
              set_layer bar (Chain.insert_after
                               (Chain.last (get_layer container))
                               (Draw.new_layer ()));
              (* TODO: is this a bit too much ?? We just want to make
                 sure the scrollbar gets mouse focus. *)
              superpose ~name [container; bar])
        else flat ~name ~margins:0 [container; bar] in
      disable_resize bar;
      (* We register a resize function that simultaneously sets the container
         and the bar sizes. It will hide the bar when the container is large
         enough to display the whole content. *)
      container.resize <- (fun (w,h) ->
          let keep_resize = true in
          set_height ~keep_resize bar h;
          if scrollbar_inside then set_size ~keep_resize container (w,h)
          else begin
            set_size ~keep_resize container (w - width bar, h);
            setx ~keep_resize bar (w - width bar)
          end;
          let dh = height room - height bar in
          let sli = Widget.get_slider wsli in
          if dh >= 1 then Slider.set_max sli dh
          else set_voffset container 0;
          (* Warning: we set the voffset directly, because when the bar is
             hidden, the Tvar will never be activated -- except if the user
             scrolls with the mouse. *)
          let h = height bar in
          if height room <> 0
          then Slider.set_tick_size sli
              (imax (Slider.min_tick_size sli) (h * h / (height room)));
          let v = Slider.update_value sli; Slider.value sli in
          if v < 0 then Slider.set sli 0;
          if dh <= 0
          then rec_set_show false bar
          else rec_set_show true bar);
      r
    end
    else container in

  sety result y0;
  let x0 = getx room in
  setx room 0;
  setx result x0;
  (* We copy the shadow. TODO: this has no effect at the moment, because of the
     'clip' flag, the layout is sharply clipped to its bounding box when
     rendering, so the shadow is hidden. *)
  let shadow = room.shadow in
  result.shadow <- shadow;
  room.shadow <- None;
  result

let relayout createfn ?(duration=200) layout =
  let rooms = get_rooms layout in
  let old_pos = List.map (fun r -> getx r, gety r) rooms in
  (* [createfn] will change the rooms positions, but warning, this may also set
     a new house, we have to set it back. *)
  let () = ignore (createfn rooms) in
  List.iter (fun r -> r.house <- Some layout) rooms;
  if duration <> 0 then
    List.iter2 (fun (oldx,oldy) room ->
        let newx = getx room in
        if oldx <> newx
        then animate_x room (Avar.fromto ~duration oldx newx);
        let newy = gety room in
        if oldy <> newy
        then animate_y room (Avar.fromto ~duration oldy newy))
      old_pos rooms

(* adjust an existing layout to arrange its rooms in a "flat" fashion, as if
   they were created by Layout.flat. Will be animated if duration <> 0 *)
let reflat  (*?(sep = Theme.room_margin / 2)*) ?align
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin) ?margins =
  relayout (fun rooms -> flat ~hmargin ~vmargin ?margins ?align rooms)

(* same as reflat but with Layout.tower *)
let retower (*?(sep = Theme.room_margin / 2)*) ?align
    ?(hmargin = Theme.room_margin) ?(vmargin = Theme.room_margin) ?margins =
  relayout (fun rooms -> tower ~hmargin ~vmargin ?margins ?align rooms)

(* typically in a tower, enlarge all rooms to have the width of the house.  This
   is not recursive: only rooms of depth 1. *)
let expand_width house =
  let w = width house in
  iter_rooms (fun room ->
      let x = getx room in
      if w-x < 1 then printd debug_warning "Cannot expand_width because house x position is larger than width.";
      set_width room (w-x)) house

(* Replace "room" by "by" in lieu and place of the initial room. No size
   adjustments are made. Of course this is dangerous, because it modifies both
   the house and "by". See also [add_room]. *)
(* TODO copy old (x,y) position *)
let replace_room ~by room =
  match room.house with
  | None ->
    printd (debug_error+debug_user)
      "Cannot use \"replace_room\" because room %s does not belong to a \
       house."
      (sprint_id room)
  | Some h when ok_to_add_room ~dst:h room ->
    printd debug_warning "Replacing room %s by room %s inside %s."
      (sprint_id room) (sprint_id by) (sprint_id h);
    detach room;
    attach ~dst:h by;
    h.content <- Rooms (list_replace (equal room) (get_rooms h) by);
  | _ -> printd (debug_board + debug_error) "Cannot replace room %s"
           (sprint_id room)

(* move a room to a new house, trying to keep the same visual
   position. Optionnally adding a scrollbar (in which case the returned layout
   is not the same as the original one). *)
(* WARNING this doesn't take voffset into account, so it won't work if a 'hide'
   animation was used to the room. *)
let relocate ~dst ?(scroll=true) ?(auto_scale=false) room =
  (* TODO check they have the same top_house? *)
  let x0,y0 = compute_pos dst in
  let x1,y1 = compute_pos room in
  (* 'pos_from' won't work here because this is called (by Select) before rooms
     positions are computed... *)

  if room.house <> None then detach room;
  let room2 = if not scroll then room
    else let y2 = y1-y0 + height room in
      printd debug_board "Relocate room : y2=%i y1=%i y0=%i room=%i \
                          dst=%i" y2 y1 y0 (height room) (height dst);
      if y2 <= height dst then room
      else begin (* [here++] TODO If scroll=true it's probably better to use
                    make clip anyway, just in case the size of the house
                    shrinks. *)
        (* sety room 0; *)
        make_clip ~h:(height dst - y1 + y0) ~scrollbar_inside:true
          ~scrollbar_width:4 room
      end in

  (* We add it to the dst *)
  add_room ~dst room2;
  setx room2 (x1-x0);
  sety room2 (y1-y0);
  if auto_scale then scale_resize room2;
  room2

(********************)
(** display section *)
(********************)

let debug_box ~color room x y =
  let w,h = Draw.scale_size (get_size room) in
  let x,y = Draw.scale_pos (x,y) in
  let bg = if room.mouse_focus then Some (Draw.lighter color) else None in
  Draw.rect_to_layer ?bg ~color (get_canvas room) (get_layer room)
      (x,y) w h

let scale_rect rect =
  Sdl.Rect.(create
              ~x:(Theme.scale_int (x rect))
              ~y:(Theme.scale_int (y rect))
              ~h:(Theme.scale_int (h rect))
              ~w:(Theme.scale_int (w rect)))

let scale_clip clip =
  map_option clip scale_rect

let show_keyboard_focus room _transform rect =
  (* TODO use transform *)
  let layer = get_layer room in
  let canvas = get_canvas room in
  let blits = Draw.box_shadow ~offset:(0,0) ~size:(Theme.scale_int 3) ~fill:false
      canvas layer (scale_rect rect) in
  List.iter Draw.blit_to_layer blits

(* Display a room: *)
(* this function sends all the blits to be displayed to the layers, *)
(* it does not directly interact with the renderer. *)
(* pos0 is the position of the house containing the room *)
let display ?pos0 room =
  let x0,y0 = match pos0 with
    | None -> house_pos room
    | Some p -> p in
  let rec display_loop x0 y0 h0 clip0 tr0 r =
    (* clip contains the rect that should contain the current room r. But of
       course, clip can be much bigger than r. *)
    if not r.show then ()
    else begin
      let g = geom r in
      let x = x0 + g.x
      and y = y0 + g.y + h0
      and voffset = g.voffset in
      (* update current position, independent of clip *)
      r.current_geom <- { g with x; y };
      (*print_endline ("ALPHA=" ^ (string_of_float (Avar.old
         room.geometry.transform.alpha)));*)
      let rect = Sdl.Rect.create ~x ~y ~w:g.w ~h:g.h in

      (* if there is a nonzero offset, we perform a new clip : this is used for
         "show/hide" animation *)
      (* TODO clip should be enlarged in case of shadow *)
      let clip = if (*voffset = 0*) not r.clip || !no_clip then clip0
        else Draw.intersect_rect clip0 (Some rect) in
      let sclip = scale_clip clip in
      match clip with
      | Some clip_rect when not (Sdl.has_intersection clip_rect rect) ->
        (r.hidden <- true;
         printd debug_warning "Room #%u is hidden (y=%d)" r.id y)
      (* Because of clip, the rendered size can be smaller than what the geom
         says. *)
      (* If the clip is empty, there is nothing to display. Warning: this means
         that all children will be hidden, even if they happen to pop out of
         this rect. *)
      | _ -> begin
          r.hidden <- false;
          let transform =
            let tr = get_transform r in
            (* printd debug_board "TRANSFORM alpha=%f" tr.Draw.alpha; *)
            let open Draw in
            (* printd debug_board "COMPOSED TRANSFORM alpha=%f" (tr.alpha *. tr0.alpha); *)
            (*{ tr0 with alpha = tr0.alpha *. tr.alpha } in*)
            (* TODO: compose also rotations with centres, flips !! *)
            compose_transform tr0 tr in

          (* background (cf compute_background)*)
          let bg = map_option r.background (fun bg ->
              let box = match bg with
                | Style style ->
                  (* let c = Draw.random_color () in *)  (* DEBUG *)
                  (* let style = Style.create ~background:(Style.Solid c)
                   *     ?shadow:r.shadow () in *)
                  let b = Box.(create ~width:g.w ~height:g.h ~style ()) in
                  r.background <- (Some (Box b));
                  b
                | Box b -> b in
              let blits = Box.display (get_canvas r) (get_layer r) box
                  Draw.(scale_geom {x; y; w = g.w; h = g.h;
                                    voffset = - voffset}) in
              blits) in
          (* !!! in case of shadow, the blits contains several elements!! *)

          if !debug && r.keyboard_focus = Some true
          then show_keyboard_focus r transform rect;

          begin match r.content with
            | Rooms h ->
              (* We only draw the background. Make sure that the layer of the
                 room r is at least as deep as the layers of the Rooms h *)
              do_option bg
                (List.iter
                   (fun blit ->
                      let open Draw in
                      let t = compose_transform transform blit.transform in
                      blit_to_layer { blit with clip = sclip; transform = t }));
              if !draw_boxes then begin
                let rect = debug_box ~color:(0,0,255,200) r x y in
                let open Draw in
                let t = compose_transform transform rect.transform in
                blit_to_layer { rect with clip = sclip; transform = t }
              end;
              List.iter (display_loop x y voffset clip transform) h
            | Resident w ->
              let blits = Widget.display (get_canvas r) (get_layer r) w
                  Draw.({x; y; w = g.w; h = g.h; voffset}) in
              let blits = match bg with
                | None -> blits
                | Some b -> List.rev_append b blits in

              (* debug boxes *)
              let blits = if !draw_boxes
                then
                  let color = (255,0,0,200) in
                  let rect = debug_box ~color r x y in
                  rect :: blits
                else blits in

              List.iter
                (fun blit ->
                   let open Draw in
                   let t = compose_transform transform blit.transform in
                   let clip = sclip in
                   blit_to_layer { blit with clip; transform = t }) blits
          end;
          if !draw_boxes (* we print the room number at the end to make sure
                            it's visible *)
          then let label = B_label.create ~size:7 ~fg:(Draw.(transp blue))
                   (sprint_id r) in
            let geom = Draw.scale_geom {Draw.x; y; w=g.w+1; h=g.h+1; voffset} in
            let blits = B_label.display (get_canvas r) (get_layer r) label geom in
            List.iter Draw.blit_to_layer blits;
            List.iter Draw.unload_blit blits
        end
    end in
  display_loop x0 y0 0 None (Draw.make_transform ()) room

let get_focus room =
  room.mouse_focus

let set_focus room =
  room.mouse_focus <- true

let unset_focus room =
  room.mouse_focus <- false

let set_cursor roomo =
  let cursor = match roomo with
    | None -> go (Draw.create_system_cursor Sdl.System_cursor.arrow)
    | Some room -> match room.content with
      | Rooms _ -> go (Draw.create_system_cursor Sdl.System_cursor.arrow)
      | Resident w -> Widget.get_cursor w in
  Sdl.set_cursor (Some cursor)

(* comme display sauf qu'on ne trace que si nécessaire *)
(* not used anymore *)
let update_old room =
  let rec update_loop x0 y0 h0 clip0 room =
    if not room.show then ()
    else begin
      let g = geom room in (* attention, ça met anim <- None si anim = finished... *)
      let x = x0 + g.x
      and y = y0 + g.y + h0 in
      let clip = if g.voffset = 0 || !no_clip then clip0
        else Draw.intersect_rect clip0 (Some (Sdl.Rect.create ~x ~y ~w:g.w ~h:g.h)) in
      room.current_geom <- current_geom ~x ~y ~w:g.w ~h:g.h ();
      match room.content with
      | Rooms h -> List.iter (update_loop x y g.voffset clip) h
      | Resident w -> if not (Widget.is_fresh w) then begin
          (* if !draw_boxes then Draw.box (renderer room) ~bg:(200,10,20,50) x y g.w g.h; *)
          (* TODO background , transform *)
          let blits = Widget.display (get_canvas room) (get_layer room) w
              {Draw.x = x; y; w = g.w; h = g.h; voffset = g.voffset} in
          List.iter (fun blit -> Draw.(blit_to_layer { blit with clip })) blits
        end
    end in
  let x0,y0 = house_pos room in
  update_loop x0 y0 0 None room

(** check is the room has some non-fresh components. *)
(* optimize (Bogue)? *)
let rec is_fresh room =
  match room.content with
  | Rooms list -> let rec loop = function
      | [] -> true
      | r::h -> if not (is_fresh r) then false
        else loop h in
    loop list
  | Resident w -> Widget.is_fresh w

let room_has_anim room =
  Avar.has_anim room.geometry.transform.alpha ||
  Avar.has_anim room.geometry.transform.center ||
  Avar.has_anim room.geometry.transform.flip ||
  Avar.has_anim room.geometry.transform.angle ||
  List.fold_left (fun b v -> b || (Avar.has_anim v))
    false (get_int_avars room)

(* optimize (Bogue)? *)
(* TODO one could transfer it into Layout.display, which would return the anim
   status of what was displayed (and not what was hidden...) *)
let rec has_anim room =
  if !debug && (not room.show) && (room.hidden) && (room_has_anim room)
  then printd debug_error "Room %s has unfinished animation but it is not shown."
      (sprint_id room);
  room.show && (not room.hidden) &&
  (room_has_anim room ||
   match room.content with
   | Rooms list -> List.fold_left (fun b r -> b || (has_anim r)) false list
   | Resident _ -> false)

(* Flip buffers. Here the layout SHOULD be the main layout (house) of the
   window. Only one canvas/renderer is used, the one specified by the layout. *)
let flip ?(clear=false) ?(present=true) layout =
  printd debug_graphics "flip layout %s" (sprint_id layout);
  (* go (Sdl.set_render_target (renderer layout) None); *)
  if clear then Draw.clear_canvas (get_canvas layout);
  printd debug_graphics "Render layers";
  Var.protect_do Draw.current_layer (fun () ->
      (* : we assume that the layout layer is in the same component as the
         current_layer... TODO do better *)
      Draw.render_all_layers (get_layer layout));
  if present then begin
    printd debug_graphics "Present";
    Draw.(sdl_flip (renderer layout))
  end

(* prerender the layout to the layers *)
let render layout =
  (* let renderer = renderer layout in *)
  (* go (Sdl.render_set_clip_rect renderer None); *)
  (* Draw.(set_color renderer (opaque black)); *)
  (* go (Sdl.render_clear renderer); *)
  (* Draw.clear_canvas (get_canvas layout); *)
  (* We should not clear the canvas here, since all rendering is done at the end
     of the main loop, with flip *)
  if Draw.window_is_shown (window layout) then display layout
  else printd debug_board "Window (layout #%u) is hidden" layout.id

(* The function to call when the window has been resized *)
let resize_from_window ?(flip=true) layout =
  let top = top_house layout in
  if not (equal layout top)
  then printd debug_error "The layout for resizing window should be the top \
                           layout";
  let w,h = Draw.get_window_size (window top)
            |> Draw.unscale_pos in
  let w', h' = get_size top in
  if (w',h') <> (w,h)
  then begin
    (* TODO in rare occasions, it might happen that this test is different
       from get_physical_size top <> Draw.get_window_size win*)
    printd debug_graphics "Resize (%d,%d) --> (%d,%d)" w' h' w h;
    set_size ~keep_resize:true ~check_window:false top (w,h);
    Draw.update_background (get_canvas top);
    layout.resize (w,h);
    if flip then Draw.sdl_flip (renderer top)
  end
(* : somehow we need this intermediate flip so that the renderer takes into
    account the new size. Otherwise texture are still clipped to the old
    size... On the other hand it might flicker if triggered too quickly *)
(* fit_content layout;;*) (* not useful *)

(** initialize SDL if necessary and create a window of the size of the layout *)
let make_window ?window layout =
  printd debug_graphics "Make window";
  let top = top_house layout in
  if not (equal layout top)
  then printd debug_error
      "  The layout for creating a window should be the top layout";
  Draw.video_init (); (* this will compute the scale. If we don't do this here,
                         the size below will be (0,0). *)
  let w,h = get_physical_size top in
  let wmax, hmax = 4096, 4096 in
  (* = TODO ? instead clip to ri_max_texture_width,ri_max_texture_height ? *)
  if wmax < w || hmax < h
  then printd debug_error
      "  The layout has size (%u,%u), which exceeds the max size (%u,%u)."
      w h wmax hmax;
  let w = min w wmax in
  let h = min h hmax in
  let x,y = get_window_pos layout in
  let canvas = Draw.init ?window ?name:layout.name ?x ?y ~w ~h () in
  global_set_canvas top canvas

(* adjust the window size to the top layout *)
(* This should be enforced all the time *)
(* this is not executed immediately, but sent to Sync *)
(* TODO move this directly to the render loop, since it has to be done anyway,
   and should not be done more than once per step. *)
(* TODO combine with [adjust_window_size] *)
let adjust_window ?(display=false) layout =
  Sync.push (fun () ->
      let top = top_house layout in
      if not (equal layout top)
      then printd debug_error
          "The layout for resizing window should be the top layout";
      let w,h = get_physical_size top in
      let win = window top in
      printd debug_graphics "SDL set window size %d x %d" w h;
      Draw.set_window_size win ~w ~h;


      (* resize ~flip:display top; *)
      (* : of course, top didn't really change size, but somehow the texture was
         clipped to the old window size, and if we don't update it, the previous
         clipped texture is stretched to the new window size. *)
      (* render top; *)
      (* flip top; *)
      (* Draw.(flip top.canvas.renderer); *)

      (* Now we render and flip. This is not strictly necessary, as it will surely
         be done by the main loop anyway. But it doesn't hurt to do it twice... *)
      (* it should not be done if the window is hidden, because render targets
         don't work well *)
      if display && Draw.window_is_shown (window top) then begin
        render top;
        flip top
      end)

(* Emit the close-window event to the window containing the layout *)
let push_close r =
  let id = Sdl.get_window_id (window r) in
  Trigger.push_close id

let destroy_window r =
  match window_opt r with
  | Some w ->
    let window_id = Sdl.get_window_id w in
    Trigger.push_destroy_window (r.id) ~window_id
  | None ->
    printd (debug_board + debug_error + debug_user)
      "Cannot destroy window for layout %s because it is not associated with any \
       SDL window." (sprint_id r)

(* the display function we export *)
(* NO we need pos for snapshot.ml *)
(* let display r : unit = display r *)

let inside_geom geometry (x,y) =
  x <= geometry.x + geometry.w && x >= geometry.x &&
  y <= geometry.y + geometry.h && y >= geometry.y

let inside room (x,y) =
  match room.mask with
  | None -> inside_geom room.current_geom (x,y)
  | Some mask -> (* TODO vérifier aussi qu'on est dans la dimension du mask *)
    let x0,y0 = room.current_geom.x, room.current_geom.y in
    let _,_,_,a = Draw.get_pixel_color mask ~x:(x-x0) ~y:(y-y0) in
    a <> 0

(** get the smallest room (with Resident) containing point (x,y), or None *)
(* not used anymore *)
let rec over_focus x y t =
  let g = to_current_geom t.geometry
  (* one should also take into account transforms, clipping... and also if a
     SUBroom is animated, the geometry can extend beyond the initial (fixed)
     house geometry,... TODO *) in
  if t.show && (inside_geom g (x,y))
  then match t.content with
    | Resident _ -> Some t
    | Rooms h ->
      list_check (fun r -> over_focus (x - g.x) (y - g.y) r) h
      (* we translate because geometry is relative *)
  else None

(* instead of the first one, get the complete list *)
(* cf remarks above *)
let rec focus_list_old x y t =
  let g = to_current_geom t.geometry in
  if t.show && (inside_geom g (x,y)) then match t.content with
    | Resident _ -> [ t ]
    | Rooms h -> List.flatten (List.map (fun r -> focus_list_old (x - g.x) (y - g.y) r) h)
  else []

(* instead of the first one, get the complete list *)
(* in each layer, the first element of the list has priority (TODO this is not
   consistent with the fact that it is the first displayed) *)
(* cf remarks above *)
let rec focus_list x y t =
  if t.show && (inside t (x,y)) then match t.content with
    | Resident _ -> [ t ]
    | Rooms h -> List.flatten (List.rev_map (fun r -> focus_list x y r) h)
  else []

(* get the focus element in the top layer *)
let top_focus x y t =
  let flist = focus_list x y t in
  printd debug_graphics "Number of layers detected under mouse: %u (%s)"
    (List.length flist)
    (String.concat " " (List.map (fun r -> "#" ^ (string_of_int r.id)) flist));
  let compare r1 r2 = Chain.compare (get_layer r1) (get_layer r2) in
  list_max compare flist

(** get the smallest room (with Rooms or Resident) containing (x,y), or None *)
(* only used for testing *)
(* TODO à fusionner avec le précédent pour retourner une paire ? *)
(* TODO vérifier qu'on est dans le même calque (layer) *)
let rec hover x y t =
  let g = to_current_geom t.geometry in
  if t.show && (inside_geom g (x,y)) then match t.content with
    | Resident _ -> Some t
    | Rooms h -> (match
                    list_check (fun r -> hover (x - g.x) (y - g.y) r) h
                  with
                  | None -> Some t
                  | o -> o)
  else None
