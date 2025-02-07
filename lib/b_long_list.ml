(* This file is part of BOGUE, by San Vu Ngoc *)

(* A LongList is a layout composed of a list of layouts to be displayed on a
   given region, with scroll bar when necessary, and memory management: only a
   limited number of items are kept alive in memory *)

(* There is no data (variable) attached to a long list. Data management should
   be implemented by the user, for instance via widgets, cf example 34. Cf also
   b_table.ml *)

(* L'interaction utilisateur vient uniquement de la barre de scrolling (slider).
   On utilise donc le TVar, comme dans les scrolling habituels (Layout.clip).
   Cependant ici la variation de cette variable doit entraîner d'autres
   modifications.

 * D'une part on est lié au voffset du layout principal (avec Tvar, car si le
   voffset est changé par ailleurs (ex une animation) il faut que la barre de
   scrolling change aussi)

 * D'autre part le voffset du Layout doit être calculé en fonction de la position
   actuelle de la Longue Liste. Peut-être pas de façon bidirectionnelle (sauf si
   on autorise l'utilisateur à modifier directement la position de la Longue
   Liste).

* Le principe de base est

1. le layout généré (room) a en gros 5 (= factor) fois la hauteur de l'élément
   voulu (de façon à pouvoir déjà scroller un peu sans générer de nouvelle
   entrées, soit 2 avant, 2 après. Peut-être augmenter le chiffre 5 ?

2. on a une taille mémoire à ne pas dépasser: on supprime les textures des
   entrées déjà calculées si on dépasse cette taille. La mémoire utilisée est
   calculée seulement par la surface (en pixels²) des textures. Ça dépend du
   scaling imposé par le thème, donc on utilise Layout.get_physical_size.

   Le layout virtuel qui contiendrait l'ensemble de la liste possède une hauteur
   qu'il est important de connaître, pour ajuster la position de la barre de
   scrolling, mais qu'on ne connaît pas au début, puisqu'on ne veut pas générer
   toutes les entrées d'un coup. On va commencer par estimer cette hauteur
   totale en faisant la moyenne des hauteurs de chaque entrée calculée,
   multipliée par le nombre d'entrées.

*)

(*
                     ________________
      ^           ^ |                |       ^
      |           | |  virtual (ll)  |       |
    A |           | |                |       |  ll.offset >0
      |           | |                |       |
      v           | |     ________________   |
                  | |    |                |  |  ^          ^
                  | |    |                |  |  |          | scroll_margin
                  | |    |---          ---|  |  |          v
                  | |    |   layout       |  |  | container.geometry.voffset <0
                  | |    |  (room+screen) |  |  |
                  | |    |                |  v  v
                  | |    |     _______________
      ll.height   | |    |    |(0,0)          |   S  ^
                  | |    |    |               |   C  |
                  | |    |    | real display  |   R  | max = length slider units
                  | |    |    | (container)   |   O  | to represent the whole
                  | |    |    |               |   L  | ll.offset range
                  | |    |    |_______________|   L  v
                  | |    |                |
                  | |    |---          ---|             ^
                  | |    |                |             | scroll_margin
                  | |    |________________|             v
                  | |                |
                  v |________________|

+ constraint: voffset <= 0 and -voffset + display.height  <= layout.height
+ warning: slider is from bottom to top: 0 = bottom position
+ ll.offset takes values between 0 (included) and ll.height - containrer height (included)

   A = ll.offset + voffset
 *)

(*
Because total height is not known a priori, one has to be very careful when
directly jumping to last entry (clicking on the bottom of the slider)

now the expected behaviour is:

+ If we went too far; then we adjust the voffset and the last entry should show
  up at the very bottom of the container.

+ If we didn't reach the bottom by clicking on the bottom of the slider, we stay
  at the reached position, but we adjust the slider to show that there is some
  more room left to visit below.
*)

open Tsdl
open B_utils
module Layout = B_layout
module Widget = B_widget
module Avar = B_avar
module Theme = B_theme
module Time = B_time
module Var = B_var
module Tvar = B_tvar
module Trigger =  B_trigger
module Draw = B_draw
module Slider = B_slider
module Sync = B_sync
module Update = B_update

type entry =
  | Void
  | Freed
  | Computed of Layout.t

type direction =
  | Up
  | Down

let factor = 5
(* Texture memory for functioning is roughly [factor] times the memory of the
   visible texture. It must be > 3/2. Ensures smoother scrolling. *)
let min_tick_size = 10 (* scrollbar handle min size *)
let scroll_margin = 70
(* we try to keep at least this amount of pixels above and below the clipped
   layout in order to allow normal mouse wheel scroll. *)

(* unless specified, "pixel" means "logical pixel". The real ("physical") size
   onscreen is obtained by Theme.scale *)
type internal = {
  length : int; (* total number of elements (= rows, entries). *)
  mutable total_height : int option;
  (* = Total height pixels that would be necessary to render all entries. None
     if we are not sure. *)
  mutable computed_height :  int;
  (* = total height in pixels of computed entries, ie entries that have been
     present at some point in the current room. *)
  offset : (int Avar.t) Var.t;
  (* = the starting vertical position we want to see onscreen. 0 means first
     entry on top.  *)
  mutable computed : int;
  (* = number of already generated entries (even if they have been freed
     afterward). *)
  mutable min_rendering_height : int;
  (* = approx. height of the computed layout; it's just used as a hint. In
       principle it will be factor * height of the target (clipped)
       layout. The real height will differ because we always render an integer
       number of entries. *)
  generate : (int -> Layout.t); (* the function to generate the ieth entry. *)
  cleanup : (Layout.t -> unit);
  (* cleanup the memory associated with the entry layout *)
  max_memory : int option;
  (* = if not None, then tell the program to do memory management to use only
     some approximate memory maximum for storing the textures (in pixel²). It
     should be at least twice the area of the visible texture, and also twice
     the area of the largest entry. *)
  mutable used_memory : int;
  array : entry array;
  (* We store all the computed layouts in an array, and free them when they are
     not used anymore to monitor memory footprint. This choice is questionable,
     because for a large list, only a small part will be kept in memory. The
     solution we take here is to set "None" to entries we want to forget, hoping
     that this won't take much memory space. Maybe we could use a Weak.array *)
  linear : bool; (* linear scale for slider (by default) *)
  mutable first : int;
  mutable last : int;
  (* = index of first & last entries (starting from 0) computed in the room
     below *)
  mutable first_mem : int;
  mutable last_mem : int;
  (* = index of first and last entries computed and still in memory (in the
     array) *)
  (* MUTABLE room : Layout.t; *) (* NOT USED mais ça pourrait être pratique*)
  (* = the complete layout to clip & display. It contains entries from ll.first
       to ll.last, inclusive. *)
  (* the geometry.voffset of the room should always be 0; scrolling is done by
     vofsetting a container layout (see below). The absolute position of the
     room in ll is ll.offset *)
  mutable container_voffset : int;
  (* Currently, the container voffset may be changed directly by mouse wheel
     (see bogue.ml and Layout.scroll). Hence we need to save the value here in
     order to sync with these external changes. *)
  heights : (int option) array;
  (* = the array of heights of all entries. It may or may not be initialized
       at startup. Value None means the we don't know, the real height will
       then be computed on the fly.*)
  mutable width_warning : bool;
  (* Record if a width warning has been sent, see [check_width]. It will be
     reset to false if the width becomes ok again. *)
  scale_width : bool
  (* If [scale_width] is true, the width of the entries follow the width of the
     main layout. *)
}

type t = {
  layout : Layout.t;
  slider : Widget.t;
  regenerate : unit -> unit;
  ll : internal
}

let to_str = function
  | Up -> "Up"
  | Down -> "Down"

(* When an entry i of the ll.array is freed, the field ll.last_mem should be
   updated. *)
let update_last_mem ll i =
  printd debug_memory "New memory range for Long_list = [%u,_%u_]"
    ll.first_mem ll.last_mem;
  let rec loop j =
    if j < 0 then (ll.last_mem <- 0; failwith "BOOOh")
    (* this should not happen... *)
    else match ll.array.(j) with
      | Computed _ -> ll.last_mem <- j
      | Void | Freed -> loop (j-1)
  in loop i

(* idem *)
let update_first_mem ll i =
  printd debug_memory "New memory range for Long_list = [_%u_,%u]"
    ll.first_mem ll.last_mem;
  let rec loop j =
    if j >= ll.length then (ll.last_mem <- ll.length - 1; failwith "BAAAAh")
    else match ll.array.(j) with
      | Computed _ -> ll.first_mem <- j
      | Void | Freed -> loop (j+1)
  in loop i

(* Reduce memory usage by deleting some entries *)
(* REMARK: instead of this complicated memory management, one could instead
   store entries in a Weak Array, and let them be collected by the GC. (and the
   "free" function can be called via Gc.finalise) *)
(* TODO do we check that we don't delete the one that has just been created? *)
let reduce_memory ll direction =
  printd debug_memory "Long list: Reduce_memory...";
  let mm = match ll.max_memory with
    | Some mm -> mm
    | None ->
      failwith "[reduce_memory] is only called when [ll.max_memory] is not None" in
  let rec loop j next  =
    if j < 0 || j >= ll.length
    then printd (debug_error + debug_memory)
        "Memory usage for LongList exceeds maximum value. Beware."
    else
      let j' = next j in
      match ll.array.(j) with
      | Void
      | Freed -> loop j' next
      | Computed l ->
        if j >= ll.first && j <= ll.last
        then printd debug_error
            "OOPS! cannot remove Long_list entry #%u because it belongs to the \
             current room..." j
            (* TODO = this is not completely correct because this function is
               called before the room is finalized... I think it can be really
               problematic in some situations with big jumps *)
        else begin
          let mem = let (w,h) = Layout.get_physical_size l in w*h in
          printd debug_memory "Cleaning up entry #%d of LongList" j;
          ll.cleanup l;
          Layout.send_to_cemetery l; (* for debugging *)
          ll.array.(j) <- Freed;
          if j >= ll.last_mem then update_last_mem ll j
          else if j <= ll.first_mem then update_first_mem ll j;
          ll.used_memory <- ll.used_memory - mem;
          if ll.used_memory > mm then loop j' next
          (* TODO use a factor eg 3/4 to reduce more memory at once? but then
             make sure that (3/4)memory is enough to avoid deleting currently
             viewed items... *)
        end
  in
  match direction with
  | Down -> (* this means that we are generating entries at the bottom: we delete
               from the top: *)
    printd debug_memory "...from top";
    loop ll.first_mem (fun i -> i+1)
  | Up ->
    printd debug_memory "...from bottom";
    loop ll.last_mem (fun i -> i-1)

(* Return value or approximation of the total height: *)
let total_height ll =
  match ll.total_height with
  | Some h -> h
  | None -> round (float (ll.computed_height * ll.length) /. (float ll.computed))

(* What is the minimal height (mh) that we should really render to the [room]
   layout?  Here are some explanations.

   1. mh should be at least the height of the container (h) (container = visible
   part, see the sketch above) plus twice the [scroll_margin]:

   mh >= h + 2*scroll_margin

   otherwise the room will be constantly updated.

   2. When we scroll to the end of the room, as soon as we reach the bottom
   margin, we update the room, and place the current entry roughly in the middle
   of the room, see [update_room]. The current entry finds itself at the bottom
   of the container, and has roughly half of the rendering height above
   it. Therefore, this half should be no less than h + scroll_margin:

   mh/2 >= h + scroll_margin

   This gives mh >= 2*h + 2*scroll_margin, so this is strictly stronger than the
   first constraint 1. above, which we may hence forget.

   3. For safety, and also for reducing the number of calls to [update_room]
   (for instance: in order to get a fluid scrolling), it is good to be far above
   the constraint 2. Hence we introduce a [factor] variable and decide to use a
   video memory equal to [factor] times the area of the visible container. This
   means roughly that the rendering height will be [factor] times [h], but not
   quite: the memory should also keep spare bits for possible extra due to the
   fact we render an integer number of entries. This extra is a most twice the
   maximum height of the entries (one for the first displayed entry, one for the
   last displayed entry). But, we don't know this height... unless we scan all
   entries, which we don't want to do. So, as a rule of thumb, we reserve 1/3
   memory for the extra: we take mh to be 2/3 of the "theoretical height"
   [factor * h].

   In view of 2., we need [2 * factor * h / 3 >= 2*h + 2*scroll_margin], so

   factor >= 3 * (1 + scroll_margin/h)

   For standard layouts, scroll_margin<h, so factor=6 is good. In problematic
   cases, Instead of modifying [factor] we simply take [mh] to be the max of the
   two constraints.

   4. When [max_memory] is hinted by the user, we need to compute the
   corresponding factor, which we call then [x].  *)
let compute_min_rendering_height ll (w,h) =
  assert (w*h  <> 0);
  let mh = match ll.max_memory with
    | Some mm when Theme.scale_was_init () ->
      let container_area =  Theme.((scale_int w) * (scale_int h)) + 1 in
      let x = float mm /. float container_area in
      (* the required memory contains [x] times the container; So [x] is the
         custom [factor]. *)
      let min_factor = 3. *. (1. +. float scroll_margin /. float h) in
      if x < min_factor then printd (debug_error + debug_memory + debug_user)
          "[max_memory=%i] for Long_list is too small; we need at least %i"
          mm (round (min_factor *. float container_area));
      let x = max x min_factor in
      round (2. *. x *. (float h) /. 3.)
    | _ -> 2 * factor * h / 3 in
  let mh = imax (2 * scroll_margin + 2 * h + 2) mh in
  printd debug_memory "Long_list [min_rendering_height] = %i (h=%i, w=%i)" mh h w;
  mh

(* Get ieth entry; if it was already computed, we return it but also detach it
   from its house. *)
(* TODO: not thread safe *)
let get ll i direction =
  match ll.array.(i) with
  | Computed l -> if not (Layout.is_detached l) then Layout.detach l;
    l
  | Void
  | Freed -> begin
      (* print_string (sprintf "GET (compute) %d " i); *)
      let entry = ll.generate i in
      let (w,h) = Layout.get_physical_size entry in
      ll.used_memory <- ll.used_memory + w*h;
      if i > ll.last_mem then ll.last_mem <- i
      else if i < ll.first_mem then ll.first_mem <- i;
      printd debug_memory "Long list: used memory: %d" ll.used_memory;
      if ll.array.(i) = Void then (* we may have to update height *)
        begin
          let h = Layout.height entry in
          match ll.heights.(i) with
          | None ->
            ll.computed_height <- ll.computed_height + h;
            ll.computed <- ll.computed + 1;
            if ll.computed = ll.length
            then ll.total_height <- Some ll.computed_height;
            ll.heights.(i) <- Some h
          | Some hh ->
            if hh <> h
            then begin
              printd debug_error "Computed height (%u) for long_list element #%u \
                                  differs from given height (%u)" h i hh;
              ll.heights.(i) <- Some h;
              ll.computed_height <- ll.computed_height + h - hh;
              do_option ll.total_height (fun _ ->
                  ll.total_height <- Some ll.computed_height)
            end
        end;
      ll.array.(i) <- Computed entry;
      do_option ll.max_memory (fun mm ->
          if ll.used_memory > mm then reduce_memory ll direction);
      entry
    end

(* TODO write a cleanup function, to update everything in case the user modifies
   the layouts updtream (like in a model-view system) *)

(* Compute entries until reaching at most the given height. Update
   ll.first/last. Return the room and the index of last generated entry. Note
   that the width of the room may vary with i_start. Note that contrary to
   [addup_entries], [i_start] is really the first (smallest) index; here,
   [direction] is only used for memory management.

   WARNING: this modifies the previous [room]! Because already computed entries
   will be detached from [room] to be placed in the the one.

   Warning, the [room] height should never be modified after creation! *)
let compute_room ~height ~width ll i_start direction =
  printd debug_custom "[Long_list.compute_room] start = %i" i_start;
  if ll.length > 0 then begin
    assert (i_start >= 0);
    ll.first <- i_start;
    (* print "COMPUTE start=%d" i_start; *)
    let rec loop i ~h list =
      if h >= height || i >= ll.length then List.rev list, (i-1)
      else begin
        ll.last <- i;
        (* = this is to protect from cleaning up already generated entries *)
        let line = get ll i direction in
        do_option width (Layout.set_width line);
        let dh = Layout.height line in
        loop (i+1) ~h:(h+dh) (line::list)
      end
    in
    let list, i_final = loop i_start ~h:0 [] in
    let room = Layout.tower ~name:(Printf.sprintf "long_list room %i" i_start)
        ~margins:0 list in
    if !debug then assert (ll.last = i_final);
    room, i_final
  end else begin
    printd (debug_warning + debug_user) "Long_list is empty.";
    Layout.empty ~w:0 ~h:0 (), -1
  end

let add_heights_NO ~first ~last ll direction =
  let heights = ll.heights in
  let rec loop i h =
    if i > last then h
    else let dh = match heights.(i) with
        | None -> Layout.height (get ll i direction)
        | Some dh -> dh in
      loop (i+1) (h+dh) in
  loop first 0

(* Lookup new entries in the given direction, starting from index "start"
   (included) until the sum of the heights of all new entries added reaches the
   desired height parameter. Return the computed height and i = next integer
   after the last looked-up entry (next means + ou - 1 depending on
   [direction]). *)
(* For very long lists, this can take a lot of time if the user didn't provide
   the heights array; thus we change the cursor in case of wait > 100 ms *)
let addup_entries ll ~start ~height direction =
  assert (height >= 0);
  let heights = ll.heights in
  let time = Time.now () in
  let slow = ref false in
  let cursor = ref None in
  let rec loop i h =
    if h >= height || i < 0 || i > ll.length - 1 then h,i
    (* note, i = -1 is a valid output *)
    else let dh = match heights.(i) with
        | None -> Layout.height (get ll i direction)
        | Some dh -> dh
      in
      if not !slow && Time.now () - time > 100 then
        (slow := true;
         cursor := Sdl.get_cursor ();
         Sdl.set_cursor (Some (go (Draw.create_system_cursor Sdl.System_cursor.wait))));
      loop (if direction = Up then i-1 else i+1) (h+dh)
  in
  let h,i = loop start 0 in
  if h < height then printd debug_warning
      "Long_list: [addup_entries] bottom reached before desired height.";
  if !slow then Sdl.set_cursor !cursor;
  printd debug_memory "Long_list ADDUP dir=%s start=%d height=%d ==> h=%u, i=%d"
    (to_str direction) start height h i;
  h,i

let shift_voffset container dv =
  if dv <> 0
  (* : this test is important because shift_offset creates a new animation... *)
  then Layout.shift_voffset container dv

let check_width ll w room =
  match ll.width_warning, Layout.width room > w with
  | true, true -> ()
  | false, true -> printd debug_user "Long_list rows are larger than the room width.";
    ll.width_warning <- true
  | true, false -> ll.width_warning <- false
  | false, false -> ()

(* Given the required new value offset [o] for of [ll.offset] we do all the
   necessary side-effects: changing the container voffset and possibly compute a
   new room. Note that even if [o] does not change, the height of the container
   way have been modified.  *)
let update_room ?(force=false) ll container o =
  let scrolling, room =
    let open Layout in
    match container.content with
    | Rooms [scrolling] ->
      (match scrolling.content with
       | Rooms [_active_bg; room] -> scrolling, room
       | _ -> failwith "The container should contain a single layout with a list \
                        of 2 rooms!")
    | _ -> failwith "The container should contain a single layout with a list of \
                     2 rooms!"
  in
  let h = Layout.height container in
  let ll_height = total_height ll in
  (* Var.protect Layout.(container.geometry.voffset); *) (* useful? *)
  let offset = Avar.get (Var.get ll.offset) in
  let voffset = Layout.get_voffset container in
  let offset, o =
    if voffset <> ll.container_voffset
    (* we need to shift both ll.offset and o; this can happen after mouse wheel
       scroll *)
    then let offset = offset + ll.container_voffset - voffset in
      Avar.set (Var.get ll.offset) offset;
      let o = o + ll.container_voffset - voffset in
      ll.container_voffset <- voffset;
      offset, o
    else offset, o in

  Var.protect_do ll.offset (fun () ->
      (* We compute the required [voffset] according to [o]: if [o] increases,
         then voffset should decrease (remember: voffset<=0) *)
      (* In case of equality voffset2 = 0 or voffset2 = - height of room , one
         should still do the room update (except at very top or bottom of
         list), in order to allow mouse wheel scroll to go past the computed
         room. This is the role of [scroll_margin]. *)
      let voffset2 = voffset + offset - o in
      if not force
      && ((voffset2 + scroll_margin < 0) (* top margin is still enough *)
          || (ll.first = 0) (* this is the first entry of the list *)
         )
      && ((h - voffset2 < Layout.height room - scroll_margin )
          (* bottom margin is still enough *) ||
          (ll.last = ll.length - 1) (* last entry of the list *)
         )
      then begin (* the room is still usable *)
        printd debug_custom "Long_list: room still usable, o=%i" o;
        shift_voffset container (voffset2 - voffset);  (* = offset - o *)
        (* ==> the new value of the container voffset is voffset2 *)
        (* Var.release Layout.(container.geometry.voffset); *)
        ll.container_voffset <- voffset2;
        check_width ll (Layout.width container) room
      end
      else begin
        let width = if ll.scale_width then Some (Layout.width scrolling) else None in
        let room2 = (* need to compute a new room *)
          (* print_string "NEW ROOM"; *)
          printd debug_memory
            "Update Long_list [%d,%d] => newoffset=%d oldoffset=%d voffset=%d \
             voffset2=%d (approx)height=%d, min_rendering_height=%d, \
             room.height=%d, MEM=[%d,%d] " ll.first ll.last o offset voffset voffset2
            ll_height ll.min_rendering_height (Layout.height room)
            ll.first_mem ll.last_mem;

          if voffset2+scroll_margin >= 0 && ll.first > 0
          then begin  (* we need to compute upwards *)
            (* print_endline "==>UP"; *)
            let direction = Up in
            let add_h = imax (* how many pixels to add above the room *)
                ((ll.min_rendering_height - h) / 2 - scroll_margin)
                (* : we try to keep in the middle of min_rendering_height *)
                (voffset2 + scroll_margin)
                (* : if >0, this is how far we are above the top margin *) in
            let add_h = min (offset + voffset2) add_h in
            (* we cannot ask for more than the distance to the first entry (=A
               in the sketch) *)

            (* dh is the exact number of pixels that we finally add: *)
            let dh, i_first = addup_entries ll ~start:(ll.first-1)
                ~height:add_h direction in
            let room', _ = compute_room ~width ~height:ll.min_rendering_height
                ll (i_first+1) direction in
            (* Avar.set (Var.get ll.offset) o; *) (* redundant with tvar... *)
            let new_voffset = voffset2 - dh in
            shift_voffset container (new_voffset - voffset);
            (* Var.release Layout.(container.geometry.voffset); *)
            ll.container_voffset <- new_voffset;
            room'
          end
          else begin
            (* print_endline "==>DOWN"; *)
            let direction = Down in
            let add_h =
              (* height between bottom margin of [room] and bottom of
                 [container]. If >0, this means that the container is too low
                 (below the margin). *)
              h - voffset2 - Layout.height room + scroll_margin in
            (* Here we don't check that we don't exceed [ll_height], because
               this should be taken care of by [addup_entries]. *)
            let add_h = imax add_h
                ((ll.min_rendering_height - h) / 2 - scroll_margin) in
            (* : we try to keep in the middle of min_rendering_height *)

            (* This determines our new tentative last entry: *)
            let hdown, i_last = addup_entries ll ~start:(ll.last+1)
                ~height:add_h direction in
            (* Now we compute the new [ll.first]: *)
            let hup, i_first = addup_entries ll ~start:(i_last-1)
                ~height:ll.min_rendering_height Up in

            (* dh is the exact number of pixels that we finally add above: *)
            let dh = hup - Layout.height room - hdown in
            let room', _ = compute_room ~width ~height:hup
                ll (i_first+1) direction in
            (* Avar.set (Var.get ll.offset) o; *)
            let new_voffset = voffset2 - dh in
            shift_voffset container (new_voffset - voffset);
            (* Var.release Layout.(container.geometry.voffset); *)
            ll.container_voffset <- new_voffset;
            room'
          end
        in
        printd debug_graphics
          "Room for Long_list is replaced with new range [%d,%d]"
          ll.first ll.last;
        (* if !debug then printd debug_custom "Before replace: %s" *)
        (*     (string_of_option B_print.layout_down (Layout.get_house room)); *)
        (* Finally we replace the old room by the new one: *)
        Layout.(set_height ~keep_resize:true scrolling (height room2));
        assert (Layout.replace_room ~by:room2 room);
        (* We immediately update the entry positions so that a new mouse focus
           can be detected without waiting another frame after redraw: *)
        Layout.update_current_geom container;
        (* if !debug then printd debug_custom "After replace: %s" *)
        (*     (string_of_option B_print.layout_down (Layout.get_house room2)); *)
        if width = None then begin
          check_width ll (Layout.width container) room2;
          Layout.disable_resize room2
        end else Layout.resize_follow_width room2;
        Layout.remove room;
        Layout.send_to_cemetery room

        (* Remark: don't use kill_rooms on room or container, because it would
           also kill the entries that are kept in the ll.array. *)
        (* We replace rooms immediately, not waiting for sync, because the
           slider will likely call again this function before rendering
           (rendering a slider involves a call to the Tvar), and then it should
           have the new room. Otherwise we sometimes have artifacts when the old
           room interferes with the new one (and some entries are displayed on
           top of each other, probably because their geometry is not updated
           correctly). The problem is that the scrollbar is on the right, so it
           is naturally rendered *after* the room... too bad *)
      end)

(* not widely tested... be careful *)
(* Remarque: pour le fun on pourrait ne libérer que certains et les autres on les
   anime pour retourner à leur place, haha *)
let free_all container ll =
  if ll.length > 0 then begin
    ll.used_memory <- 0;
    let a = ll.array in
    for i = 0 to ll.length - 1 do a.(i) <- Void done;
    ll.first_mem <- 0;
    ll.last_mem <- 0;
    update_room ~force:true ll container (Avar.get (Var.get ll.offset))
  end

(* [dummy_clip] is used for an empty list. The (dummy) slider needs to be part
   of the (dummy) layout for the refresh function to operate. *)
let dummy_clip (w,h) =
  let slider = Widget.slider 0 in
  let dum = Layout.resident ~w ~h slider in
  Layout.set_show dum false;
  Layout.hide ~duration:0 dum;
  dum, dum, slider

(* A variant of Layout.make_clip with virtual height and optional nonlinear
   slider. [steps] is the minimal number of steps that the slider should
   have. [bottom_reached] is true if the provided room contains the bottom of
   the virtual room. The scrollbar is added to the right, but inside the
   required [w] width. (Hence it may cover the right part of the room content.)

   If [scale_width] is true, all entries will be stretched to the desired width
   [w]. *)
let make_clip ?name ~w ~h ~scrollbar_width ll room =
  check_width ll w room;
  let module L = Layout in
  (* cf comments in Layout.clip *)
  (* let background = L.color_bg Draw.(set_alpha 40 red) in (\* DEBUG *\) *)
  let active_bg = Widget.empty ~w ~h:(L.height room) ()
                  |> L.resident ~name:"active_bg" (* ~background *)  in
  let scrolling = L.superpose ~name:"scrolling" ~w [active_bg; room] in
  if ll.scale_width then L.resize_follow_width room else L.disable_resize room;
  (* set this via a parameter? Scaling the width could be interesting, but it's
     difficult because a priori we don't know what is the max width of
     entries. *)
  let container =
    L.(tower ~name:"long_list container" ~clip:true ~margins:0 [scrolling]) in
  L.set_size ~keep_resize:true container ~w ~h;
  (*  Because of [clip:true] when creating [container], the [resize] field of
      [scrolling] is not automatically created. *)
  L.resize_follow_width scrolling;

  let ll_height = total_height ll in
  let clicked_value = ref None in
  let tick_size = max min_tick_size ((h * h) / ll_height) in
  let steps = imax ll.length h in (* TODO? can do better, taking tick size into
                                     account? *)
  let var = Tvar.create ll.offset (* the var for the scrollbar (slider) *)
      ~t_from: (* from offset we set slider new position *)
        (fun v -> let o = Avar.get v in
          (* print "FROM o=%i" o; *)
          (* here we just have to verify if the user did a mouse wheel
             scroll... *)
          (* TODO it would be better not to call update_room each time we want
             the value of this var. On the other hand now I have modified
             slider.ml to reduce the number of calls. *)
          update_room ll container o;
          let tt_height = total_height ll in
          let o_new = Avar.get v in
          let h = L.height container in (* may change in case of resize *)
          steps - round (float (steps * o_new) /. (float (tt_height - h))))
      ~t_to: (* from slider position we compute the offset *)
        (fun s ->
           (* print_endline "TO"; *)
           let lf = float steps in
           let ss = if ll.linear
             then lf -. float s
             else match !clicked_value with
               | None -> lf -. float s
               | Some cv -> let x0 = 1. -. float cv /. lf in
                 Slider.slow 4 lf x0 (1. -. float s /. lf) in
           let tt_height = total_height ll in
           let h = L.height container in
           let o = imax 0 (round (float (tt_height - h) *. ss /. lf)) in
           update_room ll container o;
           (* now [total_height ll] may have a better precision *)
           (* we re-update in case we went too far. *)
           let o2 = imin (total_height ll - h) o in
           if o <> o2 then begin (* o > o2 *)
             printd debug_custom
               "Long_list: slider went too far; o=%i o2=%i h=%i" o o2 h;
             shift_voffset container (o - o2);
             ll.container_voffset <- ll.container_voffset + o - o2
           end;
           Avar.var o2) in
  (* Note that in the definition of this Tvar the container is a global
     variable. Thus it should not be destroyed. However it should be ok to
     modify its contents. *)
  let slider = Widget.slider ~kind:Slider.Vertical ~length:h ~step:1
      ~thickness:scrollbar_width ~tick_size ~var steps in
  if not ll.linear then begin
    let on_click sl _ _ =
      clicked_value := Slider.clicked_value (Widget.get_slider sl) in
    let c = Widget.connect_main slider slider on_click Trigger.buttons_down in
    Widget.add_connection slider c;
    let on_release _ _ _ =
      clicked_value := None in
    let c2 = Widget.connect_main slider slider on_release
        Trigger.buttons_up in
    Widget.add_connection slider c2
  end;
  let bar = L.(
      resident ~name:"bar" ~background:(color_bg Draw.scrollbar_color) slider) in
  let name = default name "long_list" in
  let layout = L.(superpose ~name [container; bar]) in
  L.disable_resize bar;
  container.resize <- (fun (w, hh) -> (* size of the long_list layout *)
      let open L in let open Resize in
      let th = total_height ll in
      let h = imin hh th in
      set_height bar hh;
      set_size container ~w ~h;
      setx bar (w - width bar);
      ll.min_rendering_height <- compute_min_rendering_height ll (w, h);
      let s = Tvar.get var in (* this can trigger update_room (too much?) *)
      (* print "slider=%i, height=%i" s (total_height ll); *)
      let sli = Widget.get_slider slider in
      if s < 0 then Slider.set sli 0;
      (* one could simply use Tvar.set var 0 *)
      if h <> hh then begin
        (* hh > ll.total_height: we display everything and remove the bar *)
        set_voffset container 0;
        (* we don't use not Tvar for setting voffset because the bar will be
           removed and hence won't look up Tvar. *)
        if is_shown bar then rec_set_show false bar;
        (* set_width container w *)
      end else begin
        Slider.set_tick_size sli (imax min_tick_size (h * h / th));
        if not (is_shown bar) then rec_set_show true bar;
        set_width container (imax 0 (w - width bar));
      end);
  container.resize (w, h); (* this takes care of show/hide bar at startup *)
  (* if !debug then printd debug_custom "At creation: %s" *)
  (*     (string_of_option B_print.layout_down (Layout.get_house room)); *)
  container, layout, slider

let pixel_area w h =
  Theme.((scale_int w) * (scale_int (h+2*scroll_margin)))

(* Increase max_memory if necessary; no decrease. *)
let adjust_max_memory ~w ~h = function
  | None -> None
  | Some mm -> let wh = pixel_area w h in
    if mm < wh * factor
    then (printd (debug_user + debug_memory)
            "Memory for this long_list should be at least %u for smoother \
             behaviour" (wh * factor);
          Some (wh * factor))
    else Some mm

(* Public interface *)

let create ?name ~w ~h ~length ?(first=0) ~generate ?height_fn
    ?(cleanup=Layout.delete_textures) ?max_memory ?(linear=true)
    ?(scrollbar_width=10) ?(scale_width=false) () : t =
  let length = if length >= 0 then length else begin
      printd (debug_error + debug_user)
        "[Long_list.create]: the [length] argument should be non negative, got \
         [%i] instead." length; 0 end in
  let h = if h > 0 then h else begin
      printd (debug_error + debug_user)
        "Long_list height should be positive, got [%i] instead." h; 10 end in
  let w = if w > 0 then w else begin
      printd (debug_error + debug_user)
        "Long_list width should be positive, got [%i] instead." w; 20 end in

  (* Now some memory computations... TODO they are not completely correct,
     because they assume that all generated layouts will have same
     width... which is desirable but not enforced a this point. As a rule of
     thumb, the minimal factor should be 2, ie the memory should be enough to
     store 2x the size of the display. *)
  let max_memory = adjust_max_memory ~h ~w max_memory in
  let min_rendering_height = 2 * factor * h / 3 in (* will be updated below *)
  let no_height_fn_provided = (height_fn = None) in
  let height_fn = default height_fn (fun _ -> None) in
  let heights = Array.init length height_fn in
  let computed_height, computed =
    if no_height_fn_provided then 0, 0
    else let rec loop i comp h =
           if i >= length then h, comp
           else let comp', h' = match height_fn i with
               | None -> comp, h
               | Some y -> (comp+1), (h+y) in
             loop (i+1) comp' h' in
      loop 0 0 0 in

  (*let box = Widget.box ~w ~h ~background:(Box.Solid Draw.none) () in *)
  (*let dummy_room = Layout.resident box in*)
  let first = if first < length && first >= 0 then first
    else begin
      if length <> 0 then printd (debug_error + debug_user)
          "[Long_list.create]: the [first] argument should be between 0 and \
           [length-1], got [%i] instead." first;
      if first < 0 then 0 else length - 1
      (* so length = 0 && first = -1 is "valid" *)
    end in
  let ll =
    { total_height = if computed = length then Some computed_height else None;
      computed_height;
      length;
      offset = Var.create (Avar.var 0);
      computed;
      min_rendering_height;
      generate;
      cleanup;
      max_memory;
      used_memory = 0;
      array = Array.make length Void;
      (* or Array.make length None, if it takes too long *)
      linear;
      first;
      last = first;
      first_mem = 0;
      last_mem = 0;
      container_voffset = 0;
      heights;
      width_warning = false;
      scale_width } in
  Sync.push (fun () ->
      (* We sync this because of Theme.scale. Most of the time this is not
         necessary because creation of room has already initialized Video, and
         [min_rendering_height] is already set by the call to [container.resize]
         above.  *)
      ll.min_rendering_height <- compute_min_rendering_height ll (w, h));
  let width = if scale_width then Some w else None in
  let room, i_final = compute_room ~height:min_rendering_height ~width
      ll first Down in
  let ll_height = total_height ll in
  printd (debug_memory + debug_board)
    "Long list of height %d was initialized with %d entries (%d..%d) ouf of %d \
     and height=%d, rendered_height=%d, approx. total height is %d"
    h (i_final+1-first) first i_final ll.length (Layout.height room)
    ll.min_rendering_height ll_height;
  let container, layout, slider =
    if length > 0
    then make_clip ?name ~w ~h ~scrollbar_width ll room
    else dummy_clip (w, h) in
  let regenerate () = free_all container ll in
  { regenerate; layout; slider; ll }

let get_layout t = t.layout

let redraw t =
  Update.push t.slider

let regenerate t = t.regenerate ()

let create_layout ?name ~w ~h ~length ?(first=0) ~generate ?height_fn
    ?(cleanup=Layout.delete_textures) ?max_memory ?(linear=true)
    ?(scrollbar_width=10) ?(scale_width=false) () =
  get_layout (create ?name ~w ~h ~length ~first ~generate ?height_fn ~cleanup
                ?max_memory ~linear ~scrollbar_width ~scale_width ())

(** Return the maximal value of the scrollbar attached to the Long_list (if any). *)
let get_scroll_steps t =
  Slider.get_max (Widget.get_slider t.slider)

let get_scroll_value t =
  let s = Widget.get_slider t.slider in
  Slider.update_value s;
  let v = Slider.value s in
  printd debug_custom "Scroll value=%i, ll.offset=%i" v (Avar.get (Var.get t.ll.offset));
  v

let set_scroll_value t v =
  let oldo = (Avar.get (Var.get t.ll.offset)) in
  Slider.set (Widget.get_slider t.slider) v;
  printd debug_custom "Set_scroll %i, old offset=%i, new_offset=%i" v oldo
    (Avar.get (Var.get t.ll.offset));
  redraw t

let get_scroll t = (* percentage *)
  1. -. float (get_scroll_value t) /. (float (get_scroll_steps t))

let set_scroll t x =
  set_scroll_value t (round ((1. -. x) *. (float (get_scroll_steps t))))
