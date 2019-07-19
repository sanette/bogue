(* A LongList is a layout composed of a list of layouts to be displayed on a
   given region, with scroll bar when necessary, and memory management: only a
   limited number of items are kept alive in memory *)

(* there is no data (variable) attached to a long list. Data management should
   be implemented by the user, for instance via widgets, cf example 34. Cf also
   table.ml *)

(* l'interaction utilisateur vient uniquement de la barre de scrolling (slider).
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
toutes les entrées d'un coup. On va commencer par estimer cette hauteur totale
en faisant la moyenne des hauteurs de chaque entrée calculée, multipliée par le
nombre d'entrées.

                       ________________
                    ^ |                |       ^
                    | |  virtual (ll)  |       |
                    | |                |       |  ll.offset >0
                    | |                |       | 
                    | |     ________________   |             
                    | |    |                |  |  ^          ^
                    | |    |                |  |  |          | scroll_margin
                    | |    |----------------|  |  |          v
                    | |    |   layout       |  |  |  container.geometry.voffset <0
                    | |    |   (room)       |  |  |
                    | |    |                |  v  v
                    | |    |     _______________
        ll.height   | |    |    |               |   S  ^
                    | |    |    |               |   C  |
                    | |    |    | real display  |   R  | max = length slider units
                    | |    |    | (container)   |   O  | to represent the whole 
                    | |    |    |               |   L  | ll.offset range
                    | |    |    |_______________|   L  v
                    | |    |                |             
                    | |    |----------------|             ^
                    | |    |                |             | scroll_margin
                    | |    |________________|             v
                    | |                |
                    v |________________|
                    
contraint: voffset <= 0 and -voffset + display.height  <= layout.height     
warning: slider is from bottom to top: 0 = bottom position 
              
 *)


(* TODO bug d'affichage lorsqu'on change de room, certaines anciennes entrées
ont une mauvaise géométrie et d'affichent par dessous les nouvelles.  Un
Trigger.push_redraw ???; dans Layout.set_rooms permettrait de corriger ça, mais
ce n'est certainement pas le mieux... *)

open Tsdl;;
open B_utils;;
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
  

type entry = | Void
             | Freed
             | Computed of Layout.t;;

type direction = | Up
                 | Down;;
                   
let factor = 5;; (* ok ? factor > 1 to ensure smoother scrolling *)
let min_tick_size = 10;; (* scrollbar handle min size *)
let scroll_margin = 70;;
(* we try to keep at least this amount of pixels above and below the clipped
   layout in order to allow normal mouse wheel scroll *)
  
(* unless specified, "pixel" means "logical pixel". The real ("physical") size
   onscreen is obtained by multiplying by Theme.scale *)
type t = {
    length : int; (* total number of elements *)
    mutable total_height : int option;
    (* = Total height pixels that would be necessary to render all
         entries. None if we are not sure *)
    mutable computed_height :  int;
    (* = total height in pixels of computed entries, ie entries present in the
         current room *)
    offset : (int Avar.t) Var.t;
    (* = the starting vertical position we want to see onscreen *)
    mutable computed : int;
    (* = number of already generated entries (even if they have been freed
         afterward) *)
    mutable rendered_height : int;
    (* = approx. height of the computed layout; it's just used as a hint. In
         principle it will be factor * height of the target (clipped)
         layout. The real height will differ because we always render an integer
         number of entries. *)
    generate : (int -> Layout.t); (* the function to generate the ith entry *)
    cleanup : (Layout.t -> unit); (* cleanup the memory associated with the entry layout *)
    max_memory : int option;
    (* = if not None, then tell the program to do memory management to use only
       some approximate memory maximum for storing the textures (in pixel²) *)
    mutable used_memory : int;
    array : entry array;
    (* we store everything in an array. This choice is questionable, because for
       a large list, only a small part will be kept in memory. The solution we
       take here is to set "None" to entries we want to forget, hoping that this
       won't take much memory space. Maybe we could use a Weak.array *)
    linear : bool; (* linear scale for slider (by default) *)
    mutable first : int;
    mutable last : int;
    (* = index of first & last entries (starting from 0) computed in the room
         below *)
    mutable first_mem : int;
    mutable last_mem : int;
    (* = index of first and last entries computed and still in memory (in the
    array) *)
    (* room : Layout.t; *) (* NOT USED *)
    (* = the complete layout to clip & display. It contains entries from ll.first
         to ll.last, inclusive. *)
    (* the geometry.voffset of the room should always be 0; scrolling is done by
     vofsetting a container layout (see below). The absolute position of the
     room in ll is ll.offset *)
    mutable container_voffset : int;
    (* currently, the container voffset may be changed directly by mouse wheel
       (see bogue.ml and Layout.scroll). Hence we need to save the value here in
       order to sync with these external changes *)
    heights : (int option) array;
    (* = the array of heights of all entries. It may or may not be initialized
         at startup. Value None means the we don't know, the real height will
         then be computed on the fly.*)
  };;

let to_str = function
  | Up -> "Up"
  | Down -> "Down";;

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
  in loop i;;

(* idem *)
let update_first_mem ll i =
  printd debug_memory "New memory range for Long_list = [_%u_,%u]"
         ll.first_mem ll.last_mem;
  let rec loop j =
    if j >= ll.length then (ll.last_mem <- ll.length - 1; failwith "BAAAAh")
    else match ll.array.(j) with
         | Computed _ -> ll.first_mem <- j
         | Void | Freed -> loop (j+1)
  in loop i;;
    
(* reduce memory usage by deleting some entries *)
(* REMARK: instead of this complicated memory management, one could instead
   store entries in a Weak Array, and let them be collected by the GC. (and the
   "free" function can be called via Gc.finalise) *)
(* TODO do we check that we don't delete the one that has just been created? *) 
let reduce_memory ll direction =
  printd debug_memory "Long list: Reduce_memory...";
  let mm = match ll.max_memory with
    | Some mm -> mm
    | None -> failwith "reduce_memory is only called with ll.max_memory is not None" in
  let rec loop j next  =
    if j<0 || j >= ll.length
    then printd debug_error "Memory usage for LongList exceeds maximum value. Beware."
    else
      let j' = next j in
      match ll.array.(j) with
      | Void
      | Freed -> loop j' next
      | Computed l ->
        if j >= ll.first && j <= ll.last
        then printd debug_error "OOPS! cannot remove Longlist entry #%u because it belongs to the current room..." j
        (* TODO = this is not completely correct because this function is
           called before the room is finalized... I think it can be really
           problematic in some situations with big jumps *)
        else begin
          let mem = let (w,h) = Layout.get_physical_size l in w*h in
          printd debug_memory "Cleaning up entry #%d of LongList" j;
          ll.cleanup l;
          (* not necessary in principle. This is done by the Gc.finalise (NOT
             anymore)*)
          Layout.send_to_cemetery l; (* for debugging *)
          ll.array.(j) <- Freed;
          if j >= ll.last_mem then update_last_mem ll j
          else if j <= ll.first_mem then update_first_mem ll j;
          ll.used_memory <- ll.used_memory - mem;
          if ll.used_memory > mm then loop j' next
          (* TODO use a factor eg 3/4 to reduce more memory at once ? but then
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
    loop ll.last_mem (fun i -> i-1);;
  
(* return value or approximation of the total height: *)
let total_height ll =
  default ll.total_height (round (float (ll.computed_height * ll.length) /. (float ll.computed)));;

(* get ith entry *)
(* TODO: not thread safe *)
let get ll i direction =
  match ll.array.(i) with
  | Computed l -> l
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
              printd debug_error "Computed height (%u) for long_list element #%u differs from given height (%u)" h i hh;
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
    end;;

(* TODO write a cleanup function, to update everything in case the user modifies
   the layouts updtream (like in a model-view system) *)

(* compute entries until reaching at most the given height. updates
   ll.first/last. Return the room and the index of last generated entry *)
let compute_room ~height ll i_start direction =
  ll.first <- i_start;
  (* print_endline (sprintf "COMPUTE start=%d" i_start); *)
  let rec loop i ~h list =
    if h >= height || i >= ll.length then List.rev list, (i-1)
    else begin
        ll.last <- i;
        (* = this is to protect from cleaning up already generated entries *)
        let line = get ll i direction in
        let dh = Layout.height line in
        loop (i+1) ~h:(h+dh) (line::list)
      end
  in
  let list, i_final = loop i_start ~h:0 [] in
  let room = Layout.tower ~name:"long_list room" ~sep:0 ~hmargin:0 ~vmargin:0
      list in
  if !debug then assert (ll.last = i_final);
  room, i_final;;

(* lookup new entries in the given direction, starting from index "start"
   (included) until the sum of the heights of all new entries added reaches the
   desired height parameter. Return the computed height and i = next after the
   last looked-up entry *)
(* for very long lists, this can take a lot of time if the user didn't provide
   the heights array; thus we change the cursor in case of wait > 100 ms *)
let addup_entries ll ~start ~height direction =
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
  if !slow then Sdl.set_cursor !cursor;
  (* print_endline (sprintf "ADDUP dir=%s start=%d height=%d ==> h=%u, i=%d" (to_str direction) start height h i); *)
  h,i;;

let update_voffset container dv =
  if dv <> 0 (* this test is important because shift_offset creates a new
               animation... *)
  then Layout.shift_voffset_unsafe container dv;;
    
(* given the required new value offset for of ll.offset we do all the necessary
   side-effects: changing the container voffset and possibly compute a new
   room.  *)
let update_room ll container o =
  let room, active_bg =
    let open Layout in
    match container.content with
    | Rooms [superp] ->
       (match superp.content with
        | Rooms [room; active_bg] -> room, active_bg
        | _ -> failwith "The container should contain a single layout with a list of 2 rooms !")
    | _ -> failwith "The container should contain a single layout with a list of 2 rooms !"
  in
  let h = Layout.height container in
  let ll_height = total_height ll in
  Var.protect Layout.(container.geometry.voffset); (* useful ? *)
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

  Var.protect ll.offset;
  let voffset2 = voffset + offset - o in
  if ((voffset2+scroll_margin < 0) || (ll.first = 0)) &&
       ((h-voffset2+scroll_margin < Layout.height room) || (ll.last = ll.length-1))
  then begin (* then the room is still usable *)
      (* in case of equality voffset2 = 0 or voffset2 = ... , one should still
       do the update except at very top or bottom of list, in order to allow
       mouse wheel scroll to go past the computed room. *)
      update_voffset container (voffset2 - voffset);  (* = offset - o *)
      (* ==> the new value of the container voffset is voffset2 *)
      Var.release Layout.(container.geometry.voffset); 
      ll.container_voffset <- voffset2;
      Var.release ll.offset;
    end
  else begin
      let room' = (* need to compute a new room *)
        printd debug_memory "UPDATE LONG_LIST [%d,%d] => newoffset=%d oldoffset=%d voffset=%d voffset2=%d (approx)height=%d, rendered_height=%d, room.height=%d, MEM=[%d,%d] " ll.first ll.last o offset voffset voffset2 ll_height ll.rendered_height (Layout.height room) ll.first_mem ll.last_mem;
        if voffset2+scroll_margin >= 0 (* we need to compute upwards *)
        then begin
            let direction = Up in
            let add_h = max ((ll.rendered_height - h) / 2 - scroll_margin) (voffset2+scroll_margin) in
            let add_h = min (offset + voffset2) add_h in
            let dh, i_first = addup_entries ll ~start:(ll.first-1) ~height:add_h direction in
            let room', _ = compute_room ~height:ll.rendered_height
                             ll (i_first+1) direction in
            (* Avar.set (Var.get ll.offset) o; *) (* redundant with tvar... *)
            let new_voffset = voffset2 - dh in
            update_voffset container (new_voffset - voffset);
            Var.release Layout.(container.geometry.voffset); 
            ll.container_voffset <- new_voffset;
            room'
          end
        else begin
            let direction = Down in
            let excess_below = (h-voffset2+scroll_margin-Layout.height room) in
            (* TODO should we make sure we don't exceed ll_height ? but this
           should be taken care of by addup_entries *)
            let dh_min, _ = addup_entries ll ~start:(ll.last+1)
                              ~height:excess_below direction in
            let wanted_dh = max dh_min ((ll.rendered_height - h)/2 - scroll_margin) in
            (* since we want to add wanted_dh pixels below, we need to remove
           approx same amout above: so we need to compute the new ll.first *)
            let dh, i_first = addup_entries ll ~start:ll.first
                                ~height:wanted_dh direction in
            let room', _ = compute_room ~height:ll.rendered_height
                             ll i_first direction in
            (* Avar.set (Var.get ll.offset) o; *)
            let new_voffset = voffset2 + dh in
            update_voffset container (new_voffset - voffset);
            Var.release Layout.(container.geometry.voffset); 
            ll.container_voffset <- new_voffset;
            room'
          end
      in
      printd debug_graphics "Room for Long_list is replaced with new range [%d,%d]" ll.first ll.last;
      (* finally we replace the old room by the new one *)
      let active_bg' = Widget.empty ~w:(Layout.width container) ~h:(Layout.height room') () in
      (* TODO we could also keed the old active_bg and just change its size... *)
      (* remark don't use kill_rooms on room or container, because it would also
       kill the entries that are kept in the ll.array. *)
      (* we replace rooms immediately, not waiting for sync, because the slider
       will likely call again this function before rendering (rendering a slider
       involves a call to the Tvar), and then it should have the new
       room. Otherwise we sometimes have artifacts when the old room interferes
       with the new one (and some entries are displayed on top of each other,
       probably because their geometry is not updated correctly). The problem is
       that the scrollbar is on the right, so it is naturally rendered *after*
       the room... too bad *)
      Layout.(set_rooms ~sync:false container [superpose [room'; resident active_bg']]);
      Layout.set_height container h;
      (* Sync.push (fun () -> Layout.detach room; Layout.kill room); *) (* ne
                                                                         sert à rien ? et en plus fait bugguer board.mouse_focus *)
      List.iter Layout.send_to_cemetery [room; active_bg];
      (* TODO the house of room should also be killed (removed from the table) *)
      Var.release ll.offset
    end;;


let create ~w ~h ~length ?(first=0) ~generate ?height_fn
    ?(cleanup=Layout.delete_textures) ?max_memory ?(linear=true)
    ?(scrollbar_width=10) () =
  let wh = Theme.((scale_int w) *  (scale_int (h+2*scroll_margin))) in
  (* Now some memory computations... TODO they are not completely correct,
     because they assume that all generated layouts will have same
     width... which is desirable but not enforced a this point. As a rule of
     thumb, the minimal factor should be 2, ie the memory should be enough to
     store 2x the size of the display. *)
  let max_memory = map_option max_memory (fun mm ->
      if mm < 2*wh
      then (printd debug_error "Memory for long_list is insuficient; taking %u instead" (2*wh); 2*wh)
      else mm) in
  let rendered_height =
    (* it is crucial that max_memory be enough to fill the rendered_height +
       possible extra due to the fact we render an integer number of entries. As a
       rule of thumb we take 2/3 of the "theoretical height". *)
    (* TODO verify if scroll_margin should be taken into account *)
    match max_memory with
    | None -> 2*factor*h/3
    | Some mm ->
      if mm < wh * factor
      then (printd debug_memory "Memory for this long_list should be at least %u for smoother behaviour" (wh * factor);
            Theme.(unscale_int (2*mm/(3*scale_int w))))
      else 2*factor*h/3 in
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
  let ll =
    { total_height = if computed = length then Some computed_height else None;
      computed_height;
      length;
      offset = Var.create (Avar.var 0);
      computed;
      rendered_height;
      generate;
      cleanup;
      max_memory;
      used_memory = 0;
      array = Array.make length Void;
      linear;
      first;
      last = first;
      first_mem = 0;
      last_mem = 0;
      container_voffset = 0;
      heights;
      (* or Array.make length None, if it takes too long *)
    }
  in
  let room, i_final = compute_room ~height:rendered_height ll first Down in
  let ll_height = total_height ll in
  printd debug_memory "Long list of height %d was initialized with %d entries (%d..%d) ouf of %d and height=%d, rendered_height=%d, approx. total height is %d"
    h (i_final+1-first) first i_final ll.length (Layout.height room) ll.rendered_height ll_height;
  (* cf comments in Layout.clip *)
  let active_bg = Widget.empty ~w ~h:(Layout.height room) () in
  let container = Layout.(tower ~name:"long_list container"
                            ~sep:0 ~hmargin:0 ~vmargin:0
                            [superpose [room; resident ~name:"active_bg" active_bg]]) in
  Layout.set_height container h;
  Layout.set_width container w;
  Layout.set_clip container; (* this allows the mouse wheel to change the
                                container.voffset *)
  if h >= ll_height && i_final = ll.length - 1
  then container (* no need for scrollbar *)
  else begin
    let clicked_value = ref None (* TODO protect this *) in
    let steps = max ll.length h in (* TODO can do better, taking tick size into
                                      account *)
    let var = Tvar.create ll.offset (* the var for the scrollbar (slider) *)
        ~t_from: (* from offset we set slider new position *)
          (fun v -> let o = Avar.get v in
            (* here we just have to verify if the user
               did a mouse wheel scroll... *)
            (* TODO it would be better not to call update_room
               each time we want the value of this var. On the
               other hand now I have modified slider.ml to
               reduce the number of calls. *)
            update_room ll container o;
            let tt_height = total_height ll in
            let o_new = Avar.get v in
            steps - round (float (steps * o_new) /. (float (tt_height - h))))
        ~t_to: (* from slider position we compute the offset *)
          (fun s ->
             let lf = float steps in
             let ss = if linear
               then lf -. float s
               else match !clicked_value with
                 | None -> lf -. float s
                 | Some cv -> let x0 = 1. -. float cv /. lf in
                   Slider.slow 4 lf x0 (1. -. float s /. lf) in
             let tt_height = total_height ll in
             let o = round (float (tt_height - h) *. (ss) /. lf) in
             update_room ll container o;
             Avar.var o) in
    (* not that in the definition of this Tvar the container is a global
       variable. Thus it should not be destroyed. However it should be ok to
       modify its contents *)
    let slider = Widget.slider ~kind:Slider.Vertical ~length:h ~step:1
        ~thickness:scrollbar_width
        ~tick_size:(max min_tick_size ((h * h) / ll_height))
        ~var steps in 
    let on_click sl _ _ =
      clicked_value := Slider.clicked_value (Widget.get_slider sl) in
    let c = Widget.connect_main slider slider on_click Trigger.buttons_down in
    Widget.add_connection slider c;
    let on_release _ _ _ =
      clicked_value := None in
    let c2 = Widget.connect_main slider slider on_release Trigger.buttons_up in
    Widget.add_connection slider c2;
    let bar = Layout.(resident ~background:(Solid Draw.scrollbar_color) slider) in
    Layout.(flat ~name:"long_list" ~sep:0 ~hmargin:0 ~vmargin:0 [container; bar])
  end;;

