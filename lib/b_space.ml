(* New implementation of Space, based on the Layout resize mechanism. *)
(* Since we cannot know in advance where the space layout will reside, we invoke
   a Sync action to install the resize actions. Warning: Sync actions are
   executed in the same order as they are registered. Another possibility (as in
   the previous Space implementation) is to use an Avar (animated variable),
   which would force actions to be executed in the order of the layout hierarchy
   (top to bottom). However: 1. the Avar is computed *after* the resize functions
   are executed. 2. Avar actions are not triggered if the layout is not shown. *)

(* Warning: currently Space elements can not be applied to a scrollable layout
   obtained by [make_clip]. *)

module Layout = B_layout
module Avar = B_avar
module Sync = B_sync

open B_utils

(* Not used *)
let push_avar room action =
  let update _ _ = action (); 0 in
  let avar = Avar.create ~duration:0 ~update 0 in
  Layout.animate_w room avar

let push _ = Sync.push

(* split a list into two lists: the one before and the one after the first
   element for which test is true. This element is not included in the
   result. *)
let split_at test list =
  let rec loop before = function
    | [] -> failwith "split_at: no matching element was not found in the list."
    | a::rest -> if test a
      then List.rev before, rest
      else loop (a::before) rest in
  loop [] list

(* Only one hfill element will work in a flat. [right_margin] is the margin you
   would like to keep at the right end of the list of rooms. By default we take
   the same as the left margin. *)
let make_hfill_sync ?right_margin layout =
  let open Layout in
  let rooms = siblings layout in
  List.iter resize_fix_x rooms;
  let before,after = split_at (equal layout) rooms in
  let bx,_,bw,_ = bounding_geometry before in
  let ax,_,aw,_ = bounding_geometry after in
  let initial_width = width layout in
  (* The margins around the layout: *)
  let margin_left = getx layout - bx - bw in
  let margin_right = ax - getx layout - width layout in
  (* The margin at the right end of the flat: *)
  let right_margin = default right_margin
                       (if before = [] then margin_left else bx) in
  let old_resize = layout.resize in
  let resize (w,h) =
    let keep_resize = true in
    let available_width = w - aw - bw - bx
                          - margin_left - margin_right - right_margin
                          |> imax initial_width in
    (* TODO compute bounding_geometry here? *)
    let offset = available_width - width layout in
    old_resize (w,h);
    set_width ~keep_resize layout available_width;
    List.iter (fun r ->
        setx ~keep_resize r (getx r + offset)) after in
  layout.resize <- resize

let make_hfill ?right_margin layout =
  push layout (fun () ->
      make_hfill_sync ?right_margin layout;
      Layout.resize layout)

let hfill ?right_margin () =
  let l = Layout.empty ~w:0 ~h:0 ~name:"hfill-space" () in
  make_hfill ?right_margin l;
  l

let make_vfill_sync ?bottom_margin layout =
  let open Layout in
  let rooms = siblings layout in
  List.iter resize_fix_y rooms;
  let before,after = split_at (equal layout) rooms in
  let _,by,_,bh = bounding_geometry before in
  let _,ay,_,ah = bounding_geometry after in
  let initial_height = height layout in
  (* The margins around the layout: *)
  let margin_top = gety layout - by - bh in
  let margin_bottom = ay - gety layout - height layout in
  (* The margin at the bottom end of the tower: *)
  let bottom_margin = default bottom_margin
                        (if before = [] then margin_top else by) in
  let resize (_w,h) =
    let keep_resize = true in
    let available_height = h - ah - bh - by
                           - margin_top - margin_bottom - bottom_margin
                           |> imax initial_height in
    (* TODO compute bounding_geometry here? *)
    let offset = available_height - height layout in
    set_height ~keep_resize layout available_height;
    List.iter (fun r ->
        sety ~keep_resize r (gety r + offset)) after in
  layout.resize <- resize

let make_vfill ?bottom_margin layout =
  push layout (fun () ->
      make_vfill_sync ?bottom_margin layout;
      Layout.resize layout)

let vfill ?bottom_margin () =
  let l = Layout.empty ~w:0 ~h:0 ~name:"vfill-space" () in
  make_vfill ?bottom_margin l;
  l

let full_width_sync ?right_margin ?left_margin layout =
  let open Layout in
  let left_margin = default_lazy left_margin (lazy (getx layout)) in
  let right_margin = default right_margin left_margin in
  resize_fix_x layout;
  let f = layout.resize in
  let resize (w,h) =
    let keep_resize = true in
    f (w,h);
    setx ~keep_resize layout left_margin;
    set_width ~keep_resize layout (w - left_margin - right_margin) in
  layout.resize <- resize

(* Warning, [full_width] and [full_height] pile up a new resize function on top
   of the old one. Hence if it is applied many times on the same layout,
   performance will be degraded. *)
let full_width ?right_margin ?left_margin layout =
  push layout (fun () ->
      full_width_sync ?right_margin ?left_margin layout;
      Layout.resize layout)

let full_height_sync ?top_margin ?bottom_margin layout =
  let open Layout in
  let top_margin = default_lazy top_margin (lazy (gety layout)) in
  let bottom_margin = default bottom_margin top_margin in
  resize_fix_y layout;
  let f = layout.resize in
  let resize (w,h) =
    let keep_resize = true in
    f (w,h);
    sety ~keep_resize layout top_margin;
    set_height ~keep_resize layout (h - top_margin - bottom_margin) in
  layout.resize <- resize

let full_height ?top_margin ?bottom_margin layout =
  push layout (fun () ->
      full_height_sync ?top_margin ?bottom_margin layout;
      Layout.resize layout)

(* Wee warning above. Or use reset_scaling = true. *)
let keep_bottom_sync ~reset_scaling ?margin layout =
  let open Layout in
  match layout.house with
  | None -> printd (debug_board + debug_error)
              "Cannot apply [keep_bottom_sync] to room %s because it has no \
               house."
              (sprint_id layout)
  | Some house ->
     let bottom = default_lazy margin
                    (lazy (height house - gety layout - height layout)) in
     let f = layout.resize in
     let resize (w,h) =
       let keep_resize = true in
       if not reset_scaling then f (w,h);
       sety ~keep_resize layout (h - height layout - bottom) in
     layout.resize <- resize

let keep_bottom ?(reset_scaling = false) ?margin layout =
  push layout (fun () ->
      keep_bottom_sync ~reset_scaling ?margin layout;
      Layout.resize layout)

(* Wee warning above. Or use reset_scaling = true. *)
let keep_right_sync ~reset_scaling ?margin layout =
  let open Layout in
  match layout.house with
  | None -> printd (debug_board + debug_error)
              "Cannot apply [keep_right_sync] to room %s because it has no house."
              (sprint_id layout)
  | Some house ->
     let right = default_lazy margin
                   (lazy (width house - getx layout - width layout)) in
     let f = layout.resize in
     let resize (w,h) =
       let keep_resize = true in
       if not reset_scaling then f (w,h);
       setx ~keep_resize layout (w - width layout - right) in
     layout.resize <- resize

let keep_right ?(reset_scaling = false) ?margin layout =
  push layout (fun () ->
      keep_right_sync ~reset_scaling ?margin layout;
      Layout.resize layout)
