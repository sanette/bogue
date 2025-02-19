(* This file is part of BOGUE, by San Vu Ngoc *)

(* Table layout. *)

(* il faut pouvoir changer la largeur des colonnes. Donc garder accès aux
   layouts... ou au moins les recréer *)
(* TODO add line number/label *)
(* TODO it raises exception if length = 0, but should we make it work
   anyways? *)

open Printf
open B_utils
module Button = B_button
module Draw = B_draw
module Layout = B_layout
module Label = B_label
module Long_list = B_long_list
module Selection = B_selection
module Space = B_space
module Theme = B_theme
module Tvar = B_tvar
module Trigger = B_trigger
module Var = B_var
module Widget = B_widget

(* this is the public, non-mutable type *)
type column =
  { title : string;
    length : int;
    rows : int -> Layout.t;
    compare : (int -> int -> int) option;
    (* use "compare i1 i2" in order to compare entries i1 and i2 *)
    min_width : int option;
    align : Draw.align option
  }

type sort =
  | Ascending (* increasing *)
  | Descending (* decreasing *)

type column_private = {
  title : string;
  rows : int -> Layout.t;
  compare : (int -> int -> int) option;
  mutable width : int;
  align : Draw.align option;
  mutable sort : sort option;
  mutable set_sort : (sort option -> unit) option
}

type internal = {
  length : int; (* number of rows *)
  data : column_private array;
  selection : (Selection.t, Selection.t) Tvar.t; (* selection of rows *)
  max_selected : int option;
  (* maximal size of selection. If max_selected=1 then the clicked entry will
     always be selected (and the previously selected entry discarded.)
     Otherwise, clicking a row will only toggle the row.  *)
  mutable last_selected : int option; (* index ii in the currently sorted view *)
  on_click : (internal -> int -> unit) option;
  order : int array; (* we keep here the bijection ith entry --> jth displayed *)
  titles : Layout.t array;
  row_height : int;
  layout : (Layout.t option) Var.t; (* the global layout *)
  long_list : (Long_list.t option) Var.t;
  min_width : int;
  min_height : int; (* min_width and min_height are hints, it's up to the user
                       to respect them or not. *)
}

type t = internal

let redraw t = do_option (Var.get t.long_list) Long_list.redraw
let get_layout (t : t) = remove_option (Var.get t.layout)
let get_selection t = Tvar.get t.selection
let set_selection t sel =
  Tvar.set t.selection sel;
  if Selection.size sel = 1
  then Selection.iter (fun i ->
      t.last_selected <- array_find_index (fun j -> i = j) t.order) sel
  else t.last_selected <- None;
  (* We need to regenerate the entries to update their background. *)
  (* refresh t *) (* abusif? *)
  do_option (Var.get t.long_list) Long_list.regenerate
let get_ll t = remove_option (Var.get t.long_list)

let set_scroll t x = Long_list.set_scroll (get_ll t) x
let get_scroll t = Long_list.get_scroll (get_ll t)

let min_width t = t.min_width
let min_height t = t.min_height


let title_margin = 4
let title_background = Layout.color_bg Draw.(set_alpha 90 Button.color_off)
let row_hl = Layout.color_bg Draw.(set_alpha 40 Button.color_off)
let row_selected = Layout.opaque_bg Draw.(pale Button.color_off)
let icon_color = Draw.(set_alpha 100 grey)

(* [max_width c] returns the max width of the entries of the column c,
   optionally limited to the first n_max entries. *)
let max_width ?(n_max = 50) (c : column) =
  let n_max = imin n_max c.length in
  let rec loop i m =
    if i = n_max then m
    else let w = Layout.width (c.rows i) in
         loop (i+1) (imax m w) in
  loop 0 0

(* Sets the sort-indicator icon. Does nothing if column is not sortable. *)
let set_indicator icon t =
  if t.compare = None then ()
  else begin
    Label.set icon (match t.sort with
        | None -> Theme.fa_symbol "sort"
        (* terminology in font_a is reversed *)
        | Some Ascending -> Theme.fa_symbol "sort-desc"
        | Some Descending -> Theme.fa_symbol "sort-asc")
  end

let set_sort icon t sort =
  t.sort <- sort;
  set_indicator icon t

(* Make title labels with given (or guessed) minimal width, and optional icons
   for sorting columns. *)
let make_title (c : column) =
  (* We compute the label widget. *)
  let label = Widget.label ~align:Draw.Min c.title in
  (* If no width is specified, we compute the max width of the first entries of
     the column *)
  let lw,_ = Widget.default_size label in
  let w = match c.min_width with
    | Some w -> w
    | None -> (imax lw (max_width c)) in
  let layout, icon =
    if c.compare = None
    then (* First encapsulate in order to then left-align. *)
      Layout.flat_of_w ~sep:0 [label], None
    else begin (* add icon for sorting *)
      let sort_indicator = Widget.icon ~fg:icon_color "sort" in
      let icon = Widget.get_label sort_indicator in
      let sort_indicator = Layout.resident sort_indicator in
      Layout.setx sort_indicator (w - Layout.width sort_indicator);
      let layout = Layout.superpose ~w [Layout.resident label; sort_indicator] in
      Space.keep_right ~reset_scaling:true sort_indicator;
      layout, Some icon
    end in
  Layout.set_width layout w; (* not necessary in the case of sort_indicator *)
  let (_,h) = Widget.default_size label in
  let click_area = Widget.empty ~w ~h () in
  let title = Layout.(superpose [layout; resident click_area]) in (*AAA*)
  title, icon

(* [get_area] extracts the click_area widget from the title layout *)
(* Warning: this depends on the way title is created in make_title, see
   (*AAA*) *)
let get_area title =
  let open Layout in
  match title.content with
  | Rooms [_; area] -> widget area (* see AAA *)
  | _ -> failwith "table.ml: The title layout should contain [layout; area]"

let get_row _ _ (* t i *) =
  () (* ??? *)

let make_column (c: column) (w, icon) : column_private =
  let t = { title = c.title;
    rows = c.rows;
    compare = c.compare;
    width = w;
    align = c.align;
    sort = None;
    set_sort = None
          } in
  let () = match c.compare, icon with
    | None, _
    | _, None -> ()
    | _, Some icon -> t.set_sort <- Some (set_sort icon t) in
  t

let make_columns (columns : column list) widths_icons =
  List.map2 make_column columns widths_icons

let make_table ?row_height ?selection ?max_selected ?on_click
    ?on_select (columns : column list) =
  let length, rw = match columns with
    | [] -> failwith "Cannot create empty table"
    | c0::_ ->
      List.iter (fun (c : column) ->
          if c.length <> c0.length
          then failwith "Table columns must have same length")
        columns;
      c0.length,
      if c0.length > 0 then Layout.height (c0.rows 0)
      else (printd debug_warning "Table column has zero element"; 10) in
  let row_height = default row_height rw in
  let titles_icons = List.map make_title columns in
  let min_width = List.fold_left (fun m ti -> m + Layout.width ti) 0
      (List.map fst titles_icons) in
  let min_height = Layout.height (fst (List.hd titles_icons)) in
  let widths_icons = titles_icons
                     |> List.map (fun (title, icon) -> (Layout.width title, icon)) in
  let data = make_columns columns widths_icons
             |> Array.of_list in
  let sel = default selection Selection.empty in
  let on_select = default on_select nop in
  let selection = Tvar.create (Var.create sel)
      ~t_from:(fun x -> x) ~t_to:(fun s -> on_select s; s) in
  {
    length;
    data;
    selection = selection;
    last_selected = None;
    max_selected;
    on_click;
    order = Array.init length (fun i -> i);
    titles = Array.of_list (List.map fst titles_icons);
    (* : useful ? we have the layout below *)
    row_height;
    layout = Var.create None; (* will be computed afterwards *)
    long_list = Var.create None; (* will be computed afterwards *)
    min_width; min_height
  }


let unselected_bg ii =
  if ii mod 2 = 1
  then Some (Layout.color_bg Draw.(set_alpha 20 grey))
  else None

(* [get_background] returns the background to use for this entry. [i] is the
   entry number in the original array and in position [ii] in the display. *)
let get_background t i ii =
  if Selection.mem (Tvar.get t.selection) i
  then Some row_selected
  else unselected_bg ii


let is_valid_sel_size t sel =
  match t.max_selected with
  | None -> true
  | Some m -> Selection.size sel <= m

(* Get the ieth row without the click_area, but only if it is computed and
   currently in use by the Long_list. Warning, [get_row] uses the specific
   structure of the row generated by [generate] below. *)
let get_ll_row t ii =
  check_option (Var.get t.long_list) (fun ll ->
      match ll.ll.array.(ii) with
      | Computed room ->
        (match room.content with
        | Rooms [row; _ca] -> Some row
        | _ -> None)
      | _ -> None)

let make_long_list ~w ~h t  =
  (* let debug_bg = Layout.color_bg Draw.(set_alpha 40 green) in *)
  (* Generate row #ii: *)
  let generate = fun ii ->
    let i = t.order.(ii) in
    let background = get_background t i ii in
    let click_area = Widget.empty ~w ~h:t.row_height () in
    let ca = Layout.resident ~name:(sprintf "click_area %u(%u)" i ii) click_area in
    let row = let open Layout in
      Array.mapi (fun j c ->
          let wj = width t.titles.(j) in
          let name = sprintf "entry[%u,%u]" i j in
          let align = t.data.(j).align in
          let r = tower ~clip:true ~resize:Resize.Linear (* ~background:debug_bg *)
              ~hmargin:title_margin ~sep:title_margin ~vmargin:0
              ?align ~name [c.rows i] in
          set_width r (wj  + title_margin);
          (* r.resize <- (fun _ -> *)
          (*     let open Resize in *)
          (*     set_width r (width t.titles.(j) + title_margin)); *)
          r) t.data
      |> Array.to_list
      (* |> List.cons (resident left_margin) *)
      |> flat ~margins:0 ?background in
    let enter _ = (Layout.set_background ca (Some row_hl)
    (* Layout.fade_in ca ~duration:150 *)) in
    let leave _ = Layout.set_background ca None
    (* Layout.fade_out ca ~duration:150 *) in
    (* TODO: PROBLEM if one adds Layout.fade_in/out animations here, it becomes
       very slow when one tries to scroll at the same time ==> cf
       "check_mouse_motion board" dans bogue.ml *)
    Widget.mouse_over ~enter ~leave click_area;
    (* TODO click is not good with touchscreen *)
    let click _ =
      printd debug_event "Table: click on entry %i" i;
      let () = match t.on_click with None -> () | Some f -> f t i in
      if Trigger.shift_pressed ()
      then begin
        (* Click + shift *)
        printd debug_event "Table: shift-click";
        do_option t.last_selected (fun ii0 ->
            let sel = Selection.fold (fun ii s -> Selection.add s t.order.(ii))
                (Selection.range (imin ii0 ii, imax ii0 ii)) Selection.empty in
            let new_sel = Selection.union sel (Tvar.get t.selection) in
            if is_valid_sel_size t new_sel then Tvar.set t.selection new_sel;
            do_option (Var.get t.long_list) Long_list.regenerate)
      end
      else let new_sel =
             (* if Trigger.ctrl_pressed () *)
             (* then Selection.toggle t.selection i *)
             (* else if Trigger.shift_pressed () *)
             (* then (match t.last_selected with *)
             (*       | Some i0 -> *)
             (*          Selection.(union t.selection [Range (min i i0, max i i0)]) *)
             (*       | None -> Selection.[Range (i,i)]) *)
             (* else Selection.[Range (i,i)] in *)
             (* TODO: At this point this (standard) selection mechanism
                         with CRTL and SHIFT does not work because we need to
                         recompute the how long list to update all backgrounds. This
                         will have to be added afterwards. For the moment we only
                         toggle: *)
             if t.max_selected = Some 1
             then Selection.range (i,i)
             else Selection.toggle (Tvar.get t.selection) i in
        if is_valid_sel_size t new_sel then Tvar.set t.selection new_sel;
        Layout.set_background row (get_background t i ii);

        if t.max_selected = Some 1
        (* We need to reset the previously selected background. Problem, from
           here we don't know the previous [row]... we have to use
           [get_ll_row]. *)
        then do_option t.last_selected (fun li ->
            if li <> ii then do_option (get_ll_row t li) (fun row ->
                Layout.set_background row (unselected_bg li)));

        if Selection.mem new_sel i then t.last_selected <- Some ii

    in
    Widget.on_click ~click click_area;

    (* Selection using keyboard *)
    let keyboard _ _ ev =
      let open Tsdl.Sdl in
      (* Select all on CTRL-A *)
      if (t.max_selected = None || remove_option t.max_selected >= t.length) &&
         Event.(get ev keyboard_keycode) = K.a &&
         Event.(get ev keyboard_keymod) land Kmod.ctrl <> 0
      then begin
        Tvar.set t.selection (Selection.range (0,t.length - 1));
        do_option (Var.get t.long_list) Long_list.regenerate
      end in
    let c = Widget.connect_main click_area click_area keyboard [Trigger.key_down]
    in Widget.add_connection click_area c;

    Layout.(set_width ca (width row));
    (* [width row] might be different from [w] in case of resizing. *)
    Layout.(superpose [row; ca])
  in
  let height_fn _ = Some t.row_height in
  Long_list.create ~w ~h ~generate ~height_fn ~scale_width:true ~length:t.length ()

let make_layout ~h t =
  let align = Draw.Max in (* bottom align *)
  let titles_list = Array.to_list t.titles in
  let titles_row = Layout.flat ~name:"titles_row" ~margins:title_margin
      ~background:title_background ~align titles_list in
  let w = Layout.width titles_row
    (* title_margin + (List.fold_left (fun y r -> y + title_margin + Layout.width r) *)
    (*                   0 titles_list)  *) in
  let long = make_long_list t ~w ~h:(h - Layout.height titles_row) in
  titles_row, long

(* In-place reverse bijection of array *)
let reverse_array a =
  if Array.length a > 0 then
  let l = Array.length a - 1 in
  for i = 0 to l/2 do
    let x = Array.unsafe_get a i in
    Array.unsafe_set a i (Array.unsafe_get a (l-i));
    Array.unsafe_set a (l-i) x
  done

(* Refreshes the table by creating a new long_list. Warning this changes the
   height variable [h] and hence the max value of the slider.  *)
let refresh t =
  Var.with_protect t.layout (function
      | None -> failwith "table.ml: field t.layout should not be None"
      (* TODO don't crash here and provide a default ? But this should never
         happen *)
      | Some r ->
        let w,h,titles_row =
          let open Layout in
          match r.content with
          | Rooms [titles_row; long_old] ->
            width titles_row, height long_old, titles_row
          | _ -> failwith "table.ml: layout content is corrupted"
          (* TODO don't crash ? *)
        in
        let long = make_long_list ~w ~h t in
        Var.set t.long_list (Some long);
        let long_room = Long_list.get_layout long in
        (* this is the dangerous part: *)
        (* Layout.(long.geometry <- g); *)
        (* Layout.(long.current_geom <- to_current_geom g); *)
        (* = not really necessary, because I have removed do_adjust in set_rooms *)
        Layout.set_rooms ~sync:false r [titles_row; long_room];
        Layout.retower ~duration:0 ~margins:0 r;
        Layout.rec_set_layer titles_row r.layer;
        Layout.rec_set_layer long_room r.layer;
        (* Layout.update_current_geom r; *)
        (* Layout.resize_tower ~hmargin:0 ~vmargin:0 ~sep:0 ~align:Draw.Min r; *)
        (* long.resize (Layout.get_size r); *)
        Layout.resize_follow_width titles_row
    )

(* Change sorting order. We don't try to modify the long_list in-place, we
   create a new one *)
let change_order t j sort =
  let column = t.data.(j) in
  do_option column.compare (fun compare ->
      Array.stable_sort compare t.order;
      if sort = Some Descending then reverse_array t.order;
      apply_option column.set_sort sort;
      for i = 0 to Array.length t.titles - 1 do
        if i <> j then apply_option t.data.(i).set_sort None
      done;
      refresh t)

(* Return Some (j, true) if jeth column is sorted in reverse order, (false for
   normal order). FIXME: this does not give information about the secondary
   order of other columns (which at this point we didn't record anywhere,
   anyway... This depends on the [sort] algo that we used, whether is preserve
   secondary orders or not) *)
let get_sorted_column t =
  let n = Array.length t.titles in
  let rec loop j =
    if j = n then None
    else match t.data.(j).sort with
      | Some sort -> Some (j, sort = Descending)
      | None -> loop (j+1) in
  loop 0

let connect_title t j =
  if t.data.(j).compare = None then ()
  else begin
    let widget = get_area t.titles.(j) in
    let click _ =
      let sort = match t.data.(j).sort with
        | None -> Some Ascending
        | Some Ascending -> Some Descending
        | Some Descending -> Some Ascending in
      change_order t j sort in
    Widget.on_click ~click widget;
    let enter _ =
      let title = t.titles.(j) in
      Layout.set_background title (Some title_background) in
    let leave _ =
      let title = t.titles.(j) in
      Layout.set_background title None in
    Widget.mouse_over ~enter ~leave widget;
  end


(* We just share the selection variable via a Tvar to automatically update the
   layout when the selection is changed. *)
(* let make_selection_tvar t = *)
(*   let t_from sel = sel in (\* the user can access the selection via this *\) *)
(*   let t_to sel = (\* this is what is done when the user will modifiy the *)
(*                     selection using Tvar.set *\) *)
(*     TVar.set t.selection sel; (\* this is redundant with Tvar.set, but we need it *)
(*                                 to be done *before* refresh... *\) *)
(*     refresh t; *)
(*     sel in *)
(*   Tvar.create (t.selection) ~t_from ~t_to *)


(* this returns the main layout and the selection variable *)
let create ~h ?row_height ?(name="table") ?on_click ?max_selected
    ?selection ?on_select (columns : column list) =
  let t = make_table columns ?row_height ?on_click
      ?max_selected ?selection ?on_select in
  let titles_row, long = make_layout ~h t in
  let long_layout = Long_list.get_layout long in
  let table = Layout.tower ~margins:0 ~name
      [titles_row; long_layout] in
  Layout.resize_follow_width titles_row;
  Layout.resize_keep_margins long_layout;
  Var.set t.layout (Some table);
  for j = 0 to List.length columns - 1 do
    connect_title t j
  done;
  Var.set t.long_list (Some long);
  let w0, h0 = Layout.get_size titles_row in
  table.resize <- (let open Layout.Resize in fun (w, h) ->
      set_size table ~w:(imax w0 w) ~h:(imax (2 * h0) h));
  t

(* Create table from text array a.(i).(j) : row i, column j *)
let of_array ~h ?widths ?row_height ?name ?on_click ?max_selected
    ?selection  ?on_select ?align headers a =
  let head = Array.of_list headers in
  let ni = Array.length a in
  if ni = 0 then failwith "Cannot create table with empty array."
  else let nj = Array.length a.(0) in
    if nj <> Array.length head
    then failwith "Cannot create table: headers size does not fit the number of \
                   columns."
    else let widths = match widths with
        | None -> List.map (fun _ -> None) headers
        | Some list -> list in
      let widths = Array.of_list widths in
      if Array.length widths <> nj
      then failwith "Cannot create table: list of widths does not fit the number \
                     of columns."
      else let columns =
             head
             |> Array.mapi (fun j title ->
                 { title;
                   length = ni;
                   rows = (fun i -> Layout.resident (Widget.label ?align a.(i).(j)));
                   compare = Some (fun i1 i2 -> compare a.(i1).(j) a.(i2).(j));
                   min_width = widths.(j);
                   align })
             |> Array.to_list in
        create ~h ?on_click ?on_select ?row_height ?name ?max_selected ?selection columns

(* From a Csv.t style list of rows (first row must be the header). Warning: this
   functions first converts to an array, ie. the data is likely to be duplicated
   in memory *)
let of_list ~h ?widths ?row_height ?name
    ?max_selected ?selection ?on_select ?align = function
  | [] -> failwith "Cannot create table with empty list."
  | headers::rows ->
    let a = List.map Array.of_list rows
            |> Array.of_list in
    of_array ~h ?on_select ?widths ?row_height ?name ?max_selected
      ?selection ?align headers a

let sort_column t ?(reverse=false) j = change_order t j
    (if reverse then Some Descending else Some Ascending)

(* * * * *)

(* Bizarre, [table] rame bcp plus que juste [long_list]. *)
