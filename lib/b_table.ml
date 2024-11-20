(* This file is part of BOGUE, by San Vu Ngoc *)

(* Table layout. *)

(* il faut pouvoir changer la largeur des colonnes. Donc garder accès aux
   layouts... ou au moins les recréer *)
(* TODO add line number/label *)
(* TODO it raises exception if length = 0, but should we make it work
   anyways? *)

open Printf
open B_utils
module Layout = B_layout
module Widget = B_widget
module Theme = B_theme
module Var = B_var
module Tvar = B_tvar
module Draw = B_draw
module Selection = B_selection
module Label = B_label
module Long_list = B_long_list

(* this is the public, non-mutable type *)
type column =
  { title : string;
    length : int;
    rows : int -> Layout.t;
    compare : (int -> int -> int) option;
    (* use "compare i1 i2" in order to compare entries i1 and i2 *)
    min_width : int option;
  }

type sort =
  | Ascending (* increasing *)
  | Descending (* decreasing *)

type column_private = {
    title : string;
    rows : int -> Layout.t;
    compare : (int -> int -> int) option;
    mutable width : int;
    mutable sort : sort option
  }

type t = {
    length : int;
    data : column_private array;
    selection : Selection.t Var.t; (* selection of rows *)
    mutable last_selected : int option;
    order : int array; (* we keep here the bijection ith entry --> jth displayed *)
    titles : Layout.t array;
    row_height : int;
    layout : (Layout.t option) Var.t (* the global layout *)
  }

let title_margin = 5
let title_background = Layout.color_bg Draw.(set_alpha 40 blue)
let row_hl = Layout.color_bg Draw.(set_alpha 30 blue)
let row_selected = Layout.opaque_bg Draw.(pale (pale (pale blue)))
let icon_color = Draw.(set_alpha 40 grey)

(* [max_width c] returns the max width of the entries of the column c,
   optionally limited to the first n_max entries. *)
let max_width ?(n_max = 50) (c : column) =
  let n_max = imin n_max c.length in
  let rec loop i m =
    if i = n_max then m
    else let w = Layout.width (c.rows i) in
         loop (i+1) (imax m w) in
  loop 0 0

let make_title (c : column) =
  (* We compute the label widget. *)
  let label = Widget.label c.title in
  (* If no width is specified, we compute the max width of the first entries of
     the column *)
  let lw,_ = Widget.default_size label in
  let w = match c.min_width with
    | Some w -> w
    | None -> (imax lw (max_width c)) in
  let layout =
    if c.compare = None
    then (* First encapsulate in order to then left-align. *)
      Layout.flat_of_w ~sep:0 [label]
    else begin (* add icon for sorting *)
      let sort_indicator = Widget.icon ~fg:icon_color "sort" in
      let sw,_ = Widget.default_size sort_indicator in
      let lw,lh = Widget.default_size label in
      let w' = w - sw - lw in
      if w' >= 0
      then (* we can add sort_indicator *)
        Layout.flat_of_w ~sep:0 ~align:(Draw.Max)
          [label; Widget.empty ~w:w' ~h:lh (); sort_indicator]
      else Layout.flat_of_w ~sep:0 [label]
    end in
  Layout.set_width layout w; (* not necessary in the case of sort_indicator *)
  let (_,h) = Widget.default_size label in
  let click_area = Widget.empty ~w ~h () in
  let title = Layout.(superpose [layout; resident click_area]) in (*AAA*)
  title

(* [get_area] extracts the click_area widget from the title layout *)
(* Warning: this depends on the way title is created in make_title, see
   (*AAA*) *)
let get_area title =
  let open Layout in
  match title.content with
  | Rooms [_; area] -> widget area (* see AAA *)
  | _ -> failwith "table.ml: The title layout should contain [layout; area]"

(* [get_indicator] extracts the sort_indicator widget from the title layout *)
(* Warning: this depends on the way title is created in make_title, see AAA. *)
let get_indicator title =
  let open Layout in
  match title.content with
  | Rooms [{ content = Rooms [_; _; sort_indicator]; _ }; _] (* see AAA *)
    -> Some (widget sort_indicator)
  | _ -> None

let get_row _ _ (* t i *) =
  () (* ??? *)

let make_column (c: column) w : column_private =
  { title = c.title;
    rows = c.rows;
    compare = c.compare;
    width = w;
    sort = None
  }

let make_columns (columns : column list) widths =
  List.map2 make_column columns widths

let make_table ?row_height (columns : column list) =
  let length, rw = match columns with
    | [] -> failwith "Cannot create empty table"
    | c0::_ -> List.iter (fun (c : column) ->
        if c.length <> c0.length
        then failwith "Table columns must have same length")
        columns; c0.length, Layout.height (c0.rows 0) in
  let row_height = default row_height rw in
  let titles = List.map make_title columns in
  let widths = List.map Layout.width titles in
  let data = make_columns columns widths
             |> Array.of_list in
  {
    length;
    data;
    selection = Var.create Selection.empty;
    last_selected = None;
    order = Array.init length (fun i -> i);
    titles = Array.of_list titles; (* useful ? we have the layout below *)
    row_height;
    layout = Var.create None (* will be computed afterwards *)
  }

(* [get_background] returns the background to use for this entry. [i] is the
   entry number in the original array and in position [ii] in the display. *)
let get_background t i ii =
  if Selection.mem (Var.get t.selection) i
  then Some row_selected
  else
  if ii mod 2 = 1
  then Some (Layout.color_bg Draw.(set_alpha 20 grey))
  else None

let make_long_list ~w ~h t  =
  (* Generate row #ii: *)
  let generate = fun ii ->
    let i = t.order.(ii) in
    let background = get_background t i ii in
    let left_margin = Widget.empty ~w:title_margin ~h:t.row_height () in
    let click_area = Widget.empty ~w ~h:t.row_height () in
    let ca = Layout.resident ~name:(sprintf "click_area %u(%u)" i ii) click_area in
    let row =
      Array.mapi (fun j c ->
          let width = Layout.width t.titles.(j) in
          let name = sprintf "entry[%u,%u]" i j in
          let r = Layout.flat ~margins:0 ~name [c.rows i] in
          Layout.set_width r (width  + title_margin);
          r.Layout.resize <- (fun _ ->
              print_endline "AAAA";
              let open Layout.Resize in
              set_width r (Layout.width t.titles.(j) + title_margin));
          r) t.data
      |> Array.to_list
      |> List.cons (Layout.resident left_margin)
      |> Layout.flat ~keep_resize:true ~margins:0 ?background in
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
      (t.last_selected <- Some i;
       let new_sel =
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
         Selection.toggle (Var.get t.selection) i in
       Var.set t.selection new_sel;
       Layout.set_background row (get_background t i ii);
      ) in
    Widget.on_click ~click click_area;
    Layout.(superpose [row; ca])
  in
  let height_fn _ = Some t.row_height in
  Long_list.create ~w ~h ~generate ~height_fn ~scale_width:true ~length:t.length ()

let make_layout ?w ~h t =
  let align = Draw.Max in (* bottom align *)
  let titles_list = Array.to_list t.titles in
  let w = match w with
    | Some w -> w
    | None -> title_margin +
              (List.fold_left (fun y r -> y + title_margin + Layout.width r)
                 0 titles_list) in
  let titles_row = Layout.flat ~name:"titles_row" ~sep:title_margin ~hmargin:title_margin
      ~vmargin:title_margin ~background:title_background
      ~align titles_list in
  let long = make_long_list t ~w ~h:(h - Layout.height titles_row) in
  titles_row, long

(* in-place reverse bijection of array *)
let reverse_array a =
  let l = Array.length a - 1 in
  for i = 0 to l/2 do
    let x = Array.unsafe_get a i in
    Array.unsafe_set a i (Array.unsafe_get a (l-i));
    Array.unsafe_set a (l-i) x
  done

(* sets the sort-indicator symbol. Does nothing if column is not sortable *)
let set_indicator t j =
  if t.data.(j).compare = None then ()
  else begin
    let sort = t.data.(j).sort in
    do_option (get_indicator t.titles.(j)) (fun indicator ->
        let label = Widget.get_label indicator in
        Label.set label (match sort with
            | None -> Theme.fa_symbol "sort"
            (* terminology in font_a is reversed *)
            | Some Ascending -> Theme.fa_symbol "sort-desc"
            | Some Descending -> Theme.fa_symbol "sort-asc"))
  end

(* refreshes the table by creating a new long_list *)
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

        (* this is the dangerous part: *)
        (* Layout.(long.geometry <- g); *)
        (* Layout.(long.current_geom <- to_current_geom g); *)
        (* = not really necessary, because I have removed do_adjust in set_rooms *)
        Layout.set_rooms ~sync:false r [titles_row; long];
        Layout.retower ~duration:0 ~margins:0 r;
        (* Layout.update_current_geom r; *)
        (* Layout.resize_tower ~hmargin:0 ~vmargin:0 ~sep:0 ~align:Draw.Min r; *)
        (* long.resize (Layout.get_size r); *)
        Layout.resize_follow_width titles_row
    )


(* changes sorting order. We don't try to modify the long_list in-place, we
   create a new one *)
let change_order t j sort =
  let column = t.data.(j) in
  do_option column.compare (fun compare ->
      Array.stable_sort compare t.order;
      if sort = Descending then reverse_array t.order;
      (* TODO modify t.titles.(j) *)
      refresh t;
      column.sort <- Some sort;
      for i = 0 to Array.length t.titles - 1 do
        if i <> j then t.data.(i).sort <- None;
        set_indicator t i
      done
    )

let connect_title t j =
  if t.data.(j).compare = None then ()
  else begin
    let widget = get_area t.titles.(j) in
    let click _ =
      let sort = match t.data.(j).sort with
        | None -> Ascending
        | Some Ascending -> Descending
        | Some Descending -> Ascending in
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


(* we just share the selection variable via a Tvar to automatically update the
   layout when the selection is changed. *)
let make_selection_tvar t =
  let t_from sel = sel in (* the user can access the selection via this *)
  let t_to sel = (* this is what is done when the user will modifiy the
                    selection using Tvar.set *)
    Var.set t.selection sel; (* this is redundant with Tvar.set, but we need it
                                to be done *before* refresh... *)
    refresh t;
    sel in
  Tvar.create (t.selection) ~t_from ~t_to


(* this returns the main layout and the selection variable *)
let create ?w ~h ?row_height ?(name="table") (columns : column list) =
  let t = make_table columns ?row_height in
  let titles_row, long = make_layout ?w ~h t in
  let table = Layout.tower ~margins:0 ~name
      [titles_row; long] in
  Layout.resize_follow_width titles_row;
  (* Layout.disable_resize titles_row; (\* TODO do better, cf example 35 *\) *)
  Var.set t.layout (Some table);
  for j = 0 to List.length columns - 1 do
    connect_title t j
  done;
  table, make_selection_tvar t

(* Create table from text array a.(i).(j) : row i, column j *)
let of_array ?w ~h ?widths ?row_height ?name headers a =
  let head = Array.of_list headers in
  let ni = Array.length a in
  if ni = 0 then failwith "Cannot create table with empty array."
  else let nj = Array.length a.(0) in
    if nj <> Array.length head
    then failwith "Cannot create table: \
                   headers size does not fit the number of columns."
    else let widths = match widths with
        | None -> List.map (fun _ -> None) headers
        | Some list -> list in
      let widths = Array.of_list widths in
      if Array.length widths <> nj
      then failwith "Cannot create table: \
                     list of widths does not fit the number of columns."
      else let columns =
             head
             |> Array.mapi (fun j title ->
                 { title;
                   length = ni;
                   rows = (fun i -> Layout.resident (Widget.label a.(i).(j)));
                   compare = Some (fun i1 i2 -> compare a.(i1).(j) a.(i2).(j));
                   min_width = widths.(j) })
             |> Array.to_list in
           create ?w ~h ?row_height ?name columns

(* From a Csv.t style list of rows (first row must be the header). Warning: this
   functions first converts to an array, ie. the data is likely to be duplicated
   in memory *)
let of_list ?w ~h ?widths ?row_height ?name = function
  | [] -> failwith "Cannot create table with empty list."
  | headers::rows ->
     let a = List.map Array.of_list rows
             |> Array.of_list in
     of_array ?w ~h ?widths ?row_height ?name headers a

(* * * * *)
