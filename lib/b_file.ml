(* This file is part of BOGUE, by San Vu Ngoc *)

(* This module construct a layout implementing a file dialog:

   + file or directory chooser
   + one selection or multiple selections
   + file changes monitor to refresh the layout
   + filter by mimetype

   2024-2025: IN PROGRESS

   TODO::

   + dynamic filtering
   + create directory button

*)

(*

* liste directory

https://v2.ocaml.org/releases/5.0/api/Unix.html#1_Directories

Use Unix.readdir ou Sys.readdir to read the file list, then (List.filter
Sys.is_directory) to separate files and dirs (works also for symlinks). Use
Unix.stat (ou UnixLabels) to get info.

http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html

 See also

https://caml.inria.fr/pub/docs/manual-ocaml/libref/Filename.html

* Attention aux dossiers avec grand nombre de fichiers => utiliser Long_list (ou
Table)

* faire un filtre dynamique pour les noms de fichiers

* aperçu d'images?

* création de dossier ?

* fontawesome:
fa-folder fa-folder-open fa-folder-o fa-folder-open-o

fa-file fa-file-text fa-file-o fa-files-o fa-file-text-o fa-file-pdf-o fa-file-photo-o fa-file-word-o fa-file-excel-o fa-file-powerpoint-o fa-file-picture-o fa-file-zip-o fa-file-sound-o fa-file-movie-o fa-file-code-o

* type de fichier
https://v2.ocaml.org/releases/5.0/api/Unix.LargeFile.html#VALstat

https://v2.ocaml.org/releases/5.0/api/Unix.html#TYPEfile_kind
character devices: fa-plug
block devive: fa-hdd-o
link: fa-link
pipe: fa-exchange
soxket: fa-phone ?? fa-phone-square

* icones

https://specifications.freedesktop.org/icon-naming-spec/latest/
$ man xdg-icon-resource
https://www.freedesktop.org/wiki/Specifications/icon-theme-spec/

   #!/bin/bash
mime=$(xdg-mime query filetype "$1")
icon_name=$(echo "$mime" | sed 's|/|-|g')
echo "Nom de l'icône : $icon_name"

   $ gio info b_file.ml | grep icon
  standard::icon: /home/san/Images/icons/ocaml-icon.png, text-x-ocaml, text-x-generic, /home/san/Images/icons/ocaml-icon.png-symbolic, text-x-ocaml-symbolic, text-x-generic-symbolic
  standard::symbolic-icon: /home/san/Images/icons/ocaml-icon.png-symbolic, text-x-ocaml-symbolic, text-x-generic-symbolic, /home/san/Images/icons/ocaml-icon.png, text-x-ocaml, text-x-generic
san@san-XPS-13-9350 ~/prog/ocaml/bogue (file_dialog) $ gwenview /home/san/Images/icons/ocaml-icon

   ou

   $ gio info -a standard::icon b_file.ml
   uri: file:///home/san/prog/ocaml/bogue/b_file.ml
local path: /home/san/prog/ocaml/bogue/b_file.ml
unix mount: /dev/nvme0n1p6 /home ext4 rw,relatime
attributes:
  standard::icon: /home/san/Images/icons/ocaml-icon.png, text-x-ocaml, text-x-generic, /home/san/Images/icons/ocaml-icon.png-symbolic, text-x-ocaml-symbolic, text-x-generic-symbolic*
* mimetype

$ file --mime b_file.ml
b_file.ml: text/x-ruby; charset=utf-8
$ file -b --mime-type themes/paper/paper.png
image/png
** mieux:
$ mimetype -b b_file.ml
text/x-ocaml
$ xdg-mime query filetype b_file.ml
text/x-ocaml
$ kioclient stat b_file.ml
NAME                  b_file.ml
SIZE                  24795
FILE_TYPE             0100000
DEVICE_ID             66310
INODE                 15893614
MIME_TYPE             text/x-ocaml
ACCESS                0664
MODIFICATION_TIME     Fri Dec 13 13:55:10 2024
ACCESS_TIME           Fri Dec 13 13:48:29 2024
CREATION_TIME         Fri Dec 13 12:54:59 2024

   voir aussi
$ kioclient --commands

$ gio info -a standard::content-type b_file.ml
uri: file:///home/san/prog/ocaml/bogue/b_file.ml
local path: /home/san/prog/ocaml/bogue/b_file.ml
unix mount: /dev/nvme0n1p6 /home ext4 rw,relatime
attributes:
  standard::content-type: text/x-ocaml


https://github.com/mirage/conan

* recentyl used files

   .local/share/recently-used.xbel

   https://stackoverflow.com/questions/24007459/how-to-query-recent-items-in-mac-os-x

 *)

module Avar = B_avar
module Button = B_button
module Draw = B_draw
module Empty = B_empty
module Label = B_label
module L = B_layout
module Long_list = B_long_list
module Main = B_main
module Popup = B_popup
module Selection = B_selection
module Space = B_space
module Style = B_style
module Sync = B_sync
module Table = B_table
module Text_input = B_text_input
module Theme = B_theme
module Trigger = B_trigger
module Update = B_update
module Var = B_var
module W = B_widget

open B_utils
open Tsdl

type entry = {
    name : string;
    target_stat : Unix.stats option;
    (* computed only in case of link, to get the target stats *)
    stat : Unix.stats option }

let filename e = e.name
let lstat_opt e = e.stat
let stat_opt e = e.target_stat

(* type t = { *)
(*   dir : string; *)
(*   data : entry list *)
(*   } *)

let ( // ) = Filename.concat

let file_stat name =
  try Some (Unix.lstat name) with
  | _ -> None

let is_directory path =
  try Sys.is_directory path with _ -> false

let entry_size e = match e.stat with
  | Some s -> s.Unix.st_size
  | None -> -1

let entry_mtime e =
  match e.stat with
  | None -> 0.
  | Some s -> s.st_mtime

(* for Ocaml < 4.14 *)
let input_line ic =
  match Stdlib.input_line ic with
  | s -> Some s
  | exception End_of_file -> None

let dummy_stat = Unix.{
    st_dev = 0;      (* Device number *)
    st_ino = 0;      (* Inode number *)
    st_kind = S_REG; (* Kind of the file *)
    st_perm = 0;     (* Access rights *)
    st_nlink = 0;    (* Number of links *)
    st_uid = 0;      (* User id of the owner *)
    st_gid = 0;      (* Group ID of the file's group *)
    st_rdev = 0;     (* Device ID (if special file) *)
    st_size = 0;     (* Size in bytes *)
    st_atime = 0.;   (* Last access time *)
    st_mtime = 0.;   (* Last modification time *)
    st_ctime = 0.;   (* Last status change time *)
  }

module SSet = Set.Make(String)

(* Monitoring changes in a directory. All functions are non-blocking and return
   very fast, even if the path is remote or the directory is huge. To achieve
   this, monitoring is done in a separate thread and one has to accept a delay
   before actual changes to the filesystem are reported. We provide two
   implementations, one is based on the external [fswatch] program, and the
   other is based only on built-in Ocaml functions (which are probably more
   memory and cpu intensive). *)
module type Monitor = sig
  type t
  val path : t -> string
  val start : ?delay:float -> ?action:(t -> unit) -> string -> t
  (* [start path] starts monitoring the directory or file [path] and immediately
     returns. It is not an error if [path] does not exist or is deleted, see
     below. The [delay] parameter is the time interval (in seconds) between
     polls. The [action] function is executed for each modification (it might be
     a false positive; it should be fast and non blocking (typically just
     sending an event). Check [modified] below to get the actual changes.) Thus,
     what we call "current" will always mean "not older than delay".  The
     default delay is 1 second. It may be internally increased if the polls take
     too much time. *)

  val delay : t -> float
  val stop : t -> unit
  val ls : t -> string list
  (* [ls m] returns the "old" list of files watched by the monitor [m] when the
     last [*modified] function was called. If [m] monitors a directory, [ls m]
     is the content of the directory (without "." and ".."), otherwise [ls m] is
     "." if the file exists, or [] if not. This function takes advdantage of the
     monitoring data to return faster (in general) than rescanning the directory
     with [Sys.readdir]. *)

  val size : t -> int option
  (* If [t] monitors a directory, then [size t] is the number of elements of
     this directory, before recent modifications. Otherwize, [size t] returns
     None. Calling [size t] is equivalent to but faster than [List.length (ls
     t)]. *)

  val modified : t -> string list * string list * string list
  (* Return three lists of files (or directories) names that have been modified
     since last call of this function or of [was_modified]:

     list of deleted elements, list of added elements, list of modified elements

     File names do not include the directory path. These lists can be equal to
     the singleton ["."] in some special cases:

     + if the [path] now exists but did not exist in the previous call to
     [*modified], then the [added] list is ["."] and the others are empty (even
     if some contents of [path] were modified in the meantime: remember that we
     only compare to the previous call to [*modified].)

     + if the [path] existed in the previous call to [*modified] but not
     anymore, then the [deleted] list is ["."] and the others are empty.

     + if the [path] existed in the previous call to [*modified], then has
     disappered and then reappeared again, the [modified] function will return
     [], ["."], [] instead of the explicit difference, telling you that it is
     safer to read the contents again (using [ls] for instance). *)

  val was_modified : t -> bool
  (* Simply returns true if files were modified since the last call of this
     function or of [modified]. The list of modified files cannot be
     retrieved. This is (semantically) equivalent to checking if the list
     returned by [modified] is empty, but possibly faster. *)
end

(* The first Monitor that we propose is based on fswatch, which can be installed
   on many OSes. https://emcrisostomo.github.io/fswatch/

   The fswatch command is run in the background, and we launch a Thread to poll
   it regulary (every [delay=1] second, which is fswatch's default latency).

   The accuracy of the [*modified] functions has to be taken modulo [delay]:
   files modified more recently than [delay] might not be included.

   Corner case: at least on my linux machine, when watching a non-existent path,
   the creation of the path is detected by fswatch only after the second
   modification of the path...
*)
module Fswatch : Monitor = struct
  (* see also ? https://github.com/kandu/ocaml-fswatch

     or ? https://github.com/whitequark/ocaml-inotify *)

  let _name = "fswatch"

  type t =
    { path : string;
      id : int;
      inch : in_channel;
      outd : Unix.file_descr;
      mutable exists : bool;
      mutable updated : SSet.t;
      mutable created : SSet.t;
      mutable removed : SSet.t;
      mutable stop : bool;
      mutable old_content : SSet.t;
      delay : float
    }

  let latency = 1.
  let dot = SSet.singleton "."

  let path p = p.path
  let delay p = p.delay

  let _test_loop path =
    let fswatch_command =
      "fswatch --event-flag-separator '|' --event Updated --event Removed \
       --event Created --event Renamed --event MovedFrom --event MovedTo --event \
       AttributeModified --event OwnerModified -x \"" ^
      (Filename.quote path) ^ "\"" in
    let in_channel, out_channel = Unix.open_process fswatch_command in
    let rec loop () =
      match input_line in_channel with
      | None ->
        close_in in_channel;
        close_out out_channel
      | Some line ->
        print_endline line;
        loop ()
    in
    loop ()

  let get_initial_content path =
    try if Sys.is_directory path
      then Sys.readdir path |> Array.to_list |> SSet.of_list
      else dot
    with _ ->  SSet.empty

  (* On va plutôt utiliser create_process pour pouvoir le tuer quand on n'en a
     plus besoin *)
  let create_process delay path =
    let ind, outd = Unix.pipe () in
    let e = "--event" in
    let fswatch_process =
      Unix.create_process "fswatch"
        [| "fswatch";
           "--event-flag-separator"; "|";
           e; "Updated";
           e; "Removed";
           e; "Created";
           e; "Renamed";
           e; "MovedFrom";
           e; "MovedTo";
           e; "AttributeModified";
           e; "OwnerModified";
           "--latency"; (string_of_float (max 0.1 delay));
           "-x"; path |]
        ind outd Unix.stderr in
    let inch = Unix.in_channel_of_descr ind in
    let old_content = get_initial_content path in
    let exists = Sys.file_exists path in
    if not exists then printd (debug_warning + debug_user + debug_io)
        "Monitoring path [%s] which does not exist (yet)." path
    else printd debug_io  "Monitoring path [%s]." path;
    { path; id = fswatch_process; inch; outd; delay; exists;
      updated = SSet.empty; created = SSet.empty; removed = SSet.empty;
      old_content; stop = false }

  (* The output of fswatch should be something like:
     /tmp/aaa Removed|MovedFrom
     The function [parse_event] should return: ("/tmp/aaa", ["Removed"; "MovedFrom"])
  *)
  let parse_event dir_path s =
    match String.rindex_opt s ' ' with
    | Some i ->
      let path = String.sub s 0 i in
      let path = if path = dir_path then "." else Filename.basename path in
      let flags = String.split_on_char '|'
          (String.sub s (i+1) (String.length s - (i+1))) in
      (path, flags)
    | None -> s, []

  (* On my machine re-creation of the path is not detected at first, fswatch
     needs another modif to detect it. *)
  let new_path p =
    printd debug_io "New path [%s] was created and is now monitored." p.path;
    p.exists <- true;
    p.removed <- SSet.empty;
    p.created <- dot;
    assert (SSet.is_empty p.updated);
    assert (SSet.is_empty p.old_content);
    p.old_content <- SSet.empty

  let update_path p =  (* path is updated, if this is a dir we don't care *)
    if not (is_directory p.path) then p.updated <- dot

  (* On my machine [rmdir] (so on an empty dir) is not reported by fswatch,
     hence [remove_path] will not be called... Hopefully this only happens when
     the dir is empty. But [remove_path] will be called when the dir is
     renamed. *)
  let remove_path p =
    printd debug_io "Monitored path [%s] has vanished (moved or deleted)." p.path;
    p.exists <- false;
    p.removed <- dot;
    p.created <- SSet.empty;
    p.updated <- SSet.empty;
    p.old_content <- SSet.empty

  (* This function is blocking when there is no change to the path *)
  let rec record ?action p =
    if not p.stop then match
        (* printd (debug_thread + debug_io) "Waiting from fswatch input..."; *)
        input_line p.inch with
    | None ->
      printd debug_io "fswatch: Nothing new";
      if not p.stop then Thread.delay p.delay;
      record ?action p
    | Some line ->
      printd debug_io "fswatch sent: %s" line;
      apply_option action p; (* typically we use this to send an event *)
      let file, flags = parse_event p.path line in
      printd debug_io "Path=[%s], File=[%s], Flags=[%s]"
        p.path file (String.concat "; " flags);
      if file <> "." && p.exists then begin
        if SSet.equal p.created dot
        then printd debug_io "Monitord path [%s] was created but not checked \
                              yet; we don't record modifications untill then." p.path
        else if List.mem "Created" flags then
          (if SSet.mem file p.removed
           then (p.removed <- SSet.remove file p.removed;
                 p.updated <- SSet.add file p.updated)
           else p.created <- if SSet.equal p.created dot then SSet.singleton file
               else SSet.add file p.created)
        else if List.mem "Removed" flags then
          (if SSet.mem file p.created
           then (p.created <- SSet.remove file p.created;
                 p.updated <- SSet.add file p.updated)
           else p.removed <- SSet.add file p.removed)
        else if not (SSet.mem file p.created)
        then p.updated <- SSet.add file p.updated
      end else begin
        (* Here we treat the special case where the path itself is modified. On
           my linux machine, when removing the path, instead of "Removed",
           fswatch emits "AttributeModified" for a file, and nothing for a
           dir. *)
        if Sys.file_exists p.path
        then if p.exists
          then update_path p
          else new_path p  (* path was just created *)
        else remove_path p (* path has disappeared *)
      end;
      record ?action p
    else begin
      printd debug_io "Closing fswatch input channel.";
      close_in p.inch;
      printd debug_thread "fswatch thread terminated.";
    end

  let start ?(delay = latency) ?action path =
    let path = let b = Filename.basename path in
    if b = Filename.dir_sep then b else Filename.(dirname path // b) in
    (* this removes possible trailing "/" *)
    let p = create_process (max 0.1 delay) path in
    let _th = Thread.create (record ?action) p in
    p

  let update p =
    p.old_content <- if SSet.equal p.created dot then get_initial_content p.path
      else SSet.diff p.old_content p.removed |> SSet.(union p.created);
    p.updated <- SSet.empty;
    p.removed <- SSet.empty;
    p.created <- SSet.empty

  let stop p =
    if not p.stop then begin
      printd debug_io "Stop monitoring [%s]" p.path;
      p.stop <- true;
      update p;
      try
        printd debug_thread "sending TERM to fswatch process.";
        Unix.kill p.id Sys.sigterm;
        printd debug_thread "sending KILL to fswatch process.";
        Unix.kill p.id Sys.sigkill;
        (* This would block : why? We do it later *)
        (* print_endline "Closing inch"; *)
        (* In_channel.close p.inch; *)
        printd debug_io "Closing fswatch output channel.";
        Unix.close p.outd
      with
        _ -> printd (debug_error + debug_io + debug_thread)
               "Fswatch process not cleanly stopped"
    end else printd debug_io "Fswatch process for [%s] was already stopped." p.path

  (* Return the list of files that have been modified since last call of this
     function, modulo [delay]. *)
  let modified p =
    if p.stop then printd (debug_error + debug_io) "File Monitor is already stopped.";
    let updated = p.updated |> SSet.elements in
    let removed = p.removed |> SSet.elements in
    let created = p.created |> SSet.elements in
    update p;
    removed, created, updated

  let was_modified p =
    if p.stop then printd (debug_error + debug_io) "File Monitor is already stopped.";
    let m = not SSet.(is_empty p.updated && is_empty p.created && is_empty p.removed) in
    update p;
    m

  let ls p =
    try if Sys.is_directory p.path
      then SSet.elements p.old_content
      (* (SSet.diff p.old_content p.removed) *)
      (*    |> SSet.union p.created *)
      (* |> SSet.elements *)
      else ["."]
    with _ -> []

  let size p =
    try if Sys.is_directory p.path
      then Some (SSet.cardinal p.old_content)
      else (printd (debug_error + debug_io + debug_user)
              "[Monitor.size] (Fswatch): [%s] is not a directory" p.path; None)
    with _ -> printd debug_io "Monitor (Fswatch): path [%s] cannot be accessed." p.path;
      None

end (* of module Fswatch*)

(* Test if [fswatch] is available. One could do a more thorough test. *)
let fswatch_check () =
  which "fswatch" <> None

module Diff = struct

  let add table i (key, value) = Hashtbl.add table key (i, value)

  let get_opt array i =
    if i >= 0 && i < Array.length array
    then Some (Array.unsafe_get array i)
    else None

  let pop table x =
    let key = fst x in
    match Hashtbl.find_opt table key with
    | Some (j, v) ->
      Hashtbl.remove table key;
      (* print_string (Printf.sprintf " found j=%i." j); *)
      Some (j, (key, v))
    | None ->
      (* print_string " not found "; *)
      None

  let differ_fast a1 a2 = a1 <> a2

  let table_to_list table =
    Hashtbl.fold (fun key _value list -> key::list) table []

  (* [diff a1 a2] returns the diff between the arrays a1 and a2 in the form: list
     of deleted elements, list of added element, list of modified elements

     More precisely, elements of the arrays have the form (data, time); only the
     data is returned. Each of these list is returned with no specific
     ordering. (expect random)

     [deleted] means present in a1 but not in a2.

     [added] means present in a2 but not in a1.

     [modified] means present both in a1 and a2, but the version in a2 is newer.

     The algo used should be efficient for few additions and deletions, when the
     common elements are in the same order in a1 and a2. (Roughly linear time
     O(N)). It will be suboptimal if the orders of a1 and a2 are reverse from each
     other: O(N^2). For random order, an algorithm based on sorting would be
     better (O(N log N)).

     TODO optimize by playing with [d2] (currently not used); indeed, if only one
     file is added or deleted, setting d2= =/- 1 would roughly divide the number
     of operations by 2.
  *)

  let diff a1 a2 =
    let same x1 x2 = fst x1 = fst x2 in
    let newer x2 x1 = snd x2 > snd x1 in
    let suppress_maybe = Hashtbl.create 16 in
    let add_maybe = Hashtbl.create 16 in
    let modified = Hashtbl.create 16 in
    let rec loop i d2 =
      (* print_newline (); *)
      (* print_string (Printf.sprintf "(%i, %i)" i (i + d2)); *)
      match get_opt a1 i, get_opt a2 (i + d2) with
      | Some x1, Some x2 when same x1 x2 ->
        if newer x2 x1 then add modified i x2;
        loop (i + 1) d2
      | None, None ->
        ( table_to_list suppress_maybe,
          table_to_list add_maybe,
          table_to_list modified )
      | ox1, ox2 ->
        begin match ox1 with
          | Some x1 ->
            begin match pop add_maybe x1 with
              | Some (j, y2) ->
                if newer y2 x1 then (add modified j y2);
              | None -> add suppress_maybe i x1;
            end
          | None -> ()
        end;
        begin match ox2 with
          | Some x2 ->
            begin match pop suppress_maybe x2 with
              | Some (j, y1) ->
                if newer x2 y1 then (add modified j x2);
              | None -> add add_maybe i x2;
            end
          | None -> ()
        end;
        loop (i + 1) d2 in
    loop 0 0

  let array_rev a =
    let n = Array.length a in
    Array.init n (fun i -> Array.unsafe_get a (n-i-1))

  let test_diff () =

    let test (a,b,(x,y,z)) =
      let r,c,u = diff a b in
      assert (List.length r = x && List.length c = y && List.length u = z) in

    let n = 1000000 in
    let a = Array.init n (fun i -> (i,0)) in
    itime_it "[diff] no change" test (a, a, (0,0,0)) ;
    let aa = array_rev a in
    itime_it "[diff] no change but reversed" test (a, aa, (0,0,0));
    let a1 = Array.init (n+2) (fun i -> (i,0)) in
    itime_it "[diff] two added at the end" test (a, a1, (0,2,0));
    let a2 = Array.init (n+2) (fun i -> (i-2,0)) in
    itime_it "[diff] two added at the top" test (a, a2, (0,2,0));
    let b = Array.init n (fun i -> (i,1)) in
    itime_it "[diff] all updated" test (a,b, (0,0,n));

end

(* The second Monitor does not use any external tool, only [Unix.stat].  *)
module Unix_stat : Monitor = struct

  type stat = (string * float)
  (* Each files is represented by a pair (base name, time stamp). *)

  type dir = stat array

  let _name = "unix_stat"

  type t = {
    path : string;
    mutable old_time : float;
    mutable new_time : float;
    old_content : dir Var.t;
    new_content : dir Var.t;
    mutable stop : bool;
    mutable delay : float }

  let latency = 1.

  let delay t = t.delay
  let path t = t.path

  let get_time path =
    let open Unix in
    try let s = lstat path in
      s.st_ctime +. s.st_mtime +. s.st_atime
    with _ -> printd (debug_io + debug_warning)
                "Monitor: error while getting stats for [%s]." path;
      0.

  let readdir path =
    let a = try Sys.readdir path with Sys_error _ | Unix.Unix_error _  ->
      printd (debug_io + debug_warning) "Monitor: error while reading directory [%s]."
        path; [||] in
    Array.map (fun file -> file, get_time (path // file)) a

  let stop t =
    if not t.stop then begin
      t.stop <- true;
      Var.set t.old_content [||];
      Var.set t.new_content [||]
    end

  let adjust_delay t dt =
    if dt > t.delay /. 50.
    then begin
      t.delay <- 60. *. dt;
      printd debug_io
        "Reading directory %s is taking a long time: %f ms. Increasing the \
         Monitor delay to %f." t.path dt t.delay;
    end else if t.delay > latency && dt < t.delay /. 70.
    then begin
      t.delay <- max 1. (dt *. 60.);
      printd debug_io "Setting Monitor delay to %f." t.delay
    end

  let get_time_or_zero path =
    try get_time path with Unix.Unix_error _ | Sys_error _ -> 0.

  let daemon ?action t =
    let cache = ref (Var.get t.new_content) in
    let rec loop () =
      if Sys.file_exists t.path
      then begin
        (* We update [new_content] even if old_time = 0. because [readdir] may
           take some time, so we don't want to it on demand with [modified]. *)
        let t1 = t.new_time in
        t.new_time <- get_time_or_zero t.path;
        if is_directory t.path
        then let dt = time_it "dt" (Var.set t.new_content) (readdir t.path) in
          adjust_delay t dt;
          if t1 = 0.
          then begin
            printd debug_io "New path [%s] was created and is now monitored." t.path;
            t.old_time <- 0.;
            Var.set t.old_content [||]
            (* Var.set t.old_content (Var.get t.new_content); *)
            (* t.old_time <- t.new_time *)
          end
      end
      else if t.new_time <> 0.
      then (printd debug_user "Monitored path [%s] has vanished (moved or deleted)."
              t.path;
            t.new_time <- 0.;
            Var.set t.new_content [||]);
      if not t.stop then begin
        Thread.delay t.delay;
        let () = match action with
          | None -> ()
          | Some f ->
            if Diff.differ_fast !cache (Var.get t.new_content)
            then begin
              cache := Var.get t.new_content;
              f t end in
        loop ()
      end
    in
    ignore (Thread.create loop ())

  let start ?(delay = latency) ?action path : t =
    let stop = false in
    let old_time = get_time_or_zero path in
    if old_time = 0. then printd (debug_warning + debug_user + debug_io)
        "Monitoring path [%s] which does not exist (yet)." path
        else printd debug_io "Monitoring path [%s]." path;
    let c = readdir path in
    let old_content = Var.create c in
    let t =  { path; old_time; new_time = old_time;
               old_content; new_content = Var.create c;
               stop; delay } in
    daemon ?action t;
    t

  (* In this monitor we can return "for free" the timestamps along with the
     files. *)
  let lsstat t =
    try if Sys.is_directory t.path
      then Var.get t.old_content |> Array.to_list
      else [ ".", t.old_time ]
    with _ ->
      printd debug_io "Monitor: path [%s] cannot be accessed."
        t.path;
      []

  let ls t = lsstat t |> List.map fst

  let size t =
    try if Sys.is_directory t.path
      then Some (Array.length (Var.get t.old_content))
      else (printd (debug_error + debug_io + debug_user)
              "[Monitor.size] (Unix_stat): [%s] is not a directory" t.path; None)
    with _ ->
      printd debug_io "Monitor (Unix_stat): path [%s] cannot be accessed." t.path;
      None

  let modified t =
    if t.stop then (printd (debug_error + debug_io) "File Monitor is already stopped.";
                    [], [], [])
    else begin
      let t0 = t.old_time in
      t.old_time <- t.new_time;
      if t.new_time = 0. && t0 > 0. then begin (* path has disappeared *)
        Var.set t.old_content [||];
        ["."], [], []
      end else if t0 = 0. && t.new_time > 0. then begin (* path reappeared *)
        (let@ newc = Var.with_protect t.new_content in
         Var.set t.old_content newc);
        [], ["."], []
      end else
        begin
          match Var.protect_fn t.new_content (fun newc ->
              let del, add, mdf = Diff.diff (Var.get t.old_content) newc in
              Var.set t.old_content newc;
              del, add, mdf) with
          | [], [], [] ->
            if t.new_time > t0 then [], [], ["."]
            else [], [], []
          | x -> x (* deleted, added, modified *)
        end
    end

    let was_modified t =
    if t.stop
    then (printd (debug_error + debug_io) "File Monitor is already stopped."; false)
    else begin
      if t.old_time < t.new_time
      then begin
        t.old_time <- t.new_time;
        Var.set t.old_content (Var.get t.new_content);
        true end
      else let del, add, mdf = modified t in
      del <> [] || add <> [] || mdf <> []
    end

  end (* of module Unix_stat *)

module Watchman = struct
  (* TODO *)
  (* https://github.com/facebook/watchman *)

(*
san@san-XPS-13-9350 /tmp $ watchman clock /tmp/
{
    "version": "4.9.0",
    "clock": "c:1730115752:11471:2:40"
}
san@san-XPS-13-9350 /tmp $ watchman since /tmp/ "c:1730115752:11471:2:40"
{
    "is_fresh_instance": false,
    "version": "4.9.0",
    "warning": "opendir(/tmp/snap-private-tmp) -> Permission denied. Marking this portion of the tree deleted\nTo clear this warning, run:\n`watchman watch-del /tmp ; watchman watch-project /tmp`\n",
    "files": [
        {
            "cclock": "c:1730115752:11471:2:2",
            "nlink": 2,
            "dev": 36,
            "ctime": 1730116576,
            "new": false,
            "mtime": 1730115752,
            "gid": 1000,
            "mode": 17856,
            "size": 120,
            "oclock": "c:1730115752:11471:2:41",
            "ino": 2427,
            "uid": 1000,
            "exists": true,
            "name": "san-state"
        }
    ],
    "clock": "c:1730115752:11471:2:43"
}

*)
end

(* GIO ?

$ gio monitor /tmp/

*)

module Monitor =
  (val (if fswatch_check ()
        then begin
          printd debug_io "Using Fswatch for file monitoring.";
          (module Fswatch) end
        else begin
          printd debug_io
            "Fswatch program not found; using Unix_stat for file monitoring.";
          (module Unix_stat)
        end) : Monitor)

let test_monitor () =

  (* adapted from ocaml 5.1*)
  let temp_dir prefix suffix =
  let rec try_name counter =
    let name = Filename.temp_file prefix suffix in
    try
      Sys.remove name;
      Unix.mkdir name 0o700;
      name
    with Sys_error _ as e ->
      if counter >= 20 then raise e else try_name (counter + 1)
  in try_name 0 in

  let temp_dir = temp_dir "bogue_file" ".d" in
  let t = Monitor.start ~delay:1. temp_dir in
  Thread.delay 0.2;
  let foo = Filename.temp_file ~temp_dir "foo-" ".txt" in
  Thread.delay 1.2;
  let d, a, m = Monitor.modified t in
  assert (d = []);
  assert (a = [Filename.basename foo]);
  assert (m = []);
  assert (Monitor.path t = Filename.dirname foo);
  Sys.remove foo;
  Thread.delay 1.2;
  let d, _a, _m = Monitor.modified t in
  assert (d = [Filename.basename foo]);
  Monitor.stop t

module Mime = struct
  module Imap = Map.Make(String)

  let get_ext name = String.lowercase_ascii (Filename.extension name)

  let ext2mime_map =
    let add map (k, v) = Imap.add k v map in
    List.fold_left add Imap.empty Ext2mime_data.alist

  let from_ext s = (* s = file extension starting with "." *)
    Imap.find_opt s ext2mime_map
    |> Option.map (String.split_on_char '/')

  let from_filename s = from_ext (get_ext s)

  let type_string file =
    let s = get_ext file in
    default (Imap.find_opt s ext2mime_map) ""

  let from_magic _file = () (* TODO *)

  let test () =
    assert (from_filename "MYIMAGE.JPG" = Some ["image"; "jpeg"])

end

let regular_fa_name file =
  let default = "file-o" in
  match Filename.extension file with
  | "" -> default
  | ext -> match Mime.from_ext ext with
    | None -> printd debug_io "Cannot find Mime type for file [%s]." file;
      default
    | Some ext -> begin match ext with
        | "image" :: _ -> "file-image-o"
        | "video" :: _ -> "file-video-o"
        | "audio" :: _ -> "file-audio-o"
        | "application" :: rest :: _ -> begin match rest with
            | "pdf" -> "file-pdf-o"
            | "x-bzip2"
            | "x-bzip"
            | "x-gzip"
            | "zip" -> "file-archive-o"
            | "vnd.ms-excel" -> "file-excel-o"
            | "msword"
            | "vnd.wordperfect"
            | "x-abiword"
            | "vnd.kde.kword" -> "file-word-o"
            | _ -> default
          end
        | _ -> default (* TBC *)
      end

let entry_fa_name e =
  match e.stat with
  | None -> "car" (* debug *)
  | Some s -> let open Unix in match s.st_kind with
    | S_REG -> regular_fa_name e.name (* Regular file *)
    | S_DIR -> "folder-o" (* Directory *)
    | S_CHR -> "plug" (* Character device *)
    | S_BLK -> "plug" (* Block device *)
    | S_LNK -> begin
        match e.target_stat with
        | Some s when s.st_kind = S_DIR -> "folder-o"
        |_ -> "link" (* Symbolic link *)
      end
    | S_FIFO -> "?"(* Named pipe *)
    | S_SOCK -> "exchange"


(*******)
(* GUI *)
(*******)

(* Widgets: search bar (Text_input), files () *)

type options = {
  width : int option;
  height : int;
  dirs_first : bool; (* partially implemented *)
  show_hidden : bool;
  hide_backup : bool;
  max_selected : int option;
  hide_dirs : bool;
  only_dirs : bool;
  select_dir : bool;
  allow_new : bool; (* allow entering non-existing files in the new_file entry *)
  default_name : string;
  breadcrumb : bool;
  system_icons : bool;
  open_dirs_on_click : bool; (* this should not be true if select_dir is true *)
  mimetype : Str.regexp option; (* check mimetype -- from file extension only *)
  on_select : ((int * int) -> unit) option
}

let set_options ?width ?(height = 400)
    ?(dirs_first = true)
    ?(show_hidden = false)
    ?(hide_backup = false)
    ?max_selected
    ?(hide_dirs = false)
    ?(only_dirs = false)
    ?(select_dir = false)
    ?(allow_new = false)
    ?(default_name = "")
    ?(breadcrumb = true)
    ?(system_icons = false)
    ?(open_dirs_on_click = false)
    ?mimetype
    ?on_select () =
  assert (not select_dir || not open_dirs_on_click);
  {
    width; height;
    dirs_first;
    show_hidden;
    hide_backup;
    max_selected;
    hide_dirs;
    only_dirs;
    select_dir;
    allow_new;
    default_name;
    breadcrumb;
    system_icons;
    open_dirs_on_click;
    mimetype;
    on_select
  }

type directory = {
  monitor : Monitor.t;
  stats_hash : ((string, Unix.stats * Unix.stats option) Hashtbl.t) Var.t;
  mutable table : Table.t;
  mutable entries : entry array;
  (* filtered names; used only for generating the final selection *)
}

type input = {
  text_input : W.t;
  mutable breadcrumb : (W.t * string) list;
  mutable layout : L.t
}

type message = {
  label : W.t;
  mutable clicked_entry : entry option }

type t = {
  controller : W.t;
  input : input;
  message : message;
  new_file : W.t;
  name_filter : (string -> bool) option;
  full_filter : (entry -> bool) option;
  layout : L.t;
  mutable directory : directory;
  options : options
}

let size_to_string =
  let prefixes = [| "o"; "Kio"; "Mio"; "Gio"; "Tio" |] in
  fun x ->
    if x < 0 then "?" else
      let rec loop prefix x =
        let y = x lsr 10 in
        if y = 0 || prefix = 5 then x, prefix - 1
        else loop (prefix + 1) y in
      let x, prefix = loop 1 x in
      string_of_int x ^ " " ^ prefixes.(prefix)

let test_size_to_string () =
  assert (size_to_string (1 lsl 30) = "1 Gio");
  assert (size_to_string 1023 = "1023 o");
  assert (size_to_string 2049 = "2 Kio")

let is_hidden name = name <> "" && name.[0] = '.'

let is_backup name =
  name <> "" &&
  (name.[String.length name -1] = '~' || Filename.extension name = ".bak")

(*
# let a = [|0;1;2;3;4;5;6;7|];;
val a : int array = [|0; 1; 2; 3; 4; 5; 6; 7|]
# let show = [|false;true;true;false;true;true;false;false|];;
val show : bool array = [|false; true; true; false; true; true; false; false|]
# extract_array a show;;
- : int array = [|1; 2; 4; 5|]
*)
let extract_array a show =
  if Array.length a = 0 then [||]
  else
    let () = assert (Array.length a = Array.length show) in
    let len = Array.fold_left (fun s b -> if b then s+1 else s) 0 show in
    let fa = Array.make len Array.(unsafe_get a 0) in
    let rec loop i j =
      if j < len
      then if Array.unsafe_get show i
        then (Array.unsafe_set fa j (Array.unsafe_get a i);
              loop (i+1) (j+1))
        else loop (i+1) j in
    loop 0 0;
    fa

let entry_is_directory (e : entry) =
  match e.stat with
  | Some s when s.st_kind = S_DIR -> true
  | Some s when s.st_kind = S_LNK -> (match e.target_stat with
      | Some ts when ts.st_kind = S_DIR -> true
      | _ -> false)
  | _ -> false

let find_entry entries name =
  array_find_index (fun e -> e.name = name) entries

let dir_icon_color = Draw.(opaque (find_color "#887a5f"))
let file_icon_color = Draw.(opaque (find_color "#513d34"))
let path_entry_color = Draw.(opaque (find_color "#dbc8a4"))
let fg = Draw.(opaque label_color)
let hidden_color = Draw.(transp label_color)

(* Final table with filtered entries *)
let make_f_table ~options message entries =
  let height = round (float Theme.label_font_size *. 1.5) in
  let font_size = round (float Theme.label_font_size *. 0.9) in
  let length = Array.length entries in

  let compi compare =
    fun i j ->
      let e1 = entries.(i) in
      let e2 = entries.(j) in
      if not options.dirs_first then compare e1 e2
      else match entry_is_directory e1, entry_is_directory e2 with
        | false, false
        | true, true -> compare e1 e2
        | true, false -> -1
        | false, true -> 1
  in

  let generate j =
    let e = entries.(j) in
    let fg = if is_hidden e.name || is_backup e.name then hidden_color else fg in
    let label = W.label ~size:font_size ~fg e.name in
    (* NE MARCHE PAS, cf Table.make_long_list *)
    (* W.connect_main label label (\* controller? *\) (fun w _ ev -> *)
    (*     if Trigger.was_double_click () then begin *)
    (*       print_endline "change dir !"; *)
    (*       Trigger.push_var_changed (W.id w) *)
    (*     end) *)
    (*   Trigger.buttons_up *)
    (* |> W.add_connection label; *)
    L.resident ~h:height label in

  let name_col = Table.{
      title = "Name";
      length;
      rows = generate;
      compare = Some (compi (fun e1 e2 -> String.compare e1.name e2.name));
      min_width = Some 200;
      align = Some Draw.Min
    } in

  let generate_size j =
    let e = entries.(j) in
    let s = if entry_is_directory e then ""
      else size_to_string (entry_size e) in
    let fg = if is_hidden entries.(j).name then hidden_color else fg in
    L.resident ~h:height (W.label ~fg ~size:font_size s) in

  let size_col = Table.{
      title = "Size";
      length;
      rows = generate_size;
      compare = Some (compi (fun e1 e2 -> Int.compare (entry_size e1) (entry_size e2)));
      min_width = Some 58;
      align = Some Draw.Max
    } in

  let generate_icon j =
    let e = entries.(j) in
    let fg = if entry_is_directory e then dir_icon_color else file_icon_color in
    let icon = W.icon ~fg (entry_fa_name e) in
    L.resident ~h:height icon in

  let icon_col = Table.{
      title = "";
      length;
      rows = generate_icon;
      compare = Some (compi (fun e1 e2 ->
          String.compare (entry_fa_name e1) (entry_fa_name e2)));
      min_width = Some 16;
      align = Some Draw.Max
    } in

  let generate_mod j =
    let t = entry_mtime entries.(j) in
    let text = if t = 0. then "?"
      else let tm = Unix.localtime t in
        if Unix.gettimeofday () -. t < 86400. (* 60*60*24 number of seconds in 1 day *)
        then Printf.sprintf "%02u:%02u" tm.tm_hour tm.tm_min
        else Printf.sprintf "%04u/%02u/%02u"
            (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday in
    L.resident ~h:height (W.label ~fg ~size:font_size text) in

  let mod_col = Table.{
      title = "Modified";
      length;
      rows = generate_mod;
      compare = Some (compi (fun e1 e2 ->
          Float.compare (entry_mtime e1) (entry_mtime e2)));
      min_width = Some 80;
      align = Some Draw.Min
    } in

  Table.create ~h:400 ~row_height:height ?max_selected:options.max_selected
    ~on_select:(fun _ -> Update.push message.label)
    ~on_click:(fun _ j -> message.clicked_entry <- Some entries.(j))
    [icon_col; name_col; size_col; mod_col]
(* The height of 400 will be changed later. We could also directly call
   [update_message] from the [on_select] function (there should not be any
   circular definition preventing this); the advantage of using the [Update]
   mechanism is that in the (admittedly not likely) case of many calls to
   [on_select], we ensure that there will be only one call to [update_message]
   per frame. *)

(* Get the unix stats and save it in [stats_hash] to avoid multiple calls to the
   external [stat_os] function involved in [Sys.is_directory] and
   [Unix.lstat]. *)
let get_stat path name stats_hash =
  match Hashtbl.find_opt stats_hash name with
  | None -> begin try
        printd debug_io "Loading stats for file [%s]" name;
        let ls = Unix.lstat (path // name) in
        if ls.st_kind = S_LNK
        then let ts = try Some (Unix.stat (path // name)) with _ -> None in
          Hashtbl.add stats_hash name (ls, ts);
          Some (ls, ts)
        else Some (ls, None)
      with _ -> (
          printd debug_io "Cannot access file %s for stats" name;
          None)
    end
  | s -> s

let entry_from_name path stats_hash name =
  let stat, target_stat = match get_stat path name stats_hash with
    | None -> None, None
    | Some (ls, ts) -> Some ls, ts in
  { name; target_stat; stat }

let compare_fn dirs_first compare =
  if dirs_first then
    fun e1 e2 -> match entry_is_directory e1, entry_is_directory e2 with
      | false, false
      | true, true -> compare e1.name e2.name
      | true, false -> -1
      | false, true -> 1
  else fun e1 e2 -> compare e1.name e2.name

(* [make_table] constructs the Table.t layout from the file monitor [mon]. It is
   designed to work fast even on very large directories (like /usr/bin).  It
   only shows entries for which [filter] returns true. TODO?: It should also do
   well on remote files systems, where Unix.stat can be slow. We could delay
   calling stats, but since [filter] applies on stats, we cannot pre-filter the
   names; we need to progressively update the table. *)
let make_table ~options message mon ?name_filter ?full_filter stats_hash =
  let path = Monitor.path mon in
  (* We first filter with the fast [name_filter]. *)
  let names = Monitor.ls mon
              |> opt_map (map_option name_filter List.filter) in
  (* Now we use the full filter ; TODO move this to Thread if slow, and use
     [Var.protect_fn stats_hash...] *)
  let entry_list = match full_filter with
    | None -> List.map (entry_from_name path stats_hash) names
    | Some f -> List.filter_map (fun name ->
        let e = entry_from_name path stats_hash name
        in if f e then Some e else None) names in
  (* We sort the entry list alphabetically and save it in an array for faster
     access. *)
  let entries = List.sort (compare_fn options.dirs_first String.compare)
      entry_list |> Array.of_list in

  let finalize _ = () in (* TODO *)
  (* (\* We will finish populating the stats in a separate Thread *\) *)
  (* let finalize t = *)
  (*   printd debug_thread *)
  (*     "Starting file dialog [finalize] for [%s]..." (Monitor.path t.monitor); *)
  (*   for i = 0 to length - 1 do *)
  (*     ignore (get_stats i); *)
  (*   done; *)
  (*   print_endline "DONE!"; *)
  (*   let table = Table.create ~h:400 ~row_height:height [name_col; size_col] in *)
  (*   t.table <- table; *)
  (*   Table.refresh table; *)
  (*   printd debug_thread "End of file dialog [finalize]." *)
  (* in *)

  let table = make_f_table ~options message entries in
  (* let height_fn _ = Some 30 in *)
  (* let long = Long_list.create ~w:150 ~h:400 ~generate ~height_fn *)
  (*     ~length () in *)
  table, entries, finalize (* , long *)

let get_table_layout t =
  Table.get_layout (t.directory.table)

(* Dichotomy search. Returns immediately if x is the first or last element. *)
let find_index_sorted ?first ?last a x compare =
  let open Array in
  let rec loop i1 i2 =
    if compare x (unsafe_get a i1) = 0 then Some i1
    else if compare x (unsafe_get a i2) = 0 then Some i2
    else if i1 = i2 || i1 + 1 = i2 then None
    else let mid = (i1 + i2) / 2 in
      if compare x (unsafe_get a mid) > 0 then loop mid i2 else loop i1 mid in
  loop (default first 0) (default last (Array.length a - 1))

let test_find_index_sorted () =
  let c = compare in
  let a = Array.init 100 (fun i -> i+10) in
  assert (find_index_sorted a 15 c = Some 5);
  assert (find_index_sorted ~first:6 ~last:50 a 15 c = None);
  assert (find_index_sorted ~first:50 ~last:70 a 60 c = Some 50)

(* The elements of [sub] must be a subset of [a]. Both must be sorted according
   to the [compare] function. Then [sorted_subarray_to_selection a sub compare]
   returns the Selection.t variable corresponding to the indices of the elements
   of [sub] within [a]. *)
let sorted_subarray_to_selection a sub compare =
  let uget = Array.unsafe_get in
  let rec subloop smin smax amin amax =
    if smin > smax then []
    else
    if smax - smin = amax - amin then [Selection.Range (amin, amax)]
    else let find ~first ~last j =
           find_index_sorted ~first ~last a (uget sub j) compare |> Option.get in
      let i1 = find ~first:amin ~last:amax smin in
      let i2 = find ~first:amin ~last:amax smax in
      if i1 = i2 || i1 + 1 = i2 then [Selection.Range (i1, i2)]
      else let rec loop j =
             if  smin + j > smax ||
                 compare (uget sub (smin + j)) (uget a (i1 + j)) <> 0
             then j - 1 else loop (j + 1) in
        let j1 = loop 1 in (* first (smallest) sel index after smin for which
                                 a.(i1+j) and sub.(smin+j) are equal. *)
        (* print_endline ("Select" ^ Selection.sprint [Selection.Range (i1, i1 + j1)]); *)
        (Selection.Range (i1, i1 + j1)) ::
        subloop (smin + j1 + 1) smax (i1 + j1 + 1) i2 in
  subloop 0 (Array.length sub - 1) 0 (Array.length a - 1)

let test_sorted_subarray_to_selection () =
  let a = [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" |] in
  let sub = [| "a"; "b"; "e"; "g"; "h" |] in
  let sel = sorted_subarray_to_selection a sub compare in
  Selection.sprint sel |> print_endline;
  assert (sel = Selection.[Range (0,1); Range (4,4); Range (6,7)]);
  let sub = [| "b"; "d"; "f"; "h" |] in
  let sel = sorted_subarray_to_selection a sub compare in
  Selection.sprint sel |> print_endline;
  assert (sel = Selection.[Range (1,1); Range (3,3); Range (5,5); Range (7,7)]);
  let sel = sorted_subarray_to_selection a a compare in
  Selection.sprint sel |> print_endline;
  assert (sel = Selection.[Range (0,7)])

let selected_entries entries sel =
  Selection.fold (fun i list -> entries.(i) :: list) sel []
  |> List.rev (* TODO: tail modulo cons *)

let install_new_table old_table new_table =
  L.setx new_table (L.getx old_table);
  L.sety new_table (L.gety old_table);
  let w, h = L.get_size old_table in L.set_size new_table ~w ~h;
  if L.replace_room old_table ~by:new_table
  then (* We reinstall the resize functions: *)
    L.resize_keep_margins new_table
  else printd (debug_error + debug_io) "Error installing new table for file dialog"
(* L.retower ~duration:0 ~margins:0 t.layout; *)
(* B_sync.push (fun () -> Trigger.push_from_id Sdl.Event.mouse_motion 0) *)
(* Trigger.push_mouse_focus (-1) *)
(* If the list of files becomes smaller then the layout, there will be some
   blank space below. We could shrink the layout size using: *)
(* Sync.push (fun () -> L.fit_content ~sep:0 layout); *)
(* But this does not seem to be the usual behaviour amongst file dialogs out
   there.*)

(* In this function [update_table] we create a new Table.t if the path content
   was modified, but the monitor was not changed (same directory). We transfer
   the scrollbar position, the choice of sorted column, and the file selection,
   from the old table to the new. We also delete the obsolete entries from the
   [stats_hash] table. For modified files, this will force a new invocation of
   Unix.stats. *)
(* TODO if there are only modified files (no deletion or creation) it's not
   necesary to recreate the table, we could update the individual widgets by
   keeping a Weay.array of visible widgets, populated by the "generate"
   functions.  *)
let update_table ?(force = false) t =
  printd debug_io "[File.update_table].";
  let d = t.directory in
  let mon = d.monitor in
  let dl, ad, md = Monitor.modified mon in
  if force || dl <> [] || ad <> [] || md <> [] then begin
    printd debug_io "Table needs to be udpated.";
    let path = Monitor.path mon in
    let stats_hash = Var.get d.stats_hash in (* lock ? *)
    let comp = compare_fn t.options.dirs_first String.compare in
    (* update Hash table: *)
    List.iter (Hashtbl.remove (Var.get d.stats_hash)) dl;
    List.iter (Hashtbl.remove (Var.get d.stats_hash)) md;
    (* remove [dl] (deleted files) from old selection: *)
    let dl_sub = List.map (entry_from_name path stats_hash) dl
                 |> Array.of_list in Array.(sort comp dl_sub);
    let dl_sel = sorted_subarray_to_selection d.entries dl_sub comp in
    printd debug_io "Deleted entries: %s" (Selection.sprint dl_sel);
    let sel = Selection.minus (Table.get_selection d.table) dl_sel in
    printd debug_io "remaining selection: %s." (Selection.sprint sel);
    (* Construct the new table: *)
    let table2_t, entries, _finalize =
      make_table ~options:t.options t.message mon
        ?name_filter:t.name_filter ?full_filter:t.full_filter
        (Var.get d.stats_hash) in
    (* 1. Restore the updated selection: *)
    let selected_files = Array.of_list (selected_entries d.entries sel) in
    let sel2 = sorted_subarray_to_selection entries selected_files comp in
    printd debug_io "Restoring selection = %s." (Selection.sprint sel2);
    Table.set_selection table2_t sel2;
    Update.push t.message.label;
    (* 2. Restore choice of sorted column. Warning, this regenerates the
       table. Hence the selection must be updated before. *)
    do_option (Table.get_sorted_column d.table)
      (fun (i, reverse) -> Table.sort_column table2_t ~reverse i);
    (* Reinstall the new table in the layout: *)
    install_new_table (get_table_layout t) (Table.get_layout table2_t);
    (* 3. Restore scroll value. Warning, this depends on the size of the layout
       (the mapping between the long_table offset and the slider position
       depends on the height of the room). *)
    let scroll = Table.get_scroll d.table in
    Table.set_scroll table2_t scroll;
    L.update_current_geom (Table.get_layout table2_t);

    d.table <- table2_t;
    d.entries <- entries
  end
  else printd debug_io "Table does not need to be updated"

let get_layout (t : t) = t.layout

let get_selected_entries t =
  let sel = Table.get_selection t.directory.table in
  let entries = t.directory.entries in
  selected_entries entries sel

let get_selected t =
  if t.options.allow_new && t.options.max_selected = Some 1
  then [W.get_text t.new_file]
  else get_selected_entries t
  |> List.map (fun e -> e.name)

let set_selection t sel = Table.set_selection t.directory.table sel

let basedir t =
   Monitor.path t.directory.monitor

let start_monitor controller path =
  let action _ = Update.push controller in
  Monitor.start ~action path

let path_selector text =
  let ti = W.text_input ~prompt:"Enter path" ~text () in
  Text_input.last (W.get_text_input ti);
  ti

let new_directory controller message ~options ?full_filter ?name_filter path =
  let monitor = start_monitor controller path in
  let stats_hash = Var.create (Hashtbl.create (default (Monitor.size monitor) 100)) in
  let table, entries, _finalize = make_table ~options message monitor
      ?name_filter ?full_filter (Var.get stats_hash) in
  { monitor; stats_hash; table; entries }

let plural x = if x > 1 then "s" else ""
let pluraly x  = if x > 1 then "ies" else "y"

let update_message t =
  printd debug_io "[File.update_message]";
  let entries = get_selected_entries t in
  let n_files, n_dirs = List.fold_left (fun (f, d) e ->
      if entry_is_directory e then (f, d+1) else (f+1, d))
      (0, 0) entries in
  let text = match n_files, n_dirs with
    | 0, 0 -> "No selection"
    | 0, d -> Printf.sprintf "%u director%s selected" d (pluraly d)
    | f, 0 -> Printf.sprintf "%u file%s selected" f (plural f)
    | f, d -> Printf.sprintf "%u file%s and %u director%s selected"
                f (plural f) d (pluraly d) in
  W.set_text t.message.label text;
  if n_files + n_dirs = 1 then begin
    if (n_files = 1 && not t.options.select_dir) ||
       (n_dirs = 1 && t.options.select_dir)
    then W.set_text t.new_file (let e = List.hd entries in e.name)
  end;
  n_dirs, n_files

let breadcrumb path =
  let rec loop acc p =
    let b = Filename.basename p in
    if b = p then b::acc else loop (p::acc) (Filename.dirname p) in
  let split = loop [] path in
  List.map (fun p -> W.button (Filename.basename p), p) split

let install_new_directory t path =
  printd debug_io "Installing new directory [%s]" path;
  Monitor.stop t.directory.monitor;
  let d = new_directory ~options:t.options t.controller t.message
      ?name_filter:t.name_filter ?full_filter:t.full_filter path in
  let old_table = get_table_layout t in
  t.directory <- d;
  Update.push t.message.label;
  install_new_table old_table (Table.get_layout d.table)

let get_selected_dir t =
  match get_selected_entries t with
  | [] -> printd debug_error "[File.open_new_dir]: no directory selected!";
    None
  | list -> let e = match list with
      | [] -> failwith "[open_new_dir] should not happen"
      | [e] -> e
      | e :: _ ->
        printd debug_error "[File.open_new_dir]: only one path should be selected";
        e in
    if entry_is_directory e
    then Some e.name
    else (printd debug_io "Selected entry [%s] is not a directory." e.name;
          None)

let validate_new_file_input t ti =
  let name = W.get_text ti in
  if name <> "" then begin
    match find_entry t.directory.entries name with
    | None ->
      if get_selected_entries t <> []
      then set_selection t []
    | Some i -> (* if possible, we add it to the selection. *)
      if t.options.select_dir || not (entry_is_directory (t.directory.entries.(i)))
      then let sel = Selection.range (i,i) in
        if Table.get_selection t.directory.table <> sel
        then set_selection t sel
  end

let open_dir t path =
  t.message.clicked_entry <- None;
  install_new_directory t path;
  if t.options.select_dir then W.set_text t.new_file ""
  else if t.options.max_selected = Some 1 then validate_new_file_input t t.new_file;
  Update.push t.input.text_input

let connect_breadcrumb t =
  List.iter (fun (b, path) ->
      W.on_click b ~click:(fun _ -> open_dir t path)) t.input.breadcrumb

let update_input t =
  let path = Monitor.path t.directory.monitor in
  printd debug_io "Updating file dialog path= [%s]." path;
  W.set_text t.input.text_input path;
  Text_input.last (W.get_text_input t.input.text_input);
  t.input.breadcrumb <- breadcrumb path;
  let layout = L.flat_of_w ~sep:0 (List.map fst t.input.breadcrumb) in
  let x, y = L.getx t.input.layout, L.gety t.input.layout in
  let rsz = t.input.layout.resize in
  let show = L.is_shown t.input.layout in
  L.setx layout x; (* used only for temporary animations *)
  L.sety layout y;
  if L.replace_room ~by:layout t.input.layout
  then printd debug_board "File dialog: installing new input %s"
      (L.sprint_id layout)
  else printd (debug_board + debug_error) "File dialog: cannot install new input %s"
      (L.sprint_id layout);
  layout.resize <- rsz;
  if not show then L.hide ~duration:0 layout;
  Sync.push (fun () -> L.resize layout);
  (* : TODO find a way to not wait for Sync? e have to make sure layout is
     correctly installed before asking for its size. *)
  t.input.layout <- layout;
  (* do_option (L.containing_widget t.input.text_input) (L.hide ~duration:0); *)
  connect_breadcrumb t

let open_selected_dir t =
  printd debug_io "[File.open_selected_dir].";
  do_option (get_selected_dir t) (fun name ->
      let path = Monitor.path t.directory.monitor // name in
      open_dir t path)

let open_clicked_dir t =
  do_option (t.message.clicked_entry) (fun e ->
      if entry_is_directory e then
        let path = Monitor.path t.directory.monitor // (e.name) in
        open_dir t path)

let validate_text_input t ti =
  let old_path = Monitor.path t.directory.monitor in
  let path = W.get_text ti in
  if path <> old_path then begin
    if is_directory path then begin
      install_new_directory t path;
      if t.options.max_selected = Some 1 then validate_new_file_input t t.new_file;
      update_input t
    end
    else let dir = Filename.dirname path in
      if is_directory dir then begin
        (* We use the "basename" part to automatically select the file *)
        install_new_directory t dir;
        W.set_text ti dir;
        W.set_text t.new_file (Filename.basename path);
        validate_new_file_input t t.new_file;
        update_input t
      end
      else W.set_text ti (Monitor.path t.directory.monitor)
  end

(* If the user enters a valid dir and presses RETURN, we change to it. If it is
   a "dir/bla" when dir is valid, we switch to dir and set bla to the new_file
   input. *)
let connect_text_input t =
  let on_key_down = fun ti _ ev ->
    if Sdl.Event.(get ev keyboard_keycode) = Sdl.K.return
    then validate_text_input t ti
    else if Sdl.Event.(get ev keyboard_keycode) = Sdl.K.escape
    then W.set_text ti (Monitor.path t.directory.monitor)
  in
  let ti = t.input.text_input in
  W.connect_main ti ti on_key_down [Sdl.Event.key_down]
  |> W.add_connection ti

(* let path_selector_combo path = *)
(*   L.superpose *)

let bg1 = L.style_bg
    Style.(of_bg (gradient ~angle:90. [(Draw.(opaque (pale Button.color_off)));
                                       path_entry_color])
           |> with_border (mk_border ~radius:5 (mk_line ~width:0 ())))

let bg_over = Some (Style.opaque_bg Draw.grey)

let show room = L.show ~duration:0 room; L.fade_in room

(* We create a combo layout which superposes a text input (for entering path)
   and a "breadcrumb" with a list of buttons that allows to select parent
   directories by directly clicking. There is an "edit/check" button that allows
   to switch between both. Note that we have a cycle of connections: clicking on
   the breadcrumb updates the file table, and clicking on a directory in the
   file table updates the breadcrumb. *)
let make_input_layout w input =
  let path_label = L.resident ~name:"path_label" ~background:bg1 input.text_input in
  let ok = W.button ~kind:Button.Switch ~bg_over (* ~border_radius:12 *)
      ~action:(fun b ->
          if b then (show path_label; L.fade_out ~hide:true input.layout)
          else (L.fade_out ~hide:true path_label; show input.layout))
      ~label_on:(Label.icon "check")
      ~label_off:(Label.icon "edit") "" in
  let lok = L.resident ~name:"ok" ok in
  L.setx lok (w - L.width lok);
  L.set_height lok (L.height input.layout);
  (* Bottom align: *)
  L.sety lok (L.height input.layout - L.height lok);
  L.sety path_label (L.height input.layout - L.height path_label);
  let container = L.superpose ~w [ input.layout; path_label; lok ] in
  L.set_clip container;
  path_label.resize <- (let open L.Resize in fun (w, _h) ->
      set_width path_label (w - L.width lok));
  path_label.resize (w, 0);
  lok.resize <- (let open L.Resize in fun (w, _h) ->
      setx lok (w - L.width lok));
  L.hide ~duration:0 path_label;
  input.layout.resize <- (fun (w, _h) ->
      let dx = L.width input.layout - w + L.width lok in
      (* dx > 0 when the input layout is too wide *)
      let x0 = L.getx input.layout in
      let x1 = imin 0 (-dx) in
      (* print_endline (Printf.sprintf "dx=%i ==> %i x0=%i x1=%i" dx (imin 0 (-dx)) x0 x1); *)
      if x0 <> x1 then L.animate_x input.layout (Avar.fromto x0 x1)
      else L.stop_pos input.layout;
      (* setx input.layout () *));
  container, ok


(* This is the layout where the user can enter the file name (for saving, in
   general, but it also works for selecting an existing file).
   * It is displayed only if one file or dir must be chosen.
   * It is updated when the user clicks on a file (if a file should be
     selected) or a directory (if a dir should be selected).
   * When the name is modified, the selection disappears,
     except if the entered text matches an existing file:
     then that file is automatically selected. *)
let new_file_layout ~label name =
  let label = W.label label in
  let inp = W.text_input ~text:name () in
  let bg = Style.(of_border (mk_border ~radius:5 (mk_line ~width:1 ()))
                  |> with_bg (color_bg Draw.(opaque white)))
           |> L.style_bg in
  let inp_l = L.resident ~background:bg inp in
  let room = L.flat ~vmargin:0 ~resize:L.Resize.Disable ~align:Draw.Center ~sep:10
      [L.resident label; inp_l] in
  L.resize_keep_margins inp_l;
  room, inp
  (* TODO add connections, etc... *)

(* We construct the main layout. Note that the table layout and the breadcrumb
   layout are both self-destructing; in order to avoid circular definitions we
   use a controller, see the tutorial:
   https://sanette.github.io/bogue-tutorials/bogue-tutorials/modif_parent.html *)
let dialog ?full_filter ?options path =
  let path =
    if Filename.is_relative path then
      if path = "" || path = Filename.current_dir_name then Sys.getcwd ()
      else Sys.getcwd () // path
    else path in
  let options = default options (set_options ()) in
  let controller = W.empty ~w:0 ~h:0 () in
  let message_label = W.label ~fg:hidden_color "No selection" in
  let message = { label = message_label; clicked_entry = None } in
  let new_file_room, new_file = new_file_layout ~label:"Name :" options.default_name in
  let name_filter name = (options.show_hidden || not (is_hidden name)) &&
                         (not options.hide_backup || not (is_backup name)) in
  let full_filter = match options.mimetype with
    | None -> full_filter
    | Some reg -> let f e = entry_is_directory e
                            || Str.string_match reg (Mime.type_string e.name) 0 in
      match full_filter with
      | None -> Some f
      | Some ff -> Some (fun e -> ff e && f e) in
  let directory = new_directory ~options controller message
      ?full_filter ~name_filter path in
  let path = Monitor.path directory.monitor in
  let text_input = path_selector path in
  let w = L.width (Table.get_layout directory.table) in
  (* let _background = L.style_bg *)
  (*     Style.(of_bg (color_bg path_entry_color) *)
  (*            |> with_border (mk_border ~radius:12 (mk_line ()))) in *)

  let ariane = breadcrumb path in
  let input = { text_input;
                breadcrumb = ariane;
                layout = L.flat_of_w ~sep:0 (List.map fst ariane) } in
  let path_selector_combo, combo_btn = make_input_layout w input in
  let table_room = Table.get_layout directory.table in
  let open_button = W.button "Open dir" in
  let open_btn_room = if options.open_dirs_on_click
    then L.empty ~name:"empty" ~w ~h:0 ()
    else L.resident open_button in
  let message_room = L.resident ~name:"message" ~w message.label in
  let layout =
    L.tower ~resize:L.Resize.Disable ~name:"file_dialog" ~vmargin:0 ~hmargin:0 ~sep:10
      (List.filter_map (fun x -> x)
         [ Some (L.resident ~name:"controller" controller);
           Some path_selector_combo;
           Some table_room;
           Some open_btn_room; (* If we want to try some tower resize strategies, don't
                                  put this at the end because when not displayed it will
                                  prevent the tower resizing function from working. *)
           if options.max_selected = Some 1 then Some new_file_room else None;
           Some message_room ]) in
  L.resize_keep_margins table_room;
  L.resize_follow_width path_selector_combo;
  L.resize_follow_width message_room;
  L.resize_follow_width new_file_room;
  Space.keep_bottom_sync ~reset_scaling:false new_file_room;
  Space.keep_bottom_sync ~reset_scaling:true open_btn_room;
  Space.keep_bottom_sync ~reset_scaling:false message_room;

  L.set_size layout ?w:options.width ~h:options.height;

  let t = { controller;
            input;
            message;
            new_file;
            name_filter = Some name_filter;
            full_filter;
            layout;
            directory;
            options } in
  Empty.on_unload (W.get_empty controller) (fun () ->
      Monitor.stop t.directory.monitor);
  (* let _ = Thread.create finalize t in *)

  let open_button_action _w _ _ =
    open_selected_dir t in
  W.connect_main open_button controller open_button_action Trigger.buttons_down
  |> W.add_connection open_button;

  connect_text_input t;

  W.connect_main controller controller
    (fun _ _ _ -> update_table t) [Trigger.update]
  (* We could use [W.connect] here to launch the update in a different thread,
     provided we use Sync.push for installing the new layout and scroll. *)
  |> W.add_connection controller;

  W.connect_main text_input text_input
    (fun _ _ _ -> update_input t) [Trigger.update]
  |> W.add_connection text_input;

  W.on_button_release combo_btn ~release:(fun b ->
      if not (W.get_state b)
      then validate_text_input t text_input
      (* let path = W.get_text text_input in *)
      (* if is_directory path then (install_new_directory t path; update_input t) *)
      (* else W.set_text text_input (Monitor.path t.directory.monitor) *)
    );

  (* Examine the new selection and react. *)
  W.connect_main message.label open_button
    (fun _ _btn _ ->
       let n_dirs, n_files = update_message t in
       apply_option t.options.on_select (n_dirs, n_files);
       if Trigger.mouse_left_button_pressed () && n_dirs >= 1
          && t.options.open_dirs_on_click
       then open_clicked_dir t
       else if not t.options.open_dirs_on_click
       then begin if n_dirs = 1 && n_files = 0
         then L.(show open_btn_room)
         else L.(hide open_btn_room)
       end) [Trigger.update]
  |> W.add_connection message.label;

  W.connect_main new_file new_file (fun ti _ _ ->
      validate_new_file_input t ti) Text_input.triggers
  |> W.add_connection new_file;

  connect_breadcrumb t;

  Update.push message.label;
  t

(* Return label and max_selected *)
let get_label2 ?n_dirs ?n_files () =
 match n_dirs, n_files with
    | Some 0, Some 0 -> "Continue", Some 0
    | Some 1, Some 0 -> "Select directory", Some 1
    | Some 0, Some 1 -> "Select file", Some 1
    | _, Some 0 -> "Select dirs", n_dirs
    | Some 0, _ -> "Select files", n_files
    | None, _
    | _, None -> "Select", None
    | Some d, Some f -> "Select", Some (d + f)

let select_popup ?dst ?board ?w ?h path ?n_files ?n_dirs ?mimetype ?name
    ?(allow_new = false) ?button_label continue =
  let select_one = (n_files = Some 1 && n_dirs = Some 0) ||
                   (n_dirs = Some 0 && n_files = Some 1) in
  (* allow_new=true should be set only for selecting ONE file or ONE dir *)
  if allow_new then assert select_one;
  let w, h = match dst with
    | Some dst ->
      let w0, h0 = L.width dst - 4 * Theme.room_margin,
                   L.height dst - 4 * Theme.room_margin in
      let width = match w with
        | Some w -> imin w w0
        | None -> w0 in
      let height = match h with
        | Some h -> imin h h0
        | None -> h0 in
      Some width, Some height
    | None -> w, h in
  let sel_dirs, sel_files = ref 0, ref 0 in
  let on_select (n_d, n_f) =
    sel_dirs := n_d; sel_files := n_f in
  let label2, max_selected = get_label2 ?n_dirs ?n_files () in
  let label2 = default button_label label2 in
  let options = set_options (* ~width ~height *) ~on_select
      ~default_name:(default name "")
      ~select_dir:(n_dirs = Some 1 && n_files = Some 0)
      ~open_dirs_on_click:(n_dirs = Some 0)
      ~allow_new
      ~hide_backup:true ?max_selected ?mimetype () in
  let fd = dialog ~options path in

  (* New file name should not be an existing dir *)
  let ok_new_file name =
    match find_entry fd.directory.entries name with
    | None -> true
    | Some i -> not (entry_is_directory (fd.directory.entries.(i))) in

  let enable btn2 b2 =
    let ok = ((n_dirs = None && !sel_dirs > 0) || Some !sel_dirs = n_dirs) &&
             ((n_files = None && !sel_files > 0) || Some !sel_files = n_files) in
    let ok = ok || (allow_new && let name = W.get_text (fd.new_file) in
                    name <> "" && (n_dirs = Some 1 || ok_new_file name)) in
    printd debug_io "File dialog enabling select button: %b" ok;
    (* print "[%s] sle_dirs=%i, sel_files=%i, ok=%b" *)
    (*   (W.get_text (fd.new_file)) !sel_dirs !sel_files ok; *)
    if ok then begin
      (* if allow_new then sel_dirs and sel_files must be 0. The b2 label never changes. *)
      if not allow_new && !sel_dirs + !sel_files > 0
      (* always true unless n_dirs = n_files = Some 0 *)
      then W.set_text btn2 (fst (get_label2 ~n_dirs:!sel_dirs ~n_files:!sel_files ()));
      if b2.L.disabled then (L.fade_in ~from_alpha:0.5 ~to_alpha:1. b2; L.enable b2)
    end else begin
      L.fade_out ~to_alpha:0.5 b2;
      L.disable b2;
      W.set_text btn2 label2
    end
  in

  (* The button2 can be enabled/disabled either by a change of selection, or a
     change of the new_file text_input. *)
  let connect2 t btn2 =
    do_option (L.containing_widget btn2) (fun b2 ->
        enable btn2 b2;
        (* We update the "Select" button when the status message changes. *)
        let c = W.connect_main t.message.label btn2
            (fun _ _ _ -> enable btn2 b2) [Trigger.update] in
        W.add_connection t.message.label c;
        if select_one
        then begin
          (* If we press ENTER on the new_file input, we simulate pressing the
             "Select" button. *)
          let c = W.connect_main t.new_file btn2
              (fun _ b ev ->
                 if not b2.L.disabled && Sdl.Event.(get ev keyboard_keycode) = Sdl.K.return
                 then (printd debug_board "[File.dialog] Simulate button up.";
                       W.wake_up_all Trigger.(create_event E.mouse_button_up) b))
              Sdl.Event.[key_up] in
          W.add_connection t.new_file c;
          let c = W.connect_main t.new_file btn2
              (fun _ b ev ->
                 if not b2.L.disabled && Sdl.Event.(get ev keyboard_keycode) = Sdl.K.return
                 then (printd debug_board "[File.dialog] Simulate button down.";
                       W.wake_up_all Trigger.(create_event E.mouse_button_down) b))
              Sdl.Event.[key_down] in
          W.add_connection t.new_file c
        end;
        if allow_new
        (* We enable the "Select" button if the user enters a string in the
           "new_file" input. *)
        then let c = W.connect_main t.new_file btn2
                 (fun _ _ _ -> enable btn2 b2) Sdl.Event.[key_up] in
          W.add_connection t.new_file c)
  in

  Popup.two_buttons ?dst ?board ?w ?h ~label1:"Cancel" ~label2
    ~action1:(fun () ->
        Monitor.stop fd.directory.monitor)
    ~action2:(fun () ->
        Monitor.stop fd.directory.monitor;
        continue (List.map (Filename.concat (basedir fd)) (get_selected fd)))
    ~connect2:(connect2 fd)
    fd.layout

let select_file ?dst ?board ?w ?h ?mimetype ?name path continue =
  select_popup ?dst ?board ?w ?h ?mimetype ?name path ~n_files:1 ~n_dirs:0
    (fun list -> continue (List.hd list))

let select_files ?dst ?board ?w ?h ?mimetype ?n_files path continue =
  select_popup ?dst ?board ?w ?h ?mimetype path ?n_files ~n_dirs:0 continue

(* FIXME when selecting all with CTRL-A, if the selection contains a unique dir,
   this dir is immediately opened, this should not happen. **)


let select_dir ?dst ?board ?w ?h ?name path continue =
  select_popup ?dst ?board ?w ?h ?name path ~n_files:0 ~n_dirs:1
    (fun list -> continue (List.hd list))

let select_dirs ?dst ?board ?w ?h ?n_dirs path continue =
  select_popup ?dst ?board ?w ?h path ~n_files:0 ?n_dirs continue

let save_as ?dst ?board ?w ?h ?name path continue =
  select_popup ?dst ?board ?w ?h ?name path ~n_files:1 ~n_dirs:0
    ~allow_new:true ~button_label:"Save"
    (fun list -> continue (List.hd list))

(* Not used any more. Use [select_file] with no [dst] option instead. *)
(* let select_file_new_window ?board ?w ?(h=400) path _continue = *)
(*   let options = set_options ?width:w ~height:h ~open_dirs_on_click:true *)
(*       ~hide_backup:true ~max_selected:1 () in *)
(*   let fd = dialog ~options path in *)
(*   let m = Theme.room_margin in *)
(*   L.setx fd.layout m; *)
(*   L.sety fd.layout m; *)
(*   let frame = L.superpose [ fd.layout ] *)
(*       ~w:(L.width fd.layout + 2 * m) *)
(*       ~h:(L.height fd.layout + 2 * m) in *)
(*   L.resize_keep_margins fd.layout; *)
(*   match board with (\* etc. à faire dans Popup, use [continue]... *\) *)
(*   | Some b -> ignore (Main.add_window b ((\* L.cover ~name:"file-dialog cover" *\) frame)) *)
(*   | None -> L.add_window fd.layout *)


let ( let@ ) dialog f = dialog f  (* same as Utils.( let@ ) *)

(* let () = let@ file = select_file "essai" in *)
(* print_endline file *)
