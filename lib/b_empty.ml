(* This file is part of BOGUE, by San Vu Ngoc *)

(* An empty widget. Does not draw anything, but can be used to get mouse focus,
   and to execute an action when unloading. *)

type t = {
  mutable size: int * int;
  mutable unload : unit -> unit
  }

let on_unload t f =
  t.unload <- f

let size e = e.size

let create ?(unload=fun () -> ()) size =
  { size; unload }

let resize size t =
  t.size <- size

let free _ = ()

let unload t = t.unload ()

let display _ _ _ _ = []
