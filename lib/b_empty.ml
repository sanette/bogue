(* An empty widget. Does not draw anything, but can be used to get mouse focus.

This file is part of BOGUE *)

type t = {
    mutable size: int * int
  }

let size e = e.size

let create size =
  { size }

let resize size t =
  t.size <- size

let free _ = ()

let unload _ = ()

let display _ _ _ _ = []
