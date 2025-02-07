(* This file is part of BOGUE, by San Vu Ngoc *)

(* A Tvar, or transform variable of type ('a,'b) is a variable of type 'b
   attached to a variable of type 'a Var.t by a bi-directional transformation *)
(* there is no caching *)

module Var = B_var

type ('a, 'b) t =
{ var : 'a Var.t;
  t_from : 'a -> 'b;
  t_to : 'b -> 'a
}

(* Setting/getting the a value should be done directly via the variable [var] *)

(* Get the b value from the var *)
let get v =
  Var.get v.var
  |> v.t_from

(* Update the local var by giving the b value. Consider sendind the var_changed
   event. Note that the composition "get o set" may not be the identity
   function! *)
let set v value =
  Var.set v.var (v.t_to value)

(* [var] is an Avar of type 'a *)
let create var ~t_from ~t_to =
  { var;
    t_from;
    t_to
  }

(* A simple tvar which simply executes an action when the local value 'b is
   changed, and 'a has the same type as 'b *)
let local_action action value =
  let var = Var.create value in
  let t_from x = x in
  let t_to x = action (); x in
  create var ~t_to ~t_from
