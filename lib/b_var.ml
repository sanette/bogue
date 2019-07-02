(** variables with mutex *)
(* WARNING contrary to what the OCaml Mutex doc seems to say:

   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Mutex.html
   
   calling Mutex.lock on a mutex locked by the SAME thread will ALSO block. *)

open B_utils;;

type 'a t = {
  mutable data : 'a;
  mutable thread_id : int option;
  (* = the id of the thread currently locking this var *)
  mutex : Mutex.t;
};;

let create data =
  { data;
    thread_id = None;
    mutex = Mutex.create ();
  };;

let release v = (* do not use this without checking thread_id, especially in
                   case of recursion *)
  Mutex.unlock v.mutex;
  v.thread_id <- None;;


(* Execute an action on the given variable if it is not locked by *another*
   thread. Can be used in recursions. When the action wants to access v, it
   should use the unsafe_ versions (although this is not necessary, only
   faster). *)
let protect_fn v action =
  let was_free = Mutex.try_lock v.mutex in
  if was_free then begin (* this should be the vast majority of cases *)
    (* The variable is now locked *)
    if !debug then assert (v.thread_id = None); (* just for debugging *)
    v.thread_id <- Some Thread.(id (self ()));
    let result = action () in
    release v;
    (* The variable is now unlocked *)
    result
  end else if v.thread_id = Some (Thread.(id (self ())))
  then begin
    printd debug_thread "We can access the variable because it was locked by same thread.";
    action ()
  end else begin (* the variable was already locked by another thread *)
    printd debug_thread "Waiting for locked variable (thread #%i) to unlock..."
      (default v.thread_id (-1));
    Mutex.lock v.mutex;
    v.thread_id <- Some Thread.(id (self ()));
    printd debug_thread "...ok, the variable was unlocked, we proceed.";
    let result = action () in
    release v;
    result
  end;;
    

(* usually we don't need to protect when getting the value. But warning, if the
   value itself is a reference, then one should explicitely protect it *)

(* TODO in fact one should. See
   https://en.wikipedia.org/wiki/Readers%E2%80%93writers_problem *)
let get v = v.data;;

let unsafe_get v = v.data;;

let set_old v value =
    Mutex.lock v.mutex;
    v.data <- value;
    Mutex.unlock v.mutex;;

let set v value =
  protect_fn v (fun () -> v.data <- value);;

let unsafe_set v value =
  v.data <- value;;

let protect v =
  Mutex.lock v.mutex;
  v.thread_id <- Some Thread.(id (self ()));;



let incr v =
  protect v;
  v.data <- v.data + 1;
  release v;;

let decr v =
  protect v;
  v.data <- v.data - 1;
  release v;;

(*******)
(* for initialization of global constant by a lazy eval *)
(* TODO: use Lazy module ? *)

exception Not_initialized;;

type 'a init = {
  mutable init : unit -> 'a; (* the function which creates the value *)
  var : ('a option) t
};;

let init init =
  { init; (* ou Var ? *)
    var = create None
  }

let create_init () =
  init (fun () -> raise Not_initialized);;

let set_init i f =
  i.init <- f;
  set i.var None;;

let init_get i = match get i.var with
  | None -> let data = i.init () in set i.var (Some data); data
  | Some d -> d;;

(*
ocamlmktop -thread -custom -o threadtop unix.cma threads.cma -cclib -lthreads
*)


(*
   Local Variables:
   tuareg-interactive-program:"ocaml unix.cma"
   typerex-interactive-program:"./threadtop -I +threads"
   compile-command:"make -k"
   End:
*)
