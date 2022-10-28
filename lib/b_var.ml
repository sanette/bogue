(** variables with mutex, to perform atomic operations *)

(* WARNING contrary to what the OCaml Mutex doc seems to say: (fixed in ocaml
   4.12)

   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Mutex.html

   calling Mutex.lock on a mutex locked by the SAME thread will ALSO block.

TODO starting from ocaml 4.12 one could use the Atomic module in order to avoid
   locking. Anyways, for single reading/assignement, it seems that we can get
   rid of locks. See
   https://discuss.ocaml.org/t/threads-and-atomicity-of-reading-and-assignement/8923/3

Remark: if optimization is needed, one could check whether
   [Utils.threads_created <> 0] before playing with mutexes.  *)

open B_utils

type 'a t = {
  mutable data : 'a;
  mutable thread_id : int option;
  (* = the id of the thread currently locking this var *)
  mutex : Mutex.t;
}

let create data =
  { data;
    thread_id = None;
    mutex = Mutex.create ();
  }

(* lock *)
let protect v =
  Mutex.lock v.mutex;
  v.thread_id <- Some Thread.(id (self ()))

(* unlock *)
let release v =
  match v.thread_id with
  | Some i when i = Thread.(id (self ())) ->
     Mutex.unlock v.mutex;
     v.thread_id <- None
  | Some i ->
     printd (debug_thread + debug_error)
       "Thread %u cannot release variable locked by thread %u"
       Thread.(id (self ())) i
  | None ->
     printd (debug_thread + debug_error)
       "Trying to release a variable that was not locked"

(* Execute an action on the given variable if it is not locked by *another*
   thread. Can be used in recursions. *)
let protect_do v action =
  let was_free = Mutex.try_lock v.mutex in
  if was_free then begin (* this should be the vast majority of cases *)
      (* The variable is now locked *)
      if !debug then assert (v.thread_id = None); (* just for debugging *)
      v.thread_id <- Some Thread.(id (self ()));
      let result = try action () with exn -> release v; raise exn in
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
      let result = try action () with exn -> release v; raise exn in
      release v;
      result
    end

let protect_fn v f =
  protect_do v (fun () -> f v.data)

(* usually we don't need to protect when getting the value. (But if the value
   itself is a reference, then one should explicitely protect the target when
   needed...) *)

(* Some people think that in fact one should. See
   https://en.wikipedia.org/wiki/Readers%E2%80%93writers_problem

But probably not for Ocaml. TODO starting 4.12, use Atomic.  *)
let get v = v.data

let unsafe_get v = v.data

let set_old v value =
    Mutex.lock v.mutex;
    v.data <- value;
    Mutex.unlock v.mutex

(* [safe_set] should be used when we want to register which thread is setting
   this value. (After assignement, thread_id is set back to None). Thus, this
   prevents other threads to modify the value at the same time. But in Ocaml,
   assignement is (essentially?) atomic. Hence, for the moment I don't see in
   which case [safe_set] should be required... *)
let safe_set v value =
  protect_fn v (fun () -> v.data <- value)

(* [set] will set the value without touching the thread_id field. *)
let set v value =
  v.data <- value

let incr v =
  protect v;
  v.data <- v.data + 1;
  release v

let decr v =
  protect v;
  v.data <- v.data - 1;
  release v

(*******)
(* for initialization of global constant by a lazy eval *)
(* TODO: use Lazy module? *)

exception Not_initialized

type 'a init = {
  mutable init : unit -> 'a; (* the function which creates the value *)
  var : ('a option) t
}

let init init =
  { init; (* ou Var ? *)
    var = create None
  }

let create_init () =
  init (fun () -> raise Not_initialized)

let set_init i f =
  i.init <- f;
  set i.var None

let init_get i =
  protect_fn i.var (function
      | None -> let data = i.init () in set i.var (Some data); data
      | Some d -> d)

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
