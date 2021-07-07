(** syntax.ml *)

(** argument of an atom *)
type arg =
  | Atom of string * arg list   (** atom. e.g. a(X, Y) *)
  | Link of string              (** link. e.g. X *)

(** process *)
type proc = 
  | Zero
  | Ind of string option * arg  (** indirection. e.g. X -> a(Y) *)
  | Mol of proc * proc          (** molecule. e.g. (P, Q) *)  
  | New of string * proc        (** link creation. e.g. \X.P *)
  | Rule of proc * proc         (** rule. e.g. P :- Q. *)
