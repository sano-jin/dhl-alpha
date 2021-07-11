(** front_end.ml *)

open Util
(* open Breakdown *)
include Breakdown
       
(** parse : string -> proc 
    - Returns an AST
*)
let parse = Parser.main Lexer.token <. Lexing.from_string

(** semantic analyzer
    - Obtains semantic graph from the given AST
 *)
let sem_graph_of proc = 
  match breakdown proc with
  | ((((local_indegs, []), []), inds), rules) -> ((local_indegs, classify_inds inds), rules)
  | _ -> failwith "free links are not allowed in the initial graph"


(** The toplevel of the front end *)
let front_end = sem_graph_of <. parse
