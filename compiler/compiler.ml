(** Compiler *)

open Parse
open Analyzer
open Generator
open Util


(** Compiles given string and return the generated intermediate code *)
let compile = gen_ic <. sem_graph_of <. parse


