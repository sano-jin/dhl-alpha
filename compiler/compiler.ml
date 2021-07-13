(** Compiler *)

open Parse
open Analyzer
open Generator
open Util

let compile = gen_ic <. sem_graph_of <. parse


