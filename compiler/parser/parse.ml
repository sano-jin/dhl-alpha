(** Parse *)

open Util
include Syntax
       


(** @return AST *)
let parse = Parser.main Lexer.token <. Lexing.from_string

