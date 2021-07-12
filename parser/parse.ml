(** front_end.ml *)

open Util
include Syntax
       
(** parse : string -> proc 
    - Returns an AST
*)
let parse = Parser.main Lexer.token <. Lexing.from_string

