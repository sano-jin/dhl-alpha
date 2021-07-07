(* Lexer *)

{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z' '_']
let alpha = lower | upper	      
let alnum = digit | alpha | '\''
			      
rule token = parse
  (* Operators *)
  | '\\'      { BACKSLASH }
  | '.'       { DOT }
  | ','       { COMMA }
  | "->"      { ARROW }
  | ":-"      { COLMIN }

  (* Parentheses *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  
  (* symbol atom name *)
  | lower alnum*
    { AtomName (Lexing.lexeme lexbuf) }
  
  (* link name *)
  | upper alnum*
    { LinkName (Lexing.lexeme lexbuf) }
  
  (* end of file *)
  | eof       { EOF }

  (* spaces *)
  | space+    { token lexbuf }

  (* comments *)
  | '%' [^ '\n']*  { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token '%s' near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
