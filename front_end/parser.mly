(*  Parser *)
     
%{
  open Syntax
%}

%token <string> AtomName (* x, y, abc, ... *)
%token <string> LinkName (* X, Y, ABC, ...  *)

(*  operators *)
%token BACKSLASH (*  '\\' *)
%token DOT       (*  '.' *)
%token COMMA     (*  ',' *)
%token ARROW     (*  "->" *)
%token COLMIN    (*  ":-" *)

(*  Parentheses *)
%token LPAREN   (*  '(' *)
%token RPAREN   (*  ')' *)

(*  End of file *)
%token EOF 

(*  Operator associativity *)
(* %left DOT *)

%nonassoc COLMIN
%left COMMA
%nonassoc DOT
      
%start main
%type <Syntax.proc> main

%%

(*  Main part must end with EOF (End Of File) *)
main:
  | block EOF  { $1 }
;

(*  args_inner *)
args_inner:
  | arg { [$1] }
  | arg COMMA args_inner { $1::$3 }
;

(*  arg *)
arg:
  | LinkName { Link $1 }
  | atom { $1 }
;
    
(*  atom *)
atom:
  | AtomName                    { Atom ($1, []) }
  | AtomName LPAREN RPAREN      { Atom ($1, []) }
  | AtomName LPAREN args_inner RPAREN { Atom ($1, $3) }
;
  
(*  proc *)
proc:
  | arg { Ind (None, $1) }
  | LinkName ARROW arg { Ind (Some $1, $3) }
  
  | proc COMMA proc { Mol ($1, $3) }
  | proc COLMIN proc { Rule ($1, $3)}
  | proc COLMIN { Rule ($1, Zero)}
  
  | BACKSLASH LinkName DOT proc { New ($2, $4)}
  
  | LPAREN proc RPAREN { $2 }
;
    
(* block *)
block:       
  | proc DOT block { Mol ($1, $3) }
  | proc DOT { $1 }
  | proc { $1 }

  | error
    { 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  }
;

