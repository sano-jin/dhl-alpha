# Front end

## Lexical/Syntax analyzer

- [syntax.ml](syntax.ml)
  - The abstract syntax definition

- [lexer.mll](lexer.mll)
  - Defines a token for lexing

- [parser.mly](parser.mly)
  - Defines a grammar for parsing

## Semantic analyzer

- [alpha.ml](alpha.ml)
  - Convert local link names to fresh ids and partition atoms and rules

- [link_check.ml](check.ml)
  - Collect link information and check them

- [breakdown.ml](breakdown.ml)
  - Check rule conditions and break down atoms

- [front_end.ml](front_end.ml)
  - The toplevel of the front end
  
  
