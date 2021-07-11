# Source programs

## Overview

- util.ml
  - Some utility functions

### Compiler

**Syntax analysis**

- syntax.ml
  - The abstract syntax definition
- lexer.mll
  - Defines a token for lexing
- parser.mly
  - Defines a grammar for parsing

**Semantic analysis**

- alpha.ml
  - Convert local link names to fresh ids and partition atoms and rules
- link_check.ml
  - Collect link information and check them
- breakdown.ml
  - Check rule conditions and break down atoms

### VM
- vm.ml
  - Type definition of an atom and pretty printer for its multi-set
- match.ml
  - Perform graph pattern matching
- pushatom.ml
  - Generate and push atoms
- eval.ml
  - The one step reducer

### Repl
- main.ml
  - File loader and the main execution loop

