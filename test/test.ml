(** test.ml *)

open Test_compiler

let () =
  print_endline "hello world!";
  test_compiler
    "append(cons(a, cons(b, nil)), cons(c, nil)). 
     R -> append(cons(H, T), L) :- R -> cons(H, append(T, L)). 
     R -> append(nil, L) :- R -> L"
