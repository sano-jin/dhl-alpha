(** test_compiler.ml *)

open Front_end
open Gen_ic
open Util


(** ルールをコンパイルした結果を出力する *)
let dump_rule i (reg_size, (lhs_insts, rhs_insts)) =
  prerr_newline ();
  Printf.eprintf ">>>> rule #%d <<<<\n" i;
  debug_print "register_size" @@ string_of_int reg_size;
  debug_print "genereted lhs_insts" @@ string_of_lhs_insts lhs_insts;
  debug_print "genereted rhs_insts" @@ string_of_rhs_insts rhs_insts


  
let dump_init_rule (reg_size, insts) =
  prerr_newline ();
  prerr_endline ">>>> rule for constructing the initial state <<<<";
  debug_print "register_size" @@ string_of_int reg_size;
  debug_print "genereted insts" @@ string_of_rhs_insts insts


       
let test_compiler prog =
  let init_insts, rules = uncurry gen_ic @@ front_end prog in
  dump_init_rule init_insts;
  List.iteri dump_rule rules
  

