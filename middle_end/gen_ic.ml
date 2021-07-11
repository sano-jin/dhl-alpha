(** gen_ic.ml *)

open Front_end
open Util
open Register_table
include Instruction



(** ルールから中間命令列を生成する
    - 戻り値は，生成した中間命令列と必要になるレジスタの数
 *)
let gen_ic_of_rule
      (BRule ((lhs_local_indegs, lhs_atoms, lhs_free_non_incidences)
	     , free_indeg_diffs
	     , (rhs_local_indegs, ((_, _, redirs) as rhs)))) =

  let env, lhs_insts =
    Match.match_ lhs_free_non_incidences (redirs, free_indeg_diffs) lhs_local_indegs lhs_atoms in
  debug_print "genereted lhs_insts" @@ string_of_lhs_insts lhs_insts;
  
  let reg_length, rhs_insts =
    Pushout.push_atoms env rhs_local_indegs rhs in
  debug_print "genereted rhs_insts" @@ string_of_rhs_insts rhs_insts;  
  debug_print "register_length" @@ string_of_int reg_length;

  reg_length, (lhs_insts, rhs_insts)
						  


(** ルールセットから中間命令列とレジスタの数のタプルのリストを生成する *)
let gen_ic_of_rules = List.map gen_ic_of_rule



(** 初期状態を生成するための中間命令と必要なレジスタの数を返す *)
let gen_ic_of_init = Pushout.push_atoms empty_env 

		      
