(** eval.ml *)

open Front_end
open Gen_ic
open Util
open Vm
open Match
open Pushout


       
(** Try to reduce one step with the given atoms and a rule *)				      
let reduce atom_list (reg_size, (lhs_insts, rhs_insts)) =
  let register = init_register size in
  
  let+ env = match_ (redirs, free_indeg_diffs) lhs_local_indegs atom_list lhs_atoms in
  (*  print_endline "matched"; *)
  let local_addrs = List.map snd env.local2addr in
  let redirected_addrs = List.map (flip List.assoc env.free2addr) @@ List.map fst redirs in
  let atom_list = set_minus_q atom_list @@ local_addrs@redirected_addrs in
  List.iter free_atom local_addrs;
  push_atoms rhs_local_indegs env.free2addr rhs @ atom_list



(** Try reduce one step with the given atoms and rules *)
let run_once = one_of <. reduce



(** push the initial graph and return their references *)			   
let init_atoms local_indegs inds =
    push_atoms local_indegs [] inds
		      
