(** eval.ml *)

open Breakdown
open Util
open Vm
       
(** Try to reduce one step with the given atoms and a rule *)				      
let reduce atom_list
	   (BRule ((lhs_local_indegs, lhs_atoms)
		  , free_indeg_diffs
		  , (rhs_local_indegs, ((_, _, redirs) as rhs)))) =
  let+ env = Match.match_ (redirs, free_indeg_diffs) lhs_local_indegs atom_list lhs_atoms in
  (*  print_endline "matched"; *)
  let local_addrs = List.map snd env.local2addr in
  let redirected_addrs = List.map (flip List.assoc env.free2addr) @@ List.map fst redirs in
  let atom_list = set_minus_q atom_list @@ local_addrs@redirected_addrs in
  List.iter free_atom local_addrs;
  Pushout.push_atoms rhs_local_indegs env.free2addr rhs @ atom_list

(** Try reduce one step with the given atoms and rules *)
let run_once = one_of <. reduce

(** push the initial graph and return their references *)			   
let init_atoms local_indegs inds =
    Pushout.push_atoms local_indegs [] inds
		      
