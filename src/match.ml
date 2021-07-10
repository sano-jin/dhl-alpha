(** match.ml  *)

open Breakdown
open Util
open Vm

let check_arg src_reg_i env (x, port_i) =
  let dst_reg_i, env = get_free_reg_i env in
  let deref = Deref dst_reg_i src_reg_i port_i in	      
  let check_ref_neq_of reg_i = CheckRefNeq (dst_reg_i, reg_i) in
  function
  | BFreeLink x ->
       begin
	 match List.assoc_opt x env.free2reg_i with
	 | None ->
	    let check_ref_neqs = List.map check_ref_neq_of env.local2reg_i in 
	    let env = {env with free2reg_i = (x, reg_i)::env.free2reg_i} in
	    env, deref::check_ref_neqs
	 | Some reg_i ->
	    (* check if the free link name has already mathced to the address  *)
	    let check_ref_eq = CheckRefEq (dst_reg_i, reg_i) in
	    env, [deref; check_ref_eq]
       end
  | BLocalLink x ->
     begin
       match List.assoc_opt x env.local2reg_i with 
       | None ->
	  let check_ref_neqs = List.map check_ref_neq_of env.free2reg_i in
	  let env = {env with local2reg_i = (x, reg_i)::env.local2reg_i} in
	  env, deref::check_ref_neqs
       | Some addr ->
	  let check_ref_eq = CheckRefEq (dst_reg_i, reg_i) in
	  env, [deref; check_ref_eq]
     end
       
let functor_of (p, xs) = (p, List.length xs)

let check_atom (p, xs) env reg_i =
  let functor_ = (p, List.length xs) in
  let check_functor = CheckFunctor (reg_i, functor_) in
  let check_ref_neqs =
      let matched_atom_reg_idxs = List.filter ((=) functor_ <. fst) env.matched_atoms in
      let check_ref_neq_of reg_j = CheckRefEq (reg_i, reg_j) in
      List.map check_ref_neq_of matched_atom_reg_idxs
  in
  let env = {env with matched_atoms = (functor_, reg_i)::env.matched_atoms} in
  let xs = List.mapi pair xs in
  let env, insts = List.fold_map_left check_arg reg_i env xs in
  env, check_functor::check_ref_neqs@insts
		      
	      
(* node_ref must be pointing at a VMAtom not VMInd *)
let check_ind local_indegs env reg_i = function
  | BLocalInd (x, (p, xs)) ->
     let check_indeg =
       CheckIndeg (reg_i, List.assoc x local_indegs)
     let env, insts = 
       check_atom
	 (p, xs)

	 (* if x is the key of `addr` in `env.free2addr`, then the `addr` should be equal to `node_ref`.
	    Since is the `x` is in the `env.free2addr`, then we should have conducted dereference hence
	    the `node_ref` is lookuped from the `env.free2addr` in the former phase (`try_deref` in `find_atoms`).        
	  *)
	 {env with local2addr = insert x reg_i env.local2reg_i}
	 reg_i
     in env, check_indeg::insts
  | BFreeInd (x, (p, xs)) ->
     (* no indeg checking for a free link *)
     check_atom
       (p, xs)  
       {env with free2addr = insert x reg_i env.free2reg_i}
       reg_i 
  | _ -> failwith @@ "Indirection on LHS is not supported"



(*
let functor_of = function
  | (BLocalInd (_, (p, xs)) | BFreeInd  (_, (p, xs))) -> (p, List.length xs)
  | BRedir  -> failwith @@ "Indirection on LHS is not supported"
 *)		       

(** （残りの）ルール左辺の情報と env を受け取って，更新した env と 生成した中間命令列を返す *)
let rec find_atom local_indegs env =
  let try_deref x link2addr ind t =
    match List.assoc_opt x link2reg_i with
    | None -> (* Could not dereference. has not matched yet. *)
       (* アトムリストの先頭から随時アトムへの参照をレジスタ reg_i に格納する命令を発行する *)
       let reg_i, env = get_free_reg_i env in
       let peak_atom = PeakAtom (reg_i, functor_) in
       let env, insts = check_ind local_indegs env reg_i ind  in
       env, peak_atom::insts
    | Some reg_i ->
       (* Was able to dereference with already known reference.
	  In this case, the node_ref is guaranteeded to point an atom but an indirection atom.
	  Since we have traversed indirection in the former process.
	*)
       (* レジスタにすでに格納されている *)
       env, insts = check_ind local_indegs env reg_i ind 
  in	 
  function
  | BLocalInd (x, _) as ind -> try_deref x env.local2reg_i ind t
  | BFreeInd  (x, _) as ind -> try_deref x env.free2reg_i ind t
  | _ -> failwith @@ "Indirection on LHS is not supported"
(*
  | [] -> Redir.check_redirs free_link_info env
*)

let find_atoms = List.fold_map_left <. find_atom 		       
		       
let match_ = find_atoms empty_env 
