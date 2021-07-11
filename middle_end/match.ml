(** match.ml *)

open Front_end
open Util
open Register_table
open Instruction


       
(** アトムの引数のリンクのマッチングを行う命令を生成する *)
let check_arg src_reg_i env port_i =
  let dst_reg_i, env = get_free_reg_i env in
  let deref = Deref (dst_reg_i, src_reg_i, port_i) in	      
  function
  | BFreeLink x ->
     ( match List.assoc_opt x env.free2reg_i with
       | None -> {env with free2reg_i = (x, dst_reg_i)::env.free2reg_i}, [deref]
       | Some reg_i -> env, [deref; CheckRefEq (dst_reg_i, reg_i)]
     )
  | BLocalLink x ->
     ( match List.assoc_opt x env.local2reg_i with 
       | None -> {env with local2reg_i = (x, dst_reg_i)::env.local2reg_i}, [deref]
       | Some reg_i -> env, [deref; CheckRefEq (dst_reg_i, reg_i)]
     )



(** リンクの非単射的マッチングを行うための命令 *)       
let check_ref_neq_of reg_i reg_j = CheckRefNeq (reg_i, reg_j)



(** アトムのマッチングを行う命令を生成する *)
let check_atom (p, xs) env reg_i =
  let functor_ = (p, List.length xs) in
  let check_functor = CheckFunctor (reg_i, functor_) in
  let check_ref_neqs =
      let matched_atoms = List.filter ((=) functor_ <. fst) env.matched_atoms in
      List.map (check_ref_neq_of reg_i <. snd) matched_atoms
  in
  let env = {env with matched_atoms = (functor_, reg_i)::env.matched_atoms} in
  let xs = List.mapi pair xs in
  let env, check_args = List.fold_left_map (uncurry <. check_arg reg_i) env xs in
  env, List.concat @@ (check_functor::check_ref_neqs)::check_args
		      


(** インダイレクションのマッチングを行う命令を生成する *)
let check_ind local_indegs env reg_i = function
  | BLocalInd (x, (p, xs)) ->
     let check_indeg = CheckIndeg (reg_i, List.assoc x local_indegs) in
     let env, insts = 
       check_atom (p, xs) {env with local2reg_i = insert x reg_i env.local2reg_i} reg_i
	 (* if x is the key of `addr` in `env.free2addr`, then the `addr` should be equal to `node_ref`.
	    Since is the `x` is in the `env.free2addr`, then we should have conducted dereference hence
	    the `node_ref` is lookuped from the `env.free2addr` in the former phase (`try_deref` in `find_atoms`).        
	  *)
     in env, check_indeg::insts
  | BFreeInd (x, (p, xs)) -> check_atom (p, xs)  {env with free2reg_i = insert x reg_i env.free2reg_i} reg_i 
  | _ -> failwith @@ "Indirection on LHS is not supported"


(** ルール左辺のインダイレクションからファンクタを取得する *)
let functor_of = function
  | (BLocalInd (_, (p, xs)) | BFreeInd  (_, (p, xs))) -> (p, List.length xs)
  | BRedir (_, _) -> failwith @@ "Indirection on LHS is not supported"


			     
(** env を受け取って，更新した env と 生成した中間命令列を返す *)
let find_atom local_indegs env =
  let try_deref x link2reg_i ind =
    match List.assoc_opt x link2reg_i with
    | None -> (* Could not dereference. has not matched yet. *)
       (* アトムリストの先頭から随時アトムへの参照をレジスタ reg_i に格納する命令を発行する *)
       let reg_i, env = get_free_reg_i env in
       let peak_atom = PeakAtom (reg_i, functor_of ind) in
       let env, insts = check_ind local_indegs env reg_i ind  in
       env, peak_atom::insts
    | Some reg_i ->
       (* レジスタにすでに格納されているリンクからの参照の場合 *)
       check_ind local_indegs env reg_i ind 
  in	 
  function
  | BLocalInd (x, _) as ind -> try_deref x env.local2reg_i ind
  | BFreeInd  (x, _) as ind -> try_deref x env.free2reg_i ind
  | _ -> failwith @@ "Indirection on LHS is not supported"


(** 引数に local_indegs env inds をとる *)
let find_atoms = second List.concat <... List.fold_left_map <. find_atom


(** ルール左辺を中間命令列に変換する *)
let match_ lhs_free_non_incidences (redirs, free_indeg_diffs) lhs_local_indegs lhs_atoms =
  let env, insts = find_atoms lhs_local_indegs empty_env lhs_atoms in
  let reg_i_of x = List.assoc x env.free2reg_i in
  let check_non_injects =
    (* ルール左辺でアトムを参照していない自由リンクが局所リンクと非単射的マッチングをしていないかチェックする *)
    let check_ref_neq_of reg_i reg_j = CheckRefNeq (reg_i, reg_j) in
    let check_non_inject_of reg_i = List.map (check_ref_neq_of reg_i <. snd) env.local2reg_i in
    List.concat_map (check_non_inject_of <. reg_i_of) lhs_free_non_incidences
  in
  let check_redirs =
    (* 不正なリダイレクションを行っていないのかのチェックを行う *)
    let redirs = List.map (first reg_i_of <. second reg_i_of) redirs in
    let free_indeg_diffs = List.map (first reg_i_of) free_indeg_diffs in
    CheckRedirs (redirs, free_indeg_diffs)
  in
  env, List.concat [insts; check_non_injects; [check_redirs]]

  
  
