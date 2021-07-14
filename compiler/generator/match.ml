(** match.ml *)

open Analyzer
open Util
open Register_table
open Instruction


       
(** アトムの引数のリンクのマッチングを行う命令を生成する *)
let check_arg src_reg_i reg_tbl port_i =
  let dst_reg_i, reg_tbl = get_free_reg_i reg_tbl in
  let deref = Deref (dst_reg_i, src_reg_i, port_i) in	      
  function
  | BFreeLink x ->
     ( match List.assoc_opt x reg_tbl.free2reg_i with
       | None -> {reg_tbl with free2reg_i = (x, dst_reg_i)::reg_tbl.free2reg_i}, [deref]
       | Some reg_i -> reg_tbl, [deref; CheckRefEq (dst_reg_i, reg_i)]
     )
  | BLocalLink x ->
     ( match List.assoc_opt x reg_tbl.local2reg_i with 
       | None -> {reg_tbl with local2reg_i = (x, dst_reg_i)::reg_tbl.local2reg_i}, [deref]
       | Some reg_i -> reg_tbl, [deref; CheckRefEq (dst_reg_i, reg_i)]
     )



(** リンクの非単射的マッチングを行うための命令 *)       
let check_ref_neq_of reg_i reg_j = CheckRefNeq (reg_i, reg_j)



(** アトムのマッチングを行う命令を生成する *)
let check_atom (p, xs) reg_tbl reg_i =
  let functor_ = (p, List.length xs) in
  let check_ref_neqs =
      let matched_atoms = List.filter ((=) functor_ <. fst) reg_tbl.matched_atoms in
      List.map (check_ref_neq_of reg_i <. snd) matched_atoms
  in
  let reg_tbl = {reg_tbl with matched_atoms = (functor_, reg_i)::reg_tbl.matched_atoms} in
  let xs = List.mapi pair xs in
  let reg_tbl, check_args = List.fold_left_map (uncurry <. check_arg reg_i) reg_tbl xs in
  reg_tbl, List.concat @@ check_ref_neqs::check_args
		      


(** インダイレクションのマッチングを行う命令を生成する *)
let check_ind local_indegs reg_tbl reg_i = function
  | BLocalInd (x, (p, xs)) ->
     let check_indeg = CheckIndeg (reg_i, List.assoc x local_indegs) in
     let reg_tbl, insts = 
       check_atom (p, xs) {reg_tbl with local2reg_i = insert x reg_i reg_tbl.local2reg_i} reg_i
	 (* if [x] is the key of [addr] in [reg_tbl.free2addr], then the [addr] should be equal to [node_ref].
	    Since is the [x] is in the [reg_tbl.free2addr], then we should have conducted dereference hence
	    the [node_ref] is lookuped from the [reg_tbl.free2addr] in the former phase ([try_deref] in [find_atoms]).        
	  *)
     in reg_tbl, check_indeg::insts
  | BFreeInd (x, (p, xs)) -> check_atom (p, xs)  {reg_tbl with free2reg_i = insert x reg_i reg_tbl.free2reg_i} reg_i 
  | _ -> failwith @@ "Indirection on LHS is not supported"


(** ルール左辺のインダイレクションからファンクタを取得する *)
let functor_of = function
  | (BLocalInd (_, (p, xs)) | BFreeInd  (_, (p, xs))) -> (p, List.length xs)
  | BRedir (_, _) -> failwith @@ "Indirection on LHS is not supported"


			     
(** reg_tbl を受け取って，更新した reg_tbl と 生成した中間命令列を返す *)
let find_atom local_indegs reg_tbl =
  let try_deref x link2reg_i ind =
    match List.assoc_opt x link2reg_i with
    | None -> (* Could not dereference. has not matched yet. *)
       (* アトムリストの先頭から随時アトムへの参照をレジスタ reg_i に格納する命令を発行する *)
       let reg_i, reg_tbl = get_free_reg_i reg_tbl in
       let peak_atom = PeakAtom (reg_i, functor_of ind) in
       let reg_tbl, insts = check_ind local_indegs reg_tbl reg_i ind  in
       reg_tbl, peak_atom::insts
    | Some reg_i ->
       (* レジスタにすでに格納されているリンクからの参照の場合 *)
       let check_functor = CheckFunctor (reg_i, functor_of ind) in
       let reg_tbl, insts = check_ind local_indegs reg_tbl reg_i ind in	
       reg_tbl, check_functor::insts 
  in	 
  function
  | BLocalInd (x, _) as ind -> try_deref x reg_tbl.local2reg_i ind
  | BFreeInd  (x, _) as ind -> try_deref x reg_tbl.free2reg_i ind
  | _ -> failwith @@ "Indirection on LHS is not supported"


(** 引数に local_indegs reg_tbl inds をとる *)
let find_atoms = second List.concat <... List.fold_left_map <. find_atom


(** ルール左辺を中間命令列に変換する *)
let match_ (lhs_local_indegs, lhs_atoms) (redirs, free_indeg_diffs) =
  let reg_tbl, insts = find_atoms lhs_local_indegs empty_reg_tbl lhs_atoms in
  let reg_i_of x = List.assoc x reg_tbl.free2reg_i in
  let check_redirs =
    (* 不正なリダイレクションを行っていないのかのチェックを行う *)
    let redirs = List.map (first reg_i_of <. second reg_i_of) redirs in
    let free_indeg_diffs = List.map (first reg_i_of) free_indeg_diffs in
    CheckRedirs (redirs, free_indeg_diffs)
  in
  reg_tbl, insts @ [check_redirs]

  
  
