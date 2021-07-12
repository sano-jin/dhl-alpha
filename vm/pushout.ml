(** pushatom.ml *)

open Gen_ic
open Util
open Vm



(** ルール右辺の命令を一回実行して，更新したアトムリストを返す *)
let pushout register atom_list =
  (* node_ref をアトムリストから除去する（メモリ解放はしない） *)
  let remove node_ref = List.filter ((!=) node_ref) atom_list in

  function
  | PushAtom (reg_i, indeg, functor_) ->
     (* ルール右辺で生成する入次数 [indeg]，ファンクタ [functor_] のシンボルアトムを生成し，
	レジスタ [reg_i] に代入して，アトムリストへ追加する
      *)
     let (p, arity) = functor_ in
     let node_ref = ref (indeg, VMAtom (p, Array.make arity null_ptr)) in
     register.(reg_i) <- node_ref;
     node_ref::atom_list
     
  | ReplaceAtom (reg_i, functor_) ->
     (* ファンクタ [functor_] の（シンボル）アトムで，レジスタ [reg_i] が参照する先のアトムを置き換える *)
     let node_ref = register.(reg_i) in
     let indeg = fst !node_ref in
     let (p, arity) = functor_ in
     node_ref := (indeg, VMAtom (p, Array.make arity null_ptr));
     atom_list
    
  | Redir (reg_i, reg_j) -> 
  (* [Redir reg_i reg_j] はレジスタ [reg_i] が参照する先のアトムを
     レジスタ [reg_j] が参照する先のアトムを参照する Indirection アトムで置き換え，
     アトムリストから [reg_i] を参照していた要素を除去する
      - 入次数はマッチング末尾の CheckRedirs 命令によってすでにセット済み
      - 入次数がゼロになっている場合はメモリを解放する
   *)
     let node_ref = register.(reg_i) in
     (* アトムリストからこの参照を除去 *)
     let atom_list = remove node_ref in 
     let indeg = fst !node_ref in
     ( if indeg = 0 then free_atom node_ref (* メモリ解放 *)
       else 
	 let node_ref_to = register.(reg_j) in
	 node_ref := (indeg, VMInd (ref node_ref_to))
     );
     atom_list
     
  | FreeAtom reg_i -> 
     (* レジスタ [reg_i] が参照する先のアトムをアトムリストから除去し，メモリを解放する *)
     let node_ref = register.(reg_i) in
     (* アトムリストからこの参照を除去 *)
     let atom_list = remove node_ref in 
     free_atom node_ref; (* メモリ解放 *)
     atom_list
       
  | SetLink (src_reg_i, port_i, dst_reg_i) ->
  (* レジスタ [dst_reg_i] が参照する先の（シンボル）アトムの [port_i] 番目のリンクに
     レジスタ [src_reg_j] に格納されたアドレスを代入する
   *)
     let (_, xs) = deref_symbol_atom register.(src_reg_i) in
     xs.(port_i) <- register.(dst_reg_i);
     atom_list
			       
  | FailPushout message -> failwith message
     (* 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)


(** ルール右辺の命令を実行する
    - アトムリストを受け取り，更新したアトムリストを返す
 *)
let pushouts = List.fold_left <. pushout

				   
