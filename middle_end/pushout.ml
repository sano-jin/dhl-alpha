(** pushatom.ml *)

open Front_end
open Util
open Register_table
open Instruction
       
let functor_of = second List.length

			
(** 局所リンクに参照されるアトムを生成する命令を生成する *)
let push_local_atom local_indegs env (x, p_xs) =
  let reg_i, env = get_free_reg_i env in
  let indeg = List.assoc x local_indegs in
  env, ((x, reg_i), PushAtom (reg_i, indeg, functor_of p_xs))


(** 局所リンクに参照されるアトムを生成する命令のリストを生成する 
   - 戻り値は [env, (local2reg_i, push_atoms)]
     - ただし，[local2reg_i] は局所リンク名とレジスタ番号の連想リスト
     - [push_atoms] は PushAtom 命令のリスト
 *)
let push_local_atoms = second List.split <... List.fold_left_map <. push_local_atom



(** 自由リンクに参照されるアトムを置き換える命令を生成する *)
let replace_free_atom free2reg_i (x, p_xs) =
  let reg_i = List.assoc x free2reg_i in
  ReplaceAtom (reg_i, functor_of p_xs)


(** 自由リンクに参照されるアトムを置き換える命令のリストを生成する *)
let replace_free_atoms = List.map <. replace_free_atom 

	      
								 
(** リンクが参照するアドレスが格納されたレジスタ番号を取得する 
    - [free2reg_i] はマッチング終了後の [env.free2reg_i]
    - [local2reg_i] は [push_local_atoms] によって得られた連想リスト
      - [env.local2reg_i] ではない
*)								 
let get_arg (free2reg_i, local2reg_i) = function
  | BFreeLink x -> List.assoc x free2reg_i
  | BLocalLink x -> List.assoc x local2reg_i


let set_link link2reg_idxs src_reg_i (port_i, x) =
  let dst_reg_i = get_arg link2reg_idxs x in
  SetLink (src_reg_i, port_i, dst_reg_i)



(** 局所リンク・自由リンクに参照される（シンボル）アトムのリンクをセットする命令のリストを生成する 
    - [link2reg_i] は局所リンクに参照されるアトムを生成する場合は [local2reg_i]，
      自由リンクに参照されるアトムを生成する場合は [free2reg_i]
*)
let set_links_of_atom link2reg_i link2reg_idxs (x, (_, xs)) =
  let src_reg_i = List.assoc x link2reg_i in 
  List.map (set_link link2reg_idxs src_reg_i) @@ List.mapi pair xs

(* link2reg_i は（恐らくは）単相性制限のために eta 変換できなかった *)  
let set_links_of_atoms link2reg_i = List.concat_map <. set_links_of_atom link2reg_i
							   


(** ルール左辺で局所に参照されていたアトムを消去する命令を生成する *)
let free_local_atom reg_i = FreeAtom reg_i

(** ルール左辺で局所に参照されていたアトムを消去する命令のリストを生成する *)
let free_local_atoms = List.map free_local_atom



(** リダイレクトを行う命令を生成する *)
let redir_of free2reg_i (x, y) =
  Redir (List.assoc x free2reg_i, List.assoc y free2reg_i)

(** リダイレクトを行う命令のリストを生成する *)
let redirs_of = List.map <. redir_of



(** 確保すべきレジスタのサイズと生成した命令列を返す *)			       
let push_atoms env local_indegs (local_inds, free_inds, redirs)  =
  let free_atoms = free_local_atoms @@ List.map snd env.local2reg_i in
  
  let env, (local2reg_i, push_atoms) = push_local_atoms local_indegs env local_inds in

  let replace_atoms = replace_free_atoms env.free2reg_i free_inds in
  
  let link2reg_idxs = (env.free2reg_i, local2reg_i) in
  let set_links_of_free_atoms  = set_links_of_atoms env.free2reg_i link2reg_idxs free_inds in
  let set_links_of_local_atoms = set_links_of_atoms local2reg_i    link2reg_idxs local_inds in

  let redirs = redirs_of env.free2reg_i redirs in
  
  env.free_reg_i (* 確保すべきレジスタのサイズ *)
  , List.concat [free_atoms; push_atoms; replace_atoms; set_links_of_local_atoms; set_links_of_free_atoms; redirs]
		
  
