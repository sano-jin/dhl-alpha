(** register_table.ml *)

open Instruction

(** An environment for the matching and pushout *)
type env = {
  matched_atoms: (functor_ * reg_i) list;
  (** all the indices of registers which store the address of the matched atoms on lhs *)
  
  local2reg_i: (int * reg_i) list;
  (** 局所リンクがマッチしたアドレスを格納しているレジスタ番号の連想リスト *)
  
  free2reg_i: (string * reg_i) list;
  (** 自由リンクがマッチしたアドレスを格納しているレジスタ番号の連想リスト *)

  free_reg_i: int;
  (** まだ使っていないレジスタ番号の最小値（= レジスタの数） *)
}

let empty_env =
  {matched_atoms = []; local2reg_i = []; free2reg_i = []; free_reg_i = 0}	     

let get_free_reg_i env =
  env.free_reg_i, {env with free_reg_i = succ env.free_reg_i}
    
