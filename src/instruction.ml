(** instruction.ml *)

open Vm

(** ルール左辺におけるマッチングのための中間命令 *)
type lhs_inst =
  | PeakAtom of reg_i * functor_
  (** アトムリストの先頭から随時アトムへの参照をレジスタ reg_i に格納してゆく 
      - まだアトムリストはファンクタで分類されていないので，とりあえずは functor_ は必要ない
      - failable and possibly rewind
   *)

  | CheckFunctor of reg_i * functor_
  (** reg_i に格納したアトムのファンクタが functor_ であることを確認する
      - failable and does not rewind
   *)
		 
  | CheckIndeg of reg_i * int
  (** reg_i に格納したアトムの参照カウンタの値が int であることを確認する
      - 局所リンクに必要なチェック
      - failable and does not rewind
   *)
	       
  | Deref of reg_i * reg_i * int
  (** [Deref dst_reg_i src_reg_i port_i] は レジスタ [src_reg_i] が参照するアトムの 
      [port_i] 番目の引数をトラバースしてその先に接続されているシンボルアトムへの参照を
      レジスタ [dst_reg_i] に格納する
      - not failable and does not rewind
   *)

  | CheckRefEq of reg_i * reg_i
  (** [CheckRefEq reg_i reg_j] は レジスタ [reg_i] に格納されているアドレスと
      レジスタ [reg_j] に格納されているアドレスが等しいことを確認する
      - failable and does not rewind
   *)

  | CheckRefNeq of reg_i * reg_i
  (** [CheckRefEq reg_i reg_j] は レジスタ [reg_i] に格納されているアドレスと
      レジスタ [reg_j] に格納されているアドレスが異なることを確認する
      - failable and does not rewind
   *)

  | CheckRedirs of (reg_i * reg_i) list * (reg_i * int) list
  (** CheckRedirs redirs free_indeg_diffs
      - 引数は
        - リダイレクトされる自由リンクが格納されたレジスタ番号の連想リストと，
        - 自由リンクが格納されたレジスタと，対応する自由リンクの入次数の変化分の連想リスト
      - もっと細かい命令に分けるべきな気はする
      - failable and does not rewind
   *)
		

type lhs_insts = lhs_inst list
		
