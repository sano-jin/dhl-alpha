(** Instructions of the intermediate code *)

open Util



(** レジスタ番号 *)       
type reg_i = int


(** ファンクタ := (アトム名, リンクの数) *)
type functor_ = string * int



(** ルール左辺におけるマッチングのための中間命令
    - failable は失敗する可能性があるということ
    - rewind は後続の命令が失敗したときに巻き戻しの起点になるということ
 *)
type lhs_inst =
  | PeakAtom of reg_i * functor_
  (** アトムリストの先頭から随時，ファンクタが [functor_] であるアトムへの参照を，レジスタ [reg_i] に格納してゆく 
      - failable and possibly rewind
   *)

  | CheckFunctor of reg_i * functor_
  (** [reg_i] に格納したアトムのファンクタが [functor_] であることを確認する
      - failable and does not rewind
   *)
		 
  | CheckIndeg of reg_i * int
  (** [reg_i] に格納したアトムの参照カウンタの値が [int] であることを確認する
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
      - 自由リンクの入次数もセットする
      - もっと細かい命令に分けるべきな気はする
      - failable and does not rewind
   *)
							
  | FailMatching of string (** 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)
							
							
type lhs_insts = lhs_inst list

		

(** ルール右辺におけるマッチングのための中間命令．
    どれも失敗することはない．
 *)
type rhs_inst =
  | PushAtom of reg_i * int * functor_
  (** [PushAtom reg_i indeg functor_] はルール右辺で生成する入次数 [indeg]，ファンクタ [functor_] の（シンボル）アトムを生成し，レジスタ [reg_i] に代入する
      - アトムリストへの追加も行う
      - ただし，リンクの値は正しい値にセットされない
      - 局所リンクに参照されるアトムを生成するための命令
      - シンボルアトムのみ対応（Indirection アトムは局所リンクに参照されないため）
   *)
				
  | ReplaceAtom of reg_i * functor_
  (** [ReplaceAtom reg_i functor_] はファンクタ [functor_] の（シンボル）アトムで，レジスタ [reg_i] が参照する先のアトムを置き換える
      - アトムリストがファンクタごとに分類されているのであれば，ファンクタが異なる場合はアトムリスト間を移動する必要がある
        - 効率のためには異なるファンクタで置き換える場合と，同一のファンクタの場合とで異なる命令を出したほうが良いと思われる
	- 現状はアトムリストは一本のリストなので，とりあえずこれだけでいく
      - ただし，リンクの値は正しい値にセットされない
      - 入次数はマッチング末尾の CheckRedirs 命令によってすでにセット済み
      - 自由リンクに参照されるアトムを置き換えるための命令
      - シンボルアトムのみ対応
   *)
			     
  | Redir of reg_i * reg_i
  (** [Redir reg_i reg_j] はレジスタ [reg_i] が参照する先のアトムをレジスタ [reg_j] が参照する先のアトムを参照する Indirection アトムで置き換え，
      アトムリストから [reg_i] を参照していた要素を除去する
      - リンクの値も正しい値にセットされる
      - 入次数はマッチング末尾の CheckRedirs 命令によってすでにセット済み
        - 入次数がゼロになっている場合はメモリを解放する
      - 自由リンクに参照されるアトムを置き換えるための命令
      - Indirection アトムのみ対応
      - CheckRedirs ですでにセットしてしまうのでも良いかもしれない
   *)

  | FreeAtom of reg_i
  (** [FreeAtom reg_i] はレジスタ [reg_i] が参照する先のアトムをアトムリストから除去し，メモリを解放する
      - ただし，リンクの値は正しい値にセットされない <-- ???
      - 入次数はマッチング末尾の CheckRedirs 命令によってすでにセット済み
      - 自由リンクに参照されるアトムを置き換えるための命令
      - シンボルアトムのみ対応
   *)
		  
  | SetLink of reg_i * int * reg_i
  (** [SetLink src_reg_i port_i dst_reg_i] はレジスタ [dst_reg_i] が参照する先の（シンボル）アトムの [port_i] 番目のリンクにレジスタ [src_reg_j] に格納されたアドレスを代入する
      - シンボルアトムのみ対応
   *)
			       
  | FailPushout of string (** 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)

		     
type rhs_insts = rhs_inst list



(** 中間コードを文字列へ変換する
    - 中間命令列のファイルを生成するため
      - CheckRedir 命令はこのままだと構文解析が若干面倒なので，後で変更が必要
    - とデバッグ時の dump のため
 *)

(** *)
let string_of_functor (p, arity) = Printf.sprintf "'%s'_%d" p arity

let string_of_check_redirs redirs free_indeg_diffs =
  let string_of_assoc =
    let string_of_tuple (x, y) = Printf.sprintf "%d %d" x y in
    String.concat "\n" <. List.map (indent 3 <. string_of_tuple)
  in
  let redirs = string_of_assoc redirs in
  let free_indeg_diffs = string_of_assoc free_indeg_diffs in
  String.concat "\n" [ "CheckRedirs  ["; indent 2 "["; redirs; indent 2 "] ["; free_indeg_diffs; indent 2 "]"; indent 1 "]"]
									    
						  
let string_of_lhs_inst = function
  | PeakAtom     (reg_i, functor_)		-> Printf.sprintf "PeakAtom     [ %d %s ]"    reg_i @@ string_of_functor functor_
  | CheckFunctor (reg_i, functor_)		-> Printf.sprintf "CheckFunctor [ %d %s ]"    reg_i @@ string_of_functor functor_
  | CheckIndeg   (reg_i, indeg)			-> Printf.sprintf "CheckIndeg   [ %d %d ]"    reg_i indeg
  | Deref        (dst_reg_i, src_reg_i, port_i)	-> Printf.sprintf "Deref        [ %d %d %d ]" dst_reg_i src_reg_i port_i
  | CheckRefEq   (reg_i, reg_j)			-> Printf.sprintf "CheckRefEq   [ %d %d ]"    reg_i reg_j
  | CheckRefNeq  (reg_i, reg_j)			-> Printf.sprintf "CheckRefNeq  [ %d %d ]"    reg_i reg_j
  | CheckRedirs  (redirs, free_indeg_diffs)	-> string_of_check_redirs redirs free_indeg_diffs
  | FailMatching message			-> Printf.sprintf "FailMatching [ %s ]" message
							 
let string_of_rhs_inst = function
  | PushAtom     (reg_i, indeg, functor_)	-> Printf.sprintf "PushAtom    [ %d %d %s ]"  reg_i indeg @@ string_of_functor functor_
  | ReplaceAtom  (reg_i, functor_)		-> Printf.sprintf "ReplaceAtom [ %d %s ]"     reg_i @@ string_of_functor functor_
  | Redir        (reg_i, reg_j)			-> Printf.sprintf "Redir       [ %d %d ]"  reg_i reg_j
  | FreeAtom     reg_i				-> Printf.sprintf "FreeAtom    [ %d ]"     reg_i
  | SetLink      (src_reg_i, port_i, dst_reg_i)	-> Printf.sprintf "SetLink     [ %d %d %d ]"  src_reg_i port_i dst_reg_i
  | FailPushout  message			-> Printf.sprintf "FailPushout [ %s ]"  message

								  
let string_of_lhs_insts = String.concat "\n" <. List.map (indent 1 <. string_of_lhs_inst)
let string_of_rhs_insts = String.concat "\n" <. List.map (indent 1 <. string_of_rhs_inst)


			       
									
		     
