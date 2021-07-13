(** Breakdown argument atoms on right hand side of indirections.
    Also check rule condition and reconstruct rule.
*)

open Util
open Alpha
open Link_check


(** リンク：アトムの引数部分に埋め込まれたアトムは全て分解され，アトムの引数はリンクのみとなる *)
type b_arg =
  | BFreeLink of string  (** 自由リンクは文字列のまま扱う *)
  | BLocalLink of int    (** 局所変数は全て一意な整数に変換される *)


(** アトムはアトム名とリンクのリスト *)
type b_atom = string * b_arg list


type b_ind =
  | BLocalInd of int * b_atom     (** 局所リンクによって指されるアトム *)
  | BFreeInd of string * b_atom   (** 自由リンクによって指されるアトム *)
  | BRedir of string * string     (** 自由リンクのリダイレクト *)



(** [(local_indegs, b_ind list)] *)
type lhs = (int * int) list * b_ind list


(** ルール左辺・右辺における自由リンクの入次数の差分
    - ただし，indirection アトムからの参照数は数えないものとする
 *)
type free_indeg_diffs = (string * int) list

				    
(** [(local_inds, free_inds, redirs)] *)
type rhs_graph = (int * b_atom) list * (string * b_atom) list * (string * string) list 


(** [(local_indegs, rhs_graph)] *)
type rhs = (int * int) list * rhs_graph


type b_rule = BRule of lhs * free_indeg_diffs * rhs



(** Breakdown argument atoms *)										 
let rec breakdown_arg env = function
  | AFreeLink x  -> (env, (BFreeLink x, []))
  | ALocalLink (x, _) -> (env, (BLocalLink x, []))
  | AAtom (p, xs) ->
     let ((link_id, local_indegs), (xs, inds)) = breakdown_args env xs in
     (succ link_id, (link_id, 1)::local_indegs),
      (BLocalLink link_id, BLocalInd (link_id, (p, xs))::inds)
and breakdown_args env =
  second (second List.concat <. List.split) <. List.fold_left_map breakdown_arg env



(** Breakdown argument atoms on right hand side of indirection. 
    Indirection from/to local link is not allowed (should be resolved in the former phase, 
    which is not implemented) and raise an error [Redundant indirection].
*)		 
let breakdown_ind env = function
  | ALocalInd ((x, _), AAtom (p, xs)) ->
     let env, (xs, inds) = breakdown_args env xs in
     env, BLocalInd (x, (p, xs))::inds
  | AFreeInd (x, AAtom (p, xs)) ->
     let env, (xs, inds) = breakdown_args env xs in
     env, BFreeInd (x, (p, xs))::inds
  | AFreeInd (x, AFreeLink y) -> env, [BRedir (x, y)]
  | _ -> failwith @@ "Redundant indirection"    



(** Breakdown argument atoms on right hand side of indirections.
    Returns [(local_indegs, b_ind list)].
    Takes [link_id] which has returned from the [Alpha.prep].
 *)
let breakdown_inds = first snd <. second List.concat <.. List.fold_left_map breakdown_ind 


									 
(** Bheck a rule whether meets the conditions *)							    
let check_rule (((lhs_links, _), lhs_rules), ((rhs_links, _), rhs_rules)) =
  if lhs_rules <> [] then failwith @@ "rule(s) on LHS"
  else if rhs_rules <> [] then failwith @@ "rule(s) on RHS is not supported ..."
  else check_link_cond (lhs_links, rhs_links)


(** インダイレクションを分類する *)
let classify_ind = function
  | BLocalInd (x, p_xs) -> Left3   (x, p_xs)
  | BFreeInd  (x, p_xs) -> Middle3 (x, p_xs)
  | BRedir    (x, y)    -> Right3  (x, y)


(** インダイレクションを分類に基づいて分割する *)
let partition_inds = partition_map3 classify_ind



let free_indeg_diff_of rhs_free_indegs (lhs_free_link, lhs_free_indeg) =
  (lhs_free_link, maybe 0 (List.assoc_opt lhs_free_link rhs_free_indegs) - lhs_free_indeg)


(** ルール左辺・右辺における自由リンクの入次数の差分を求める *)
let free_indeg_diffs_of lhs_free_indegs rhs_free_indegs =
  List.map (free_indeg_diff_of rhs_free_indegs) lhs_free_indegs



(** AST を一本のリストにする
    - リンク条件のチェックも行う
 *)
let rec breakdown proc =
  let link_id, (atoms, rules) = alpha proc in
  let (local_indegs, free_indegs), free_incidences = collect_link_info atoms in
  let local_indegs, inds = breakdown_inds (link_id, local_indegs) atoms in
  (((local_indegs, free_indegs), free_incidences), inds), List.map breakdown_rule rules
and breakdown_rule (lhs, rhs) =
  let rule = (breakdown lhs, breakdown rhs) in
  check_rule rule;
  let ((((lhs_local_indegs, lhs_free_indegs), _), l_graph), _),
      ((((rhs_local_indegs, rhs_free_indegs), _), r_graph), _) = rule in
  let rhs_graph = partition_inds r_graph in
  let free_indeg_diff = free_indeg_diffs_of lhs_free_indegs rhs_free_indegs in
  BRule ((lhs_local_indegs, l_graph), free_indeg_diff, (rhs_local_indegs, rhs_graph))

