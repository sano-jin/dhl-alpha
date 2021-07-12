(** redir.ml  *)

open Util



(** レジスタからリダイレクトされるアトムへの参照を求める *)
let redir2addr register (x, y) = (register.(x), register.(y))


(** レジスタからリダイレクトされるアトムへの参照のリストを求める *)
let redir2addrs register = List.map @@ redir2addr register


    
(** 順方向に間接ノードを辿る *)
let rec traverse_ind redirs visited node_ref =
  if List.memq node_ref visited then (node_ref, true) (* detected a loop *)
  else match List.assq_opt node_ref redirs with
       | None -> (node_ref, false) (* 本物のノードに到達した *)
       | Some next_node_ref ->
	  traverse_ind redirs (node_ref::visited) next_node_ref (* 間接ノードだったのでまだ辿る *)


		       
(** 間接参照の辺を逆にする *)
let rev_redir rredirs (from, to_) =
  updateq (fun _ -> [from]) (List.cons from) to_ rredirs
		       
let rev_redirs_of redirs = List.fold_left rev_redir [] redirs

				

(** 間接参照の逆辺のうち，間接参照ノードを指している辺のみを抽出 *)
let rev_ind_redirs redirs rev_redirs =
  List.filter (flip List.mem_assq redirs <. fst) rev_redirs 



(** ループを検出した場合は逆方向に間接ノードを辿って参照カウンタの値が全てゼロであることを確認する
    - ダメだったら None を返す
    - 大丈夫だったら訪れたノードの集合を Some に包んで返す
 *)
let rec check_zero addr2indeg rev_ind_redirs start visited node_ref  =
  let visited = node_ref::visited in (* 訪れたノードをマークする *)
  if List.assoc node_ref addr2indeg <> 0 then None (* 参照カウンタの値がゼロでなかったら None *)
  else if start == node_ref then Some visited (* ループに辿り着いたら訪れたノードを返す *)
  else
    maybe (Some visited) (* 逆辺で辿れるノードが存在しないなら訪れたノードの集合を返す *)
    @@ let+ parents = List.assq_opt node_ref rev_ind_redirs in
       (* 逆辺で辿れる間接ノードについてもチェックする *)
       foldM (check_zero addr2indeg rev_ind_redirs start) visited parents


			 
(** ループを検出しなかった場合は，逆辺を深さ優先探索して，帰りがけ順に被参照数を加算してゆく
    - 帰り値はノードと被参照数の連想リストと被参照数のタプル
      （この連想リストから訪れたノードの集合も導出できる）
 *)
let rec rdfs addr2indeg rev_redirs node_ref = 
  let indeg = List.assoc node_ref addr2indeg in (* 現ノードの間接参照からの参照を考慮しない被参照数 *)
  maybe ([(node_ref, 0)], indeg) (* 逆辺で辿れるノードが存在しないなら現ノードの被参照数に関する情報のみを返す *)
    @@ let+ parents = List.assq_opt node_ref rev_redirs in
       (* 逆辺で辿れる間接ノードを深さ優先探索する *)
       let addr2indeg, parent_indegs =
	 List.split 
	 @@ List.map (rdfs addr2indeg rev_redirs) parents in
       let indeg_diff = List.fold_left (+) 0 parent_indegs in (* 増えた分の被参照数 *)
       ((node_ref, indeg_diff)::List.concat addr2indeg, indeg + indeg_diff)

  

(** node_ref からの redirection をチェックする
    - 不正な間接循環参照を検出した場合は None を返す
    - そうでない場合はノードと被参照数の連想リストを Some に包んで返す
 *) 
let ref_count_redir addr2indeg redirs rev_redirs rev_ind_redirs visited node_ref =
  if List.memq node_ref visited then Some [] (* もうすでに訪れたノードなら何もしない *)
  else
    let node_ref, hasCycle = traverse_ind redirs visited node_ref in
    if hasCycle then (* ループを検知した場合 *)
      let+ visited =
	check_zero addr2indeg rev_ind_redirs node_ref visited 
	(* node_ref が指している次のノードから探索を始める．これは循環しているので必ず間接ノードになる *)
	@@ List.assq node_ref redirs in 
      zip_const 0 visited (* もし大丈夫なら全て被参照数はゼロなはずなのでゼロと zip する *)
    else
      Some (fst @@ rdfs addr2indeg rev_redirs node_ref)


(** node_ref のリストについて，redirection をチェックする *) 
let ref_count_redirs addr2indeg redirs rev_redirs rev_ind_redirs =
  let check new_addr2indeg node_ref =
       let visited = List.map fst new_addr2indeg in
       let+ new_addr2indeg' = 
	 ref_count_redir addr2indeg redirs rev_redirs rev_ind_redirs visited node_ref in
       (new_addr2indeg' @ new_addr2indeg)
  in
  let node_refs = List.map fst redirs in
  foldM check [] node_refs



(** 自由リンクの参照カウンタの値を [free_indeg_diff] で加算する *)
let update_free_indeg register (x, free_indeg_diff) =
  let node_ref = register.(x) in
  update_ref (first @@ (+) free_indeg_diff) node_ref


(** リダイレクトのリストのそれぞれについて，始点の自由リンクの参照カウンタの値を free_indeg_diffs で加算する *)
let update_free_indegs register = List.iter @@ update_free_indeg register



(** リダイレクションのチェックのトップレベル *)								  
let check_redirs register (redirs, free_indeg_diffs) =
  let redirs = redir2addrs register redirs in

  (* 間接参照からの参照を考慮しない被参照数で更新 *)
  update_free_indegs register free_indeg_diffs;

  (* 上述で更新した被参照数を取得する 
     - 同一の node_ref を含む可能性がある
       （これを除去する必要はあるかと言われるとない気がするがあまり綺麗ではないのも事実）
   *)
  let free_node_refs = List.map (Array.get register <. fst) free_indeg_diffs in
  let addr2indegs = List.map (fun node_ref -> (node_ref, fst !node_ref)) free_node_refs in

  (* 間接参照の逆辺 *)
  let rev_redirs = rev_redirs_of redirs in

  (* 間接参照の逆辺のうち，間接参照ノードを指している逆辺のみ取得 *)
  let rev_ind_redirs = rev_ind_redirs redirs rev_redirs in

  match ref_count_redirs addr2indegs redirs rev_redirs rev_ind_redirs with
  | None -> (* ダメだったので巻き戻し *)
     let inversed_free_indeg_diffs = List.map (second @@ ( * ) (-1)) free_indeg_diffs in
     update_free_indegs register inversed_free_indeg_diffs; (* rewind *)
     false
  | Some addr2indegs ->
     let update_indeg (node_ref, indeg) =
       update_ref (first @@ (+) indeg) node_ref
     in
     List.iter update_indeg addr2indegs; 
     true
