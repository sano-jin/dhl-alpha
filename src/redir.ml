(** redir.ml  *)

open Util
open Vm

let redir2addr free2addr (x, y) = 
  (List.assoc x free2addr, List.assoc y free2addr)

let redir2addrs = List.map <. redir2addr    

    
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
		       
let rev_redirs = List.fold_left rev_redir [] 

				
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
       fold_opt (check_zero addr2indeg rev_ind_redirs start) visited parents


			 
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
  

(* node_ref からの redirection をチェックする
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

let ref_count_redirs addr2indeg redirs rev_redirs rev_ind_redirs =
  let node_refs = List.map fst redirs in
  let rec check_all new_addr2indeg = function
    | [] -> Some new_addr2indeg
    | node_ref::rest ->
       let visited = List.map fst new_addr2indeg in
       let* new_addr2indeg' = 
	 ref_count_redir addr2indeg redirs rev_redirs rev_ind_redirs visited node_ref in
       check_all (new_addr2indeg' @ new_addr2indeg) rest
  in
  check_all [] node_refs

  
let check_redirs (redirs, free_indeg_diffs) env =
  let redirs = redir2addrs env.free2addr redirs in

  (* 間接参照からの参照を考慮しない被参照数で更新 *)
  update_free_indegs env.free2addr free_indeg_diffs env.free2addr;

  (* 上述で更新した被参照数を取得する 
     - 同一の node_ref を含む可能性がある
       （これを除去する必要はあるかと言われるとない気がするがあまり綺麗ではないのも事実）
   *)
  let addr2indegs = List.map (fun (_, node_ref) -> (node_ref, fst !node_ref)) env.free2addr in

  (* 間接参照の逆辺 *)
  let rev_redirs = rev_redirs redirs in

  (* 間接参照の逆辺のうち，間接参照ノードを指している逆辺のみ取得 *)
  let rev_ind_redirs = rev_ind_redirs redirs rev_redirs in

  match ref_count_redirs addr2indegs redirs rev_redirs rev_ind_redirs with
  | None -> (* ダメだったので巻き戻し *)
     let inversed_free_indeg_diffs = List.map (second @@ ( * ) (-1)) free_indeg_diffs in
     update_free_indegs env.free2addr inversed_free_indeg_diffs env.free2addr; (* rewind *)
     None
  | Some addr2indegs ->
     let update_indeg (node_ref, indeg) =
       update_ref (first @@ (+) indeg) node_ref
     in
     List.iter update_indeg addr2indegs; 
     Some env
