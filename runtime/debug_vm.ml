(* vm.ml *)

open Util
open Eval



(** Dumper 用に node_ref から順方向に辿れる全ての indirection アトムへの参照を取得する
    - Indirection アトムを順方向にトラバースしても，循環することはないという前提に基づく
 *)
let rec ind_refs_from_ind node_ref =
  match snd !node_ref with
  | VMAtom _ -> []
  | VMInd y -> node_ref :: ind_refs_from_ind !y


(** Dumper 用にアトムリストから辿れる全ての Indirection アトムへの参照を取得する *)
let get_ind_node_refs atom_list =
  let traverese_symbol_atom node_ref =
    let _, xs = deref_symbol_atom node_ref in
    set_unions_q @@ List.map ind_refs_from_ind @@ Array.to_list xs
  in
  set_unions_q @@ List.map traverese_symbol_atom atom_list



(** Dumper 用のアトムの型
    - indirection アトムとの区別は（アトム名でしか）しない
 *)       
type dump_atom =
    DumpAtom of int * int * (string * int list) (** DumpAtom (addr, indeg, (atom_name, addrs)) *)



(** [node_ref] から [dump_atom] 型への変換
    - 参照と整数値のキー（[link_id]）への対応付けは既に済んでいるものとする
 *)
let get_dump_atom addr2link_id (x, node_ref) =
  let (indeg, atom) = !node_ref in
  match atom with
  | VMAtom (p, xs) ->
     let xs = List.map (flip List.assq addr2link_id) @@ Array.to_list xs in
     DumpAtom (x, indeg, (p, xs))
  | VMInd y ->
     let y = List.assq !y addr2link_id in
     DumpAtom (x, indeg, ("->", [y]))
			   

let get_dump_atoms atom_list =
  let ind_node_refs = get_ind_node_refs atom_list in
  let ind_link_id2addrs = List.mapi (fun i -> pair @@ - i - 1) ind_node_refs in
  let link_id2addrs = ind_link_id2addrs @ List.mapi pair atom_list in
  let addr2link_id = List.map swap link_id2addrs in
  List.map (get_dump_atom addr2link_id) link_id2addrs



(************************************************)
       



(** 参照とアドレスの対応を管理するための環境 *)
type addr_tbl = {
  addr2link: (node_ref * int) list;  (** アトムへの参照とアドレスとの対応 *)
  free_link_id: int;  (** indirection アトムのためのまだ割り当てられていないアドレス（ゼロ以下の値である） *)
}
			      

(** アドレスの環境に node_ref を追加し，対応するアドレスを取得する *)
let get_ind_addr addr_tbl node_ref =
  addr_tbl.free_link_id,
  { addr2link = (node_ref, addr_tbl.free_link_id)::addr_tbl.addr2link;
    free_link_id = pred addr_tbl.free_link_id
  }

  
			      
(** node_ref から一意なアドレスを取得し，（訪問済みでない場合は）トラバースする
    - まだ登録されていないアドレスだった場合は, indirection アトムである
      - アトムリストのものは全て登録済みなはずなので 
      - indirection アトムにはマイナスのアドレスを振る
 *)
let rec get_dump_addr addr_tbl node_ref =
  match List.assq_opt node_ref addr_tbl.addr2link with
  | None ->
     let link_id, addr_tbl = get_ind_addr addr_tbl node_ref in
     let (addr_tbl, (dumped_ind_atom, inds)) = get_dump_ind addr_tbl (link_id, node_ref) in
     (addr_tbl, (link_id, dumped_ind_atom :: inds))
  | Some link_id -> (addr_tbl, (link_id, []))

(** 既に割り当てられたアドレス [x] も用いて， [node_ref] から [DumpAtom] を取得する *)
and get_dump_ind addr_tbl (x, node_ref) =
  let (indeg, atom) = !node_ref in
  let (addr_tbl, (p_xs, inds)) = match atom with
    | VMAtom (p, xs) ->
       let (addr_tbl, xs_inds) = List.fold_left_map get_dump_addr addr_tbl @@ Array.to_list xs in
       let (xs, inds) = List.split xs_inds in
       (addr_tbl, ((p, xs), List.concat inds))
    | VMInd x ->
       let (addr_tbl, (x, inds)) = get_dump_addr addr_tbl !x in
       (addr_tbl, (("->", [x]), inds))
  in
  (addr_tbl, (DumpAtom (x, indeg, p_xs), inds))



  
(** @return （アトムリストに登録された）シンボルアトムのリストと Indirection アトムのリストのタプル *)  
let get_dump_atoms atom_list =
  let addr2link = List.mapi (flip pair) atom_list in
  let init_addr_tbl = {addr2link = addr2link; free_link_id = -1} in
  let link2node_ref = List.mapi pair atom_list in
  let (dbg_atoms, inds) =
    List.split @@ snd @@ List.fold_left_map get_dump_ind init_addr_tbl link2node_ref in
  (dbg_atoms, List.concat inds)


    
let string_of_dump_atom = function
  | DumpAtom (x, indeg, (p, xs)) ->
     "#" ^ string_of_int x ^ " -> " ^ string_of_int indeg ^ " : "
     ^ p ^ " [" ^ String.concat ", " (List.map ((^) "#" <. string_of_int) xs) ^ "]"

let dbg_dump atom_list =
  let (dbg_atoms, inds) = get_dump_atoms atom_list in
  let (dbg_atoms, inds) =
    (List.map string_of_dump_atom dbg_atoms, List.map string_of_dump_atom inds)
  in
  "\n" ^ String.concat "\n" (inds @ dbg_atoms) ^ "\n"
