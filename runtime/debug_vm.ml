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


(** [dump_atom] を文字列に変換する *)    
let string_of_dump_atom = function
  | DumpAtom (x, indeg, (p, xs)) ->
     "#" ^ string_of_int x ^ " -> " ^ string_of_int indeg ^ " : "
     ^ p ^ " [" ^ String.concat ", " (List.map ((^) "#" <. string_of_int) xs) ^ "]"


(** Dump する *)
let dbg_dump atom_list =
  let dump_atoms =
    List.map string_of_dump_atom @@ get_dump_atoms atom_list in
  "\n" ^ String.concat "\n" dump_atoms ^ "\n"



