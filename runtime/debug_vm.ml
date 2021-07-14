(** Debug 用の Dumper *)

open Util
open Eval
       
       

type dump_atom =
    DumpAtom of int * int * (string * int list) (** DumpAtom (addr, indeg, (atom_name, addrs)) *)

									
let rec get_dump_addr ((addr2link, link_id) as env) (node_ref: node_ref) =
  match List.assq_opt node_ref addr2link with
  | None ->
     let env = ((node_ref, link_id)::addr2link, pred link_id) in
     let (env, (dump_atom, inds)) = get_dump_ind env (link_id, node_ref) in
     (env, (link_id, dump_atom::inds))
  | Some link_id -> (env, (link_id, []))

and get_dump_ind env (x, node_ref) =
  let (indeg, atom) = !node_ref in
  let (env, (p_xs, inds)) = match atom with  
    | VMAtom (p, xs) ->
       let (env, xs_inds) = List.fold_left_map get_dump_addr env @@ Array.to_list xs in
       let (xs, inds) = List.split xs_inds in
       (env, ((p, xs), List.concat inds))
    | VMInd x ->
       let (env, (x, inds)) = get_dump_addr env !x in
       (env, (("->", [x]), inds))
  in
  (env, (DumpAtom (x, indeg, p_xs), inds))

    

(** アトムリストに登録されているアトムのリスト（順序も保存する）と登録されていないがトラバースできたアトムのリストの組を返す *)
let get_dump_atoms atom_list =
  let addr2link = List.mapi (flip pair) atom_list in
  let link2node_ref = List.mapi pair atom_list in
  let (dump_atoms, inds) =
    List.split @@ snd @@ List.fold_left_map get_dump_ind (addr2link, -1) link2node_ref in
  (dump_atoms, List.concat inds)


    
let string_of_dump_atom = function
  | DumpAtom (x, indeg, (p, xs)) ->
     "#" ^ string_of_int x ^ " -> " ^ string_of_int indeg ^ " : "
     ^ p ^ " [" ^ String.concat ", " (List.map ((^) "#" <. string_of_int) xs) ^ "]"

let dbg_dump atom_list =
  let (dump_atoms, inds) = get_dump_atoms atom_list in
  let (dump_atoms, inds) =
    (List.map string_of_dump_atom dump_atoms, List.map string_of_dump_atom inds)
  in
  "\n" ^ String.concat "\n" (inds @ dump_atoms) ^ "\n"
