(* vm.ml *)

open Util
open Vm

       
type dbg_atom =
    DBGAtom of int * int * (string * int list) (* DBGAtom (addr, indeg, atom_name, addrs) *)
									
let rec get_dbg_addr ((addr2link, link_id) as env) (node_ref: node_ref) =
  match List.assq_opt node_ref addr2link with
  | None ->
     let env = ((node_ref, link_id)::addr2link, pred link_id) in
     let (env, (dbg_atom, inds)) = get_dbg_ind env (link_id, node_ref) in
     (env, (link_id, dbg_atom::inds))
  | Some link_id -> (env, (link_id, []))

and get_dbg_atom env = function  
  | VMAtom (p, xs) ->
     let (env, xs_inds) = List.fold_left_map get_dbg_addr env @@ List.map (!) xs in
     let (xs, inds) = List.split xs_inds in
     (env, ((p, xs), List.concat inds))
  | VMInd x ->
     let (env, (x, inds)) = get_dbg_addr env !x in
     (env, (("->", [x]), inds))

and get_dbg_ind env (x, node_ref) =
  let (indeg, atom) = !node_ref in
  let (env, (p_xs, inds)) = get_dbg_atom env atom in
  (env, (DBGAtom (x, indeg, p_xs), inds))

    
let get_dbg_atoms atom_list =
  let addr2link = List.mapi (flip pair) atom_list in
  let link2node_ref = List.mapi pair atom_list in
  let (dbg_atoms, inds) =
    List.split @@ snd @@ List.fold_left_map get_dbg_ind (addr2link, -1) link2node_ref in
  (dbg_atoms, List.concat inds)


    
let string_of_dbg_atom = function
  | DBGAtom (x, indeg, (p, xs)) ->
     "#" ^ string_of_int x ^ " -> " ^ string_of_int indeg ^ " : "
     ^ p ^ " [" ^ String.concat ", " (List.map ((^) "#" <. string_of_int) xs) ^ "]"

let dbg_dump atom_list =
  let (dbg_atoms, inds) = get_dbg_atoms atom_list in
  let (dbg_atoms, inds) =
    (List.map string_of_dbg_atom dbg_atoms, List.map string_of_dbg_atom inds)
  in
  "\n" ^ String.concat "\n" (inds @ dbg_atoms) ^ "\n"
