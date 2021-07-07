(** pushatom.ml *)

open Breakdown
open Util
open Vm
       
let new_atom indeg p = ref (indeg, VMAtom (p, []))
let push_local_atom local_indegs (x, (p, _)) =
  let indeg = List.assoc x local_indegs in
  (x, new_atom indeg p)

let push_local_atoms = List.map <. push_local_atom 

let get_arg (free2addr, local2addr) = function
  | BFreeLink x -> List.assoc x free2addr
  | BLocalLink x -> List.assoc x local2addr

let assign_xs xs node_ref =
  match !node_ref with
  | indeg, VMAtom (p, _) -> node_ref := (indeg, VMAtom (p, List.map ref xs))
  | indeg, VMInd _ ->
     match xs with
     | [y] -> node_ref := (indeg, VMInd (ref y))
     | _ -> failwith "Bug: assigning not one argument to an indirection"
			       
let update_args link2addr (_, xs) node_ref =
  let xs = List.map (get_arg link2addr) xs in
  assign_xs xs node_ref

let update_local_ind link2addr local_or_free2addr (x, p_xs) =
  let node_ref = List.assoc x local_or_free2addr in
  update_args link2addr p_xs node_ref

let update_local_inds link2addr = List.iter <. update_local_ind link2addr

						    
let update_free_ind link2addr free2addr (x, (p, xs)) =
  let node_ref = List.assoc x free2addr in 
  node_ref := (fst !node_ref, VMAtom (p, List.map (ref <. get_arg link2addr) xs))
				     
let update_free_inds = List.iter <.. update_free_ind

		
						    
let push_redir free2addr (x, y) =
  let node_ref = List.assoc x free2addr in
  (* We have already set the indeg at the very end of the matching *)
  let indeg = fst !node_ref in
  if indeg = 0 then free_atom node_ref
  else
    let y = List.assoc y free2addr in
    node_ref := (indeg, VMInd (ref y))

let push_redirs = List.iter <. push_redir
	       
let push_atoms local_indegs free2addr (local_inds, free_inds, redirs) =
  let local2addr = push_local_atoms local_indegs local_inds in
  let link2addr = (free2addr, local2addr) in
  update_local_inds link2addr local2addr local_inds;
  update_free_inds link2addr free2addr free_inds;
  push_redirs free2addr redirs;
  List.map snd local2addr
