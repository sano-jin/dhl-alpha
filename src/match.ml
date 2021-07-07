(** match.ml  *)

open Breakdown
open Util
open Vm

       
let check_arg env node_ref_mut =
  (* traverse indirection atoms till reach a symbol atom *)
  let node_ref = traverse node_ref_mut 1 in 
  function
  | BFreeLink x ->
       begin
	 match List.assoc_opt x env.free2addr with
	 | None ->
	    (* if the free link name has not mathced to any address *)
	    if List.memq node_ref
	       @@ List.map snd env.local2addr then None (* Already matched with a local link  *)
	    else
	      Some {env with free2addr = (x, node_ref)::env.free2addr}
	    
	 | Some addr ->
	    (* if the free link name has already mathced to the address  *)
	    if addr != node_ref then None (* has matched to other address  *)
	    else Some env
       end
  | BLocalLink x ->
     match List.assoc_opt x env.local2addr with 
     | None ->
	if List.memq node_ref
	   @@ List.map snd env.free2addr then None (* already mathced with a free link  *)
	else Some {env with local2addr = (x, node_ref)::env.local2addr}
     | Some addr ->
	if node_ref != addr then None (* local link matched to different addrs  *)
	else Some env


			      
let check_atom (p, xs) env node_ref =
  if List.memq node_ref env.matched_atoms then None
  else
    match snd !node_ref with
    | VMInd _ -> failwith @@ "Bug: we should not dereference indirection from an atom list"
    | VMAtom (q, ys) ->
       if p <> q then None (* different atom name  *)
       else safe_zip ys xs
	    >>= foldM (uncurry <. check_arg)
		      {env with matched_atoms = node_ref::env.matched_atoms}
		      
	      
(* node_ref must be pointing at a VMAtom not VMInd *)
let check_ind local_indegs env node_ref =
  function
  | BLocalInd (x, (p, xs)) ->
     if fst !node_ref <> List.assoc x local_indegs then None (* indeg did not match *)
     else
       check_atom
	 (p, xs)
	 
	 (* If x is the key of `addr` in `env.local2addr`, then the `addr` should be equal to `node_ref`.
	    Since is the `x` is in the `env.local2addr`, then we should have conducted dereference hence
	    the `node_ref` is lookuped from the `env.local2addr` in the former phase (`try_deref` in `find_atoms`).
	  *)
	 {env with local2addr = insert x node_ref env.local2addr}
	 node_ref
  | BFreeInd (x, (p, xs)) ->
     (* no indeg checking for a free link *)
     check_atom
       (p, xs)  
       {env with
	 (* if x is the key of `addr` in `env.free2addr`, then the `addr` should be equal to `node_ref`.
	    Since is the `x` is in the `env.free2addr`, then we should have conducted dereference hence
	    the `node_ref` is lookuped from the `env.free2addr` in the former phase (`try_deref` in `find_atoms`).        
	  *)
	 free2addr = insert x node_ref env.free2addr;
       }
       node_ref 
  | _ -> failwith @@ "Indirection on LHS is not supported"


			      
let rec find_atoms env free_link_info local_indegs atom_list =
  let check_ind_ = flip @@ check_ind local_indegs env in
  let try_deref x link2addr ind t =
    match List.assoc_opt x link2addr with
    | None -> (* Could not dereference. has not matched yet. *)
       let rec try_match = function
	 | [] -> None (* no atom list remaining *)
	 | node_ref::rest_atom_list ->
	    (* node_ref must be pointing a VMAtom (not VMInd) *)
	    ( let* env = check_ind_ ind node_ref in
		find_atoms env free_link_info local_indegs atom_list t
	    ) <|> fun _ -> try_match rest_atom_list
       in try_match atom_list
    | Some node_ref ->
       (* Was able to dereference with already known reference.
	  In this case, the node_ref is guaranteeded to point an atom but an indirection atom.
	  Since we have traversed indirection in the formaer process.
	*)
       let* env = check_ind_ ind node_ref in
       find_atoms env free_link_info local_indegs atom_list t
  in	 
  function
  | BLocalInd (x, _) as ind ::t -> try_deref x env.local2addr ind t
  | BFreeInd  (x, _) as ind ::t -> try_deref x env.free2addr ind t
  | [] -> Redir.check_redirs free_link_info env
  | _ -> failwith @@ "Indirection on LHS is not supported"


let match_ = find_atoms empty_env 
