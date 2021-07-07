(** alpha.ml 
    Convert local link names to fresh ids and partition atoms and rules.
 *)

open Syntax
open Util

(** (link_id, link_name_for_debugging) *)       
type local_link = int * string  
			  
type a_arg =
  | AFreeLink of string
  | ALocalLink of local_link
  | AAtom of string * a_arg list

type a_atom =
  | ALocalInd of local_link * a_arg
  | AFreeInd of string * a_arg

(** (local_id, local_indeg) list, (free_name, free_indeg) list *)
type indegs = (int * int) list * (string * int) list

(** (indegs, free_incidence list) *)
type link_info = indegs * string list

				 
				 
(** A helper function for `prep_proc`
    Convert local link names to ids *)     
let rec alpha_arg env = function
  | Atom (p, xs) -> AAtom (p, List.map (alpha_arg env) xs)
  | Link x -> match List.assoc_opt x env with
	      | None   -> AFreeLink x
	      | Some i -> ALocalLink (i, x)
				     
(** A helper function for `alpha`
    Convert local link names to ids and classify atoms and rules *) 
let rec alpha_proc env link_id = function
  | Zero -> (link_id, [])
  | Ind  (None, p) ->
     (succ link_id, [Either.Left (ALocalInd ((link_id, ""), alpha_arg env p))])
  | Ind  (Some x, p) ->
     (link_id,
      [Either.Left (
	   match List.assoc_opt x env with
	   | None   -> AFreeInd (x, alpha_arg env p)
	   | Some i -> ALocalInd ((i, x), alpha_arg env p)
	 )])
  | Mol  (p, q) ->
     second List.concat @@ List.fold_left_map (alpha_proc env) link_id [p; q] 
  | New  (x, p) ->
     alpha_proc ((x, link_id)::env) (succ link_id) p
  | Rule (l, r) -> (link_id, [Either.Right (l, r)])

(** Convert local link names to fresh ids and partition atoms and rules *)     
let alpha = second partitionEithers <. alpha_proc [] 0
	      


