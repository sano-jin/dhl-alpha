(** link_check.ml 
    Collect link information and check them.
*)

open Util
open Alpha
       
(** A helper function for `collect_incidences` *)
let collect_incidence (locals, frees) = function
  | ALocalInd ((x, link_name), _) ->
     if List.mem x locals then failwith @@ "local link " ^ link_name ^ " is not univalent"
     else (x::locals, frees)
  | AFreeInd (x, _) ->
     if List.mem x frees then failwith @@ "free link " ^ x ^ " is not univalent "
     else (locals, x::frees)
	    
(** Collect free/local head link (left hand side of `->`) names *)	     
let collect_incidences = List.fold_left collect_incidence ([], [])
	    

					
(** collect indegs of `p_arg` and also check the serial condition *)
let rec collect_indeg_arg ((locals, frees) as links) = function
  | ALocalLink (x, link_name) ->
     (update (fun _ -> failwith @@ link_name ^ " is not serial") succ x locals, frees)
  | AFreeLink x -> 
     (locals, update (const 1) succ x frees)
  | AAtom (_, xs) ->
     List.fold_left collect_indeg_arg links xs

(** collect indegs of `p_atom` *)
let collect_indeg links = function
  | AFreeInd  (_, AFreeLink x) -> second (update (const 0) id x) links
  | ALocalInd (_, p) | AFreeInd  (_, p) -> collect_indeg_arg links p

let collect_indegs = List.fold_left collect_indeg

(** collect `link_info` from `p_atom list` *)
let collect_link_info atoms =
  let (_, free_incidences) as links = collect_incidences atoms in
  let indegs =
    let init_indegs =
      first (zip_const 0) @@ second (zip_const 0) @@ links in
    collect_indegs init_indegs atoms
  in (indegs, free_incidences)


       
(** Check link condition of the given rule.
    Uses the collected link_info on lhs/rhs returned by `collect_link_info`.
 *)
let check_link_cond ((lhs_indegs, lhs_free_incidences),
		     (rhs_indegs, rhs_free_incidences)) =
  let free_names = List.map fst <. snd in
  let unbound_rhs_links = set_minus (free_names rhs_indegs) (free_names lhs_indegs) in
  if unbound_rhs_links <> [] then
    failwith @@ "link(s) " ^ String.concat ", " unbound_rhs_links ^ " on RHS has/have not appeared on LHS"
  else ();
  let non_linear_incidences = sym_diff lhs_free_incidences rhs_free_incidences in
  if non_linear_incidences <> [] then 
    failwith @@ "link(s) " ^ String.concat ", " non_linear_incidences ^ " appear(s) non lineary on rule"
  else ()



       
