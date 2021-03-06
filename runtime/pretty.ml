(** Pretty printer *)

open Util
open Eval



let get_link_name ((ref2link_id, link_id) as env) node_ref =
  second ((^) "L" <. string_of_int) @@
    match List.assq_opt node_ref ref2link_id with
    | None -> (((node_ref, link_id)::ref2link_id, succ link_id), link_id)
    | Some i -> (env, i)



let rec dump_arg ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes || fst !node_ref <> 1 
  then
    (* if already have dumped 
     * or the indegree is not 1 and cannot *embed* the atom as an argument *)
    first (pair dumped_nodes) @@ get_link_name addr_env node_ref
  else
    (* *embed* the atom as an argument *)
    dump_atom false env node_ref 
and dump_atom is_top_level ((dumped_nodes, addr_env) as env) node_ref =
  match snd !node_ref with
  | VMInd y ->
     (* If this is the indirection on the *root* of the dumped atom tree,
      *	then we should remember that we have dumped that indirection.
      *	Otherwise, we will just print the link to indirect. *)
     if is_top_level then
       first (pair @@ node_ref::dumped_nodes) @@ get_link_name addr_env !y
     else
       first (pair dumped_nodes) @@ get_link_name addr_env node_ref
  | VMAtom (p, xs) ->
     let (env, xs) = List.fold_left_map dump_arg (first (List.cons node_ref) env) @@ Array.to_list xs in
     (env, p ^ if xs = [] then ""
	       else "(" ^ String.concat ", " xs ^ ")")

     

let dump_ind ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes then (env, None)
  else
    second Option.some @@
      if fst !node_ref = 0 then dump_atom true env node_ref
      else
	let (addr_env, link) = get_link_name addr_env node_ref in
	let env = second (const addr_env) env in
	second ((^) @@ link ^ " -> ") @@ dump_atom true env node_ref 



(** A helper function for [tpl_sort]
    Notice this function will also return indirections that are not in the atom_list
    but traversable from it.
*)  
let rec visit (l, visited) node_ref =
  if List.memq node_ref visited then (l, visited)
  else
    let visited = node_ref::visited in
    let xs = match snd !node_ref with
      | VMAtom (_, xs) -> Array.to_list xs
      | VMInd y -> [!y]
    in
    first (List.cons node_ref)
    @@ List.fold_left visit (l, visited) xs 

(** Topological sort *)
let tpl_sort = fst <. List.fold_left visit ([], []) 



(** functor ?????????????????? *)
let functor_sort =
  let get_functor = second Array.length <. deref_symbol_atom in
  let compare_node_refs n1 n2 =
    let (p1, arity1), (p2, arity2) = get_functor n1, get_functor n2 in
    let p2_p1 = String.compare p2 p1 in
    if p2_p1 = 0 then arity2 - arity1 else p2_p1
  in    
  List.sort compare_node_refs


  
(** Pretty printer for printing nodes *)
let dump =
  String.concat ". "
  <. List.filter_map id <. snd <. List.fold_left_map dump_ind ([], ([], 0)) 
  <. tpl_sort
  <. functor_sort
		      
