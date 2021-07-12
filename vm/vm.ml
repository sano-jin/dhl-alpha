(** vm.ml *)

open Util
(* open Array *)
       


type vm_atom =
  | VMAtom of string * node_ref array
  | VMInd of node_ref ref 
 and node_ref = (int * vm_atom) ref  (** (indeg, atom) *)



(** レジスタを初期化するためだけのアドレス（本来はいらない） *)
let null_ptr = ref (0, VMAtom ("Null", [||]))



(** レジスタ *)
type register = node_ref array
    


(** 初期状態のレジスタを確保する *)
let init_register size = Array.make size null_ptr
			      


(** シンボルアトムへの参照を dereference する *)
let deref_symbol_atom node_ref =
  match snd !node_ref with
  | VMInd _ -> failwith "Bug: Indirection atom must not be dereferenced here!"
  | VMAtom (p, xs) -> (p, xs)



(** Free memory fragment of the given address.
    Possibly implemented with `option` type and assign `None`.
 *)
let free_atom node_ref =
  match !node_ref with
  | (indeg, VMAtom (p, xs)) -> node_ref := (indeg, VMAtom ("~" ^ p, xs))
  | (indeg, VMInd x) -> node_ref := (indeg, VMAtom ("~->", [|!x|]))



(** Traverse indirection atoms and returns the pointing symbol atom.
    There is no worring of circulating indirection
    (if that exists, then the basic design is wrong).
    If the given pointer points to an indirection atom, 
    decrease the reference counter by the given `ref_count` 
    (which is 1 if this is pointed by a symbol atom)
    @return the reference to a symbol atom
 *)
let rec traverse (ref_count: int) node_ref =
  let indeg = fst !node_ref in
  match snd !node_ref with
  | VMInd y ->
     if indeg = ref_count
     then (
       free_atom node_ref; (* Free if only we are pointing *)
       traverse indeg !y
     ) else ( 
       let endpoint_ref = traverse indeg !y in
       (* Else decrease the indegree by ref_count *)
       node_ref := (indeg - ref_count, VMInd (ref endpoint_ref)); 
       endpoint_ref
     )
  | VMAtom (_, _) -> node_ref



		       
(** Resolve indirections in an atom list
    Supposed to be called in the end of the program execution.
 *)
let clean_atom_list atom_list =
  let clean_atom node_ref =
    match snd !node_ref with
    | VMInd _ -> failwith "Bug: indirection must not be dereferenced from an atom list"
    | VMAtom (_, xs) -> Array.iteri (fun i -> Array.set xs i <. traverse 1) xs 
  in List.iter clean_atom atom_list



  
	       
