(** vm.ml *)


type vm_atom =
  | VMAtom of string * node_ref ref list
  | VMInd of node_ref ref
 and node_ref = (int * vm_atom) ref  (** (indeg, atom) *)

(** レジスタ番号 *)       
type reg_i = int

(** ファンクタ:= (アトム名, リンクの数) *)
type functor_ = string * int


(** An environment for the matching and pushout *)
type env = {
  matched_atoms: (functor_ * reg_i) list;
  (** all the indices of registers which store the address of the matched atoms on lhs *)
  
  local2reg_i: (int * reg_i) list;
  (** 局所リンクがマッチしたアドレスを格納しているレジスタ番号の連想リスト *)
  
  free2reg_i: (string * reg_i) list;
  (** 自由リンクがマッチしたアドレスを格納しているレジスタ番号の連想リスト *)

  free_reg_i: int;
  (** まだ使っていないレジスタ番号の最小値（= レジスタの数） *)
}

let empty_env =
  {matched_atoms = []; local2reg_i = []; free2reg_i = []; free_reg_i = 0}	     

let get_free_reg_i env =
  env.free_reg_i, {env with free_reg_i = succ env.free_reg_i}
    
    
(*    
(** Free memory fragment of the given address.
    Possibly implemented with `option` type and assign `None`.
 *)
let free_atom node_ref =
  match !node_ref with
  | (indeg, VMAtom (p, xs)) -> node_ref := (indeg, VMAtom ("~" ^ p, xs))
  | (indeg, VMInd x) -> node_ref := (indeg, VMAtom ("~->", [x]))


(** Traverse indirection atoms and returns the pointing symbol atom.
    There is no worring of circulating indirection
    (if that exists, then the basic design is wrong).
    If the given pointer points to an indirection atom, 
    decrease the reference counter by the given `ref_count` 
    (which is 1 if this is pointed by a symbol atom)
 *)

let rec traverse node_ref_mut ref_count =
  let node_ref = !node_ref_mut in
  let indeg = fst !node_ref in
  match snd !node_ref with
  | VMInd y as vm_ind ->
    ( if indeg = ref_count then free_atom node_ref (* Free if only we are pointing *)
      else node_ref := (indeg - ref_count, vm_ind) (* Else decrease the indegree by ref_count *)
    );
    let node_ref = traverse y indeg in
    node_ref_mut := node_ref;
    (*    print_string ">>>> traversing indirection atom <<<<\n"; *)
    node_ref
  | VMAtom (_, _) -> node_ref


(* 
(** A little more efficient version of the above `traverse`.
    Which eliminated the unnecessary assignment.
*)

let rec traverse node_ref_mut ref_count =
  let rec traverse_ind node_ref_mut node_ref indeg ref_count next_mut =
    ( if indeg = ref_count then free_atom node_ref (* Free if only we are pointing *)
      else node_ref := (indeg - ref_count, vm_ind) (* Else decrease the indegree by ref_count *)
    );
    let next = !next_mut in
    let next_indeg = fst !next in
    match snd !next with
    | VMInd y as vm_ind ->
       let node_ref =
	 traverse_ind next_mut next next_indeg indeg y in
       node_ref_mut := node_ref;
       node_ref
    | VMAtom (_, _) -> next
  in
  let node_ref = !node_ref_mut in
  let indeg = fst !node_ref in
  match snd !node_ref with
  | VMInd y as vm_ind ->
     let node_ref =
       traverse_ind node_ref_mut node_ref indeg ref_count y in
     node_ref_mut := node_ref;
     node_ref
  | VMAtom (_, _) -> node_ref
*)

		       
		       
(** Resolve indirections in an atom list
    Supposed to be called in the end of the program execution.
 *)
let clean_atom_list atom_list =
  let clean_atom node_ref =
    match snd !node_ref with
    | VMInd _ -> failwith "Bug: indirection must not be dereferenced from an atom list"
    | VMAtom (_, xs) -> List.iter (ignore <. flip traverse 1) xs 
  in List.iter clean_atom atom_list


(* 自由リンクの参照カウンタの値を free_indeg_diffs で加算する *)
let update_free_indeg free2addr free_indeg_diffs (x, _) =
  let node_ref = List.assoc x free2addr in
  let free_indeg_diff = List.assoc x free_indeg_diffs in
  update_ref (first @@ (+) free_indeg_diff) node_ref

let update_free_indegs free2addr = List.iter <. update_free_indeg free2addr
  
	       
 *)
