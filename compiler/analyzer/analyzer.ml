(** Semantic analyzer *)

include Breakdown
       
  

(** semantic analyzer
    @return semantic graph obtained from the given AST
 *)
let sem_graph_of proc = 
  match breakdown proc with
  | ((((local_indegs, []), []), inds), rules) -> ((local_indegs, partition_inds inds), rules)
  | _ -> failwith "free links are not allowed in the initial graph"


