(** The toplevel of the runtime environment *)

open Compiler
open Eval
open Loader


(** Reduce as many as possible.
    Tail recursive (as it should be).
 *) 
let rec run_many tracer i rules atom_list =
  tracer i atom_list;
  match Eval.run_once atom_list rules with
  | None -> atom_list
  | Some atom_list -> run_many tracer (succ i) rules atom_list


			   
let run tracer dumper (init_insts, rules) =
  let initial_atom_list = init_atoms init_insts in
  let final_state = run_many tracer 0 rules initial_atom_list in
  Eval.clean_atom_list final_state;
  print_endline @@ "Final state: " ^ dumper final_state



(** The top level entry point *)
let main () =
  let prop = load () in
  
  let dumper = if prop.verbose then Dump.dbg_dump else Pretty.dump in
  let tracer = if prop.trace
	       then fun i atoms -> print_endline @@ string_of_int i ^ ": " ^ dumper atoms
	       else fun _ _ -> ()
  in  run tracer dumper @@ compile prop.file


