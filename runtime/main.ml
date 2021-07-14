(** main.ml *)

open Compiler
open Eval
open Util



(** Reduce as many as possible.
    Tail recursive (as it should be).
 *) 
let rec run_many tracer dumper i rules atom_list =
  tracer i atom_list;
  match Eval.run_once atom_list rules with
  | None -> atom_list
  | Some atom_list -> run_many tracer dumper (succ i) rules atom_list


			   
let run_file tracer dumper file_name =
  let init_insts, rules = file_name |> read_file |> compile in
  let initial_atom_list = init_atoms init_insts in
  let final_state = run_many tracer dumper 0 rules initial_atom_list in
  Eval.clean_atom_list final_state;
  print_endline @@ "Final state: " ^ dumper final_state



let usage_msg = "append [-t] [-v] <file1>"
let verbose = ref false
let trace = ref false
let input_files = ref []

let anon_fun filename =
  input_files := filename::!input_files

let speclist =
  [("-t", Arg.Set trace, "Trace");
   ("-v", Arg.Set verbose, "Output debug information")
  ]


(** The top level entry point *)		      
let main () =
  Arg.parse speclist anon_fun usage_msg;
  (* Main functionality here *)
  match !input_files with
  | [] -> failwith @@ "no input file"
  | (_::_::_) -> failwith @@ "too many files"
  | [file_name] ->
     let dumper = if !verbose then Debug_vm.dbg_dump else Dump.dump in
     let tracer = if !trace
		  then fun i atoms -> print_endline @@ string_of_int i ^ ": " ^ dumper atoms
		  else fun _ _ -> ()
     in  run_file tracer dumper file_name
