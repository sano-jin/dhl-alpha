(** main.ml *)

open Util
open Breakdown
       
(** read lines from the given file *)
let read_file name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
       close_in ic;
       String.concat "\n" @@ List.rev acc
  in
  loop []

(** parse : string -> proc *)
let parse = Parser.main Lexer.token <. Lexing.from_string

(** Reduce as many as possible.
    Tail recursive (as it should be).
 *) 
let rec run_many tracer dumper i rules atoms =
  tracer i atoms;
  match Eval.run_once atoms rules with
  | None -> atoms
  | Some atoms -> run_many tracer dumper (succ i) rules atoms

			   
let run_file tracer dumper file_name  =
  match file_name |> read_file |> parse |> breakdown with
  | ((((local_indegs, []), []), inds), rules) ->
     let final_state =
       run_many tracer dumper 0 rules
       @@ Eval.init_atoms local_indegs @@ classify_inds inds
     in
     Vm.clean_atom_list final_state;
     print_endline @@ "Final state: " ^ dumper final_state
  | _ -> failwith "free links are not allowed in the initial graph"


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
