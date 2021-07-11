(** main.ml *)

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

		  
let () = Main.main () 
