(** main.ml *)

open Front_end
open Gen_ic
open Util


(** ルールをコンパイルした結果を出力する *)
let dump_rule i (reg_size, (lhs_insts, rhs_insts)) =
  prerr_newline ();
  Printf.eprintf ">>>> rule #%d <<<<\n" i;
  debug_print "register_size" @@ string_of_int reg_size;
  debug_print "genereted lhs_insts" @@ string_of_lhs_insts lhs_insts;
  debug_print "genereted rhs_insts" @@ string_of_rhs_insts rhs_insts


(** 初期状態生成のための中間命令列を出力する *)  
let dump_init_rule (reg_size, insts) =
  prerr_newline ();
  prerr_endline ">>>> rule for constructing the initial state <<<<";
  debug_print "register_size" @@ string_of_int reg_size;
  debug_print "genereted insts" @@ string_of_rhs_insts insts



let compile = uncurry gen_ic <. front_end


						       
let test_compiler prog =
  let init_insts, rules = compile prog in
  dump_init_rule init_insts;
  List.iteri dump_rule rules
  


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
