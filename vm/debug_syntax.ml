(* debug_syntax.ml *)

open Syntax

let rec string_of_arg = function
  | Link x -> x
  | Atom (a, xs) ->
     if xs = [] then a 
     else a ^ "(" ^ String.concat ", " (List.map string_of_arg xs) ^ ")"
								       
let rec string_of_proc priority = function
  | Zero -> ""
  | Ind (from, _to) ->
     let str_of_from = match from with
       | None -> ""
       | Some s -> s ^ " -> "
     in str_of_from ^ string_of_arg _to
  | Rule (lhs, rhs) ->
     let str_of_rule =
       string_of_proc 1 lhs ^ " :- " ^ string_of_proc 1 rhs
     in
     if priority > 1 then "(" ^ str_of_rule ^ ")"
     else str_of_rule
  | Mol (p, q) ->
     let str_of_mol i sep =
       string_of_proc i p ^ sep ^ string_of_proc i q
     in
     if priority = 0 then str_of_mol 0 ". "
     else if priority > 2 then "(" ^ str_of_mol 2 ", " ^ ")" 
     else str_of_mol 2 ", "
  | New (x, proc) ->
     let str_of_new = 
       "\\" ^ x ^ "." ^ string_of_proc 3 proc
     in
     if priority > 3 then "(" ^ str_of_new ^ ")"
     else str_of_new
     
let string_of_proc = string_of_proc 0
