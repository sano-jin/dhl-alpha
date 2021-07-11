(** util.ml *)

(** 基本的なコンビネータなど *)


(** some very basic combinators *)
let flip f x y = f y x  

let id x = x
let const x _ = x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y


			 
(** tuple の操作のためのコンビネータ *)
let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)
let pair x y = (x, y)



(** compositional functions *)
let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
let (<...) f g = fun x y z -> f (g x y z)



(** monadic combinators for the Option type *)
let (>>=) = Option.bind
let ( let* ) = Option.bind


let (<$>) = Option.map
let (<&>) x f = Option.map f x
let ( let+ ) x f = Option.map f x


let (<|>) l r = 
  if Option.is_some l then l
  else r ()


let rec one_of f = function
  | [] -> None
  | h::t -> f h <|> fun _ -> one_of f t


let maybe default = function
  | None -> default
  | Some s -> s 



(** monadic combinators for the traversible type *)
let (<::>) h t = List.cons h <$> t

(** monadic fold_left *)
let rec foldM f acc = function
  | [] -> Some acc
  | h::t -> f acc h >>= flip (foldM f) t			     



(** 集合演算（Set を用いるようにリファクタリングしても良いかも） *)
let set_minus l r = List.filter (not <. flip List.mem r) l
let set_minus_q l r = List.filter (not <. flip List.memq r) l
let sym_diff l r = set_minus l r @ set_minus r l



(** Either 型の要素のリストを左右に振り分ける *)
let partitionEithers l = List.partition_map id l


(** zip/unzip *)

(** monadic combine (possibly renamed as [safe_combine]) *)
let rec uncurried_safe_unzip = function
  | ([], []) -> Some []
  | (xh::xt, yh::yt) -> (xh, yh) <::> uncurried_safe_unzip (xt, yt)
  | _ -> None


let safe_unzip t = curry uncurried_safe_unzip t
let zip_const c = List.map @@ flip pair c



(** 更新不可なマップオブジェクト
    - 同一のキーに対して，異なる値を挿入しようとしたら，例外 [Bug: updating] を投げる
 *)
let rec insert x v = function
  | [] -> [(x, v)]
  | (y, w) as h ::t ->
     if x = y then
       if v <> w then failwith @@ "Bug: updating" 
       else (x, v) :: t 
     else h :: insert x v t



(** A helper function for `collect_indeg_arg` and `collect_indeg` *)					
let rec update fallback f x = function
  | [] -> [x, fallback ()]
  | (y, v) as h::t ->
     if x = y then (y, f v) :: t
     else h::update fallback f x t


let rec updateq fallback f x = function
  | [] -> [x, fallback ()]
  | (y, v) as h::t ->
     if x == y then (y, f v) :: t
     else h::updateq fallback f x t
		    
		    

let rec update_assc_opt pred f fallback = function
  | [] -> fallback ()
  | (y, v) as h ::t ->
     if pred y then let+ v = f v in (y, v)::t
     else h <::> update_assc_opt pred f fallback t


let update_ref f r = r := f !r
let (!++) r = let i = !r in incr r; i
			       



    


(** Add 4 * n white spaces to the head of the string
    - "\t" の方が良いかも
 *)
let indent n = (^) @@ String.make (4 * n) ' '


(** 入出力のための関数 *)

(** デバッグ用の出力を行う *)
let debug_print description message =
  prerr_endline @@ ">>>> " ^ description;
  prerr_endline message;
  prerr_endline "<<<<"



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


		
