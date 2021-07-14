(** Utility functions.
    
*)

(** 基本的なコンビネータなど *)


(** some very basic combinators *)

(** *)
let flip f x y = f y x  

let id x = x
let const x _ = x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y


		 
(** tuple の操作のためのコンビネータ *)

(** *)
let first f (a, b) = (f a, b)

(** [second f (1, 2)] returns [(1, f 2)] *)
let second f (a, b) = (a, f b)

let pair x y = (x, y)
let swap (x, y) = (y, x)


(** compositional functions *)

(** *)
let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
let (<...) f g = fun x y z -> f (g x y z)



(** either3 *)
type ('a, 'b, 'c) either3 =
    Left3 of 'a | Middle3 of 'b | Right3 of 'c


(** [partition_map] for [Either3] type *)
let partition_map3 f =
  let helper (l, m, r) x = match f x with
    | Left3   a -> (a::l, m, r)
    | Middle3 b -> (l, b::m, r)
    | Right3  c -> (l, m, c::r)
  in
  List.fold_left helper ([], [], []) 

	 

(** monadic combinators for the Option type *)

(** *)
let (>>=) = Option.bind
let ( let* ) = Option.bind


let (<$>) = Option.map
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

(** *)
let (<::>) h t = List.cons h <$> t

(** monadic [fold_left] *)
let rec foldM f acc = function
  | [] -> Some acc
  | h::t -> f acc h >>= flip (foldM f) t			     



(** 集合演算
    - Set を用いるようにリファクタリングしても良いかも 
*)
let set_minus l r = List.filter (not <. flip List.mem r) l
let set_minus_q l r = List.filter (not <. flip List.memq r) l
let sym_diff l r = set_minus l r @ set_minus r l



(** Either 型の要素のリストを左右に振り分ける *)
let partitionEithers l = List.partition_map id l


(** zip/unzip *)

(** uncurried monadic combine (possibly renamed as [safe_combine]) *)
let rec uncurried_safe_unzip = function
  | ([], []) -> Some []
  | (xh::xt, yh::yt) -> (xh, yh) <::> uncurried_safe_unzip (xt, yt)
  | _ -> None

(** monadic combine (possibly renamed as [safe_combine]) *)
let safe_unzip t = curry uncurried_safe_unzip t


(** リストのそれぞれの要素に対して定数をペアにする *)
let zip_const c = List.map @@ flip pair c



(** マップオブジェクト 
    - Map を用いるようにリファクタリングした方が良い（と思われる）
*)
				   
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



(** A helper function for [collect_indeg_arg] and [collect_indeg] *)					
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


(** 参照型のためのコンビネータ *)
let update_ref f r = r := f !r

    


(** Add 4 * n white spaces to the head of the string
    - "\t" の方が良いかも
 *)
let indent n = (^) @@ String.make (4 * n) ' '



(** 入出力のための関数 *)


(** デバッグ用の出力を行う
    - 標準エラー出力に出す
    - TODO: カラフルにしてみたい
 *)
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


		
