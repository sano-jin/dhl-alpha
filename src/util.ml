(** util.ml *)

let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)

let (>>=) = Option.bind
let ( let* ) = Option.bind

let (<$>) = Option.map
let (<&>) x f = Option.map f x
let ( let+ ) x f = Option.map f x

let flip f x y = f y x  


let (<::>) h t = List.cons h <$> t
let rec foldM f acc = function
  | [] -> Some acc
  | h::t -> f acc h >>= flip (foldM f) t			     

let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)
let pair x y = (x, y)

let set_minus l r = List.filter (not <. flip List.mem r) l
let set_minus_q l r = List.filter (not <. flip List.memq r) l

let id x = x
let const x _ = x

let partitionEithers l = List.partition_map id l

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let rec uncurried_safe_zip = function
  | ([], []) -> Some []
  | (xh::xt, yh::yt) -> (xh, yh) <::> uncurried_safe_zip (xt, yt)
  | _ -> None

let safe_zip t = curry uncurried_safe_zip t
let zip_const c = List.map @@ flip pair c

let sym_diff l r =
  set_minus l r @ set_minus r l


let (<|>) l f = 
  if Option.is_some l then l
  else f ()

let rec one_of f = function
  | [] -> None
  | h::t -> f h <|> fun _ -> one_of f t

let rec insert x v = function
  | [] -> [(x, v)]
  | ((y, w) as h) ::t ->
     if x = y then
       (* NOT <>, but != *)
       if v != w then failwith @@ "Bug: updating" 
       else (x, v)::t 
     else h::insert x v t

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
			       
let maybe default = function
  | None -> default
  | Some s -> s 

(** fold_left の関数がオプションを返すバージョン
    - ダメだったらそこで止まって None を返す
    - 大丈夫なら順々に関数を適用していく
 *)
let rec fold_opt f unit = function
  | [] -> Some unit
  | h::t -> f unit h >>= flip (fold_opt f) t
							   
