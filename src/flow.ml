(** A tinny abstract interpreter for fun ! *)

(** {2 Language definition }  *)

module Imp = struct

module Var = Set.Make (String)

type expr =
  | Cst of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr

type cond =
  | Bool of bool
  | Eq of expr * expr
  | Le of expr * expr

type imp =
  | Assign of string * expr
  | If of cond * imp list * imp list
  | While of cond * imp list

type prog = imp list

end

(** {2 Signature of abstract domains *)

module type IntegerDom = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val is_in : int -> t -> bool
  val incl : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val top : t
  val bot : t
  val const : int -> t
  val to_string : t -> string
end

module type StoreDom = sig
  module D : IntegerDom
  type t
  val set : t -> string -> D.t -> t
  val get : t -> string -> D.t
  val join : t -> t -> t
  val incl : t -> t -> bool
  val top : t
  val bot : t
  val print : t -> unit
end

module AStore (D : IntegerDom) : StoreDom with module D = D = struct
  module D = D
  type t =
    | Bot
    | Top_but of (string * D.t) list

  let set (s : t) (x : string) (d : D.t) =
    match s with
    | Bot -> Bot
    | Top_but s -> Top_but ((x, d)::List.remove_assoc x s)

  let get (s : t) (x : string) =
    match s with
    | Bot -> D.bot
    | Top_but s ->
      match List.assoc_opt x s with
      | Some d -> d
      | None -> D.top

  let bot = Bot

  let top = Top_but []

  module VSet = Set.Make(String)

  let vars l = List.fold_left (fun v (x, _) -> VSet.add x v) VSet.empty l

  let join (s1 : t) (s2 : t) =
    match s1, s2 with
    | Bot, _ -> s2
    | _, Bot -> s1
    | Top_but s1', Top_but s2' ->
      let vs = VSet.union (vars s1') (vars s2') in
      Top_but (VSet.fold (fun x m -> (x, D.join (get s1 x) (get s2 x))::m) vs [])

  let incl (s1 : t) (s2 : t) =
    match s1, s2 with
    | Bot, _ -> true
    | _, Bot -> false
    | Top_but _, Top_but s ->
      List.for_all (fun (x, d) -> D.incl (get s2 x) d) s

  let print (s : t) =
    match s with
    | Bot -> Printf.printf "'invalid state'"
    | Top_but s ->
      List.iter (fun (x, d) ->
        Printf.printf "#%s = %s\n" (D.to_string d) x
      ) s

end

(** {2 a simple integer interval domain } *)

module IntInterval : IntegerDom = struct

type t = (int * int) option

let is_in x = function 
  | Some (a, b) -> a <= x && x <= b
  | None -> false

let add i1 i2 = 
  match i1, i2 with
  | None, _ | _, None -> None
  | Some (a, b), Some (c, d) ->
    Some (a + c, b + d)

let sub i1 i2 =
  match i1, i2 with
  | None, _ | _, None -> None
  | Some (a, b), Some (c, d) -> Some (a - d, b - c)

let incl i1 i2 =
  match i1, i2 with
  | None, _ -> true
  | _, None -> false
  | Some (a, b), Some (c, d) -> c <= a && b <= d

let join i1 i2 =
  match i1, i2 with
  | None, _ -> i2
  | _, None -> i1
  | Some (a, b), Some (c, d) -> Some (min a c, max b d)

let meet i1 i2 =
  match i1, i2 with
  | None, _ -> None
  | _, None -> None
  | Some (a, b), Some (c, d) -> Some (max a c, min b d)

let top = Some (min_int, max_int)

let bot = None

let const n = Some (n, n)

let to_string i =
  match i with
  | None -> "{}"
  | Some (a, b) ->
    let str i =
      if i = max_int then "+oo"
      else if i = min_int then "-oo"
      else string_of_int i
    in
    "[" ^ (str a) ^ ", " ^ (str b) ^ "]"

end

(** {2 postfixpoint algorithm } *)

module Pfp (D : sig
  type t
  val top : t
  val bot : t
  val incl : t -> t ->bool
end) : sig
  val set_max_iter : int -> unit
  val compute : (D.t -> D.t) -> D.t
end = struct
  let max_iter = ref 10

  let set_max_iter i = max_iter := i
  
  let rec iter i (f : D.t -> D.t) x =
    if i = 0 then D.top
    else
      let x' = f x in
      if D.incl x' x then x'
      else iter (i - 1) f (f x)
  
  let compute f =
    iter !max_iter f D.bot
end

(** {2 Analyzer } *)

module Analyzer (ST : StoreDom) = struct
  module D = ST.D
  module P = Pfp(ST)
  open Imp

  let rec eval (s : ST.t) (e : expr) =
    match e with
    | Cst n -> D.const n
    | Var x -> ST.get s x
    | Add (e1, e2) -> D.add (eval s e1) (eval s e2)
    | Sub (e1, e2) -> D.sub (eval s e1) (eval s e2)

  let rec assume_eval (s : ST.t) (res : D.t) (e : expr) =
    match e with
    | Cst n -> if D.is_in n res then s else ST.bot
    | Var v -> ST.set s v (D.meet res (ST.get s v))
    | Add (x, y) ->
      let dx = eval s x in
      let dy = eval s y in
      let resx = D.meet dx (D.sub res dy) in
      let resy = D.meet dy (D.sub res dx) in
      assume_eval (assume_eval s resy y) resx x
    | Sub (x, y) ->
      let dx = eval s x in
      let dy = eval s y in
      let resx = D.meet dx (D.add dy res) in
      let resy = D.meet dy (D.sub dx res) in
      assume_eval (assume_eval s resy y) resx x
    
  let assume_test (s : ST.t) (b : bool) (c : cond) =
    match c with
    | Bool b' -> if b = b' then s else ST.bot
    | _ -> s

  let rec run (s : ST.t) (p : prog) =
    List.fold_left step s p
  and step (s : ST.t) (x : imp) =
    match x with
    | Assign (x, e) -> ST.set s x (eval s e)
    | If (c, br1, br2) ->
      ST.join (run (assume_test s true c) br1) (run (assume_test s false c) br2)
    | While (_, p) -> P.compute (fun x -> ST.join s (run x p))
end

let test = [
  Imp.If (Bool true, 
    [Assign ("x", Cst 1); Assign ("y", Cst 10)],
    [Assign ("x", Cst 3)]);
  Assign ("z", Add (Var "x", Var "y"));
]

module ST = AStore(IntInterval)
module A = Analyzer(ST)

let () = A.run ST.top test |> ST.print