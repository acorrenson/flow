(** A tinny abstract interpreter for fun ! *)

open Flow_lib
open Dom
open Mem

(** {2 Language definition }  *)


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


(** {2 a simple integer interval domain } *)
module IntInterval : Dom.IntDom = struct

  type t = (int * int) option

  let is_in x = function 
    | Some (a, b) -> a <= x && x <= b
    | None -> false

  let le i1 i2 =
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

  let add i1 i2 = 
    match i1, i2 with
    | None, _ | _, None -> None
    | Some (a, b), Some (c, d) ->
      Some (a + c, b + d)

  let sub i1 i2 =
    match i1, i2 with
    | None, _ | _, None -> None
    | Some (a, b), Some (c, d) -> Some (a - d, b - c)

  let add_inv i1 i2 i3 = (sub i3 i2, sub i3 i1)

  let sub_inv i1 i2 i3 = (add i3 i2, sub i1 i3)

  let le_inv i1 i2 =
    match i1, i2 with
    | None, _ | _, None -> bot, bot
    | Some (a, b), Some (a', b') ->
      if a > b' then bot, bot
      else Some (a, min b b'), Some (min a a', b')


  let eq_inv i1 i2 =
    match i1, i2 with
    | None, _ | _, None -> bot, bot
    | Some _, Some _ -> (meet i1 i2, meet i1 i2)


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


(** {2 Analyzer } *)
module Analyzer (D : IntDom) = struct
  module ST = MemoryDom(D)
  module P = Fix.Pfp(ST)

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
      let dx, dy = D.add_inv (eval s x) (eval s y) res in
      assume_eval (assume_eval s dy y) dx x
    | Sub (x, y) ->
      let dx, dy = D.sub_inv (eval s x) (eval s y) res in
      assume_eval (assume_eval s dy y) dx x

  let assume_test (s : ST.t) (b : bool) (c : cond) =
    match c with
    | Bool b' -> if b = b' then s else ST.bot
    | Eq (x, y) when b ->
      let (dx, dy) = D.eq_inv (eval s x) (eval s y) in
      assume_eval (assume_eval s dy y) dx x
    | Le (x, y) when b ->
      let (dx, dy) = D.le_inv (eval s x) (eval s y) in
      assume_eval (assume_eval s dy y) dx x
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
  If (Le (Var "x", Cst 10), 
          [If (Eq (Var "x", Cst 11), [Assign ("z", Cst 1)], [Assign ("z", Cst 2)])],
          [Assign ("z", Cst 3)]);
]

module A = Analyzer(IntInterval)

let () = A.run A.ST.top test |> A.ST.print