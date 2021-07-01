(** A tinny abstract interpreter for fun ! *)

open Flow_lib
open Dom
open Mem
open Lang

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

  type table = (Pp.t * ST.t) list

  let reassoc m x v = (x, v)::List.remove_assoc x m

  let run (s : ST.t) (p : prog) =
    let tbl = ref [] in
    let bind pp s = tbl := reassoc !tbl pp s; s in
    let rec run_list (s : ST.t) (p : prog) =
      List.fold_left run_step s p
    and run_step (s : ST.t) (x : imp) =
      match x with
      | Assign (pp, x, e) ->
        bind pp @@ ST.set s x (eval s e)
      | If (pp, c, br1, br2) ->
        let s1 = run_list (assume_test s true c) br1 in
        let s2 = run_list (assume_test s false c) br2 in
        bind pp @@ ST.join s1 s2
      | While (pp, c, p) ->
        let fix x =
          let s' = assume_test x true c in
          ST.join s (run_list s' p)
        in
        bind pp @@ assume_test (P.compute fix) false c
    in
    !tbl, run_list s p

    let rec pp_expr fmt = function
      | Cst i -> Format.pp_print_int fmt i
      | Var x -> Format.pp_print_string fmt x
      | Add (e1, e2) ->
          Format.fprintf fmt "@[(%a + %a)@]" pp_expr e1 pp_expr e2
      | Sub (e1, e2) ->
        Format.fprintf fmt "@[(%a + %a)@]" pp_expr e1 pp_expr e2
      
    let pp_cond fmt = function
        | Bool b -> Format.pp_print_bool fmt b
        | Eq (e1, e2) ->
          Format.fprintf fmt "@[%a == %a@]" pp_expr e1 pp_expr e2
        | Le (e1, e2) ->
          Format.fprintf fmt "@[%a <= %a@]" pp_expr e1 pp_expr e2
    
    let rec pp_instr tbl fmt = function
      | Assign (_pp, x, e) ->
        Format.fprintf fmt "%s := %a" x pp_expr e
      | If (pp, c, br1, br2) ->
        Format.fprintf fmt "@[<v 2>if(%a) {@,%a@]@,@[<v 2>} else {@,%a@]@,}@,\x1b[31m%a\x1b[0m"
          pp_cond c (pp_prog tbl) br1 (pp_prog tbl) br2 ST.pp_print (List.assoc pp tbl)
      | _ -> ()

    and pp_prog tbl fmt = function
      | [] -> ()
      | [x] -> (pp_instr tbl) fmt x
      | x::xs -> Format.fprintf fmt "%a;@,%a" (pp_instr tbl) x (pp_prog tbl) xs

    let print_analysis p is (tbl, _s) =
      Format.printf "\x1b[31m%a\x1b[0m" ST.pp_print is;
      Format.printf "@.@[<v>%a@]@." (pp_prog tbl) p
end

let test = [
  If (Pp.new_pp (), Le (Var "x", Cst 10), 
      [
        If (Pp.new_pp (), Eq (Var "x", Cst 11),
          [Assign (Pp.new_pp (), "z", Cst 1)], 
          [Assign (Pp.new_pp (), "z", Cst 2)])
      ],
      [Assign (Pp.new_pp (), "z", Cst 3)]);
]

module A = Analyzer(IntInterval)

let watch = A.ST.watch_vars ["x"; "z"]

let () = A.run watch test |> A.print_analysis test watch

