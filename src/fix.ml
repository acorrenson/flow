open Dom

module Pfp (D : SemiLattice) : sig
  val set_max_iter : int -> unit
  val compute : (D.t -> D.t) -> D.t
end = struct

let max_iter = ref 10

let set_max_iter i = max_iter := i

let rec iter i (f : D.t -> D.t) x =
  if i = 0 then D.top
  else
    let x' = f x in
    if D.le x' x then x'
    else iter (i - 1) f (f x)

let compute f =
  iter !max_iter f D.bot

end