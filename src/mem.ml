open Dom

module MemoryDom (D : Dom) : sig
  include SemiLattice
  val set : t -> string -> D.t -> t
  val get : t -> string -> D.t
  val print : t -> unit
end = struct
  
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
      let update x m = (x, D.join (get s1 x) (get s2 x))::m in
      Top_but (VSet.fold update vs [])

  let le (s1 : t) (s2 : t) =
    match s1, s2 with
    | Bot, _ -> true
    | _, Bot -> false
    | Top_but _, Top_but s ->
      List.for_all (fun (x, d) -> D.le (get s2 x) d) s

  let print (s : t) =
    match s with
    | Bot -> Printf.printf "'invalid state'"
    | Top_but s ->
      List.iter (fun (x, d) ->
        Printf.printf "#%s = %s\n" (D.to_string d) x) s
  
end