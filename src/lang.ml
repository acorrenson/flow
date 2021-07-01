(** A small imperative language *)

type expr =
  | Cst of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr

type cond =
  | Bool of bool
  | Eq of expr * expr
  | Le of expr * expr

module Pp : sig
  type t
  (** Type of program points *)
  
  val new_pp : unit -> t
  (** Generate a unique program point *)
end = struct
  type t = int

  let counter = ref 0
  let new_pp () = incr counter; !counter
end

type imp =
  | Assign of Pp.t * string * expr
  | If of Pp.t * cond * imp list * imp list
  | While of Pp.t * cond * imp list

type prog = imp list