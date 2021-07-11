Require Import Lattice.

Module Type Dom.

  include Lattice

  val add : t -> t -> t
  (** abstract addition *)

  val sub : t -> t -> t
  (** abstract subtraction *)

  val add_inv : t -> t -> t -> t * t
  (** backward addition *)


  val sub_inv : t -> t -> t -> t * t
  (** backward subtraction *)

  val eq_inv : t -> t -> t * t
  (** refinement of domains assuming equality *)

  val ne_inv : t -> t -> t * t
  (** refinement of domains assuming no equality *)

  val le_inv : t -> t -> t * t
  (** refinement of domains assuming ordering *)

  val gt_inv : t -> t -> t * t
  (** refinement of domains assuming ordering *)

  val to_string : t -> string
end