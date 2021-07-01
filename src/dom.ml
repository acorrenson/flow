(** Semi lattices *)
module type SemiLattice = sig
  type t
  val join : t -> t -> t
  val bot : t
  val top : t
  val le : t -> t -> bool
end

module type Lattice = sig
  include SemiLattice
  val meet : t -> t -> t
end


(** Module type for arbitrary abstract domains *)
module type Dom = sig

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

  val le_inv : t -> t -> t * t
  (** refinement of domains assuming ordering *)

  val to_string : t -> string
end

(** Integer domains *)
module type IntDom = sig
  include Dom
  
  val is_in : int -> t -> bool
  (** concretization *)

  val const : int -> t
  (** singleton *)
end