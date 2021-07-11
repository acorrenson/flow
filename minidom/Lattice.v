Module Type SemiLattice.
  Parameter t : Type.
  Parameter join : t -> t -> t.
  Parameter bot : t.
  Parameter top : t.
  Parameter le : t -> t -> bool.
  
  Axiom bot_correct :
    forall d:t, le bot d = true.

  Axiom top_correct :
    forall d:t, le d top = true.

  Axiom join_correct_l :
    forall d d':t, le d (join d d') = true.

  Axiom join_correct_r :
    forall d d':t, le d' (join d d') = true.

End SemiLattice.

Module Type Lattice.
  Include SemiLattice.
  Parameter meet : t -> t -> t.

  Axiom meet_correct_l :
    forall d d':t, le (meet d d') d = true.
  
  Axiom meet_correct_r :
    forall d d':t, le (meet d d') d = true.
End Lattice.