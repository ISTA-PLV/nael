#check Nat.add_comm

theorem unsolved (a b : Nat) : a + b = b + a ∧ True := by
  constructor
  · omega

set_option trace.Meta.synthInstance true in
example : Inhabited Nat := inferInstance

example (a b : Nat) : a + b = b + a := by
  exact?
