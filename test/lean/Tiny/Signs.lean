theorem done (n : Nat) : n + 0 = n := by
  simp

theorem open_goal (a b : Nat) : a + b = b + a ∧ True := by
  constructor
  · omega

theorem admitted (n : Nat) : n = n := by
  sorry

example : Nat :=
  (1 +
   2 +
   "hello")

#check (1 : String)
