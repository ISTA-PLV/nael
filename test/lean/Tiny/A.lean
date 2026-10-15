import Tiny.B

theorem t (n : Nat) (h : n = b) : n + 0 = 1 := by
  rw [h]
  rfl
