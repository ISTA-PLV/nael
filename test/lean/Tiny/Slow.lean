theorem before : True := trivial

#eval IO.sleep 3000

theorem after : 1 + 1 = 2 := by
  rfl
