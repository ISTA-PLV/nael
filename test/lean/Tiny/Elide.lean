-- Goals in which parts of terms are elided as `⋯`.

-- A proof inside a structure (`pp.proofs`).
example (x : Fin 3) : (⟨x.val, x.isLt⟩ : Fin 3) = x := by
  rfl

-- A proof inside a subtype.
example (p : {n : Nat // n > 0}) : p = ⟨p.1, p.2⟩ := by
  rfl

-- A deep term (`pp.deepTerms`).
set_option pp.deepTerms false in
set_option pp.deepTerms.threshold 2 in
example (f : Nat → Nat) : f (f (f (f (f (f 0))))) = f (f (f (f (f (f 0))))) := by
  rfl
