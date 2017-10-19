FPTaylor 0.9.1 (working version)
----------------

- [FEATURE] Several tasks in one files (inside curly braces).

- [FIX] Correct rounding of constants which are representable as `float64`.
  It was possible to get incorrect results for rounding modes
  different from `rnd64`. For instance, the result of `a = rnd32(1p+50 + 1)` 
  was `err = 0`.
