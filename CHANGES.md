FPTaylor 0.9.3
--------------

- Minimal version of OCaml is increased to 4.03.

- [FEATURE] --print-hex-floats: Printing important floating-point values as 
  exact hexadecimal numbers.

- [FEATURE] --export-options: Exporting all options into a given file.

- [FEATURE] --print-precision: Error bounds are printed as correctly rounded
  decimal numbers with the given number of digits.

- Moved `tools/plot.py` to `scripts/plot/plot.py`.

- Added regression tests.

- [FEATURE] bb-eval optimization: the same as bb but does not require
  an OCaml compiler to be installed.

FPTaylor 0.9.2
--------------

- [FEATURE] ULP error (experimental; almost always suboptimal near powers of 2).

- [FEATURE, can break existing FPTaylor input files]
  The rounding operation `rnd` without parameters can be used
  in input files. The rounding type is defined by the
  `default-rnd` option (the default value is `rnd64`).
  Parameters of rounding operations must be given in square brackets.
  (Parameters were in parentheses before.)

- [FEATURE, can break existing FPTaylor input files]
  Variables without explicit types are assigned the default type defined
  by the `default-var-type` option (the default value is `float64`).
  (Variables without explicit types were assumed to be of the `real` type before.)

FPTaylor 0.9.1
--------------

- [FEATURE] `tools/plot.py`: a script for plotting 1d error models.

- [FEATURE] Exporting ErrorBounds input files.

- [FEATURE] Exporting to the FPCore format.

- [FEATURE] Several tasks in one files (inside curly braces).

- [FIX] Correct rounding of constants which are representable as `float64`.
  It was possible to get incorrect results for rounding modes
  different from `rnd64`. For instance, the result of `a = rnd32(1p+50 + 1)` 
  was `err = 0`.
