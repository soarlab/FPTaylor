open Interval

(* For a given positive floating-point number f,
   returns the largest floating-point number 2^n such that
   2^n <= f.
*)
let floor_power2 =
  let p2 f =
    let _, q = frexp f in
    ldexp 1.0 (q - 1) in
  fun f ->
    match (classify_float f) with
      | FP_zero -> f
      | FP_infinite -> f
      | FP_nan -> f
      | FP_normal | FP_subnormal -> 
	if f < 0.0 then -.p2 (-.f) else p2 f

let floor_power2_I x = {
  low = floor_power2 x.low;
  high = floor_power2 x.high
}

