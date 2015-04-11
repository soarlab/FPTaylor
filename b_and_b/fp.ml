open Interval

(* For a given positive floating-point number f,
   returns the largest floating-point number 2^n such that
   2^n < f.
*)
let floor_power2 =
  let p2 f =
    let s, q = frexp f in
    if s = 0.5 then
      ldexp 1.0 (q - 2)
    else
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

let sym_interval_float f = 0.0

let sym_interval_I x = 
  let f = (abs_I x).high in {
    low = -.f;
    high = f;
  }

let sub2 (x, y) = 
  if (0.5 *. x <= y && y <= 2.0 *. x) then 0.0 else x -. y

let sub2_I (x, y) = {
  low = sub2 (x.low, y.high);
  high = sub2 (x.high, y.low);
}
