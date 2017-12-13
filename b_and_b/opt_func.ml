open Interval

let asinh x = log (x +. sqrt (x *. x +. 1.0))

let acosh x = log (x +. sqrt (x *. x -. 1.0))

let atanh x = 0.5 *. log ((1.0 +. x) /. (1.0 -. x))

let asinh_I x = {
  low = 
    (let sqrt = Fpu.fsqrt_low (Fpu.fadd_low (Fpu.fmul_low x.low x.low) 1.0) in
     Fpu.flog_low (Fpu.fadd_low x.low sqrt));
  high =
    (let sqrt = Fpu.fsqrt_high (Fpu.fadd_high (Fpu.fmul_high x.high x.high) 1.0) in
     Fpu.flog_high (Fpu.fadd_high x.high sqrt));
}

let acosh_I x = 
  if x.high < 1.0 then failwith "acosh_I"
  else {
    low =
      (if x.low <= 1.0 then 0.0
       else
	  let sqrt = Fpu.fsqrt_low (Fpu.fsub_low (Fpu.fmul_low x.low x.low) 1.0) in
	  Fpu.flog_low (Fpu.fadd_low x.low sqrt));
    high = 
      (let sqrt = Fpu.fsqrt_high (Fpu.fsub_high (Fpu.fmul_high x.high x.high) 1.0) in
       Fpu.flog_high (Fpu.fadd_high x.high sqrt));
  }

let atanh_I x =
  if x.high < -1.0 || x.low > 1.0 then failwith "atanh_I"
  else {
    low = 
      (if x.low <= -1.0 then neg_infinity
       else
	  let t = Fpu.fdiv_low (Fpu.fadd_low 1.0 x.low) (Fpu.fsub_high 1.0 x.low) in
	  Fpu.fmul_low 0.5 (Fpu.flog_low t));
    high =
      (if x.high >= 1.0 then infinity
       else
	  let t = Fpu.fdiv_high (Fpu.fadd_high 1.0 x.high) (Fpu.fsub_low 1.0 x.high) in
	  Fpu.fmul_high 0.5 (Fpu.flog_high t));
  }	

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

let goldberg_ulp (prec, e_min) =
  let ulp f =
    let _, e = frexp f in
    ldexp 1.0 (max e (e_min + 1) - prec) in
  fun f ->
    match (classify_float f) with
    | FP_zero | FP_infinite | FP_nan -> f
    | FP_subnormal | FP_normal ->
      if f < 0. then -.ulp(-.f) else ulp f

let goldberg_ulp_I pars x = {
  low = goldberg_ulp pars x.low;
  high = goldberg_ulp pars x.high
}

let sub2 (x, y) = 
  if (0.5 *. x <= y && y <= 2.0 *. x) || 
     (2.0 *. x <= y && y <= 0.5 *. x) 
  then 0.0 
  else x -. y

let sub2_I (x, y) = {
  low = if (0.5 *. x.low <= y.high && y.high <= 2.0 *. x.low) ||
           (2.0 *. x.low <= y.high && y.high <= 0.5 *. x.low) 
        then 0.0 
        else Fpu.fsub_low x.low y.high;
  high = if (0.5 *. x.high <= y.low && y.low <= 2.0 *. x.high) ||
            (2.0 *. x.high <= y.low && y.low <= 0.5 *. x.high)
         then 0.0 
         else Fpu.fsub_high x.high y.low;
}

let abs_err (t, x) =
  if x >= t then 1.
  else if x <= -.t then -1.
  else 0. (* should be [-1, 1] but we cannot return an interval here; 
             this function is not important *)

let neg_one_I = {low = -1.; high = -1.}
let neg_one_one_I = {low = -1.; high = 1.}
    
let abs_err_I (t, x) =
  if x.low >= t.high then one_I
  else if x.high <= t.low then neg_one_I
  else neg_one_one_I
    
         
