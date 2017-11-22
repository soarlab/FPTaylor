(* ========================================================================== *)
(*      A simple OCaml interval library                                       *)
(*      https://github.com/monadius/ocaml_simple_interval                     *)
(*                                                                            *)
(*      Author: Alexey Solovyev                                               *)
(*      https://github.com/monadius                                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

open Num

let u_float = ldexp 1.0 (-53)

let eta_float = ldexp 1.0 (-1074)

let phi_float = u_float *. (1.0 +. 2.0 *. u_float)

let inv_u_float = 1.0 /. u_float

let bound1_float = 0.5 *. (eta_float /. (u_float *. u_float))

let bound2_float = eta_float /. u_float
                                    
let min_float2 = 2.0 *. min_float
               
let _ = assert (min_float = 0.5 *. (1.0 /. u_float) *. eta_float)
let _ = assert (min_float2 = ldexp 1.0 (-1021))
let _ = assert (bound1_float = ldexp 1.0 (-969))
let _ = assert (bound2_float = ldexp 1.0 (-1021))
               
(* fsucc and fpred from the RZBM09 paper *)
(* Algorithm 2 *)
               
let fsucc x =
  let c = abs_float x in
  if c >= bound1_float then
    x +. phi_float *. c
  else if c < bound2_float then
    x +. eta_float
  else
    let y = inv_u_float *. x in
    let e = phi_float *. abs_float y in
    (y +. e) *. u_float

let fpred x =
  let c = abs_float x in
  if c >= bound1_float then
    x -. phi_float *. c
  else if c < bound2_float then
    x -. eta_float
  else
    let y = inv_u_float *. x in
    let e = phi_float *. abs_float y in
    (y -. e) *. u_float

let is_finite x = neg_infinity < x && x < infinity

let num_of_float x =
  if x = 0. then Int 0
  else if is_finite x then
    begin
      let m, e = frexp x in
      let t = Int64.of_float (ldexp m 53) in
      num_of_big_int (Big_int.big_int_of_int64 t) */ (Int 2 **/ Int (e - 53))
    end
  else
    failwith (Printf.sprintf "num_of_float: %e" x)

(* Returns the integer binary logarithm of big_int  *)
(* Returns -1 for non-positive numbers              *)
let log2_big_int_simple =
  let rec log2 acc k =
    if Big_int.sign_big_int k <= 0 then acc
    else log2 (acc + 1) (Big_int.shift_right_big_int k 1) in
  log2 (-1)

let log2_big_int =
  let p = 32 in
  let u = Big_int.power_int_positive_int 2 p in
  let rec log2 acc k =
    if Big_int.ge_big_int k u then
      log2 (acc + p) (Big_int.shift_right_big_int k p)
    else
      acc + log2_big_int_simple k in
  log2 0

(* Returns the integer binary logarithm of the absolute value of num *)
let log2_num r =
  let log2 r = log2_big_int (big_int_of_num (floor_num r)) in
  let r = abs_num r in
  if r </ Int 1 then
    let t = -log2 (Int 1 // r) in
    if (Int 2 **/ Int t) =/ r then t else t - 1
  else log2 r

let float_of_pos_num_lo r =
  assert (sign_num r >= 0);
  if sign_num r = 0 then 0.0
  else begin
      let n = log2_num r in
      let k = min (n + 1074) 52 in
      if k < 0 then 0.0
      else
        let m = big_int_of_num (floor_num ((Int 2 **/ Int (k - n)) */ r)) in
        let f = Int64.to_float (Big_int.int64_of_big_int m) in
        let x = ldexp f (n - k) in
        if x = infinity then max_float else x
    end

let float_of_pos_num_hi r =
  assert (sign_num r >= 0);
  if sign_num r = 0 then 0.0
  else begin
      let n = log2_num r in
      let k = min (n + 1074) 52 in
      if k < 0 then ldexp 1.0 (-1074)
      else
        let t = (Int 2 **/ Int (k - n)) */ r in
        let m0 = floor_num t in
        let m = if t =/ m0 then big_int_of_num m0
                else Big_int.succ_big_int (big_int_of_num m0) in
        let f = Int64.to_float (Big_int.int64_of_big_int m) in
        ldexp f (n - k)
    end
        
let float_of_num_lo r =
  if sign_num r < 0 then
    -.float_of_pos_num_hi (minus_num r)
  else
    float_of_pos_num_lo r

let float_of_num_hi r =
  if sign_num r < 0 then
    -.float_of_pos_num_lo (minus_num r)
  else
    float_of_pos_num_hi r

let round_hi z r =
  if z = neg_infinity then -.max_float
  else if z = infinity then z
  else
    let rz = num_of_float z in
    if compare_num rz r >= 0 then z else fsucc z

let round_lo z r =
  if z = infinity then max_float
  else if z = neg_infinity then z
  else
    let rz = num_of_float z in
    if compare_num rz r <= 0 then z else fpred z

(* Correctly rounded fadd_low and fadd_high operations from JInterval *)
                                               
let fadd_low x y =
  let z = x +. y in
  if z = infinity then max_float
  else
    if y < z -. x || x < z -. y then fpred z else z

let fadd_high x y =
  let z = x +. y in
  if z = neg_infinity then -.max_float
  else
    if z -. x < y || z -. y < x then fsucc z else z

let fsub_low x y = fadd_low x (-.y)

let fsub_high x y = fadd_high x (-.y)

(* Correctly rounded fmul_low and fmul_high are based on results from
   S. Boldo's formal verification of Dekker algorithm *)

let factor = ldexp 1. 27 +. 1.
let max_product = fpred (ldexp 1. 1021)
let min_product = fsucc (ldexp 1. (-969))
let max_factor = ldexp 1. 995
         
let two_product_err x y xy =
  let px = x *. factor in
  let qx = x -. px in
  let hx = px +. qx in
  let tx = x -. hx in
  let py = y *. factor in
  let qy = y -. py in
  let hy = py +. qy in
  let ty = y -. hy in
  let r2 = hx *. hy -. xy in
  let r2 = r2 +. hx *. ty in
  let r2 = r2 +. hy *. tx in
  r2 +. tx *. ty

let fmul_low x y =
  if x = 0. || y = 0. then 0.
  else
    let z = x *. y in
    let az = abs_float z in
    if abs_float x <= max_factor && abs_float y <= max_factor
       && min_product <= az && az <= max_product then
      begin
        let r = two_product_err x y z in
        if r >= 0. then z else fpred z
      end
    else if z = infinity then max_float
    else if z = neg_infinity then z
    else
      let r = num_of_float x */ num_of_float y in
      round_lo z r

let fmul_high x y =
  if x = 0. || y = 0. then 0.
  else
    let z = x *. y in
    let az = abs_float z in
    if abs_float x <= max_factor && abs_float y <= max_factor
       && min_product <= az && az <= max_product then
      begin
        let r = two_product_err x y z in
        if r <= 0. then z else fsucc z
      end
    else if z = neg_infinity then -.max_float
    else if z = infinity then z
    else
      let r = num_of_float x */ num_of_float y in
      round_hi z r

let fdiv_low_pos x y =
  assert (x >= 0. && y > 0.);
  let z = x /. y in
  if z = infinity then max_float
  else if z = 0. then 0.
  else
    if fmul_high y z <= x then z else fpred z

let fdiv_high_pos x y =
  assert (x >= 0. && y > 0.);
  let z = x /. y in
  if z = infinity then infinity
  else if z = 0. then
    if x = 0. then 0. else eta_float
  else
    if x <= fmul_low y z then z else fsucc z
    
let fdiv_low x y =
  if x >= 0. then
    if y >= 0. then
      fdiv_low_pos x y
    else
      -.fdiv_high_pos x (-.y)
  else
    if y <= 0. then
      fdiv_low_pos (-.x) (-.y)
    else
      -.fdiv_high_pos (-.x) y

let fdiv_high x y =
  if x >= 0. then
    if y >= 0. then
      fdiv_high_pos x y
    else
      -.fdiv_low_pos x (-.y)
  else
    if y <= 0. then
      fdiv_high_pos (-.x) (-.y)
    else
      -.fdiv_low_pos (-.x) y
      
let sqr_product_err x xx =
  let px = x *. factor in
  let qx = x -. px in
  let hx = px +. qx in
  let tx = x -. hx in
  let r2 = hx *. hx -. xx in
  let r2 = r2 +. hx *. tx in
  let r2 = r2 +. hx *. tx in
  r2 +. tx *. tx

let fsqr_low x =
  let z = x *. x in
  if min_product <= z && z <= max_product then
    let r = sqr_product_err x z in
    if r >= 0. then z else fpred z
  else if z = 0. then 0.
  else if z = infinity then max_float
  else
    let t = num_of_float x in
    let r = t */ t in
    round_lo z r

let fsqr_high x =
  let z = x *. x in
  if min_product <= z && z <= max_product then
    let r = sqr_product_err x z in
    if r <= 0. then z else fsucc z
  else if z = 0. then
    if x = 0. then 0. else eta_float
  else if z = infinity then z
  else
    let t = num_of_float x in
    let r = t */ t in
    round_hi z r
         
let fsqrt_low x =
  if x < 0. then nan
  else if x = infinity then max_float
  else
    let z = sqrt x in
    if fsqr_high z <= x then z else fpred z

let fsqrt_high x =
  if x < 0. then nan
  else if x = infinity then infinity
  else
    let z = sqrt x in
    if fsqr_low z >= x then z else fsucc z

(* We assume that x^0 = 1 for any x *)
let fpown_low x n =
  match n with
  | 0 -> 1.
  | 1 -> x
  | 2 -> fsqr_low x
  | n when x = 0. -> if n < 0 then nan else 0.
  | n when is_finite x ->
     let r = num_of_float x **/ Int n in
     float_of_num_lo r
  | _ -> begin
      if x = infinity then
        if n < 0 then 0. else max_float
      else if n land 1 = 0 then 0.
      else neg_infinity
    end
  
let fpown_high x n =
  match n with
  | 0 -> 1.
  | 1 -> x
  | 2 -> fsqr_high x
  | n when x = 0. -> if n < 0 then nan else 0.
  | n when is_finite x ->
     let r = num_of_float x **/ Int n in
     float_of_num_hi r
  | _ -> begin
      if x = infinity then infinity
      else if n land 1 = 1 then 0.0
      else infinity
    end
     
let fexp_low x =
  let r = exp x in
  if r = infinity then max_float
  else if r > 0. then fpred r
  else 0.

let fexp_high x = fsucc (exp x)

let flog_low x =
  if x = 1. then 0.
  else
    let r = log x in
    if r = infinity then max_float
    else fpred r

let flog_high x =
  if x = 1. then 0.
  else
    let r = log x in
    if r = neg_infinity then -.max_float
    else fsucc r

(* Interval type and functions *)

(* [0, +infinity] contains all finite positive numbers, etc. *)
(* [+infinity, -infinity] represents the only valid empty interval *)

type interval = {
    low : float;
    high : float
  }

let empty_interval = {low = infinity; high = neg_infinity}

let entire_interval = {low = neg_infinity; high = infinity}
                       
let zero_interval = {low = 0.0; high = 0.0}

let one_interval = {low = 1.0; high = 1.0}

let is_empty {low; high} = (low = infinity && high = neg_infinity)

let is_entire {low; high} = (low = neg_infinity && high = infinity)
                          
let is_valid ({low; high} as v) =
  (low <= high && low < infinity && neg_infinity < high) || is_empty v

let make_interval a b = {low = a; high = b}

let mid_i {low = a; high = b} =
  if a = neg_infinity then
    if b = infinity then 0. else -.max_float
  else if b = infinity then max_float
  else
    let m = 0.5 *. (a +. b) in
    if m = infinity || m = neg_infinity then
      0.5 *. a +. 0.5 *. b
    else m
                          
let neg_i {low = a; high = b} = {
    low = -.b;
    high = -.a;
  }
      
let abs_i ({low = a; high = b} as v) =
  if 0. <= a || is_empty v then v
  else if b <= 0. then
   {low = -.b; high = -.a}
  else
    {low = 0.; high = max (-.a) b}

let max_ii ({low = a; high = b} as v) ({low = c; high = d} as w) =
  if is_empty v || is_empty w then empty_interval
  else {
      low = if a <= c then c else a;
      high = if b <= d then d else b;
    }

let min_ii ({low = a; high = b} as v) ({low = c; high = d} as w) =
  if is_empty v || is_empty w then empty_interval
  else {
      low = if a <= c then a else c;
      high = if b <= d then b else d;
    }

let add_ii ({low = a; high = b} as v) ({low = c; high = d} as w) =
  if is_empty v || is_empty w then empty_interval
  else {
      low = fadd_low a c;
      high = fadd_high b d
    }

let add_id ({low = a; high = b} as v) c =
  if is_empty v then empty_interval
  else {
      low = fadd_low a c;
      high = fadd_high b c;
    }

let add_di c ({low = a; high = b} as v) =
  if is_empty v then empty_interval
  else {
      low = fadd_low c a;
      high = fadd_high c b;
    }

let sub_ii ({low = a; high = b} as v) ({low = c; high = d} as w) =
  if is_empty v || is_empty w then empty_interval
  else {
      low = fsub_low a d;
      high = fsub_high b c;
    }

let sub_id ({low = a; high = b} as v) c =
  if is_empty v then empty_interval
  else {
      low = fsub_low a c;
      high = fsub_high b c;
    }

let sub_di c ({low = a; high = b} as v) =
  if is_empty v then empty_interval
  else {
      low = fsub_low c b;
      high = fsub_high c a;
    }

let mul_ii ({low = a; high = b} as v) ({low = c; high = d} as w) =
  if is_empty v || is_empty w then empty_interval
  else if a >= 0.0 then {
      low = (if c >= 0.0 then fmul_low a c else fmul_low b c);
      high = (if d >= 0.0 then fmul_high b d else fmul_high a d);
    }
  else if b <= 0.0 then {
      low = (if d <= 0.0 then fmul_low b d else fmul_low a d);
      high = (if c <= 0.0 then fmul_high a c else fmul_high b c);
    }
  else if c >= 0.0 then {
      low = fmul_low a d;
      high = fmul_high b d;
    }
  else if d <= 0.0 then {
      low = fmul_low b c;
      high = fmul_high a c;
    }
  else {
      low = min (fmul_low a d) (fmul_low b c);
      high = max (fmul_high a c) (fmul_high b d);
    }
      
let mul_id ({low = a; high = b} as v) c =
  if is_empty v then empty_interval
  else if c > 0.0 then {
      low = fmul_low a c;
      high = fmul_high b c;
    }
  else if c < 0.0 then {
      low = fmul_low b c;
      high = fmul_high a c;
    }
  else if c = 0.0 then {
      low = 0.0;
      high = 0.0;
    }
  else {
      low = nan;
      high = nan;
    }
                    
let mul_di c i = mul_id i c
    
let div_ii ({low = a; high = b} as v) ({low = c; high = d} as w) =
  if is_empty v || is_empty w || (c = 0. && d = 0.) then
    empty_interval
  else if c > 0.0 then {
      low = (if a >= 0.0 then fdiv_low a d else fdiv_low a c);
      high = (if b <= 0.0 then fdiv_high b d else fdiv_high b c);
    }
  else if d < 0.0 then {
      low = (if b <= 0.0 then fdiv_low b c else fdiv_low b d);
      high = (if a >= 0.0 then fdiv_high a c else fdiv_high a d);
    }
  else if a = 0. && b = 0. then zero_interval
  else if c = 0. then {
      low = (if a >= 0. then fdiv_low a d else neg_infinity);
      high = (if b <= 0. then fdiv_high b d else infinity);
    }
  else if d = 0. then {
      low = (if b <= 0. then fdiv_low b c else neg_infinity);
      high = (if a >= 0. then fdiv_high a c else infinity);
    }
  else entire_interval

let div_id ({low = a; high = b} as v) c =
  if is_empty v then empty_interval
  else if c > 0.0 then {
      low = fdiv_low a c;
      high = fdiv_high b c;
    }
  else if c < 0.0 then {
      low = fdiv_low b c;
      high = fdiv_high a c;
    }
  else empty_interval

let div_di a w =
  if is_finite a then div_ii {low = a; high = a} w
  else {low = nan; high = nan}

let inv_i ({low = a; high = b} as v) =
  if is_empty v then empty_interval
  else if 0. < a || b < 0. then {
      low = fdiv_low 1. b;
      high = fdiv_high 1. a;
    }
  else if a = 0. then begin
      if b = 0. then empty_interval
      else {
          low = fdiv_low 1. b;
          high = infinity;
        }
    end
  else if b = 0. then {
      low = neg_infinity;
      high = fdiv_high 1. a;
    }
  else entire_interval
         
let sqrt_i ({low = a; high = b} as v) =
  if b < 0. || is_empty v then empty_interval
  else {
      low = if a <= 0. then 0. else fsqrt_low a;
      high = fsqrt_high b;
    }

let sqr_i ({low = a; high = b} as v) =
  if is_empty v then empty_interval
  else if a >= 0. then
    {low = fsqr_low a; high = fsqr_high b}
  else if b <= 0. then
    {low = fsqr_low b; high = fsqr_high a}
  else
    let t = max (-.a) b in
    {low = 0.; high = fsqr_high t}

let pown_i ({low = a; high = b} as v) n =
  if is_empty v then empty_interval
  else
    match n with
    | 0 -> one_interval
    | 1 -> v
    | 2 -> sqr_i v
    | -1 -> inv_i v
    | n when (n land 1 = 1) -> begin
        if n > 0 then
          {low = fpown_low a n; high = fpown_high b n}
        else begin
            if a = 0. && b = 0. then empty_interval
            else if a >= 0. then {
                low = fpown_low b n;
                high = if a = 0. then infinity else fpown_high a n;
              }
            else if b <= 0. then {
                low = if b = 0. then neg_infinity else fpown_low b n;
                high = fpown_high a n;
              }
            else entire_interval
          end
      end
    | _ -> begin
        if n > 0 then begin
            if a >= 0. then
              {low = fpown_low a n; high = fpown_high b n}
            else if b <= 0. then
              {low = fpown_low b n; high = fpown_high a n}
            else
              let t = max (-.a) b in
              {low = 0.; high = fpown_high t n}
          end
        else begin
            if a = 0. && b = 0. then empty_interval
            else if a >= 0. then {
                low = fpown_low b n;
                high = if a = 0. then infinity else fpown_high a n;
              }
            else if b <= 0. then {
                low = fpown_low a n;
                high = if b = 0. then infinity else fpown_high b n;
              }
            else {
                low = fpown_low (max (-.a) b) n;
                high = infinity;
              }
          end
      end
         
let exp_i ({low = a; high = b} as v) =
  if is_empty v then empty_interval
  else {
    low = fexp_low a;
    high = fexp_high b;
  }

let log_i ({low = a; high = b} as v) =
  if b < 0. || is_empty v then empty_interval
  else {
    low = if a <= 0. then neg_infinity else flog_low a;
    high = flog_high b;
  }

let sin_i {low = a; high = b} =
  if a >= -1.57 && b <= 1.57 then {
    low = fpred (sin a);
    high = fsucc (sin b);
  }
  else
    failwith "sin_i: Not implemented"

let cos_i {low = a; high = b} =
  if a >= 0. && b <= 3.14159 then {
    low = fpred (cos b);
    high = max (fsucc (cos a)) 1.0;
  }
  else
    failwith "cos_i: Not implemented"
