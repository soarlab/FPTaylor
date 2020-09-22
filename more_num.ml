(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Functions for rational and floating-point numbers                          *)
(* -------------------------------------------------------------------------- *)

open Interval
open Num

let numerator = function
  | Ratio r -> Ratio.numerator_ratio r
  | Big_int n -> n
  | Int n -> Big_int.big_int_of_int n

let denominator = function
  | Ratio r -> Ratio.denominator_ratio r
  | _ -> Big_int.unit_big_int

type gen_float = {
  base : int;
  significand : num;
  exponent : num;
}

let num_of_gen_float f =
  (* Bound the exponent to avoid huge numbers *)
  if f.exponent <=/ Int (-100000) || f.exponent >=/ Int 100000 then
    failwith "num_of_gen_float: the exponent is out of bounds"
  else
    f.significand */ (Int f.base **/ f.exponent)

let decode_num_str =
  let split_at str ch = 
    let n = String.length str in
    let i = try String.index str ch with Not_found -> -1 in
    if i < 0 || i >= n then
      str, ""
    else
      String.sub str 0 i,
      String.sub str (i + 1) (n - i - 1) 
  in
  let starts_with str prefix =
    let len_str = String.length str and
        len_p = String.length prefix in
    if len_p > len_str then
      false
    else
      String.sub str 0 len_p = prefix
  in
  let extract_exp str =
    let s1, s2 = split_at str 'p' in
    if s2 <> "" then
      s1, 2, s2
    else
      let s1, s2 = split_at str 'e' in
      if s2 <> "" then
        s1, 10, s2
      else
        s1, 10, "0"
  in
  let compute_exp_shift hex base s_frac =
    let n = String.length s_frac in
    if n = 0 then 0 else
      match (hex, base) with
  | true, 2 -> n * 4
  | false, 10 -> n
  | _ -> failwith "Fractional part is not allowed for the given base"
  in
  fun str ->
    let hex = starts_with str "0x" || starts_with str "-0x" in
    let s_significand, base, s_exp = extract_exp str in
    let s_int, s_frac = split_at s_significand '.' in
    let exp_shift = compute_exp_shift hex base s_frac in
    {
      base = base;
      significand = num_of_string (s_int ^ s_frac);
      exponent = num_of_string s_exp -/ Int exp_shift;
    }

let num_of_float_string str =
  num_of_gen_float (decode_num_str str)

let is_nan x = (compare x nan = 0)

let is_infinity x = (classify_float x = FP_infinite)

let num_of_float x =
  match classify_float x with
  | FP_nan | FP_infinite ->
     let msg = Printf.sprintf "num_of_float: %e" x in
     if Config.fail_on_exception then
       failwith msg
     else
       (Log.warning_str msg; Int 0)
  | FP_zero -> Int 0
  | FP_normal | FP_subnormal ->
     let m, e = frexp x in
     let t = Int64.of_float (ldexp m 53) in
     num_of_big_int (Big_int.big_int_of_int64 t) */ (Int 2 **/ Int (e - 53))

let log_num_simple b v =
  let rec loop t k =
    if t >/ v then k
    else loop (Int b */ t) (k + 1) in
  loop (Int 1) (-1)

let string_of_pos_finite_float_hi prec x =
  assert (x > 0. && prec > 0);
  let q = num_of_float x in
  let n = floor_num q in
  if sign_num n > 0 then
    let k = log_num_simple 10 n + 1 in
    let r, e =
      if k >= prec then
        let b = Int 10 **/ Int (k - prec) in
        let t = floor_num (n // b) in
        t, k - prec
      else
        let b = Int 10 **/ Int (prec - k) in
        n */ b +/ floor_num ((q -/ n) */ b), k - prec in
    let r = if r */ (Int 10 **/ Int e) <=/ q then succ_num r else r in




(* let string_of_pos_finite_float_hi prec x =
  assert (x >= 0. && prec > 0);
  let s = Printf.sprintf "%.*e" prec x in
  let nx = num_of_float x in
  let f = decode_num_str s in
  let nf = num_of_gen_float f in
  if nf >=/ nx then s
  else
    let f = {f with significand = succ_num f.significand} in
    assert (num_of_gen_float f >=/ nx);

    s *)

let is_exact str =
  let f = float_of_string str in
  let n0 = num_of_float_string str in
  let n1 = num_of_float f in
  (n0 -/ n1) =/ Int 0

let is_power_of_two n =
  let n = abs_num n in
  if is_integer_num n && n <>/ Int 0 then
    let k = big_int_of_num n in
    let pred_k = Big_int.pred_big_int k in
    let r = Big_int.and_big_int k pred_k in
    Big_int.eq_big_int r Big_int.zero_big_int
  else
    false

let next_float x =
  match classify_float x with
  | FP_nan -> nan
  | FP_infinite ->
     if x = infinity then x else nan
  | FP_zero -> ldexp 1. (-1074)
  | _ ->
     begin
       let bits = Int64.bits_of_float x in
       if x < 0. then
         Int64.float_of_bits (Int64.pred bits)
       else
         Int64.float_of_bits (Int64.succ bits)
     end
                               
let prev_float x =
  match classify_float x with
  | FP_nan -> nan
  | FP_infinite ->
     if x = neg_infinity then x else nan
  | FP_zero -> ldexp (-1.) (-1074)
  | _ ->
     begin
       let bits = Int64.bits_of_float x in
       if x < 0. then
         Int64.float_of_bits (Int64.succ bits)
       else
         Int64.float_of_bits (Int64.pred bits)
     end

(* Returns the integer binary logarithm of big_int.
   Returns -1 for non-positive numbers. *)
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

(* Returns the integer binary logarithm of the absolute value of num. *)
let log2_num r =
  let log2 r = log2_big_int (big_int_of_num (floor_num r)) in
  let r = abs_num r in
  if r </ Int 1 then
    let t = -log2 (Int 1 // r) in
    if (Int 2 **/ Int t) =/ r then t else t - 1
  else log2 r

let float_of_pos_num_lo r =
  assert (sign_num r >= 0);
  if sign_num r = 0 then 0.
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
    -. float_of_pos_num_hi (minus_num r)
  else
    float_of_pos_num_lo r

let float_of_num_hi r =
  if sign_num r < 0 then
    -. float_of_pos_num_lo (minus_num r)
  else
    float_of_pos_num_hi r

let interval_of_num n = {
    low = float_of_num_lo n;
    high = float_of_num_hi n
  }

let interval_of_string str =
  let n = num_of_float_string str in
  interval_of_num n

let check_float v =
  match (classify_float v) with
    | FP_infinite -> "Overflow"
    | FP_nan -> "NaN"
    | _ -> ""

let check_interval x =
  let c1 = check_float x.high in
  if c1 = "" then check_float x.low else c1