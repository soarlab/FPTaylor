(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
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
  | _ -> Big_int.big_int_of_int 1

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

let interval_of_big_int =
  let max, (<), (>>), (&) = 
    Big_int.power_int_positive_int 2 32,
    Big_int.lt_big_int,
    Big_int.shift_right_big_int,
    Big_int.and_big_int in
  let mask = Big_int.pred_big_int max in
  let max_f = Big_int.float_of_big_int max in
  let rec pos_big_int n =
    if n < max then
      let v = Big_int.float_of_big_int n in
      {low = v; high = v}
    else
      let i1 = pos_big_int (n >> 32) in
      let i2 = pos_big_int (n & mask) in
      (max_f *.$ i1) +$ i2 in
  fun n ->
    if (Big_int.sign_big_int n >= 0) then
      pos_big_int n
    else
      ~-$ (pos_big_int (Big_int.abs_big_int n))

let interval_of_num n =
  let p = interval_of_big_int (numerator n) and
      q = interval_of_big_int (denominator n) in
  p /$ q

let interval_of_string str =
  let n = num_of_float_string str in
  interval_of_num n

let num_of_float =
  let base = Int (1 lsl 10) in
  let rec to_num f =
    if f <= 0.0 then Int 0
    else
      let d = int_of_float f in
      let f' = ldexp (f -. float_of_int d) 10 in
      Int d +/ (to_num f' // base) in
  fun f ->
    let m, e = frexp f in
    let m, sign = if m < 0.0 then -.m, true else m, false in
    let n = to_num m */ (Int 2 **/ Int e) in
    if sign then minus_num n else n

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

let next_float =
  let t = ldexp 1.0 (-53) in
  fun f ->
    let m, e = frexp f in
    ldexp (m +. t) e

