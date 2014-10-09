open Interval
open Num

let numerator = function
  | Ratio r -> Ratio.numerator_ratio r
  | Big_int n -> n
  | Int n -> Big_int.big_int_of_int n

let denominator = function
  | Ratio r -> Ratio.denominator_ratio r
  | _ -> Big_int.big_int_of_int 1

let decode_num_str str =
  let int_of_string str =
    let s = if str.[0] = '+' then 
	String.sub str 1 (String.length str - 1) 
      else
	str in
    Pervasives.int_of_string s in
  let split_at str ch = 
    let i = try String.index str ch with Not_found -> -1 in
    if i < 0 || i >= String.length str then
      str, ""
    else
      String.sub str 0 i,
      String.sub str (i + 1) (String.length str - i - 1) in
  let s_int, s2 = split_at str '.' in
  let s_int, s_dec, s_exp =
    if s2 = "" then
      let s1, s2 = split_at s_int 'e' in
      s1, "", s2
    else
      let s1, s2 = split_at s2 'e' in
      s_int, s1, s2 in
  let m_int = Big_int.big_int_of_string (s_int ^ s_dec) and
      n_exp = if s_exp <> "" then int_of_string s_exp else 0 in
  let exp = n_exp - String.length s_dec in
  m_int, exp

let num_of_float_string str =
  let m, exp = decode_num_str str in
  let t = power_num (Int 10) (Int exp) in
  Big_int m */  t

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

