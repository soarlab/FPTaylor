(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Unbounded binary floating-point numbers                                    *)
(* -------------------------------------------------------------------------- *)


open Big_int
open Num
open Rounding

type bin_float = {
  sign : bool;
  significand : big_int;
  exponent : int;
}

let mk_bin_float s m e = {
  sign = s;
  significand = m;
  exponent = e;
}

let string_of_bin_float x =
  let s = if x.sign then "-" else "" in
  Format.sprintf "%s%s * 2^%d" s (string_of_big_int x.significand) x.exponent;;

let print_bin_float x =
  Format.print_string (string_of_bin_float x);;

let neg_bin_float x = {x with sign = not x.sign};;

let mk_pos_bin_float = mk_bin_float false

let mk_neg_bin_float = mk_bin_float true

let bf0 = mk_pos_bin_float zero_big_int 0

let num_of_bin_float x =
  let v = Big_int x.significand */ (Int 2 **/ Int x.exponent) in
  if x.sign then minus_num v else v;;

(* Returns the integer binary logarithm of big_int  *)
(* Returns -1 for non-positive numbers              *)
let log2_big_int_simple =
  let rec log2 acc k =
    if sign_big_int k <= 0 then 
      acc
    else
      log2 (acc + 1) (shift_right_big_int k 1) in
  log2 (-1);;

let log2_big_int =
  let p = 32 in
  let u = power_int_positive_int 2 p in
  let rec log2 acc k =
    if compare_big_int k u >= 0 then
      log2 (acc + p) (shift_right_big_int k p)
    else
      acc + log2_big_int_simple k in
  log2 0;;

(* Returns the integer binary logarithm of num *)
(* Returns -1 for non-positive numbers         *)
let log2_num r =
  let log2 r = log2_big_int (big_int_of_num (integer_num r)) in
  if sign_num r <= 0 then 
    -1 
  else
    if r </ Int 1 then
      let t = -log2 (Int 1 // r) in
      if (Int 2 **/ Int t) =/ r then t else t - 1
    else
      log2 r;;

let is_even_num r =
  sign_num (mod_num r (Int 2)) = 0;;

(* Returns a binary float x = (-1)^s * m * 2^e such that     *)
(* 2^(p - 1) <= m < 2^p and x approximates r                 *) 
(* (if r = 0 then m = 0)                                     *)
let bin_float_of_num p rnd r =
  let _ = assert (p > 0) in
  let half = Int 1 // Int 2 in
  let next m e =
    let t = succ_num m in
    if t =/ (Int 2 **/ Int p) then
      t // Int 2, succ e
    else
      t, e 
  in
  let bin_float_of_pos_num rnd r =
    let n = log2_num r in
    let k = n + 1 - p in
    let y = (Int 2 **/ Int (-k)) */ r in
    let low = integer_num y in
    let m, e = 
      if low =/ y then
	low, k
      else
	match rnd with
	  | Rnd_down | Rnd_0 -> low, k
	  | Rnd_up -> next low k
	  | Rnd_ne ->
	    let d = y -/ low in
	    begin
	      match (compare_num d half) with
		| -1 -> low, k
		| 1 -> next low k
		| _ -> 
		  if is_even_num low then 
		    low, k 
		  else
		    next low k
	    end in
    mk_pos_bin_float (big_int_of_num m) e 
  in
  match sign_num r with
    | 0 -> bf0
    | 1 -> bin_float_of_pos_num rnd r
    | _ -> 
      let rnd =
	begin 
	  match rnd with
	    | Rnd_up -> Rnd_down
	    | Rnd_down -> Rnd_up
	    | _ -> rnd
	end in
      neg_bin_float (bin_float_of_pos_num rnd (minus_num r));;
  

