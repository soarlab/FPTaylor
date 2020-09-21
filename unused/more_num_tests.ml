open Num

let num_of_float x =
  match classify_float x with
  | FP_nan | FP_infinite -> Int 0
  | FP_zero -> Int 0
  | FP_normal | FP_subnormal ->
     let m, e = frexp x in
     let t = Int64.of_float (ldexp m 53) in
     num_of_big_int (Big_int.big_int_of_int64 t) */ (Int 2 **/ Int (e - 53))

let log_big_int_floor b v =
  let open Big_int in
  let rec loop t k =
    if gt_big_int t v then k
    else loop (mult_int_big_int b t) (k + 1) in
  loop unit_big_int (-1)

let string_of_pos_float_hi0 prec x =
  assert (x > 0. && prec > 0);
  let open Big_int in
  let q = num_of_float x in
  let n = big_int_of_num (floor_num q) in
  let ten = big_int_of_int 10 in
  let r, e =
    if sign_big_int n > 0 then
      let k = log_big_int_floor 10 n + 1 in
      let e = k - prec in
      let r =
        let b = power_big_int_positive_int ten (abs e) in
        if e >= 0 then
          div_big_int n b
        else
          let x = mult_big_int n b in
          let y = big_int_of_num (floor_num ((q -/ Big_int n) */ Big_int b)) in
          add_big_int x y in
      r, e
    else
      let k = log_big_int_floor 10 (big_int_of_num (floor_num (Int 1 // q))) in
      let b = power_big_int_positive_int ten (k + prec) in
      big_int_of_num (floor_num (q */ Big_int b)), -(k + prec) in
  let r = if Big_int r */ (Int 10 **/ Int e) </ q then succ_big_int r else r in
  let s = string_of_big_int r in
  let s, e = 
    if String.length s > prec then 
      String.sub s 0 prec, succ e 
    else s, e in
  let e' = e + prec - 1 in
  let s' = String.sub s 0 1 ^ "." ^ String.sub s 1 (prec - 1) in
  if e' = 0 then s'
  else s' ^ (if e' > 0 then "e+" else "e") ^ string_of_int e'

  (* Format.sprintf "%c.%s%s" s.[0] (String.sub s 1 (prec - 1))
    (if e' = 0 then "" else 
      (if e' > 0 then "e+" else "e") ^ string_of_int e') *)


let string_of_pos_float_hi1 prec x =
  assert (x > 0. && prec > 0);
  let open Big_int in
  let m, exp = frexp x in
  let m, exp = big_int_of_int64 (Int64.of_float (ldexp m 53)), exp - 53 in
  let two_exp = shift_left_big_int unit_big_int (abs exp) in
  let mask = pred_big_int two_exp in
  let ten = big_int_of_int 10 in
  let n, rem =
    if exp >= 0 then
      shift_left_big_int m exp, zero_big_int
    else
      shift_right_big_int m (-exp), and_big_int m mask in
  let r, e, flag =
    if sign_big_int n > 0 then
      let k = log_big_int_floor 10 n + 1 in
      let e = k - prec in
      let b = power_big_int_positive_int ten (abs e) in
      if e >= 0 then
        let r, v = quomod_big_int n b in
        r, e, sign_big_int v <> 0 || sign_big_int rem <> 0
      else
        let t = mult_big_int rem b in
        let x, v = shift_right_big_int t (-exp), and_big_int t mask in
        add_big_int (mult_big_int n b) x, e, sign_big_int v <> 0
    else
      let k = log_big_int_floor 10 (div_big_int two_exp rem) in
      let b = power_big_int_positive_int ten (k + prec) in
      let t = mult_big_int rem b in
      let r, v = shift_right_big_int t (-exp), and_big_int t mask in
      r, -(k + prec), sign_big_int v <> 0 in
  let r = if flag then succ_big_int r else r in
  let s = string_of_big_int r in
  let s, e = 
    if String.length s > prec then 
      String.sub s 0 prec, succ e 
    else s, e in
  let e' = e + prec - 1 in
  let s' = String.sub s 0 1 ^ "." ^ String.sub s 1 (prec - 1) in
  if e' = 0 then s'
  else s' ^ (if e' > 0 then "e+" else "e") ^ string_of_int e'

let tests () =
  (* Random.self_init (); *)
  Random.init 0;
  let res = ref [] in
  let test a b n =
    for i = 1 to n do
      let x = Random.float (b -. a) +. a in
      let prec = Random.int 14 + 1 in
      let s0 = string_of_pos_float_hi0 prec x in
      let s1 = string_of_pos_float_hi1 prec x in
      res := (x, prec, s0, s1) :: !res
    done in
  test 1e-300 1. 100;
  test 0.5 1000. 100;
  test 1e+10 1e+100 100;
  test 1e-300 1e+300 100;
  let writer fmt lst =
    List.iter (fun (x, prec, s0, s1) -> 
      Format.fprintf fmt "%.20e, %d, %s, %s@." x prec s0 s1) lst in
  Lib.write_to_file "out.txt" writer !res

let () = tests ()

(* ocamlc -I .. unix.cma nums.cma lib.cmo more_num_tests.ml *)
