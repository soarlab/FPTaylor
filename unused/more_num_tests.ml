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

let log_big_int_floor b v =
  let open Big_int in
  let b = big_int_of_int b in
  let rec loop t b' k k'  =
    let t' = mult_big_int b' t in
    if gt_big_int t' v then
      if k' = 1 then k
      else loop t b k 1
    else loop t' (mult_big_int b' b) (k + k') (k' + 1) in
  if sign_big_int v <= 0 then -1
  else loop unit_big_int b 0 1

(* let log_big_int_floor b v =
  let open Big_int in
  let m = max_int / b in
  let rec loop t b' k k'  =
    let t' = mult_int_big_int b' t in
    if gt_big_int t' v then
      if k' = 1 then k
      else loop t b k 1
    else if b' <= m then
      loop t' (b' * b) (k + k') (k' + 1)
    else
      loop t' b' (k + k') k' in
  if sign_big_int v <= 0 then -1
  else loop unit_big_int b 0 1 *)


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

let string_of_pos_finite_float_lo prec x =
  assert (x > 0. && prec > 0);
  let open Big_int in
  let m, exp = frexp x in
  let m, exp = big_int_of_int64 (Int64.of_float (ldexp m 53)), exp - 53 in
  let two_exp = shift_left_big_int unit_big_int (abs exp) in
  let ten = big_int_of_int 10 in
  let n, rem =
    if exp >= 0 then
      shift_left_big_int m exp, zero_big_int
    else
      let mask = pred_big_int two_exp in
      shift_right_big_int m (-exp), and_big_int m mask in
  let r, e =
    if sign_big_int n > 0 then
      let k = log_big_int_floor 10 n + 1 in
      let e = k - prec in
      let b = power_big_int_positive_int ten (abs e) in
      if e >= 0 then
        div_big_int n b, e
      else
        let t = mult_big_int rem b in
        let x = shift_right_big_int t (-exp) in
        add_big_int (mult_big_int n b) x, e
    else
      let k = log_big_int_floor 10 (div_big_int two_exp rem) in
      let b = power_big_int_positive_int ten (k + prec) in
      let t = mult_big_int rem b in
      let r = shift_right_big_int t (-exp) in
      r, -(k + prec) in
  let s = string_of_big_int r in
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
      let s_lo = string_of_pos_finite_float_lo prec x in
      res := (x, prec, s0, s1, s_lo) :: !res
    done in
  test 1e-300 1. 1000;
  test 0.5 1000. 1000;
  test 1e+10 1e+100 1000;
  test 1e-300 1e+300 1000;
  let writer fmt lst =
    List.iter (fun (x, prec, s0, s1, s_lo) -> 
      Format.fprintf fmt "%.20e, %d, %s, %s, %s@." x prec s0 s1 s_lo) lst in
  Lib.write_to_file "out.txt" writer !res

let () = tests ()

(* ocamlc -I .. -I ../unittests unix.cma nums.cma lib.cmo ../unittests/test.ml more_num_tests.ml *)

open Test

let samples = 10000
let repeats = 10

let data_f_pos = array_of_stream (rand_floats samples 1 (-30) (30))
let data_f_pos_big = array_of_stream (rand_floats samples 1 200 500)

let run_f_pos ?base_mean name f =
  run_performance_test ~repeats ?base_mean ~name f data_f_pos

let run_f_big_pos ?base_mean name f =
  run_performance_test ~repeats ?base_mean ~name f data_f_pos_big

let () =
  let test0 prec  = string_of_pos_float_hi0 prec in
  let test1 prec = string_of_pos_float_hi1 prec in
  let test_lo prec = string_of_pos_finite_float_lo prec in
  let test_printf prec = Format.sprintf "%.*e" prec in
  Gc.compact ();
  print_performance_header ();
  let base_mean, _ = run_f_pos "empty" (fun a -> 0) in
  ignore @@ run_f_pos "str_hi0 (5)" ~base_mean (test0 5);
  ignore @@ run_f_pos "str_hi1 (5)" ~base_mean (test1 5);
  ignore @@ run_f_pos "str_lo (5)" ~base_mean (test_lo 5);
  ignore @@ run_f_pos "printf (5)" ~base_mean (test_printf 5);

  (* ignore @@ run_f_pos "str_hi0 (10)" ~base_mean (test0 10);
  ignore @@ run_f_pos "str_hi1 (10)" ~base_mean (test1 10);
  ignore @@ run_f_pos "printf (10)" ~base_mean (test_printf 10);

  ignore @@ run_f_pos "str_hi0 (3)" ~base_mean (test0 3);
  ignore @@ run_f_pos "str_hi1 (3)" ~base_mean (test1 3);
  ignore @@ run_f_pos "printf (3)" ~base_mean (test_printf 3); *)

  ignore @@ run_f_big_pos "big: str_hi0 (5)" ~base_mean (test0 5);
  ignore @@ run_f_big_pos "big: str_hi1 (5)" ~base_mean (test1 5);
  ignore @@ run_f_big_pos "big: str_lo (5)" ~base_mean (test_lo 5);
  ignore @@ run_f_big_pos "big: printf (5)" ~base_mean (test_printf 5);
