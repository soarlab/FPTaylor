(* -------------------------------------------------------------------------- *)
(* Symbolic Taylor forms                                                      *)
(* -------------------------------------------------------------------------- *)

open Interval
open Num
open Rounding
open Binary_float
open Expr

(* Describes an error variable *)
type error_info = {
  (* Error variables with the same index are the same *)
  index : int;
  (* The upper bound of the error is 2^exp *)
  exp : int;
}

(*  In mli:
module ExprErrSet : Set.S with type elt = Expr.expr * error_info *)

module ExprErrSet = Set.Make(
  struct
    type t = expr * error_info
    let compare (_, {index = index1}) (_, {index = index2}) = index1 - index2
  end)

let singl = ExprErrSet.singleton

type taylor_form = {
  v0 : expr;
  v1 : ExprErrSet.t;
}

let dummy_tform = {
  v0 = const_0;
  v1 = ExprErrSet.empty;
}

let mk_err_var index exp = {
  index = index;
  exp = exp;
}

let mk_sym_interval_const f =
  let t = abs_float f in
  let v = {low = -.t; high = t} in
  mk_interval_const v

let ( +^ ) = Fpu.fadd_high and
  ( *^ ) = Fpu.fmul_high

let estimate_expr, reset_estimate_cache =
  let cache = ref [] in
  let reset () = (cache := []) in
  let estimate (cs : constraints) e =
    if Config.get_bool_option "intermediate-opt" then
      let () = Log.report `Debug "Estimating: %s" (ExprOut.Info.print_str e) in
      let min, max = Opt.find_min_max (Opt_common.default_opt_pars ()) cs e in
      Log.report `Debug "Estimation result: [%f, %f]" min max;
      {low = min; high = max}
    else
      Eval.eval_interval_expr cs.var_interval e in
  let estimate_and_cache cs e =
    try Lib.assoc_eq eq_expr e !cache
    with Not_found ->
      let interval = estimate cs e in
      let _ = (cache := (e, interval) :: !cache) in
      interval
  in
  estimate_and_cache, reset

let add2 (x1, e1) (x2, e2) =
  (* Swap if e1 > e2 *)
  let x1, e1, x2, e2 =
    if e1 <= e2 then x1, e1, x2, e2 else x2, e2, x1, e1 in
  if e1 = 0 then 
    (if e2 = 0 then (0.0, 0) else (x2, e2))
  else if e2 = 0 then 
    (x1, e1)
  else if e1 = e2 then
    (x1 +^ x2, e1)
  else
    let eps = get_eps (e1 - e2) in
    ((x1 *^ eps) +^ x2, e2)

let sum_high s = Lib.itlist add2 s (0., 0)

let sum2_high s1 s2 = Lib.itlist 
    (fun (x,x_exp) s ->
       let s0 = sum_high (List.map (fun (y,y_exp) -> x *^ y, x_exp + y_exp) s2) in
       add2 s s0) 
    s1 (0.0, 0)

(* TODO: call (simplify_form f) before sum_i (eval_v1_i cs f.v1) *)
(* A better solution: insert new error terms in such a way that the form is always simplified *)
let sum_i s =
  let mul (x, e) =
    let eps = get_eps e in
    x *$ {low = -.eps; high = eps} in
  let vs = List.map mul s in
  Lib.itlist (+$) vs zero_I

let abs_eval cs ex = 
  (*  let v = Eval.eval_interval_expr cs ex in *)
  let v = estimate_expr cs ex in
  (abs_I v).high

let abs_eval_v1 cs = List.map (fun (ex, err) -> abs_eval cs ex, err.exp)

let abs_eval_v1_set cs s = ExprErrSet.fold (fun (ex, err) r -> (abs_eval cs ex, err.exp) :: r) s []

let eval_v1_i cs v1 =
  List.map (fun (e, err) -> estimate_expr cs e, err.exp) v1

let rec merge cs v1 v2 =
  let open ExprErrSet in
  let v1, v2 = if cardinal v1 > cardinal v2 then v2, v1 else v1, v2 in
  fold (fun ((ex1, err1) as e1) v2 ->
    try
      let (ex2, err2) as e2 = find e1 v2 in
      let e1' =
        if err1.index = -1 then
          let f1 = abs_eval cs ex1 in
          let f2 = abs_eval cs ex2 in
          let f, exp = add2 (f1, err1.exp) (f2, err2.exp) in
          let err = mk_err_var (-1) exp in
          mk_float_const f, err
        else if err1.exp <> err2.exp then
          failwith "merge: Incompatible exponents"
        else
          mk_add ex1 ex2, err1 in
      let v2' = remove e2 v2 in
      add e1' v2'
    with Not_found ->
      add e1 v2)
    v1 v2
