(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

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

type taylor_form = {
  v0 : expr;
  v1 : (expr * error_info) list;
}

let dummy_tform = {
  v0 = const_0;
  v1 = [];
}

let mk_err_var index exp = {
  index = index;
  exp = exp;
}

let mk_sym_interval_const f =
  let t = abs_float f in
  let v = {low = -.t; high = t} in
  mk_interval_const v

let ( +^ ) = Fpu.fadd_high
let ( *^ ) = Fpu.fmul_high

let estimate_expr, reset_estimate_cache, estimate_cache_stats =
  let cache = ExprHashtbl.create (1 lsl 16) in
  let reset () = ExprHashtbl.clear cache in
  let stats () =
    (* let tmp = Hashtbl.create 1000 in
    let add h e =
      Hashtbl.replace tmp h (e :: (try Hashtbl.find tmp h with Not_found -> [])) in
    ExprHashtbl.iter (fun e _ -> let h = hash_expr e in add h e) cache;
    let es = Hashtbl.fold (fun a es xs ->
      if List.length es > List.length xs then es else xs) tmp [] in
    es |> List.iter (fun e -> Printf.printf "%s\n" (ExprOut.Info.print_str e)); *)
    ExprHashtbl.stats cache in
  let estimate_opt (cs : constraints) e =
    Log.report `Debug "Estimating: %s" (ExprOut.Info.print_str e);
    let min, max = Opt.find_min_max (Opt_common.default_opt_pars ()) cs e in
    Log.report `Debug "Estimation result: [%f, %f]" min max;
    {low = min; high = max} in
  let estimate_and_cache cs e =
    if Config.get_bool_option "intermediate-opt" then
      try ExprHashtbl.find cache e
      with Not_found ->
        let interval = estimate_opt cs e in
        ExprHashtbl.add cache e interval;
        interval
    else
      Eval.eval_interval_expr ~cache cs.var_interval e
  in
  estimate_and_cache, reset, stats

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

let abs_eval cs ex = 
  let v = estimate_expr cs ex in
  (abs_I v).high

let abs_eval_v1 cs = List.map (fun (ex, err) -> abs_eval cs ex, err.exp)

let eval_v1_i cs v1 =
  List.map (fun (e, err) -> estimate_expr cs e, err.exp) v1

let rec merge cs v1 v2 =
  match v1, v2 with
  | [], _ -> v2
  | _, [] -> v1
  | (ex1, {index = -1; exp = exp1}) :: v1s, (ex2, {index = -1; exp = exp2}) :: v2s ->
    let f1 = abs_eval cs ex1 in
    let f2 = abs_eval cs ex2 in
    let f, exp = add2 (f1, exp1) (f2, exp2) in
    let err = mk_err_var (-1) exp in
    (mk_float_const f, err) :: merge cs v1s v2s
  | ((ex1, err1) as h1) :: v1s, ((ex2, err2) as h2) :: v2s ->
    if err1.index = err2.index then begin
      if err1.exp <> err2.exp then (
        Log.error "index1 = %d, index2 = %d; exp1 = %d, exp2 = %d" 
                  err1.index err2.index err1.exp err2.exp;
        Log.error "expr1 = %s\nexpr2=%s\n" 
          (ExprOut.Info.print_str ex1)
          (ExprOut.Info.print_str ex2);
        failwith "merge: Incompatible exponents"
      );
      (mk_add ex1 ex2, err1) :: merge cs v1s v2s
    end
    else if err1.index < err2.index then
      h1 :: merge cs v1s v2
    else
      h2 :: merge cs v1 v2s

let simplify_form cs f = f
  (* let rec add_adjacent s =
    match s with
    | (ex1, err1) :: (ex2, err2) :: t when err1.index = -1 && err2.index = -1 ->
      let f1, exp1 = abs_eval cs ex1, err1.exp and
      f2, exp2 = abs_eval cs ex2, err2.exp in
      let f, exp = add2 (f1, exp1) (f2, exp2) in
      let err = mk_err_var (-1) exp in
      add_adjacent ((mk_float_const f, err) :: t)
    | (e1, err1) :: (e2, err2) :: t ->
      if err1.index = err2.index then
        (* we must have err1.exp = err2.exp here *)
        let _ = assert(err1.exp = err2.exp) in
        add_adjacent ((mk_add e1 e2, err1) :: t)
      else
        let s = add_adjacent ((e2, err2) :: t) in
        (e1, err1) :: s
    | _ -> s in
  let v1 = List.sort (fun (_, err1) (_, err2) -> compare err1.index err2.index) f.v1 in
  let v1_new = add_adjacent v1 in
  {f with v1 = v1_new} *)

let find_index, expr_for_index, reset_index_counter, current_index =
  let counter = ref 0 in
  let cache = ExprHashtbl.create 100 in
  let inv_cache = Hashtbl.create 100 in
  let find_index expr =
    let unique_flag = Config.get_bool_option "unique-indices" in
    let i = try ExprHashtbl.find cache expr with Not_found -> -1 in
    if i > 0 && (not unique_flag) then i else begin
      incr counter;
      ExprHashtbl.add cache expr !counter;
      Hashtbl.add inv_cache !counter expr;
      !counter
    end
  and expr_for_index i = Hashtbl.find inv_cache i
  and reset_index_counter () = 
    ExprHashtbl.clear cache;
    Hashtbl.clear inv_cache;
    counter := 0  
  and current_index () = !counter
  in
  find_index, expr_for_index, reset_index_counter, current_index

(* constant *)
let const_form e = 
  Log.report `Debug "const_form";
  match e with
  | Const _ -> {
      v0 = e;
      v1 = []
    }
  | _ -> failwith ("const_form: not a constant" ^ ExprOut.Info.print_str e)

(* Constant with rounding *)
(* TODO: subnormal numbers are incorrectly handled by bin_float_of_num *)
let precise_const_rnd_form rnd e =
  Log.report `Debug "precise_const_rnd_form";
  match e with
  | Const c ->
    assert (Const.is_rat c);
    let cn = Const.to_num c in 
    let x = bin_float_of_num (-rnd.eps_exp) rnd.rnd_type cn in
    let rc = num_of_bin_float x in
    let d = cn -/ rc in
    (* exact fp number *)
    if d =/ Int 0 then
      const_form e
    else
      let err_expr = mk_num_const (d // (Int 2 **/ Int rnd.eps_exp)) in
      (*	let err = mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp in *)
      (* Exact errors for constants can cancel each other: 
         use the same artificial constant (const_0) for indices *)
      let err = mk_err_var (find_index (mk_rounding rnd const_0)) rnd.eps_exp in
      Log.report `Debug "Inexact constant: %s; err = %s"
        (ExprOut.Info.print_str e)
        (ExprOut.Info.print_str err_expr);
      {
        v0 = e;
        v1 = [err_expr, err]
      }
  | _ -> failwith ("precise_const_rnd_form: not a constant: " ^ ExprOut.Info.print_str e)


(* constant with rounding *)
let const_rnd_form rnd e =
  Log.report `Debug "const_rnd_form";
  if Config.get_bool_option "fp-power2-model" then
    precise_const_rnd_form rnd e
  else
    match e with
    | Const c ->
      assert (Const.is_rat c);
      if is_exact_fp_const rnd (Const.to_num c) then 
        const_form e
      else
        let bound = (abs_I (Const.to_interval c)).high in
        let p2 = Func.floor_power2 bound in
        let m2 = rnd.coefficient *^ p2 in
        let err_expr = mk_float_const m2 in
        let err = mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp in
        Log.report `Debug "Inexact constant: %s; err = %s" 
          (ExprOut.Info.print_str e) 
          (ExprOut.Info.print_str err_expr);
        {
          v0 = e;
          v1 = [err_expr, err]
        }
    | _ -> failwith ("const_rnd_form: not a constant: " ^ ExprOut.Info.print_str e)

let get_var_uncertainty cs eps_exp var_name =
  let v = cs.var_uncertainty var_name in
  let u = (Const.to_num v) // More_num.num_of_float (get_eps eps_exp) in
  if not (u =/ Int 0) then
    [mk_num_const u, mk_err_var (find_index (mk_var (var_name ^ "$uncertainty"))) eps_exp]
  else 
    []

(* variable *)
let var_form cs e =
  Log.report `Debug "var_form";
  match e with
  | Var v -> {
      v0 = e;
      (* FIXME: what is the right value of eps_exp here? *)
      v1 = if Config.get_bool_option "uncertainty" then get_var_uncertainty cs (-53) v else [];
    }
  | _ -> failwith ("var_form: not a variable" ^ ExprOut.Info.print_str e)

(* variable with rounding *)
let var_rnd_form cs rnd e =
  Log.report `Debug "var_rnd_form";
  match e with
  | Var v -> 
      let v1_uncertainty = 
        if Config.get_bool_option "uncertainty" then get_var_uncertainty cs rnd.eps_exp v else [] in
      let v1_rnd =
        let err_expr0 = 
          if Config.get_bool_option "const-approx-real-vars" then
            let bound = (abs_I (cs.var_interval v)).high in
            let err = Func.floor_power2 bound in
            mk_float_const err
          else if Config.get_bool_option "fp-power2-model" then
            mk_floor_power2 e
          else
            e in
        let err_expr = 
          if rnd.coefficient <> 1.0 then
            mk_mul (mk_float_const rnd.coefficient) err_expr0 
          else 
            err_expr0 in
        (* TODO: subnormal values of variables *)
        [err_expr, mk_err_var (find_index (mk_rounding rnd e)) rnd.eps_exp] in
      {
        v0 = e;
        v1 = v1_uncertainty @ v1_rnd;
      }
  | _ -> failwith ("var_rnd_form: not a variable" ^ ExprOut.Info.print_str e)

(* rounding *)
let rounded_form cs original_expr rnd f =
  Log.report `Debug "rounded_form";
  if rnd.eps_exp = 0 then {
    v0 = f.v0;
    v1 = f.v1 @ [mk_float_const rnd.coefficient, mk_err_var (-1) rnd.delta_exp]
  }
  else
    let i = find_index original_expr in
    let s1', exp1 = sum_high (abs_eval_v1 cs f.v1) in
    let s1 = get_eps exp1 *^ s1' in
    let r', m2' =
      if Config.get_bool_option "fp-power2-model" then
        mk_floor_power2 (mk_add f.v0 (mk_sym_interval_const s1)),
        get_eps rnd.delta_exp /. get_eps rnd.eps_exp
      else
        f.v0, 
        s1 +^ (get_eps rnd.delta_exp /. get_eps rnd.eps_exp) in
    let r, m2 =
      if rnd.coefficient = 1.0 then
        r', m2'
      else
        mk_mul (mk_float_const rnd.coefficient) r', rnd.coefficient *^ m2' in
    let r_err = mk_err_var i rnd.eps_exp and
        m2_err = mk_err_var (-1) rnd.eps_exp in
    {
      v0 = f.v0;
      v1 = merge cs [mk_float_const m2, m2_err; r, r_err] f.v1
    }

(* negation *)
let neg_form f = 
  Log.report `Debug "neg_form";
  {
    v0 = mk_neg f.v0;
    v1 = List.map (fun (e, err) -> mk_neg e, err) f.v1;
  }

(* addition *)
let add_form cs f1 f2 = 
  Log.report `Debug "add_form";
  {
    v0 = mk_add f1.v0 f2.v0;
    v1 = merge cs f1.v1 f2.v1;
  }

(* subtraction *)
let sub_form cs f1 f2 =
  Log.report `Debug "sub_form";
  {
    v0 = mk_sub f1.v0 f2.v0;
    v1 = merge cs f1.v1 (List.map (fun (e, err) -> mk_neg e, err) f2.v1);
  }

(* rounded subtraction *)
let rounded_sub_form cs original_expr rnd f1 f2 =
  Log.report `Debug "rounded_sub_form";
  let i = find_index original_expr in
  let s1', exp1 = sum_high (abs_eval_v1 cs f1.v1) in
  let s2', exp2 = sum_high (abs_eval_v1 cs f2.v1) in
  let s1 = get_eps exp1 *^ s1' in
  let s2 = get_eps exp2 *^ s2' in
  let r' = mk_floor_sub2 (mk_add f1.v0 (mk_sym_interval_const s1))
      (mk_add f2.v0 (mk_sym_interval_const s2)) in
  let r = 
    if rnd.coefficient = 1.0 then
      r'
    else
      mk_mul (mk_float_const rnd.coefficient) r' in
  let r_err = mk_err_var i rnd.eps_exp in
  {
    v0 = mk_sub f1.v0 f2.v0;
    v1 = merge cs [r, r_err] (merge cs f1.v1 (List.map (fun (e, err) -> mk_neg e, err) f2.v1));
  }

(* rounded addition *)
let rounded_add_form cs original_expr rnd f1 f2 =
  Log.report `Debug "rounded_add_form";
  rounded_sub_form cs original_expr rnd f1 (neg_form f2)


(* multiplication *)
let mul_form =
  let mul1 x = List.map (fun (e, err) -> mk_mul x e, err) in
  fun cs f1 f2 -> 
    Log.report `Debug "mul_form";
    let x1 = abs_eval_v1 cs f1.v1 and
      y1 = abs_eval_v1 cs f2.v1 in
    let m2, m2_exp = sum2_high x1 y1 in
    let m2_err = mk_err_var (-1) m2_exp in
    {
      v0 = mk_mul f1.v0 f2.v0;
      v1 = merge cs [mk_float_const m2, m2_err] (merge cs (mul1 f1.v0 f2.v1) (mul1 f2.v0 f1.v1));
    }

(* reciprocal *)
let inv_form cs f = 
  Log.report `Debug "inv_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d = pow_I_i (x0_int +$ s1) 3 in
  let _ = 
    if (abs_I d).low <= 0.0 then
      let msg = "inv_form: division by zero" in
      if Config.fail_on_exception () then
        failwith msg
      else
        Log.warning_str msg
    else () in
  let b_high = (abs_I (inv_I d)).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  {
    v0 = mk_div const_1 f.v0;
    v1 = merge cs [mk_float_const m3, m3_err] 
            (List.map (fun (e, err) -> mk_neg (mk_div e (mk_mul f.v0 f.v0)), err) f.v1);
  }

(* division *)
let div_form cs f1 f2 =  
  Log.report `Debug "div_form";
  mul_form cs f1 (inv_form cs f2)

(* square root *)
let sqrt_form cs f = 
  Log.report `Debug "sqrt_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
      let t = (x0_int +$ s1) in
      sqrt_I t *$ t in
  let b_high = 0.125 *^ (abs_I (inv_I d)).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let sqrt_v0 = mk_sqrt f.v0 in 
  {
    v0 = sqrt_v0;
    v1 = merge cs [mk_float_const m3, m3_err] 
            (List.map (fun (e, err) -> mk_div e (mk_mul const_2 sqrt_v0), err) f.v1);
  }

(* sine *)
let sin_form cs f =
  Log.report `Debug "sin_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d = sin_I (x0_int +$ s1) in
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let sin_v0 = mk_sin f.v0 in
  let cos_v0 = mk_cos f.v0 in 
  {
    v0 = sin_v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_mul cos_v0 e, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* cosine *)
let cos_form cs f =
  Log.report `Debug "cos_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d = cos_I (x0_int +$ s1) in
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let sin_v0 = mk_sin f.v0 in
  let cos_v0 = mk_cos f.v0 in 
  {
    v0 = cos_v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_neg (mk_mul sin_v0 e), err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* tangent *)
let tan_form cs f =
  Log.report `Debug "tan_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let xi = x0_int +$ s1 in
  let d = tan_I xi /$ pow_I_i (cos_I xi) 2 in
  let r_high = (abs_I d).high in
  let m2', m2_exp = sum2_high x1 x1 in
  let m2 = r_high *^ m2' in
  let v1_0 = mk_mul (mk_cos f.v0) (mk_cos f.v0) in
  {
    v0 = mk_tan f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m2, mk_err_var (-1) m2_exp];
  }

(* arcsine *)
let asin_form cs f =
  Log.report `Debug "asin_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    xi /$ sqrt_I (pow_I_i (one_I -$ pow_I_i xi 2) 3) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_sqrt (mk_sub const_1 (mk_mul f.v0 f.v0)) in
  {
    v0 = mk_asin f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* arccosine *)
let acos_form cs f =
  Log.report `Debug "acos_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    ~-$ (xi /$ sqrt_I (pow_I_i (one_I -$ pow_I_i xi 2) 3)) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_sqrt (mk_sub const_1 (mk_mul f.v0 f.v0)) in
  {
    v0 = mk_acos f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_neg (mk_div e v1_0), err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* arctangent *)
let atan_form cs f =
  Log.report `Debug "atan_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    ~-$ (xi /$ pow_I_i (pow_I_i xi 2 +$ one_I) 2) in
  let b_high = (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_add (mk_mul f.v0 f.v0) const_1 in
  {
    v0 = mk_atan f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* exp *)
let exp_form cs f =
  Log.report `Debug "exp_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d = exp_I (x0_int +$ s1) in
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let exp_v0 = mk_exp f.v0 in 
  {
    v0 = exp_v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_mul exp_v0 e, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* log *)
let log_form cs f =
  Log.report `Debug "log_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d = inv_I (pow_I_i (x0_int +$ s1) 2) in
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let log_v0 = mk_log f.v0 in 
  {
    v0 = log_v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e f.v0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* sinh *)
let sinh_form cs f =
  Log.report `Debug "sinh_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    sinh_I xi in
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_cosh f.v0 in
  {
    v0 = mk_sinh f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_mul v1_0 e, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* cosh *)
let cosh_form cs f =
  Log.report `Debug "cosh_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    cosh_I xi in
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_sinh f.v0 in
  {
    v0 = mk_cosh f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_mul v1_0 e, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* tanh *)
let tanh_form cs f =
  Log.report `Debug "tanh_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    ~-$ (tanh_I xi /$ pow_I_i (cosh_I xi) 2) in
  let b_high = (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_mul (mk_cosh f.v0) (mk_cosh f.v0) in
  {
    v0 = mk_tanh f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }


(* arsinh *)
let asinh_form cs f =
  Log.report `Debug "asinh_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    ~-$ (xi /$ sqrt_I (pow_I_i (one_I +$ pow_I_i xi 2) 3)) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_sqrt (mk_add const_1 (mk_mul f.v0 f.v0)) in
  {
    v0 = mk_asinh f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* arcosh *)
let acosh_form cs f =
  Log.report `Debug "acosh_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    ~-$ (xi /$ sqrt_I (pow_I_i (pow_I_i xi 2 -$ one_I) 3)) in 
  let b_high = 0.5 *^ (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  let v1_0 = mk_sqrt (mk_sub (mk_mul f.v0 f.v0) const_1) in
  {
    v0 = mk_acosh f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* artanh *)
let atanh_form cs f =
  Log.report `Debug "atanh_form";
  let x0_int = estimate_expr cs f.v0 in
  let x1 = abs_eval_v1 cs f.v1 in
  let s1 = Lib.itlist (fun (x,x_exp) s -> 
      let eps = get_eps x_exp in
      let xi = {low = -. eps; high = eps} in
      (xi *$. x) +$ s) x1 zero_I in
  let d =
    let xi = x0_int +$ s1 in
    xi /$ pow_I_i (one_I -$ pow_I_i xi 2) 2 in
  let b_high = (abs_I d).high in
  let m2, m2_exp = sum2_high x1 x1 in
  let m3 = b_high *^ m2 in
  let m3_err = mk_err_var (-1) m2_exp in
  (* let _ = Proof.add_atn_step form_index f.form_index
      m1 m2 m2_exp b_high m3 m3_err.proof_index in *)
  let v1_0 = mk_sub const_1 (mk_mul f.v0 f.v0) in
  {
    v0 = mk_atanh f.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_div e v1_0, err) f.v1)
                  [mk_float_const m3, m3_err];
  }

(* absolute value *)
(* |x + e| = |x| + abs_err(t, x) * e where
   t is an upper bound of |e| and
   abs_err(t, x) = 1 if x >= t,
   abs_err(t, x) = -1 if x <= -t,
   abs_err(t, x) = [-1, 1] if -t < x < t.

   An interval version of abs_err can be defined as
   Abs_err(t, X) = Abs_diff(X + [-t, t]) = Abs_diff(X + sym_interval(t))
   where Abs_diff is defined as
   Abs_diff([a, b]) = 1 if a >= 0,
   Abs_diff([a, b]) = -1 if b <= 0,
   Abs_diff([a, b]) = [-1, 1] if a < 0 and b > 0.

   We define Abs_err directly as
   Abs_err([t1, t2], [a, b]) = 1 if a >= t2,
   Abs_err([t1, t2], [a, b]) = -1 if b <= t1,
   Abs_err([t1, t2], [a, b]) = [-1, 1] if a < t2 and b > t1.

   Abs_err(sym_interval(t), [a, b]) = Abs_diff([a, b] + sym_interval(t)).
   We prefer the explicit definition of Abs_err since it is slightly more accurate.
*)
let abs_form cs f =
  Log.report `Debug "abs_form";
  let t =
    let s, e = sum_high (abs_eval_v1 cs f.v1) in
    get_eps e *^ s in
  let abs_err = mk_abs_err (mk_sym_interval_const t) f.v0 in
  {
    v0 = mk_abs f.v0;
    v1 = List.map (fun (e, err) -> mk_mul abs_err e, err) f.v1;
  }

(* maximum of two values *)
(* max(x, y) = (|x - y| + x + y) / 2.
   From this equality we get:

   max(x + e1, y + e2) = (|x - y + (e1 - e2)| + (x + y) + (e1 + e2)) / 2.

   Using the formula |x + e| = |x| + abs_err(t, x) * e 
   where t is an upper bound of |e|, we get:

   max(x + e1, y + e2) = max(x, y) + 0.5 * (abs_err(t, x - y) + 1) * e1 
                                   + 0.5 * (1 - abs_err(t, x - y)) * e2

   where t is an upper bound of |e1 - e2| (or |e1| + |e2| >= |e1 - e2|).
*) 
let max_form cs f1 f2 =
  Log.report `Debug "max_form";
  let t1 =
    let s, e = sum_high (abs_eval_v1 cs f1.v1) in
    get_eps e *^ s in
  let t2 =
    let s, e = sum_high (abs_eval_v1 cs f2.v1) in
    get_eps e *^ s in
  let x_sub_y = mk_sub f1.v0 f2.v0 in
  let t = mk_sym_interval_const (t1 +^ t2) in
  let err1 = mk_mul (mk_float_const 0.5) (mk_add const_1 (mk_abs_err t x_sub_y)) in
  let err2 = mk_mul (mk_float_const 0.5) (mk_sub const_1 (mk_abs_err t x_sub_y)) in
  {
    v0 = mk_max f1.v0 f2.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_mul err1 e, err) f1.v1)
                  (List.map (fun (e, err) -> mk_mul err2 e, err) f2.v1);
  }

(* minimum of two values *)
(* min(x, y) = -max(-x, -y) = (x + y - |x - y|) / 2,

   min(x + e1, y + e2) = min(x, y) + 0.5 * (1 - abs_err(t, x - y)) * e1 
                                   + 0.5 * (1 + abs_err(t, x - y)) * e2

   where t is an upper bound of |e1 - e2|.
*)
let min_form cs f1 f2 =
  Log.report `Debug "min_form";
  let t1 =
    let s, e = sum_high (abs_eval_v1 cs f1.v1) in
    get_eps e *^ s in
  let t2 =
    let s, e = sum_high (abs_eval_v1 cs f2.v1) in
    get_eps e *^ s in
  let x_sub_y = mk_sub f1.v0 f2.v0 in
  let t = mk_sym_interval_const (t1 +^ t2) in
  let err1 = mk_mul (mk_float_const 0.5) (mk_sub const_1 (mk_abs_err t x_sub_y)) in
  let err2 = mk_mul (mk_float_const 0.5) (mk_add const_1 (mk_abs_err t x_sub_y)) in
  {
    v0 = mk_min f1.v0 f2.v0;
    v1 = merge cs (List.map (fun (e, err) -> mk_mul err1 e, err) f1.v1)
                  (List.map (fun (e, err) -> mk_mul err2 e, err) f2.v1);
  }

(* Builds a Taylor form *)
let build_form (cs : constraints) =
  let cache = ExprHashtbl.create 1024 in
  let rec build e =
    try ExprHashtbl.find cache e with Not_found ->
    let tf =
      match e with
      | Const _ -> const_form e
      | Var _ -> var_form cs e
      | Rounding (rnd, Const c) when Const.is_rat c 
        (* when not Config.proof_flag *) -> const_rnd_form rnd (Const c)
      | Rounding (rnd, Var v) 
        (* when not Config.proof_flag *) -> var_rnd_form cs rnd (Var v)
      | Rounding (rnd, Bin_op (Op_add, arg1, arg2)) 
        when rnd.special_flag 
          && Config.get_bool_option "fp-power2-model" 
          && Config.get_bool_option "develop" ->
        rounded_add_form cs e rnd (build arg1) (build arg2)
      | Rounding (rnd, Bin_op (Op_sub, arg1, arg2))
        when rnd.special_flag 
          && Config.get_bool_option "fp-power2-model" 
          && Config.get_bool_option "develop" ->
        rounded_sub_form cs e rnd (build arg1) (build arg2)
      | Rounding (rnd, arg) -> 
        let arg_form = build arg in
        rounded_form cs e rnd arg_form
      | U_op (op, arg) ->
        begin
          let arg_form = build arg in
          match op with
          | Op_neg -> neg_form arg_form
          | Op_abs -> abs_form cs arg_form
          | Op_inv -> inv_form cs arg_form
          | Op_sqrt -> sqrt_form cs arg_form
          | Op_sin -> sin_form cs arg_form
          | Op_cos -> cos_form cs arg_form
          | Op_tan -> tan_form cs arg_form
          | Op_asin -> asin_form cs arg_form
          | Op_acos -> acos_form cs arg_form
          | Op_atan -> atan_form cs arg_form
          | Op_exp -> exp_form cs arg_form
          | Op_log -> log_form cs arg_form
          | Op_sinh -> sinh_form cs arg_form
          | Op_cosh -> cosh_form cs arg_form
          | Op_tanh -> tanh_form cs arg_form
          | Op_asinh -> asinh_form cs arg_form
          | Op_acosh -> acosh_form cs arg_form
          | Op_atanh -> atanh_form cs arg_form
          | _ -> failwith 
                  ("build_form: unsupported unary operation " ^ u_op_name op)
        end
      | Bin_op (op, arg1, arg2) ->
        begin
          let arg1_form = build arg1 and
          arg2_form = build arg2 in
          match op with
          | Op_add -> add_form cs arg1_form arg2_form
          | Op_sub -> sub_form cs arg1_form arg2_form
          | Op_mul -> mul_form cs arg1_form arg2_form
          | Op_div -> div_form cs arg1_form arg2_form
          | Op_max -> max_form cs arg1_form arg2_form
          | Op_min -> min_form cs arg1_form arg2_form
          | _ -> failwith
                  ("build_form: unsupported binary operation " ^ bin_op_name op)
        end
      | Gen_op (op, args) ->
        begin
          let arg_forms = List.map build args in
          match (op, arg_forms) with
          | (Op_fma, [a;b;c]) -> add_form cs (mul_form cs a b) c
          | _ -> failwith
                  ("build_form: unsupported general operation " ^ gen_op_name op)
        end
    in
    ExprHashtbl.add cache e tf; tf
  in
  let print_cache_stats title (stats : Hashtbl.statistics) =
    Log.report `Debug "%s stats: num_bindings = %d, num_buckets = %d, max_bucket_length = %d"
      title stats.num_bindings stats.num_buckets stats.max_bucket_length
  in
  fun e ->
    ExprHashtbl.clear cache;
    reset_estimate_cache ();
    reset_index_counter ();
    let r = build e in
    print_cache_stats "Taylor forms cache" (ExprHashtbl.stats cache);
    print_cache_stats "Expr cache" (estimate_cache_stats ());
    r


(*
(* Builds a test expression with explicit variables representing rounding effects *)
let build_test_expr fp err_var =
  let add_rel e =
    let i = find_index fp.rounding e in
    let v = mk_var (err_var ^ string_of_int i) in
    let eps = 
      match fp.rounding with
	| Nearest -> v
	| Directed -> mk_def_mul const_2 v in
    mk_def_mul e (mk_def_add const_1 eps) in
  let rec build e = 
    match e with
      | Const c -> if is_fp_exact fp.eps c then e else add_rel e
      | Var _ -> e
      | U_op (Op_neg, flags, arg) -> U_op (Op_neg, flags, build arg)
      | U_op (op, flags, arg) ->
	let expr = U_op (op, flags, build arg) in
	if flags.op_exact then
	  expr
	else
	  add_rel expr
      | Bin_op (op, flags, arg1, arg2) ->
	let e_arg1 = build arg1 in
	let e_arg2 = build arg2 in
	let expr = Bin_op (op, flags, e_arg1, e_arg2) in
	if flags.op_exact then
	  expr
	else
	  add_rel expr
      | Gen_op (op, flags, args) ->
	let expr = Gen_op (op, flags, List.map build args) in
	if flags.op_exact then
	  expr
	else
	  add_rel expr
  in
  fun e ->
    let _ = reset_index_counter() in
    let result = build e in
    result, current_index()

*)
