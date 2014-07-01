(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Interval
open Lib
open List
open Parser
open Rounding
open Expr
open Environment
open Taylor_form
open Log


let exprs () = env.expressions

let var_bound_float name = variable_interval name

let var_bound_rat name =
  let v = find_variable name in
  v.lo_bound.rational_v, v.hi_bound.rational_v


let opt tol e =
  let min, max =
    match Config.opt with
      | "z3" -> Opt_z3.min_max_expr tol var_bound_rat e
      | "bb" -> Opt_basic_bb.min_max_expr tol tol var_bound_float e
      | "nlopt" -> Opt_nlopt.min_max_expr tol var_bound_float e
      | s -> failwith ("Unsupported optimization engine: " ^ s) in
  min, max


let print_form f =
  let _ = report (Format.sprintf "v0 = %s" (print_expr_str f.v0)) in
  let _ = map (fun (e, err) -> 
    report (Format.sprintf "%d: exp = %d: %s" err.index err.exp (print_expr_str e))) f.v1 in
  let _ = report "\nCorresponding original subexpressions:" in
  let _ = map (fun (_, err) ->
    let i = err.index in
    if i > 0 then
      let expr = expr_for_index i in
      report (Format.sprintf "%d: %s" i (print_expr_str expr))
    else ()) f.v1 in
  ()


let add2_symbolic (e1, exp1) (e2, exp2) =
  (* Swap if exp1 > exp2 *)
  let e1, exp1, e2, exp2 = if exp1 <= exp2 then e1, exp1, e2, exp2 else e2, exp2, e1, exp1 in
  if exp1 = 0 then 
    (if exp2 = 0 then (fp_to_const 0.0, 0) else (e2, exp2))
  else if exp2 = 0 then 
    (e1, exp1)
  else if exp1 = exp2 then
    (mk_add e1 e2, exp1)
  else
    let eps = get_eps (exp1 - exp2) in
    (mk_add (mk_mul (fp_to_const eps) e1) e2, exp2)


let sum_symbolic s = itlist add2_symbolic s (const_0, 0)


let errors =
  let abs (a, b) =
    let x = abs_float a and
	y = abs_float b in
    max x y 
  in
  let compute_bound tol (e, err) =
    let min, max = opt tol e in
    let bound = abs (min, max) in
    let _ = report (Format.sprintf "%d: exp = %d: %f, %f (%f)" err.index err.exp min max bound) in
    bound, err.exp 
  in
  let rec split es =
    match es with
      | [] -> [], []
      | (e, err) :: t ->
	let es1, es2 = split t in
	if err.index < 0 then
	  es1, (e, err) :: es2
	else
	  (e, err) :: es1, es2
  in
  let abs_error tol f =
    let _ =
      if Config.opt_approx then
	let _ = report "Solving the approximate optimization problem" in
	let _ = report "\nAbsolute errors:" in
	let v1, v2 = split f.v1 in
	let bounds1 = map (compute_bound tol) v1 in
	let bounds2 = map (compute_bound tol) v2 in
	let total1', exp1 = sum_high bounds1 and
	    total2', exp2 = sum_high bounds2 in
	let total1 = get_eps exp1 *^ total1' and
	    total2 = get_eps exp2 *^ total2' in
	let total = total1 +^ total2 in
	let _ = report (Format.sprintf "total1: %e\ntotal2: %e\ntotal: %e" total1 total2 total) in
	()
      else () in
    let _ =
      if Config.opt_exact then
	let _ = report "Solving the exact optimization problem" in
	let v1, v2 = split f.v1 in
	let bounds2 = map (compute_bound tol) v2 in
	let total2', exp2 = sum_high bounds2 in
	let total2 = get_eps exp2 *^ total2' in
	let abs_exprs = map (fun (e, err) -> mk_abs e, err.exp) v1 in
	let full_expr', exp = sum_symbolic abs_exprs in
	let full_expr = if Config.simplification then Maxima.simplify full_expr' else full_expr' in
	let min, max = opt tol full_expr in
	let _ = report (Format.sprintf "exact min, max (exp = %d): %f, %f" exp min max) in
	let total = (get_eps exp *^ abs (min, max)) +^ total2 in
	let _ = report (Format.sprintf "exact total: %e" total) in
	()
      else
	() in
    ()
  in
(*  let rel_error eps tol f (f_min, f_max) =
    let f_int = {low = f_min; high = f_max} in
    let rel_tol = 0.0001 in
    if (abs_I f_int).low < rel_tol then
      report "Cannot compute the relative error: values of the function are close to zero"
    else
      let _ = report "\nRelative errors:" in
      let v1, v2 = split f.v1 in
      let v1 = map (fun (i, e) -> i, mk_def_div e f.v0) v1 in
      let v1 = 
	if Config.simplification then map (fun (i, e) -> i, Maxima.simplify e) v1 else v1 in
      let errs1 = map (compute_err tol) v1 in
      let errs2 = map (compute_err tol) v2 in
      let total1 = eps *^ sum_high errs1 in
      let b2 = ((eps *^ sum_high errs2) /.$ abs_I f_int).high in
      let total = total1 +^ b2 in
      let _ = report (Format.sprintf "rel-total1: %e\nrel-total: %e" total1 total) in
      if not Config.opt_approx then
	let abs_exprs = map (fun (_, e) -> mk_def_abs e) v1 in
	let full_expr' = 
	  if abs_exprs = [] then const_0 else end_itlist mk_def_add abs_exprs in
	let full_expr = if Config.simplification then Maxima.simplify full_expr' else full_expr' in
	let min, max = opt tol full_expr in
	let _ = report (Format.sprintf "exact min-rel, max-rel: %f, %f" min max) in
	let total = (eps *^ abs (min, max)) +^ b2 in
	let _ = report (Format.sprintf "exact total-rel: %e" total) in
	()
      else
	()
  in*)
  fun f ->
    let tol = Config.opt_tol in
    let f_min, f_max = opt tol f.v0 in
    let _ = report (Format.sprintf "bounds: [%e, %e]" f_min f_max) in
    let _ = if Config.abs_error then abs_error tol f else () in
(*    let _ = if Config.rel_error then rel_error eps tol f (f_min, f_max) else () in*)
    report ""

let compute_form e =
  let vars = var_bound_float in
  let form' = build_form vars e in
  let form = simplify_form vars form' in
  let form = 
    if Config.simplification then {
      v0 = Maxima.simplify form.v0;
      v1 = map (fun (e, err) -> (if err.index < 0 then e else Maxima.simplify e), err) form.v1;
    }
    else
      form in
  let _ = print_form form in
  let _ = report "" in
  let _ = errors form in
  ()


let forms e =
  let _ = report ("\nTaylor form for: " ^ print_expr_str e) in
  let start = Unix.gettimeofday() in
  let e = Rounding_simpl.simplify_rounding e in
  let _ = 
    try
      compute_form e
    with Failure msg -> error msg in
  let stop = Unix.gettimeofday() in
  let _ = report (Format.sprintf "Elapsed time: %.5f" (stop -. start)) in
  report ""


let process_input fname =
  let _ = report ("Loading: " ^ fname) in
  let _ = open_log (Filename.concat "log" fname) in
  let _ = parse_file fname in
  let es = exprs () in
  let _ = map forms es in
  let _ = close_log () in
  report ""


let main () =
  let p = print_string in
  if Config.input_files = [] then
    let _ = p ("Usage: "
	       ^ Sys.argv.(0) 
	       ^ " [-c config1] [-c config2 ...] input_file1 [input_file2 ...]\n") in
    exit 1
  else
    let _ = Config.print_options Format.std_formatter in
    let _ = map process_input Config.input_files in
    exit 0

let _ = main ()
