(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

open Interval
open Lib
open List
open Parser
open Expr
open Expr_sym
open Taylor_form
open Environment
open Log

let exprs () = env.expressions

let var_bound_float name = variable_interval name

let var_bound_rat name =
  let v = find_variable name in
  v.lo_bound.rational_v, v.hi_bound.rational_v

let nlopt e =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report "NLOpt: " in
  let p = print_string in
  let strs = Opt_nlopt.min_max_nlopt var_bound_float e in
  let _ = p (String.concat "\n" strs) in
  nl()

let z3opt e =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report "Z3: " in
  let p = print_string in
  let min, max = Opt_z3.min_max_expr 0.01 var_bound_rat e in
  let _ = p (Format.sprintf "min = %f, max = %f" min max) in
  nl()

let opt tol e =
  let min, max =
    match Config.opt with
      | "z3" -> Opt_z3.min_max_expr tol var_bound_rat e
      | "bb" -> Opt_basic_bb.min_max_expr tol tol var_bound_float e
      | s -> failwith ("Unsupported optimization engine: " ^ s) in
  min, max

let basic_bb_opt e =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report "Basic BB: " in
  let p = print_string in
  let min, max = Opt_basic_bb.min_max_expr 0.01 0.01 var_bound_float e in
  let _ = p (Format.sprintf "min = %f, max = %f" min max) in
  nl()

let print_form f =
  let _ = report (Format.sprintf "v0 = %s" (print_expr_str f.v0)) in
  let _ = map (fun (i, e) -> 
    report (Format.sprintf "%d: %s" i (print_expr_str e))) f.v1 in
  let _ = report (Format.sprintf "m2 = %f" f.m2) in
  ()

let errors =
  let abs (a, b) =
    let x = abs_float a and
	y = abs_float b in
    max x y 
  in
  let compute_err tol i (_, e) =
    let min, max = opt tol e in
    let err = abs (min, max) in
    let _ = report (Format.sprintf "%d: %f, %f (%f)" i min max err) in
    err 
  in
  let abs_error eps tol f =
    let _ = report "\nAbsolute errors:" in
    let errs = map2 (compute_err tol) (1--length f.v1) f.v1 in
    let total1 = eps *^ sum_high errs in
    let total = total1 +^ ((f.m2 *^ eps) *^ eps) in
    let _ = report (Format.sprintf "total1: %e\ntotal: %e" total1 total) in
    if not Config.opt_approx then
      let abs_exprs = map (fun (_, e) -> mk_def_abs e) f.v1 in
      let full_expr' = 
	if abs_exprs = [] then const_0 else end_itlist mk_def_add abs_exprs in
      let full_expr = if Config.simplification then Maxima.simplify full_expr' else full_expr' in
      let min, max = opt tol full_expr in
      let _ = report (Format.sprintf "exact min, max: %f, %f" min max) in
      let total = (eps *^ abs (min, max)) +^ ((f.m2 *^ eps) *^ eps) in
      let _ = report (Format.sprintf "exact total: %e" total) in
      ()
    else
      ()
  in
  let rel_error eps tol f (f_min, f_max) =
    let f_int = {low = f_min; high = f_max} in
    let rel_tol = 0.0001 in
    if (abs_I f_int).low < rel_tol then
      report "Cannot compute relative error: values of the function are close to zero"
    else
      let _ = report "\nRelative errors:" in
      let v1 = map (fun (i, e) -> i, mk_def_div e f.v0) f.v1 in
      let v1 = 
	if Config.simplification then map (fun (i, e) -> i, Maxima.simplify e) v1 else v1 in
      let errs = map2 (compute_err tol) (1--length v1) v1 in
      let total1 = eps *^ sum_high errs in
      let b2 = (((f.m2 *^ eps) /.$ abs_I f_int) *$. eps).high in
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
  in
  fun eps f ->
    let tol = Config.opt_tol in
    let _ = report (Format.sprintf "eps = %e" eps) in
    let f_min, f_max = opt tol f.v0 in
    let _ = report (Format.sprintf "bounds: [%e, %e]" f_min f_max) in
    let _ = if Config.abs_error then abs_error eps tol f else () in
    let _ = if Config.rel_error then rel_error eps tol f (f_min, f_max) else () in
    report ""


let create_fp_parameters fp =
  let bits, min_exp =
    match fp with
      | 16 -> 10, -14
      | 32 -> 23, -126
      | 64 -> 52, -1022
	(* min_exp = -16382, but this will give 0 for eta,
	   select min_exp such that eta is the smallest positive
	   floating point number *)
      | 128 -> 112, -1074 - 112 - 1
      | _ -> failwith ("Unsupported fp value: " ^ string_of_int fp) in
  let rounding =
    match Config.rounding with
      | "nearest" -> Nearest
      | "directed" -> Directed
      | _ -> failwith ("Unsupported rounding mode: " ^ Config.rounding) in
  let eps = ldexp 0.5 (-bits) in
  (* Normalize the value by eps^2 *)
(*  let eta = ldexp 2.0 (min_exp + bits) in { *)
  let eta = ldexp 1.0 min_exp in {
    size = fp;
    eps = eps;
    delta = if Config.subnormal then eta else 0.0;
    rounding = rounding;
    uncertainty_flag = Config.uncertainty;
  }

let compute_form e =
  let fp = create_fp_parameters Config.fp in
  let vars = var_bound_float in
  let form' = build_form fp vars e in
  let form = simplify_form form' in
  let form = 
    if Config.simplification then {
      v0 = Maxima.simplify form.v0;
      v1 = map (fun (i, e) -> i, Maxima.simplify e) form.v1;
      m2 = form.m2;
    }
    else
      form in
  let _ = print_form form in
  let _ = report "" in
  let _ = errors fp.eps form in
  ()

let forms e =
  let _ = report ("\nTaylor form for: " ^ print_expr_str e) in
  let start = Unix.gettimeofday() in
  let _ = 
    try
      compute_form e
    with Failure msg -> error msg in
  let stop = Unix.gettimeofday() in
  let _ = report (Format.sprintf "Elapsed time: %.5f" (stop -. start)) in
  report ""

let gradient e =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report "Derivatives: " in
  let p = print_string in
  let vs = vars_in_expr e in
  let ds' = map (fun v -> diff v e) vs in
  let ds = map Maxima.simplify ds' in
  let dsm = map (fun v -> Maxima.simplify_diff v e) vs in
  let _ = map (fun (v, d) ->
    p v; p ": "; 
    print_expr_std d; nl ()) (zip vs ds) in
  let _ = report "Maxima diff: " in
  let _ = map (fun (v, d) ->
    p v; p ": ";
    print_expr_std d; nl ()) (zip vs dsm) in
  let dd = map2 (fun d1 d2 -> 
    Maxima.simplify (mk_sub {op_exact=false} d1 d2)) ds dsm in
  let _ = map (fun d -> 
    if not (eq_expr d const_0) then failwith "Unequal derivatives" else ()) dd in
  ()

let test_taylor e =
  let _ = report "Taylor test" in
  let fp = create_fp_parameters Config.fp in
  let test_e, i = build_test_expr fp "e" e in
  let _ = report (Format.sprintf "New vars: %d" i) in
  let _ = report (print_expr_str test_e) in
  let vs = map (fun i -> "e" ^ string_of_int i) (1--i) in
  let ts = Maxima.taylor_coeff1 vs test_e in
  let _ = map2 (fun v e -> report (Format.sprintf "%s: %s" v e)) vs ts in
  let ts = map Parser.parse_expr ts in
  let _ = report "" in
  let form = simplify_form (build_form fp var_bound_float e) in
  let _ = map (fun (i, e) -> report (Format.sprintf "%d: %s" 
				       i (print_expr_str e))) form.v1 in
  let _ = report "" in
  let ds = map2 (fun e1 (_, e2) -> Maxima.simplify (mk_def_sub e1 e2)) ts form.v1 in
  let _ = map (fun e -> report (print_expr_str e)) ds in
  report ""

let process_input fname =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report ("Loading: " ^ fname) in
  let _ = open_log ("log/" ^ fname) in
  let _ = parse_file fname in
  let es = exprs () in
(*
  let _ = report "Original expressions: " in
  let es = exprs () in
  let _ = map (fun e -> Expr.print_expr_std e; nl ()) es in
*)
(*  let _ = map nlopt es in *)
(*
  let _ = report "Simplified expressions: " in
  let es' = map Maxima.simplify es in
  let _ = map (fun e -> Expr.print_expr_std e; nl ()) es' in
*)
(*  let _ = map nlopt es' in*)
(*  let _ = map z3opt es' in*)
(*  let _ = map basic_bb_opt es' in*)
(*  let _ = map gradient es in *)
  let _ = map forms es in
(*  let _ = map test_taylor es in*)
  let _ = close_log () in
  let _ = nl() in
  ()

let main () =
  let p = print_string in
  if Config.input_files = [] then
    let _ = p ("Usage: "
	       ^ Sys.argv.(0) 
	       ^ " [-c config1] [-c config2] ... input_file1 [input_file2 ...]\n") in
    exit 1
  else
    let _ = Config.print_options Format.std_formatter in
    let _ = map process_input Config.input_files in
    exit 0

let _ = main ()
