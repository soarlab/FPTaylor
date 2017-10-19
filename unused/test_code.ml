(*
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

let basic_bb_opt e =
  let nl () = Format.pp_print_newline Format.std_formatter () in
  let _ = report "Basic BB: " in
  let p = print_string in
  let min, max = Opt_basic_bb.min_max_expr 0.01 0.01 var_bound_float e in
  let _ = p (Format.sprintf "min = %f, max = %f" min max) in
  nl()

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
*)
