open Num
open List
open Proof_base

let str_of_num n =
  Format.sprintf "Num.num_of_string \"%s\"" (string_of_num n)

let proof_to_text fmt =
  let nl = Format.pp_print_newline fmt in
  let p' str = Format.pp_print_string fmt str; nl() in
  let p = Format.pp_print_string fmt in
  let string_of_float f = Format.sprintf "%.20e" f in
  let str_of_var v =
    Format.sprintf "{name = \"%s\"; low = %s; high = %s}" 
      v.name (str_of_num v.low) (str_of_num v.high)
  in
  let str_of_rnd rnd =
    Format.sprintf "{bits = %d; coefficient = %s}"
      rnd.bits (string_of_float rnd.coefficient)
  in
  let head pp vars =
    p' "(* Set the correct (absolute) path to the FPTaylor/formal directory or";
    p' "   copy this file into that directory *)";
    p' "(* load_path := \"~/FPTaylor/formal\" :: !load_path;; *)";
    p' "";
    p' "needs \"proof_rules.hl\";;";
    p' "";
    p' "open Proof;;";
    p' "open Build_tform2;;";
    p' "open Proof_rules;;";
    p' "";
    p' (Format.sprintf "let prec = %d;;" pp);
    p' "let x_tm, dom_tm, dom_vars, var_names = build_domain prec [";
    let _ = map (fun v -> p "  "; p (str_of_var v); p' ";") vars in ();
    p' "];;";
    p' "let dom = prec, dom_tm, dom_vars, var_names;;";
  in
  let rec tform_proof = function
    | [] -> ()
    | step :: rest ->
      let args = step.proof_args in
      let arg_ths = map (fun i -> Format.sprintf "tform_%d" i) args.arg_indices in
      let err_indices = 
	let strs = map string_of_int args.err_indices in
	let str = String.concat "; " strs in
	"[" ^ str ^ "]" in
      let _ = p (Format.sprintf "let tform_%d = " step.step_index) in
      let _ =
	begin
	  match step.proof_op with
	    | Proof_var name ->
	      p (Format.sprintf "rule_var dom (rev_assoc \"%s\" var_names)" name)
	    | Proof_rnd_bin_var (rnd, name) ->
	      let n = int_of_float (nth args.bounds 0) in
	      let b = string_of_float (nth args.bounds 1) in
	      p' (Format.sprintf "rule_rnd_bin_var dom (rev_assoc \"%s\" var_names)" name);
	      p (Format.sprintf  "  %s (%d) %s %s" (str_of_rnd rnd) n b err_indices)
	    | Proof_const c ->
	      p (Format.sprintf "rule_const dom \"%s\"" (string_of_num c))
	    | Proof_rnd_bin_const (rnd, c) ->
	      let n = int_of_float (nth args.bounds 0) in
	      let b = string_of_float (nth args.bounds 1) in
	      p' (Format.sprintf "rule_rnd_bin_const dom \"%s\"" (string_of_num c));
	      p (Format.sprintf  "  %s (%d) %s %s" (str_of_rnd rnd) n b err_indices)
	    | Proof_rnd rnd ->
	      let m2 = string_of_float (nth args.bounds 0) in
	      let b = string_of_float (nth args.bounds 1) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_rnd dom %s\n  %s %s %s %s"
		   (str_of_rnd rnd) m2 b err_indices arg)
	    | Proof_neg ->
	      p (Format.sprintf "rule_neg dom %s" (nth arg_ths 0))
	    | Proof_add ->
	      p (Format.sprintf "rule_add dom %s %s" (nth arg_ths 0) (nth arg_ths 1))
	    | Proof_sub ->
	      p (Format.sprintf "rule_sub dom %s %s" (nth arg_ths 0) (nth arg_ths 1))
	    | Proof_mul ->
	      let m = string_of_float (nth args.bounds 0) in
	      let e = int_of_float (nth args.bounds 1) in
	      let arg1 = nth arg_ths 0 in
	      let arg2 = nth arg_ths 1 in
	      p (Format.sprintf "rule_mul dom %s (%d) %s %s %s" m e err_indices arg1 arg2)
	    | Proof_inv ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_inv dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_sqrt ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_sqrt dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_sin ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_sin dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_cos ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_cos dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_exp ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_exp dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_log ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_log dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_atn ->
	      let m1 = string_of_float (nth args.bounds 0) in
	      let m2 = string_of_float (nth args.bounds 1) in
	      let e2 = int_of_float (nth args.bounds 2) in
	      let b = string_of_float (nth args.bounds 3) in
	      let m3 = string_of_float (nth args.bounds 4) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_atn dom %s %s (%d) %s %s %s %s"
		   m1 m2 e2 b m3 err_indices arg)
	    | Proof_simpl_eq (i, j) ->
	      p (Format.sprintf "rule_simpl_eq dom %d %d %s" i j (nth arg_ths 0))
	    | Proof_simpl_add (i, j) ->
	      let b = string_of_float (nth args.bounds 0) in
	      let e = int_of_float (nth args.bounds 1) in
	      let arg = nth arg_ths 0 in
	      p (Format.sprintf "rule_simpl_add dom %d %d %s (%d) %s %s"
		   i j b e err_indices arg)
	end in
      let _ = p' ";;" in
      tform_proof rest
  in
  let bound_proof i opt =
    let rule_name = 
      match opt.opt_type with
	| Proof_opt_approx -> "verify_bounds_approx"
	| Proof_opt_exact -> "verify_bounds_exact" in
    let bs = String.concat "; " (map string_of_float opt.opt_bounds) in
    let total = string_of_float opt.total_bound in
    let indices = String.concat "; " (map string_of_int opt.opt_indices) in
    p "let result = ";
    p (Format.sprintf "%s prec\n  [%s]\n  %s [%s] var_names tform_%d" 
	 rule_name bs total indices i);
    p' ";;"
  in
  fun (pp, proof) ->
    let steps = sort (fun s1 s2 -> compare s1.step_index s2.step_index) proof.proof_steps in
    let last = (nth steps (length steps - 1)).step_index in
    head pp proof.proof_vars;
    p' ""; p' "(* Taylor form theorems *)";
    tform_proof steps;
    p' ""; p' "(* Verification of inequalities *)";
    bound_proof last (hd proof.proof_opts)

let parse_args () =
  let input = ref "" and
      output = ref "" and
      prec = ref 0 in
  let rec parse = function
    | [] -> ()
    | "-p" :: arg :: rest ->
      prec := int_of_string (String.trim arg);
      parse rest
    | "-o" :: arg :: rest ->
      output := String.trim arg;
      parse rest
    | name :: rest ->
      input := name;
      parse rest 
  in
  let _ = parse (tl (Array.to_list Sys.argv)) in
  let _ = !input <> "" || 
    (print_string ("Usage: " ^ Sys.argv.(0) ^ " [-o output.hl] [-p precision] input.proof\n");
     exit 1) in
  let _ = Sys.file_exists !input || failwith ("File does not exist: " ^ !input) in
  let _ = if !output = "" then output := (Filename.chop_extension !input) ^ ".hl" in
  let _ = if !prec <= 0 then prec := 10 in
  !input, !output, !prec

let () =
  try
    let input, output, prec = parse_args() in
    let proof = load_proof input in
    Lib.write_to_file output proof_to_text (prec, proof)
  with Failure msg ->
    print_string ("Error: " ^ msg ^ "\n");
    exit 2
