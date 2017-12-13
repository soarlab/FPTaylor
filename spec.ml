open Expr

module Out = ExprOut.Info

let is_const_expr e = (vars_in_expr e = [])

let normalize_rat_expr =
  let mk_mul a b =
    if eq_expr a const_1 then b
    else if eq_expr b const_1 then a
    else mk_mul a b in
  let rec norm e =
    match e with
    | _ when is_const_expr e -> e, const_1
    | Var _ -> e, const_1
    | U_op (op, arg) -> begin
      let p, q = norm arg in
      match op with
      | Op_neg -> mk_neg p, q
      | Op_inv -> q, p
      | _ -> failwith ("normalize_rat_expr: unsupported operation " ^ u_op_name op)
    end
    | Bin_op (op, arg1, arg2) -> begin
      let p1, q1 = norm arg1 in
      let p2, q2 = norm arg2 in
      match op with
      | Op_add -> mk_add (mk_mul p1 q2) (mk_mul p2 q1), mk_mul q1 q2
      | Op_sub -> mk_sub (mk_mul p1 q2) (mk_mul p2 q1), mk_mul q1 q2
      | Op_mul -> mk_mul p1 p2, mk_mul q1 q2
      | Op_div -> mk_mul p1 q2, mk_mul p2 q1
      | _ -> failwith ("normalize_rat_expr: unsupported operation " ^ bin_op_name op)
    end
    | _ -> failwith "normalize_rat_expr: unsupported operation" in
  norm

let gen_sollya_code fmt =
  let p_expr fmt e = Out.print_fmt fmt e in

  fun (cs, expr, spec) ->
    let var_names = Lib.union (vars_in_expr expr) (vars_in_expr spec) in
    if List.length var_names <> 1 then 
      failwith ("gen_sollya_code: wrong number of variables: " ^ string_of_int (List.length var_names));
    let low, high = cs.var_rat_bounds (List.hd var_names) in
    let p, q = normalize_rat_expr expr in
    Format.fprintf fmt "f = %a;@." p_expr spec;
    Format.fprintf fmt "p = %a;@." p_expr p;
    Format.fprintf fmt "q = %a;@." p_expr q;
    Format.fprintf fmt "I = [%s, %s];@." 
      (Num.string_of_num low) (Num.string_of_num high)

let compute_spec_rel_error cs expr ~spec =
  let tmp = Lib.get_tmp_dir () in
  let sollya_name = Filename.concat tmp "spec.sollya" in
  let opt = Filename.concat Config.base_dir "spec/opt.sollya" in
  Lib.write_to_file sollya_name gen_sollya_code (cs, expr, spec);
  let cmd = Format.sprintf "sollya %s %s" opt sollya_name in
  let out = Lib.run_cmd cmd in
  let output = String.concat "\n" out in
  Log.report `Info "\nSollya output:\n%s\n" output;
  let err = Opt_common.get_float out "bound = " in
  err
