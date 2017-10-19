(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Auxiliary parser functions                                                 *)
(* -------------------------------------------------------------------------- *)

let parse_raw_expr str =
  let lexbuf = Lexing.from_string str in
  Input_parser.expr Input_lexer.token lexbuf

let parse_expr str =
  let raw = parse_raw_expr str in
  Environment.transform_raw_expr raw

let parse_string str =
  Environment.reset();
  let lexbuf = Lexing.from_string str in
  Input_parser.first Input_lexer.token lexbuf

let parse_file fname =
  let lines = Lib.load_file fname in
  let str = String.concat "\n" lines in
  parse_string str

let parse_fpbench_file fname bench_name =
  let rec parse_all acc lexbuf = try
    let s_expr = Fpbench_parser.parse_s_expr Fpbench_lexer.token lexbuf in
    let data = Fpbench_parser.translate_fpcore s_expr in
    parse_all (data :: acc) lexbuf
  with 
  | Failure msg ->
    Log.warning "Error in a benchmark: %s" msg;
    parse_all acc lexbuf 
  | End_of_file -> List.rev acc
  in
  let find_data ds name =
    try List.find (fun d -> Lib.starts_with d.Fpbench_parser.name ~prefix:name) ds
    with Not_found ->
      Log.warning "File %s does not contain the benchmark %s" fname name;
      List.hd ds
  in
  let add_var (var, (lo, hi)) =
    match (lo, hi) with
    | (Some lo, Some hi) ->
      Environment.add_variable Rounding.real_type var lo hi
    | _ ->
      failwith (Format.sprintf "Variable %s is not bounded" var) 
  in
  let lines = Lib.load_file fname in
  let str = String.concat "\n" lines in
  let lexbuf = Lexing.from_string str in
  let ds = parse_all [] lexbuf in
  let data = find_data ds bench_name in
  Log.report `Main "FPBench benchmark: %s" data.Fpbench_parser.name;
  Environment.reset();
  List.iter add_var data.Fpbench_parser.vars;
  List.iter (fun c -> Environment.add_constraint "constraint" c) 
    data.Fpbench_parser.constraints;
  Environment.add_expression_with_name 
    data.Fpbench_parser.name data.Fpbench_parser.expr
