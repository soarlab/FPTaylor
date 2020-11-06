open Js_of_ocaml

module Out = ExprOut.Make(ExprOut.JavaScriptPrinter)

let init_out ty chan =
  let flush s = 
    let obj = object%js
      val ty = ty
      val str = Js.string s
    end in
    Worker.post_message obj in
  Sys_js.set_channel_flusher chan flush

let init_fs () =
  Sys_js.create_file "./default.cfg" Default.default_cfg;
  Sys_js.create_file "user.cfg" "verbosity = 1";
  Sys_js.create_file "input.txt" "";
  ()

let run_fptaylor () =
  try
    Log.report `Main "FPTaylor, version %s" Version.version;
    Config.init ["user.cfg"];
    Fptaylor.fptaylor ["input.txt"]
  with 
  | Failure msg -> Log.error_str msg; []
  | Parsing.Parse_error -> Log.error_str "Parsing error"; []
  |_ ->  Log.error_str "Unknown error"; []

class type js_msg_type = object
  method input : Js.js_string Js.t Js.readonly_prop
  method config : Js.js_string Js.t Js.readonly_prop
end

let js_array_of_interval x =
  Js.array [|x.Interval.low; x.Interval.high|]

let js_opt_array_of_interval = function
| Some x -> Js.some (js_array_of_interval x)
| None -> Js.null

let js_string_of_number_hi prec x =
  Js.string (More_num.string_of_float_hi prec x)

let js_string_of_high prec x =
  js_string_of_number_hi prec x.Interval.high

let js_opt_string_of_high prec = function
| Some x -> Js.some (js_string_of_high prec x)
| None -> Js.null

let js_expr_obj_of_opt_expr task = function
| None -> Js.null
| Some expr ->
  let var_names = Expr.vars_in_expr expr in
  let var_intervals = List.map (Task.variable_interval task) var_names in
  if List.length var_names > 1 then Js.null
  else
    let var_names = List.map (fun v -> "var_" ^ ExprOut.fix_name v) var_names in
    let es = Expr.expr_ref_list_of_expr expr in
    let n = List.length es in
    let body = es 
      |> List.mapi (fun i e ->
          if i < n - 1 then 
            Format.sprintf "var ref_%d = %s;" i (Out.print_str e)
          else
            Format.sprintf "return %s;" (Out.print_str e))
      |> String.concat "\n" in
    let str = Format.sprintf "function(%s) {\n%s\n}"
      (String.concat ", " var_names) body in
    Js.some (object%js
      val expr = Js.string str
      val dom =
        match var_intervals with
        | [x] -> Js.some (js_array_of_interval x)
        | _ -> Js.null
    end)

let process (msg : js_msg_type Js.t) =
  let input = msg##.input |> Js.to_string in
  let config = msg##.config |> Js.to_string in
  Sys_js.update_file "input.txt" input;
  Sys_js.update_file "user.cfg" config;
  let results = run_fptaylor () in
  let prec = Config.get_int_option "print-precision" in
  let res_msg = results
    |> List.map (fun res ->
      let open Fptaylor in
      object%js
        val name = Js.string res.task.name
        val elapsedTime = res.elapsed_time
        val realBounds = js_array_of_interval res.real_bounds
        val realBoundsStr = [|res.real_bounds.low; res.real_bounds.high|] 
          |> Array.map (js_string_of_number_hi prec)
          |> Js.array
        val absErrorModel = js_expr_obj_of_opt_expr res.task res.abs_error_model
        val relErrorModel = js_expr_obj_of_opt_expr res.task res.rel_error_model
        val absErrorApprox = js_opt_array_of_interval res.abs_error_approx
        val absErrorApproxStr = js_opt_string_of_high prec res.abs_error_approx
        val absErrorExact = js_opt_array_of_interval res.abs_error_exact
        val absErrorExactStr = js_opt_string_of_high prec res.abs_error_exact
        val relErrorApprox = js_opt_array_of_interval res.rel_error_approx
        val relErrorApproxStr = js_opt_string_of_high prec res.rel_error_approx
        val relErrorExact = js_opt_array_of_interval res.rel_error_exact
        val relErrorExactStr = js_opt_string_of_high prec res.rel_error_exact
        val ulpErrorApprox = js_opt_array_of_interval res.ulp_error_approx
        val ulpErrorApproxStr = js_opt_string_of_high prec res.ulp_error_approx
        val ulpErrorExact = js_opt_array_of_interval res.ulp_error_exact
        val ulpErrorExactStr = js_opt_string_of_high prec res.ulp_error_exact
      end)
    |> Array.of_list
    |> Js.array in
  Worker.post_message res_msg

let () =
  init_out 1 Stdlib.stdout;
  init_out 2 Stdlib.stderr;
  init_fs ();
  Worker.set_onmessage process
