open Js_of_ocaml

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

let js_string_of_high prec x =
  Js.string (More_num.string_of_float_hi prec x.Interval.high)

let js_opt_string_of_high prec = function
| Some x -> Js.some (js_string_of_high prec x)
| None -> Js.null

let js_opt_string_of_opt_expr = function
| None -> Js.null
| Some expr ->
  let str = ExprOut.Info.print_str expr in
  Js.some (Js.string str)
  
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
        val name = Js.string res.name
        val elapsedTime = res.elapsed_time
        val realBounds = js_array_of_interval res.real_bounds
        val absErrorModel = js_opt_string_of_opt_expr res.abs_error_model
        val relErrorModel = js_opt_string_of_opt_expr res.rel_error_model
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
