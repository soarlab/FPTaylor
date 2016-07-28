(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Logging functions                                                          *)
(* -------------------------------------------------------------------------- *)

open Format

let open_log, close_log, append_to_log, log_fmt =
  let out = ref None in
  let fmt = ref None in
  let close_log () =
    match !out with
      | None -> ()
      | Some c ->
	close_out c;
	out := None;
	fmt := None
  in
  let open_log fname =
    let () = close_log() in
    try
      let base_log_dir = Lib.get_dir "log" in
      let log_name = Filename.concat base_log_dir (Filename.basename fname ^ ".log") in
      let c = open_out log_name in
      out := Some c;
      fmt := Some (formatter_of_out_channel c)
    with Failure _ ->
      let fmt = err_formatter in
      pp_print_string fmt "**ERROR**: Cannot open a log file. \
                           Make sure that there is no file named 'log' in the current directory.\n"
  in
  let append_to_log str =
    match !fmt with
      | None -> ()
      | Some fmt ->
	pp_print_string fmt str;
	pp_print_newline fmt ()
  in
  let log_fmt () = !fmt
  in
  open_log,
  close_log,
  append_to_log,
  log_fmt


let report_flag = ref true
let warning_flag = ref true

let report str =
  let _ = append_to_log str in
  if !report_flag then
    let fmt = std_formatter in
    pp_print_string fmt str; 
    pp_print_newline fmt ()
  else
    ()

let warning str =
  let str = "**WARNING**: " ^ str in
  let _ = append_to_log str in
  if !warning_flag then
    let fmt = err_formatter in
    pp_print_string fmt str;
    pp_print_newline fmt ()
 else
  ()

let issue_warning p str =
  if p then warning str else ()

let error str =
  let str = "**ERROR**: " ^ str in
  let _ = append_to_log str in
  let fmt = err_formatter in
  pp_print_string fmt str;
  pp_print_newline fmt ()
