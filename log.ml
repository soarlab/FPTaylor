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
  let open_log ?(base_dir = "log") fname =
    let () = close_log() in
    try
      let base_log_dir = Lib.get_dir base_dir in
      let log_name = Filename.concat base_log_dir fname in
      let c = open_out log_name in
      out := Some c;
      fmt := Some (formatter_of_out_channel c)
    with Failure _ ->
      let fmt = err_formatter in
      let msg = Format.sprintf "**ERROR**: Cannot open a log file (%s). \
                                Make sure that there is no file named '%s' in the current directory.\n"
                               (Filename.concat base_dir fname) base_dir in
      pp_print_string fmt msg
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

let report_str str =
  let _ = append_to_log str in
  if !report_flag then
    let fmt = std_formatter in
    pp_print_string fmt str; 
    pp_print_newline fmt ()
  else
    ()

let warning_str str =
  let str = "**WARNING**: " ^ str in
  let _ = append_to_log str in
  if !warning_flag then
    let fmt = err_formatter in
    pp_print_string fmt str;
    pp_print_newline fmt ()
 else
   ()

let issue_warning_str p str =
  if p then warning_str str else ()

let error_str str =
  let str = "**ERROR**: " ^ str in
  let _ = append_to_log str in
  let fmt = err_formatter in
  pp_print_string fmt str;
  pp_print_newline fmt ()

let report fmt = Format.ksprintf report_str fmt
                   
let warning fmt = Format.ksprintf warning_str fmt

let error fmt = Format.ksprintf error_str fmt

let issue_warning p fmt = Format.ksprintf (issue_warning_str p) fmt

