(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Logging functions                                                          *)
(* -------------------------------------------------------------------------- *)

open Format
       
type level = [`Main | `Important | `Info | `Debug]
       
let log_out = ref None
let log_fmt = ref None
let log_level = ref `Debug
       
let close () =
  match !log_out with
  | None -> ()
  | Some c ->
     Pervasives.close_out c;
     log_out := None;
     log_fmt := None
              
let append str =
  match !log_fmt with
  | None -> ()
  | Some fmt ->
     pp_print_string fmt str;
     pp_print_newline fmt ()

let open_log ?(base_dir = "log") fname =
  close ();
  try
    let base_log_dir = Lib.get_dir base_dir in
    let log_name = Filename.concat base_log_dir fname in
    let c = Pervasives.open_out log_name in
    log_out := Some c;
    log_fmt := Some (formatter_of_out_channel c)
  with Failure str ->
    let fmt = err_formatter in
    let msg = Format.sprintf
                "**ERROR** (%s): Cannot open a log file (%s). \
                 Make sure that there is no file named '%s' in the current directory.\n"
                str (Filename.concat base_dir fname) base_dir in
    pp_print_string fmt msg

let formatter () = !log_fmt

let level_of_int n =
  match n with
  | n when n <= 0 -> `Main
  | 1 -> `Important
  | 2 -> `Info
  | _ -> `Debug

let int_of_level (level : level) =
  match level with
  | `Main -> 0
  | `Important -> 1
  | `Info -> 2
  | `Debug -> 3

let cmp_levels l1 l2 =
  compare (int_of_level l1) (int_of_level l2)
                    
let set_log_level level = log_level := level

let report_str level str =
  append str;
  if cmp_levels level !log_level <= 0 then
    let fmt = std_formatter in
    pp_print_string fmt str; 
    pp_print_newline fmt ()
  else
    ()

let warning_str str =
  let str = "**WARNING**: " ^ str in
  append str;
  let fmt = err_formatter in
  pp_print_string fmt str;
  pp_print_newline fmt ()

let error_str str =
  let str = "**ERROR**: " ^ str in
  append str;
  let fmt = err_formatter in
  pp_print_string fmt str;
  pp_print_newline fmt ()

let report level fmt = Format.ksprintf (report_str level) fmt
                   
let warning fmt = Format.ksprintf warning_str fmt

let error fmt = Format.ksprintf error_str fmt
