(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* FPTaylor's Export Tool                                                     *)
(* -------------------------------------------------------------------------- *)

let export_file fmt fname =
  Log.report `Main "Exporting: %s" fname;
  let tasks = Parser.parse_file fname in
  let var_type = Config.get_string_option "var-type" in
  let vt = 
    if String.length var_type = 0 then None 
    else Some (Rounding.string_to_value_type var_type) in
  List.iter (Out_fpcore.generate_fpcore ?var_type:vt fmt) tasks;
  Format.pp_print_flush fmt ()

let export output input_files =
  let oc = if String.length output = 0 then stdout else open_out output in
  let fmt = Format.formatter_of_out_channel oc in
  List.iter (export_file fmt) input_files;
  close_out oc

let main () =
  Log.report `Main "FPTaylor's Export Tool, version %s" Version.version;
  let usage = 
    Printf.sprintf "\nUsage: %s [--opt_name opt_value ...] [-c config1 ...] \
        [-o output_file] input_file1 [input_file2 ...] \n\n\
        See export.cfg for a complete list of available \
        options and their values.\n"
      Sys.argv.(0) in
  Config.init ~main_cfg_fname:"export.cfg" ~usage [];
  let files = Config.input_files () in
  if files = [] then begin
    print_string usage;
    exit 1
  end;
  export (Config.get_string_option "output") files

let () = main ()