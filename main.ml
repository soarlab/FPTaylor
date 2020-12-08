(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* FPTaylor's main function                                                   *)
(* -------------------------------------------------------------------------- *)

let main () =
  Log.report `Main "FPTaylor, version %s" Version.version;
  Config.init [];
  let input_files = Config.input_files () in
  if input_files = [] then begin
    let prog_name = Sys.argv.(0) in
    Printf.printf
      "\nUsage: %s [--opt_name opt_value ...] [-c config1 ...] \
       input_file1 [input_file2 ...]\n\n\
       Run '%s --help' to see a list of available options.\n\n"
      prog_name prog_name;
    exit 1
  end;
  Fptaylor.validate_options ();
  Fptaylor.fptaylor input_files |> ignore

let () = main ()