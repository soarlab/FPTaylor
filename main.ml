let main () =
  Log.report `Main "FPTaylor, version %s" Version.version;
  Config.init ~config_files:[];
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