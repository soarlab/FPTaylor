let validate_options () =
  let open Config in
  let validate_simplification () =
    if get_bool_option "maxima-simplification" && not (Maxima.test_maxima()) then
      begin
        Log.warning "A computer algebra system Maxima is not installed. \
                     Simplifications are disabled. \
                     Go to http://maxima.sourceforge.net/ to install Maxima.";
        set_option "maxima-simplification" "false"
      end
  in
  let validate_proof_record () =
    if get_bool_option "proof-record" then
      if get_bool_option "fp-power2-model" then
        begin
          Log.warning "Proof certificates (proof-record = true) are not implemented for \
                       the improved rounding model (fp-power2-model = true).";
        end
      else if get_bool_option "develop" then
        begin
          Log.warning "Proof certificates (proof-record = true) are not implemented for \
                       some features of the development mode (develop = true).";
        end
  in
  let validate_other () =
    let prec = get_int_option "print-precision" in
    if prec < 1 || prec > 1000 then begin
      Log.warning "Bad print-precision value: %d" prec;
      set_option "print-precision" "7"
    end
  in
  begin
    validate_simplification ();
    validate_proof_record ();
    validate_other ();
  end

let main () =
  Log.report `Main "FPTaylor, version %s" Version.version;
  Config.init ();
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
  validate_options ();
  Fptaylor.fptaylor input_files

let () = main ()