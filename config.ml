(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

let param_table = Hashtbl.create 100 

let short_names = Hashtbl.create 100

let input_files_ref = ref []

let base_dir_ref = ref ""

let loaded_cfg_files = ref []

let find_option ?default p = 
  try Hashtbl.find param_table p
  with Not_found -> begin
      match default with
      | Some d -> d
      | None -> failwith ("Unknown option: " ^ p)
    end

let set_option ?(init = false) ?(short = "") name value =
  if short <> "" then begin
      if Hashtbl.mem short_names short then
        let old = Hashtbl.find short_names short in
        failwith (Format.sprintf
                    "Short name '%s' is already used for '%s' (new option name = '%s')"
                    short old name)
      else
        Hashtbl.replace short_names short name
    end;
  if init || Hashtbl.mem param_table name then
    Hashtbl.replace param_table name value
  else
    failwith (Format.sprintf
                "Unknown option: %s = %s (see available options in default.cfg)"
                name value)

let is_comment =
  let comment = Str.regexp "^[ \t]*[\\*#].*" in
  fun str -> Str.string_match comment str 0 

let is_spaces str =
  String.trim str = ""

let parse_config_file ?(init = false) fname =
  Log.report `Main "Loading configuration file: %s" fname;
  let arg_list = ref [] in
  let doc_comment = ref "" in
  let short_name = ref "" in
  let split_regexp = Str.regexp "=" in
  let short_regexp = Str.regexp "^\\[short:[ \t]*\\([a-zA-Z][a-zA-Z0-9]*\\)\\]$" in
  let parse_short_name str =
    if Str.string_match short_regexp str 0 then
      short_name := Str.matched_group 1 str
    else
      ()
  in
  let add_option param value short doc =
    let param = String.trim param in
    let value = String.trim value in
    set_option ~init ~short:short param value;
    let doc_str =
      let doc' = if short <> "" then "(-" ^ short ^ ")" else "" in
      doc' ^ doc in
    arg_list := ("--" ^ param, Arg.String (set_option param), doc_str) :: !arg_list;
    if short <> "" then
      arg_list := ("-" ^ short, Arg.String (set_option param), "") :: !arg_list 
  in
  let parse_option c short doc line =
    let strs = Str.bounded_split split_regexp line 2 in 
    match strs with 
    | [param; value] -> add_option param value short doc
    | [param] -> add_option param "" short doc
    | _ -> 
       Log.error "[File %s, line %d] Parameter parsing error: %s" fname c line;
       failwith ("Error while parsing a configuration file: " ^ fname)
  in
  let rec parse_lines c lines =
    match lines with
    | line :: rest ->
       let line = String.trim line in
       if line = "" then ()
       else if is_comment line then begin
           if Lib.starts_with line ~prefix:"##" then
             doc_comment := String.sub line 2 (String.length line - 2)
         end
       else if Lib.starts_with line ~prefix:"[" then
         parse_short_name line
       else begin
         parse_option c !short_name !doc_comment line;
         short_name := "";
         doc_comment := "";
         end;
       parse_lines (c + 1) rest
    | [] -> ()
  in
  let lines = try Lib.load_file fname
              with _ -> failwith ("Cannot open a configuration file: " ^ fname) in
  parse_lines 1 lines;
  loaded_cfg_files := !loaded_cfg_files @ [fname];
  List.rev !arg_list

let export_options fmt =
  let print (name, value) =
    if name <> "export-options" then
      Format.fprintf fmt "%s = %s\n" name value in
  Hashtbl.fold (fun name value lst -> (name, value) :: lst) param_table [] 
    |> List.sort compare
    |> List.iter print

let print_options ~level:level =
  let print name value =
    Log.report level "%s = %s" name value in
  Log.report level "Base path: %s" !base_dir_ref;
  List.iter (Log.report level "Config file: %s") !loaded_cfg_files;
  Hashtbl.iter print param_table

let print_short_names ~level:level =
  let print short name =
    Log.report level "%s (short: %s)" name short in
  Hashtbl.iter print short_names

let stob ?(name = "??") str = 
  try bool_of_string str
  with _ ->
    failwith (Format.sprintf
                "Cannot convert a string into a boolean value: %s (parameter = %s)"
                str name)
    
let stoi ?(name = "??") str = 
  try int_of_string str
  with _ -> 
    failwith (Format.sprintf
                "Cannot convert a string into an integer value: %s (parameter = %s)"
                str name)
  
let stof ?(name = "??") str = 
  try float_of_string str
  with _ ->
    failwith (Format.sprintf
                "Cannot convert a string into a floating-point value: %s (parameter = %s)"
                str name)

let get_string_option name = find_option name

let get_bool_option name = stob ~name (find_option name)

let get_int_option name = stoi ~name (find_option name)

let get_float_option name = stof ~name (find_option name)

let is_option_defined name =
  try ignore (find_option name); true with Failure _ -> false

(* General paramaters *)

let base_dir () = !base_dir_ref

let input_files () = !input_files_ref

let debug () = get_bool_option "debug"
let proof_flag () = get_bool_option "proof-record"
let fail_on_exception () = get_bool_option "fail-on-exception"

(* Returns the base directory *)
let get_base_dir () =
  try 
    let base = Sys.getenv "FPTAYLOR_BASE" in
    Log.report `Main "***** The environment variable FPTAYLOR_BASE is defined = '%s'" base;
    base
  with Not_found ->
    Filename.dirname Sys.executable_name

(* Clears all mutable values *)
let clear_all () =
  Hashtbl.clear param_table;
  Hashtbl.clear short_names;
  loaded_cfg_files := [];
  base_dir_ref := "";
  input_files_ref := []

(* Loads the main configuration file and parses arguments *)
(* Parameters from the given config_files override parameters from command line arguments *)
let init ?(main_cfg_fname = "default.cfg") ?usage config_files =
  let usage_msg =
    match usage with Some msg -> msg | None ->
      Printf.sprintf "\nUsage: %s [--opt_name opt_value ...] [-c config1 ...] \
                      input_file1 [input_file2 ...]\n\n\
                      See default.cfg for a complete list of available \
                      options and their values.\n"
                    Sys.argv.(0) in
  clear_all ();
  base_dir_ref := get_base_dir ();
  let files = ref [] in
  let add_file name = files := name :: !files in
  let parse_config_arg name = ignore (parse_config_file ~init:false name) in
  let main_cfg = Filename.concat !base_dir_ref main_cfg_fname in
  input_files_ref := begin
    try
      let c_arg = ("-c", Arg.String parse_config_arg, 
                  "filename Load options from a file.") in 
      let fpcore_arg = ("--fpcore-out", Arg.String (set_option ~init:true "fpcore-out"), 
                        "filename Exports tasks to the FPCore format") in
      let args = c_arg :: fpcore_arg :: parse_config_file main_cfg ~init:true in
      let args = Arg.align args in
      Arg.parse args add_file usage_msg;
      List.iter parse_config_arg config_files;
      List.rev !files;
    with
    | Failure msg | Sys_error msg ->
      Log.error_str msg;
      exit 2
    | _ ->
      Log.error "Cannot open %s: %s" main_cfg_fname main_cfg;
      exit 2
  end;
  let verbosity = get_int_option "verbosity" in
  if verbosity < 0 then
    Log.warning "verbosity < 0: %d" verbosity;
  Log.set_log_level (Log.level_of_int verbosity)
