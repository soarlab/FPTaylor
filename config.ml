(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT licence           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* FPTaylor configuration                                                     *)
(* The code is partially based on config.ml from                              *)
(* Victor Magron's nlcertify tool                                             *)
(* -------------------------------------------------------------------------- *)

open Lib
open List

let param_table = Hashtbl.create 100 

let find_option ?default p = 
  try Hashtbl.find param_table p
  with Not_found ->
    (match default with
     | Some d -> d
     | None -> failwith ("Unknown option: " ^ p))
    
let add_option name value =
  Hashtbl.replace param_table name value

let print_options fmt =
  let print name value =
    Format.pp_print_string fmt (name ^ " = " ^ value);
    Format.pp_print_newline fmt () in
  Hashtbl.iter print param_table 

let parse_args () =
  let cfg_files = ref [] in
  let input_files = ref [] in
  let rec parse args =
    match args with
      | [] -> ()
      | "-c" :: cfg :: rest -> 
	cfg_files := cfg :: !cfg_files;
	parse rest
      | name :: rest -> 
	input_files := name :: !input_files;
	parse rest
  in
  let _ = parse (tl (Array.to_list Sys.argv)) in
  rev !cfg_files, rev !input_files

let is_comment =
  let comment = Str.regexp "^[ \t]*[\\*#].*" in
  fun str -> Str.string_match comment str 0 

let is_spaces str =
  String.trim str = ""
     
let parse_config_file fname =
  let split_regexp = Str.regexp "=" in
  let parse (c, line) = 
    let strs = Str.bounded_split split_regexp line 2 in 
    match strs with 
      | [param; value] -> String.trim param, String.trim value
      | _ -> 
	 Log.error (Format.sprintf "[File %s, line %d] Parameter parsing error: %s" fname c line);
	 failwith ("Error while parsing a configuration file: " ^ fname)
  in
  let _ = Log.report ("Config: " ^ fname) in
  let lines = Lib.enumerate 1 (load_file fname) in
  let lines = filter 
    (fun (_, s) -> not (is_spaces s) && not (is_comment s)) 
    lines in
  let param_values = map parse lines in
  List.iter (fun (k, v) -> add_option k v) param_values;
  ()

let base_dir = 
  let path =
    try
      Sys.getenv "FPTAYLOR_BASE"
    with Not_found ->
      Filename.dirname Sys.executable_name in
  let _ = Log.report (Format.sprintf "Base path: %s" path) in
  path

let cfg_files, input_files = parse_args ()
let _ = 
  let fname = Filename.concat base_dir "default.cfg" in
  try
    parse_config_file fname
  with _ ->
    Log.error ("Cannot open default.cfg: " ^ fname)

let _ = map parse_config_file cfg_files
  
let stob ?(name = "??") str = 
  try bool_of_string str
  with _ ->
    failwith (Format.sprintf "Cannot convert a string into a boolean value: %s (parameter = %s)" str name)
    
let stoi ?(name = "??") str = 
  try int_of_string str
  with _ -> 
    failwith (Format.sprintf "Cannot convert a string into an integer value: %s (parameter = %s)" str name)
  
let stof ?(name = "??") str = 
  try float_of_string str
  with _ ->
    failwith (Format.sprintf "Cannot convert a string into a floating-point value: %s (parameter = %s)" str name)

let get_string_option name = find_option name

let get_bool_option name = stob ~name (find_option name)

let get_int_option name = stoi ~name (find_option name)

let get_float_option name = stof ~name (find_option name)
  
  (* General paramaters *)
let debug = get_bool_option "debug"
let proof_flag = get_bool_option "proof-record"
let fail_on_exception = get_bool_option "fail-on-exception"
let verbosity = get_int_option "verbosity"

  (* Optimization parameters *)
let opt_tol = 
  let v = get_float_option "opt-tol" in
  if v < 1e-10 then
    let _ = Log.warning ("Bad opt-tol value: " ^ string_of_float v) in
    0.01
  else
    v
