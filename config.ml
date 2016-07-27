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

let short_names = Hashtbl.create 100

let find_option ?default p = 
  try Hashtbl.find param_table p
  with Not_found ->
    (match default with
     | Some d -> d
     | None -> failwith ("Unknown option: " ^ p))

let add_option ?(init = false) ?(short = "") name value =
  if short <> "" then Hashtbl.replace short_names short name;
  if init then
    Hashtbl.replace param_table name value
  else
    try
      let _ = Hashtbl.find param_table name in
      Hashtbl.replace param_table name value
    with Not_found ->
	 failwith (Format.sprintf "Unknown option: %s = %s (see available options in default.cfg)" name value)

let print_options fmt =
  let print name value =
    Format.pp_print_string fmt (name ^ " = " ^ value);
    Format.pp_print_newline fmt () in
  Hashtbl.iter print param_table

let print_short_names fmt =
  let print short name =
    Format.fprintf fmt "%s (short: %s)\n" name short in
  Hashtbl.iter print short_names

let is_comment =
  let comment = Str.regexp "^[ \t]*[\\*#].*" in
  fun str -> Str.string_match comment str 0 

let is_spaces str =
  String.trim str = ""

let parse_config_file ?(init = false) fname =
  let split_regexp = Str.regexp "=" in
  let short_regexp = Str.regexp "^\\[short:[ \t]*\\([a-zA-Z][a-zA-Z0-9]*\\)\\]$" in
  let parse_short_name str =
    if Str.string_match short_regexp str 0 then
      Str.matched_group 1 str
    else
      ""
  in
  let parse_option c short_name line =
    let strs = Str.bounded_split split_regexp line 2 in 
    match strs with 
    | [param; value] -> 
       add_option ~init ~short:short_name (String.trim param) (String.trim value)
    | _ -> 
       Log.error (Format.sprintf "[File %s, line %d] Parameter parsing error: %s" 
				 fname c line);
       failwith ("Error while parsing a configuration file: " ^ fname)
  in
  let rec parse_lines c short_name lines =
    match lines with
    | line :: rest ->
       let line = String.trim line in
       if line = "" || is_comment line then
	 parse_lines (c + 1) short_name rest
       else if Lib.starts_with "[" line then
	 let short = parse_short_name line in
	 if short <> "" then
	   parse_lines (c + 1) short rest
	 else
	   let () = parse_option c short_name line in
	   parse_lines (c + 1) "" rest
       else
	 let () = parse_option c short_name line in
	 parse_lines (c + 1) "" rest
    | [] -> ()
  in
  let _ = Log.report ("Config: " ^ fname) in
  let lines = load_file fname in
  parse_lines 1 "" lines

let parse_args () =
  let input_files = ref [] in
  let get_opt_name opt =
    let k = String.length opt in
    if Lib.starts_with "--" opt then
      String.sub opt 2 (k - 2)
    else if Lib.starts_with "-" opt then
      let short = String.sub opt 1 (k - 1) in
      Hashtbl.find short_names short 
    else
      raise Not_found
  in
  let rec parse args =
    match args with
    | [] -> ()
    | name :: rest ->
       if Lib.starts_with "-" name then
	 let value, rest =
	   (match rest with
	    | value :: rs -> value, rs
	    | [] -> failwith (Format.sprintf "Option without value: %s" name)) in
	 let () =
	   if name = "-c" then
	     parse_config_file value
	   else
	     begin
	       try
		 let opt_name = get_opt_name name in
		 add_option opt_name value
	       with Not_found ->
		 failwith (Format.sprintf "Unknown command line option: %s (value: %s)" name value)
	     end
	 in
	 parse rest
       else
	 let () = input_files := name :: !input_files in
	 parse rest
  in
  let () = parse (tl (Array.to_list Sys.argv)) in
  rev !input_files

(* Set the base directory *)
let base_dir = 
  let path =
    try
      Sys.getenv "FPTAYLOR_BASE"
    with Not_found ->
      Filename.dirname Sys.executable_name in
  let _ = Log.report (Format.sprintf "Base path: %s" path) in
  path

(* Load the main configuration file *)
let () = 
  let fname = Filename.concat base_dir "default.cfg" in
  try
    parse_config_file fname ~init:true
  with _ ->
    Log.error ("Cannot open default.cfg: " ^ fname)

(* Parse options and load other configuration files *)
let input_files = parse_args ()
  
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

