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

let param_tbl = Hashtbl.create 100 

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

let is_comment s = String.contains s '*'
let is_spaces s = 
  let w = Str.global_substitute (Str.regexp "[ \t]+") (fun _ -> "") s in
  w = ""
     
let parse_config_file fname =
  let _ = Log.report ("Config: " ^ fname) in
  let split w = 
    let w = Str.global_substitute (Str.regexp "[ \t]+") (fun _ -> "") w in
    let w = Str.global_substitute (Str.regexp "~") (fun _ -> " ") w in
    let l = Str.split (Str.regexp "=") w in 
    match l with 
      | param :: v1 :: vs -> 
	let value = String.concat "=" (v1 :: vs) in
	param, value 
      | _ -> 
	Log.error ("Parameter error: " ^ w);
	failwith "Error while parsing configuration file"
  in
  let param_values = load_file fname in
  let param_values = filter 
    (fun s -> (not (is_spaces s)) && (not (is_comment s))) 
    param_values in
  let param_values = map split param_values in
  List.iter (fun (k, v) -> Hashtbl.replace param_tbl k v) param_values;
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
  try
    parse_config_file (Filename.concat base_dir "default.cfg")
  with _ ->
    Log.error "Cannot open default.cfg"

let _ = map parse_config_file cfg_files
  
let find p = 
  try Hashtbl.find param_tbl p
  with Not_found -> failwith ("Not found in parameters table: " ^ p)
    
let findd d p =
  try Hashtbl.find param_tbl p
  with Not_found -> d
    
let stob s = 
  try 
    bool_of_string s 
  with Invalid_argument mess -> Log.report (s ^ ": "); invalid_arg mess
    
let stoi s = 
  try int_of_string s
  with Failure _ -> failwith ("Cannot convert a string into a number: " ^ s)
  
let stof = float_of_string

let get_option name default = findd default name

let get_bool_option name default = 
  try stob (find name) with _ -> default

let get_int_option name default =
  try stoi (find name) with _ -> default

let get_float_option name default =
  try stof (find name) with _ -> default
  
  (* General paramaters *)
let debug = stob (findd "false" "debug")
let proof_flag = stob (findd "false" "proof-record")
let fail_on_exception = stob (findd "false" "fail-on-exception")
let uncertainty = stob (findd "false" "uncertainty")
let subnormal = stob (findd "true" "subnormal")
let simplification = 
  let v = stob (findd "false" "simplification") in
  if v && not (Maxima.test_maxima()) then
    let _ = 
      Log.error "A computer algebra system Maxima is not installed.";
      Log.error "Simplifications are disabled.";
      Log.error "Go to http://maxima.sourceforge.net/ to install Maxima." in
    false
  else
    v

let rel_error = stob (findd "false" "rel-error")
let abs_error = stob (findd "true" "abs-error")
(*let fp = stoi (findd "64" "fp")*)
let fp_power2_model = stob (findd "false" "fp-power2-model")
(*let rounding = findd "nearest" "rounding"*)
(*let real_vars = stob (findd "false" "real-vars")*)
let const_approx_real_vars = stob (findd "true" "const-approx-real-vars")
  
  (* Optimization parameters *)
let opt = findd "bb" "opt"
let opt_approx = stob (findd "true" "opt-approx")
let opt_exact = stob (findd "false" "opt-exact")
let opt_tol = 
  let v = stof (findd "0.01" "opt-tol") in
  if v < 1e-10 then
    let _ = Log.warning ("Bad opt-tol value: " ^ string_of_float v) in
    0.01
  else
    v
  
let print_options fmt =
  let pp str = Format.pp_print_string fmt str; Format.pp_print_newline fmt () in
  let ps name s = pp (Format.sprintf "%s = %s" name s) in
  let pb name b = pp (Format.sprintf "%s = %B" name b) in
  let pi name i = pp (Format.sprintf "%s = %d" name i) in
  let pf name f = pp (Format.sprintf "%s = %f" name f) in
  pb "proof-record" proof_flag;
  pb "fail-on-exception" fail_on_exception;
  pb "uncertainty" uncertainty;
  pb "subnormal" subnormal;
  pb "simplification" simplification;
  pb "abs-error" abs_error;
  pb "rel-error" rel_error;
  ps "opt" opt;
  pb "opt-approx" opt_approx;
  pb "opt-exact" opt_exact;
  pf "opt-tol" opt_tol;
  pi "opt-iterations" (get_int_option "bb-iter" (-1));
(*  pi "fp" fp;*)
  pb "fp-power2-model" fp_power2_model;
(*  ps "rounding" rounding;*)
(*  pb "real-vars" real_vars;*)
  pb "const-approx-real-vars" const_approx_real_vars;
    
    

