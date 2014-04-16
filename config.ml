(* FPTaylor                                                                   *)
(* Alexey Solovyev, University of Utah                                        *)

(* The code is based on config.ml from
   Victor Magron's nlcertify tool *)
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
  let _ = report ("Config: " ^ fname) in
  let split w = 
    let w = Str.global_substitute (Str.regexp "[ \t]+") (fun _ -> "") w in
    let l = Str.split (Str.regexp "=") w in 
    match l with 
      | [param; value] -> param, value 
      | _ -> 
	report ("Parameter error: " ^ w);
	failwith "Error while parsing configuration file"
  in
  let param_values = load_file fname in
  let param_values = filter 
    (fun s -> (not (is_spaces s)) && (not (is_comment s))) 
    param_values in
  let param_values = map split param_values in
  List.iter (fun (k, v) -> Hashtbl.replace param_tbl k v) param_values;
  ()

let cfg_files, input_files = parse_args ()
let _ = parse_config_file "default.cfg"
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
  with Invalid_argument mess -> report (s ^ ": "); invalid_arg mess
    
let stoi = int_of_string
  
let stof = float_of_string

let get_option name default = findd default name

let get_bool_option name default = 
  try stob (find name) with _ -> default

let get_int_option name default =
  try stoi (find name) with _ -> default

let get_float_option name default =
  try stof (find name) with _ -> default
  
  (* General paramaters *)
let uncertainty = stob (findd "false" "uncertainty")
let subnormal = stob (findd "true" "subnormal")
let simplification = stob (findd "false" "simplification")
let rel_error = stob (findd "false" "rel-error")
  
  (* Optimization parameters *)
let opt = find "opt"
let opt_approx = stob (findd "true" "opt-approx")
  
let print_options fmt =
  let pp str = Format.pp_print_string fmt str; Format.pp_print_newline fmt () in
  let ps name s = pp (Format.sprintf "%s = %s" name s) in
  let pb name b = pp (Format.sprintf "%s = %B" name b) in
(*  let pi name i = pp (Format.sprintf "%s = %d" name i) in *)
  pb "uncertainty" uncertainty;
  pb "subnormal" subnormal;
  pb "simplification" simplification;
  pb "rel-error" rel_error;
  ps "opt" opt;
  pb "opt-approx" opt_approx
    
