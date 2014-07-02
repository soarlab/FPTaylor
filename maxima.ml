open List
open Lib
open Expr

(* From HOL Light tutorial *)
let maxima cmd =
  let tmp = Filename.temp_file "maxima" ".out" in
  let s = 
    "echo 'linel:20000; display2d:false;" ^ cmd ^ 
      ";' | maxima | grep '^(%o3)' | sed -e 's/^(%o3) //' > " ^
      tmp in
  if Sys.command s <> 0 then failwith ("maxima: " ^ cmd) else
    let fd = open_in tmp in
    let data = input_line fd in
    let _ = close_in fd; Sys.remove tmp in
    data;;

let test_maxima () =
  try 
    let _ = maxima "factor(x * x - y * y)" in true
  with _ -> false
   
let maxima_expr str =
  let out = maxima str in
  Parser.parse_expr out;;

let simplify e =
  let str = "factor(" ^ print_expr_str e ^ ")" in
  maxima_expr str

let diff v e =
  let str = "diff(" ^ print_expr_str e ^ ", " ^ v ^ ")" in
  maxima_expr str

let simplify_diff v e =
  let str = "factor(diff(" ^ print_expr_str e ^ ", " ^ v ^ "))" in
  maxima_expr str

(* Returns the k-th Taylor coefficient in the expansion of e with respect to v around 0 *)
let taylor_coeff v k e =
  let str = Format.sprintf "ratcoeff(taylor(%s, %s, 0, %d), %s, %d)"
    (print_expr_str e) v k v k in
  maxima str

(* Returns a list of linear Taylor coefficients in the expansion of e with respect to vs around 0 *)
let taylor_coeff1 vs e =
  let substs = map (fun v -> Format.sprintf "%s = 0" v) vs in
  let subst = String.concat ", " substs in
  let e_str = print_expr_str e in
  let coeff v =
    let str = Format.sprintf "at (diff(%s, %s), [%s])" e_str v subst in
    maxima str in
  map coeff vs

