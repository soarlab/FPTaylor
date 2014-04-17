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
