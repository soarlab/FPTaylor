(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Symbolic expression printing                                               *)
(* -------------------------------------------------------------------------- *)

module type PrinterType

module type P =
  sig
    val print_fmt : Format.formatter -> Expr.expr -> unit
    val print_std : Expr.expr -> unit
    val print_str : Expr.expr -> string
  end

module Make (Printer : PrinterType) : P

module OCamlIntervalPrinter : PrinterType

module RacketPrinter : PrinterType

module CPrinter : PrinterType

module OCamlFloatPrinter : PrinterType
                         
module Info : P
