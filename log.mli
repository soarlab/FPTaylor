(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

val open_log : ?base_dir:string -> string -> unit
val close : unit -> unit
val append : string -> unit
val formatter : unit -> Format.formatter option

(** Log levels: from the highest to the lowest priorities *)
type level = [`Main | `Important | `Info | `Debug]

(** Returns a log level corresponding to an integer number:
   <= 0 -> `Main
      1 -> `Important
      2 -> `Info
   >= 3 -> `Debug *)
val level_of_int : int -> level

val set_log_level : level -> unit
                                       
val report : level -> ('b, unit, string, unit) format4 -> 'b
val report_str : level -> string -> unit
val warning : ('b, unit, string, unit) format4 -> 'b
val warning_str : string -> unit
val error : ('b, unit, string, unit) format4 -> 'b
val error_str : string -> unit
