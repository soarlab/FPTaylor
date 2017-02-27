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

(** Log levels: 
   0: print main results only
   1: print important steps
   2: print additional information for each step
   3: print all details
   4: print everything (including debug information) *)
val set_log_level : int -> unit
                                       
val report : int -> ('b, unit, string, unit) format4 -> 'b
val report_str : int -> string -> unit
val warning : int -> ('b, unit, string, unit) format4 -> 'b
val warning_str : int -> string -> unit
val error : ('b, unit, string, unit) format4 -> 'b
val error_str : string -> unit
