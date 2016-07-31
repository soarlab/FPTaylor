val open_log : ?base_dir:string -> string -> unit
val close_log : unit -> unit
val append_to_log : string -> unit
val log_fmt : unit -> Format.formatter option

val report : ('b, unit, string, unit) format4 -> 'b
val report_str : string -> unit
val warning : ('b, unit, string, unit) format4 -> 'b
val warning_str : string -> unit
val error : ('b, unit, string, unit) format4 -> 'b
val error_str : string -> unit
val issue_warning : bool -> ('b, unit, string, unit) format4 -> 'b
val issue_warning_str : bool -> string -> unit
