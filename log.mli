val open_log : ?base_dir:string -> string -> unit
val close_log : unit -> unit
val append_to_log : string -> unit
val log_fmt : unit -> Format.formatter option

val report : string -> unit
val warning : string -> unit
val error : string -> unit
val issue_warning : bool -> string -> unit
                                
                           
