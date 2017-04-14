(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* "Set" operations on lists.                                                 *)
(* -------------------------------------------------------------------------- *)

val itlist : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val rev_itlist : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val end_itlist : ('a -> 'a -> 'a) -> 'a list -> 'a

val itlist2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

val rev_itlist2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

val last : 'a list -> 'a

val mem : 'a -> 'a list -> bool

val insert : 'a -> 'a list -> 'a list

val union : 'a list -> 'a list -> 'a list

val unions : 'a list list -> 'a list

val intersect : 'a list -> 'a list -> 'a list

val subtract : 'a list -> 'a list -> 'a list

val assoc : 'a -> ('a * 'b) list -> 'b

val rev_assoc : 'b -> ('a * 'b) list -> 'a

val assoc_eq : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

val assocd : 'b -> 'a -> ('a * 'b) list -> 'b

val assocd_eq : ('a -> 'a -> bool) -> 'b -> 'a -> ('a * 'b) list -> 'b

val zip : 'a list -> 'b list -> ('a * 'b) list

val unzip : ('a * 'b) list -> 'a list * 'b list

val (--) : int -> int -> int list

val enumerate : int -> 'a list -> (int * 'a) list

(* -------------------------------------------------------------------------- *)
(* Option type operations                                                     *)
(* -------------------------------------------------------------------------- *)

val is_none : 'a option -> bool

val is_some : 'a option -> bool

val option_lift : ('a -> 'b) -> default:'b -> 'a option -> 'b

val option_default : default:'a -> 'a option -> 'a

val option_value : 'a option -> 'a

val option_first : 'a option list -> 'a
                                             
(* -------------------------------------------------------------------------- *)
(* String operations                                                          *)
(* -------------------------------------------------------------------------- *)

val implode : string list -> string

val explode : string -> string list

val print_list : ('a -> unit) -> (unit -> 'b) -> 'a list -> unit

val starts_with : string -> prefix:string -> bool

val concat_env_paths : string list -> string

(* -------------------------------------------------------------------------- *)
(* IO operations.                                                             *)
(* -------------------------------------------------------------------------- *)

val load_and_close_channel : bool -> in_channel -> string list

val load_file : string -> string list

val run_cmd : string -> string list

val write_to_file : string -> (Format.formatter -> 'a -> 'b) -> 'a -> 'b

val write_to_string : (Format.formatter -> 'a -> 'b) -> 'a -> string

val make_path : ?perm:Unix.file_perm -> string -> unit

val get_dir : string -> string

val set_tmp_dir : string -> unit

val get_tmp_dir : unit -> string
