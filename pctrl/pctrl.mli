type ctx

val create : ?timeout:float -> ?env:(string * string) list -> unit -> ctx
val destroy : ctx -> unit

type stat = string * char
val stat : int -> stat option Lwt.t
val pp_stat : stat option Fmt.t

type p

exception Dead of int * string * string

val start     : ?instrument:bool -> ctx -> string -> string list -> p Lwt.t
val clone     : p -> p Lwt.t
val terminate : p -> unit Lwt.t
val pid       : p -> int
val fd        : p -> Lwt_unix.file_descr

val serially  : p -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val signal    : p -> sign:int -> (char -> bool) -> unit Lwt.t
