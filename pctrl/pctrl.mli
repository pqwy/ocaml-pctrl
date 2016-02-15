type ctx
type t

type stat = string * char
exception Dead of int * string * string

val create :
  ?timeout:float ->
  ?env:(string * string) list ->
  ?sigout:int ->
  ?sigin:int ->
  unit ->
  ctx

val destroy : ctx -> unit

val start : ?instrument:bool -> ctx -> string -> string list -> t Lwt.t
val pid       : t -> int
val clone     : t -> t Lwt.t
val terminate : t -> unit Lwt.t

val freeze   : t -> unit Lwt.t
val unfreeze : t -> unit Lwt.t
