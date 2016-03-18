type ctx
type p

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

val start     : ?instrument:bool -> ctx -> string -> string list -> p Lwt.t
val pid       : p -> int
val clone     : p -> p Lwt.t
val terminate : p -> unit Lwt.t

val freeze   : p -> unit Lwt.t
val unfreeze : p -> unit Lwt.t
