type ctx
type t

exception Dead of int * string * string

val create :
  ?helper:string ->
  ?timeout:float ->
  ?env:(string * string) list ->
  ?sigout:int ->
  ?sigin:int ->
  unit ->
  ctx

val destroy : ctx -> unit

val pid : t -> int
val start : ?instrument:bool -> ctx -> string -> string list -> t Lwt.t
val clone : ctx -> t -> t Lwt.t
val terminate : t -> unit

val freeze   : t -> unit Lwt.t
val unfreeze : t -> unit Lwt.t
