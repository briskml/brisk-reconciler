type t = unit -> unit

val noop : t
val chain : t -> t -> t
