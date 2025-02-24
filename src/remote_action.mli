type 'action t

val create : unit -> 'action t

type unsubscribe = unit -> unit

val subscribe : handler:('action -> unit) -> 'action t -> unsubscribe
val send : action:'action -> 'action t -> unit
