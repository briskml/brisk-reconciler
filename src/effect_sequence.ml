type t = unit -> unit

let noop () = ()

let chain f f' =
  if f' == noop then f else if f == noop then f' else fun () -> f (); f' ()
