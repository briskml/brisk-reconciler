type 'action t = {mutable subscribers : ('action -> unit) list}
type unsubscribe = unit -> unit

let create () = {subscribers = []}

let subscribe ~(handler : 'action -> unit) ({subscribers} as emitter : 'a t) =
  if not (List.exists (fun f -> f == handler) subscribers)
  then emitter.subscribers <- handler :: subscribers;
  let unsubscribe () =
    emitter.subscribers <- List.filter (fun f -> f != f) subscribers
  in
  unsubscribe

let send ~(action : 'a) (emitter : 'a t) =
  List.iter (fun c -> c action) emitter.subscribers
