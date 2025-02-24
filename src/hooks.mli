type 'a hook = ..
type 'a state
type ('remaining, 'result) t
type nil
type empty
type 'a all = (nil, 'a) t

val empty : unit -> ('value, 'value) t
val ofState : 'b state option -> onStateDidChange:(unit -> unit) -> ('b, 'b) t
val toState : 'a all -> 'a state
val printState : 'a state option -> string

val processNext :
   default:'value
  -> ?merge:('value -> 'value)
  -> toWitness:('value -> 'value hook)
  -> ('value -> 'c, 'd) t
  -> 'value * ('c, 'd) t

module State : sig
  type 'a t
  type 'a hook += private State : 'a t -> 'a t hook
end

module Reducer : sig
  type 'a t
  type 'a hook += private Reducer : 'a t -> 'a t hook
end

module Ref : sig
  type 'a t
  type 'a hook += private Ref : 'a t -> 'a t hook
end

module Effect : sig
  type 'a t
  type lifecycle = Mount | Unmount | Update
  type always
  type onMount

  type 'a condition =
    | Always : always condition
    | OnMount : onMount condition
    | If : ('a -> 'a -> bool) * 'a -> 'a condition
    | OnMountAndIf : ('a -> 'a -> bool) * 'a -> 'a condition

  type handler = unit -> (unit -> unit) option
  type 'a hook += private Effect : 'a t -> 'a t hook
end

val state :
   'value
  -> ('value State.t -> 'c, 'd) t
  -> ('value * (('value -> 'value) -> unit)) * ('c, 'd) t

val reducer :
   initialState:'value
  -> ('b -> 'value -> 'value)
  -> ('value Reducer.t -> 'c, 'd) t
  -> ('value * ('b -> unit)) * ('c, 'd) t

val ref : 'value -> ('value ref -> 'c, 'd) t -> 'value ref * ('c, 'd) t

val effect :
   'value Effect.condition
  -> (unit -> (unit -> unit) option)
  -> ('value Effect.t -> 'c, 'd) t
  -> unit * ('c, 'd) t

val pendingEffects :
   lifecycle:Effect.lifecycle
  -> 'a state option
  -> Effect_sequence.t

val flushPendingStateUpdates : 'a state -> 'a state
