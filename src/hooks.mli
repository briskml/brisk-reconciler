type 'a hook = ..
type 'a state
type ('remaining, 'result) t
type nil
type empty
type 'a all = (nil, 'a) t

val empty : unit -> ('value, 'value) t

val of_state :
  'b state option -> on_state_did_change:(unit -> unit) -> ('b, 'b) t

val ofState : 'b state option -> onStateDidChange:(unit -> unit) -> ('b, 'b) t
[@@ocaml.deprecated "Use [of_state] (with [~on_state_did_change:]) instead."]

val to_state : 'a all -> 'a state

val toState : 'a all -> 'a state
[@@ocaml.deprecated "Use [to_state] instead."]

val print_state : 'a state option -> string

val printState : 'a state option -> string
[@@ocaml.deprecated "Use [print_state] instead."]

val process_next :
   default:'value
  -> ?merge:('value -> 'value)
  -> to_witness:('value -> 'value hook)
  -> ('value -> 'c, 'd) t
  -> 'value * ('c, 'd) t

val processNext :
   default:'value
  -> ?merge:('value -> 'value)
  -> toWitness:('value -> 'value hook)
  -> ('value -> 'c, 'd) t
  -> 'value * ('c, 'd) t
[@@ocaml.deprecated "Use [process_next] (with [~to_witness:]) instead."]

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

val use_effect :
   'value Effect.condition
  -> (unit -> (unit -> unit) option)
  -> ('value Effect.t -> 'c, 'd) t
  -> unit * ('c, 'd) t
(** Renamed from [effect] in OCaml-5.3+ compatibility: the bare
    identifier [effect] became a contextual keyword for the effect-
    handler feature, so the old name no longer parses at a
    declaration start position. No deprecation alias is possible
    on 5.3+ — call sites must update. *)

val pending_effects :
   lifecycle:Effect.lifecycle
  -> 'a state option
  -> Effect_sequence.t

val pendingEffects :
   lifecycle:Effect.lifecycle
  -> 'a state option
  -> Effect_sequence.t
[@@ocaml.deprecated "Use [pending_effects] instead."]

val flush_pending_state_updates : 'a state -> 'a state

val flushPendingStateUpdates : 'a state -> 'a state
[@@ocaml.deprecated "Use [flush_pending_state_updates] instead."]
