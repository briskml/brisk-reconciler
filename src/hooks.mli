(** Per-component-instance state, effects, and refs — brisk's
    React-style hooks.

    Each component instance keeps an ordered "hooks tape" — a
    {!t} threaded by the [brisk_ppx]'s [let%hook] /
    [let%component] sugar through every hook call in the
    component body. {!state}, {!reducer}, {!ref}, {!use_effect},
    {!use_latest} each consume one slot off the tape (creating
    it from a supplied default on first render, reusing the
    existing slot on subsequent renders). Because slots are
    consumed in order, {b hook declarations must run in the same
    order on every render} — no conditional [let%hook] calls.

    See the [docs/hooks-idioms.md] companion for the canonical
    patterns ({!state}-driven counters, {!reducer}-driven action
    machines, {!use_effect} acquire-on-mount / release-on-unmount
    plumbing, the {!use_latest}-trampoline pattern for callbacks
    captured by long-lived consumers). *)

type 'a hook = ..
(** Extensible variant — each built-in hook ({!State.t},
    {!Reducer.t}, {!Ref.t}, {!Effect.t}) adds its own
    constructor to this type, so [pendingEffects] and
    {!flush_pending_state_updates} can pattern-match across the
    hook list uniformly. Not extended by user code. *)

type 'a state
(** A component instance's hook-tape contents, sealed. Built by
    {!to_state} at the end of a render; consumed by
    {!pending_effects} (to dispatch effects at lifecycle
    transitions) and {!flush_pending_state_updates} (to apply
    queued state setters). Abstract: callers don't introspect. *)

type ('remaining, 'result) t
(** The hooks-tape continuation threaded through a component
    body. ['remaining] tracks the hooks still to be consumed;
    ['result] is the eventual sealed state. The PPX builds and
    refines this continuation automatically; manual callers
    typically use {!of_state} to start one and {!to_state} to
    seal one. *)

type nil
(** Type-level marker for an empty hooks tape — what's left
    after every slot has been consumed. *)

type empty
(** A fully-consumed hooks tape. *)

type 'a all = (nil, 'a) t
(** Shorthand for a hooks tape whose remaining slot list is
    [nil] — i.e., all hooks have been consumed. *)

val empty : unit -> ('value, 'value) t
(** Build an empty hooks-tape continuation. Used internally;
    rarely needed in user code. *)

val of_state :
  'b state option -> on_state_did_change:(unit -> unit) -> ('b, 'b) t
(** Start a hooks-tape continuation from an existing sealed
    [state] (the instance's previous-render hook contents) plus
    a [~on_state_did_change] callback that fires whenever a
    state setter or reducer dispatch runs. The reconciler
    supplies [on_state_did_change] = its [callStaleHandlers],
    so any setter call triggers the stale-tree flush pipeline. *)

val ofState : 'b state option -> onStateDidChange:(unit -> unit) -> ('b, 'b) t
[@@ocaml.deprecated "Use [of_state] (with [~on_state_did_change:]) instead."]

val to_state : 'a all -> 'a state
(** Seal a fully-consumed hooks-tape continuation into an
    instance [state]. Pairs with {!of_state} at the open end
    and {!pending_effects} / {!flush_pending_state_updates}
    after. *)

val toState : 'a all -> 'a state
[@@ocaml.deprecated "Use [to_state] instead."]

val print_state : 'a state option -> string
(** Debug helper — currently returns ["<Some>"] or ["<Empty>"]
    depending on whether the state is populated. Used by
    brisk's internal tracing; not load-bearing. *)

val printState : 'a state option -> string
[@@ocaml.deprecated "Use [print_state] instead."]

val process_next :
   default:'value
  -> ?merge:('value -> 'value)
  -> to_witness:('value -> 'value hook)
  -> ('value -> 'c, 'd) t
  -> 'value * ('c, 'd) t
(** Low-level hook plumbing — pull the next slot off the tape,
    falling back to [~default] if this is the first render
    (so no existing slot exists), with optional [?merge] to
    fold an existing value into a per-render update.
    [~to_witness] wraps the value in this hook's
    {!'a hook} constructor for the heterogenous list.

    Used internally by {!state} / {!reducer} / {!ref} /
    {!use_effect} to consume their respective slots; user code
    rarely calls it directly. *)

val processNext :
   default:'value
  -> ?merge:('value -> 'value)
  -> toWitness:('value -> 'value hook)
  -> ('value -> 'c, 'd) t
  -> 'value * ('c, 'd) t
[@@ocaml.deprecated "Use [process_next] (with [~to_witness:]) instead."]

(** Local mutable state hook — see {!state}. *)
module State : sig
  type 'a t
  (** Abstract per-instance state container for the {!state}
      hook. Internally tracks the current value plus a queue of
      pending updates (shared by reference across post-flush
      replacement containers, so a setter captured in a
      long-lived callback keeps routing into the live state). *)

  type 'a hook += private State : 'a t -> 'a t hook
  (** Witness constructor used by the hook-list machinery to
      identify state slots. Private — callers can pattern-match
      but not construct. *)
end

(** Reducer-style state hook — see {!reducer}. *)
module Reducer : sig
  type 'a t
  (** Abstract per-instance state container for the {!reducer}
      hook. Stores the current value plus a queue of pending
      [('a -> 'a) list ref] actions to apply on the next flush. *)

  type 'a hook += private Reducer : 'a t -> 'a t hook
end

(** Identity-stable mutable ref hook — see {!ref}. *)
module Ref : sig
  type 'a t
  (** Abstract per-instance ref. Internally just an OCaml ref
      held in the hook tape; survives across renders with
      stable identity. *)

  type 'a hook += private Ref : 'a t -> 'a t hook
end

(** Lifecycle effects hook — see {!use_effect}. *)
module Effect : sig
  type 'a t
  (** Abstract per-instance effect state. Holds the effect's
      [condition], its [handler], the cleanup closure (if any)
      returned by the last handler call, and the
      [previousCondition] used to detect dependency changes
      between renders. *)

  type lifecycle = Mount | Unmount | Update
  (** The reconciler dispatches effects across these three
      lifecycle phases. {!use_effect}'s [condition] decides
      which phases run the handler / cleanup; see
      [docs/hooks-idioms.md] for the per-condition table. *)

  type always
  (** Phantom type tag — first parameter to {!Always}. *)

  type onMount
  (** Phantom type tag — first parameter to {!OnMount}. *)

  type 'a condition =
    | Always : always condition
        (** Run handler on every render; run cleanup before
            each subsequent handler call and on unmount.
            React's [useEffect(fn)] with no deps array. *)
    | OnMount : onMount condition
        (** Run handler once on mount; run cleanup on unmount.
            React's [useEffect(fn, [])]. *)
    | If : ('a -> 'a -> bool) * 'a -> 'a condition
        (** Run cleanup + handler on Update if the comparator
            returns true when applied to [previous_dep] and
            [current_dep]. Pass [(<>)] for "fire when dep
            changed" semantics; React's [useEffect(fn, [dep])]. *)
    | OnMountAndIf : ('a -> 'a -> bool) * 'a -> 'a condition
        (** Union of {!OnMount} and {!If} — fires on mount AND
            on every dependency change. *)

  type handler = unit -> (unit -> unit) option
  (** The user-supplied effect body: takes no args, returns
      [Some cleanup] (a cleanup closure to run on unmount /
      next refire) or [None] (no cleanup needed). *)

  type 'a hook += private Effect : 'a t -> 'a t hook
end

val state :
   'value
  -> ('value State.t -> 'c, 'd) t
  -> ('value * (('value -> 'value) -> unit)) * ('c, 'd) t
(** [state initial_value] returns the current value plus a
    setter [('value -> 'value) -> unit] that takes an updater
    function (à la React's [setCount(prev => prev + 1)]). The
    setter is stable across renders — it can be safely captured
    in {!use_effect} mount closures or imperative-API
    callbacks. *)

val reducer :
   initialState:'value
  -> ('b -> 'value -> 'value)
  -> ('value Reducer.t -> 'c, 'd) t
  -> ('value * ('b -> unit)) * ('c, 'd) t
(** [reducer ~initialState reducer_fn] returns the current
    value plus a [dispatch : 'b -> unit] that enqueues actions
    of type ['b] against the reducer function ['b -> 'value -> 'value].
    Pending actions are folded into the state on the next
    flush. Use when state transitions are best expressed as a
    closed action variant. *)

val ref : 'value -> ('value ref -> 'c, 'd) t -> 'value ref * ('c, 'd) t
(** [ref initial_value] returns a {b stable} mutable cell —
    the same ref is returned on every render of this component
    instance, initialized from [initial_value] on first render
    and ignored thereafter. Mutating the ref does {b not}
    trigger a re-render — use {!state} / {!reducer} for that.
    Useful for scratch storage tied to the component's
    lifetime (e.g. holding a reference to a third-party
    library handle, or memoising a derived value across
    renders). *)

val use_latest :
  'value -> ('value ref -> 'c, 'd) t -> 'value ref * ('c, 'd) t
(** [use_latest v] is [Hooks.ref v] with one extra mechanic: on
    every render, the ref's contents are overwritten with the
    supplied [v]. So the returned [ref] has stable identity
    across renders, and dereferencing it always yields the value
    passed at the most recent render — the "trampoline" pattern
    for callbacks captured by long-lived consumers (imperative-API
    registrations, OnMount-scheduled effects, etc.) without
    spelling it out as a [Hooks.ref] + per-render assignment pair.

    Common shape:

    {[
      let%hook latest_on_tick = Hooks.use_latest on_tick in
      let%hook () =
        Hooks.use_effect OnMount (fun () ->
          register_callback (fun () -> !latest_on_tick ());
          Some (fun () -> unregister_callback ()))
      in
      ...
    ]}

    The closure passed to [register_callback] dereferences the
    ref each call, so it always invokes the latest [on_tick]
    even though the registration happened at mount-time. *)

val use_effect :
   'value Effect.condition
  -> (unit -> (unit -> unit) option)
  -> ('value Effect.t -> 'c, 'd) t
  -> unit * ('c, 'd) t
(** [use_effect condition handler] schedules a side-effect tied
    to the component's lifecycle. The handler returns
    [Some cleanup] (run at unmount / next refire) or [None].
    [condition] selects when the handler / cleanup fires — see
    {!Effect.condition} for the four shapes.

    Renamed from [effect] for OCaml-5.3+ compatibility: the bare
    identifier [effect] became a contextual keyword for the
    effect-handler feature, so the old name no longer parses at
    a declaration start position. No deprecation alias is
    possible on 5.3+ — call sites must update. *)

val pending_effects :
   lifecycle:Effect.lifecycle
  -> 'a state option
  -> Effect_sequence.t
(** Collect all the effect bodies (or cleanups, depending on
    [~lifecycle]) the hook tape wants to run on a given
    lifecycle phase. Internal — the reconciler calls this when
    queuing effects for {!Brisk_reconciler.Rendered_element.execute_pending_effects}. *)

val pendingEffects :
   lifecycle:Effect.lifecycle
  -> 'a state option
  -> Effect_sequence.t
[@@ocaml.deprecated "Use [pending_effects] instead."]

val flush_pending_state_updates : 'a state -> 'a state
(** Walk the hook tape, apply queued state setters / reducer
    dispatches, return the post-flush state. Returns the
    physically-same [state] if nothing changed (so the
    reconciler can use [!=] to detect "any state changed?"
    without per-hook comparison). Called by the reconciler
    inside [updateOpaqueInstance] before deciding whether to
    re-render. *)

val flushPendingStateUpdates : 'a state -> 'a state
[@@ocaml.deprecated "Use [flush_pending_state_updates] instead."]
