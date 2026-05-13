(** Lightweight synchronous publish / subscribe.

    Used as the canonical bridge between brisk's stale-tree
    handler and an external event loop (Lwt, terminal poll,
    etc.) — the stale-tree handler sends an action onto a
    [Remote_action.t], the event loop's subscriber drains it on
    the next tick by running the flush pipeline. UI hosts whose
    event loop can call the flush triad synchronously from the
    stale-tree handler (e.g. AppKit, where the handler always
    runs on the main thread) don't need it.

    Subscribers run synchronously in registration order;
    {!send} is not queued. *)

type 'action t
(** A publisher emitting values of type ['action] to its
    subscribers. *)

val create : unit -> 'action t
(** Make a fresh emitter with no subscribers. *)

type unsubscribe = unit -> unit
(** A no-arg function that, when invoked, removes a previously
    registered subscriber. Returned by {!subscribe}. *)

val subscribe : handler:('action -> unit) -> 'action t -> unsubscribe
(** Attach [handler] to [t]. Every subsequent [send] runs
    [handler] with the sent action. Multiple subscribers can
    coexist; calling [subscribe] with the same physical
    [handler] reference twice is a no-op (deduplicated). *)

val send : action:'action -> 'action t -> unit
(** Synchronously invoke every subscriber of [t] with
    [action]. *)
