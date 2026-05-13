(** Brisk-Reconciler — host-agnostic React-Hooks reconciler.

    Models a tree-shaped output (the {b host tree} — DOM nodes,
    AppKit views, terminal widgets, …) as a function of stateful
    {b components}, and reconciles incremental updates against
    that tree as state changes. The host plugs in by defining an
    output-node type plus four callbacks ([insertNode],
    [deleteNode], [moveNode], [configureInstance]); the
    reconciler walks the rendered element tree and turns each
    diff into a sequence of those calls.

    The public surface here is small:

    - {!element} is the unit of composition (built by
      {!component} / {!native_component} via the [brisk_ppx]
      [let%component] / [<jsx />] sugar).
    - {!Rendered_element} is the runtime side: hold a [t], call
      {!Rendered_element.flush_pending_updates},
      {!Rendered_element.execute_host_view_updates}, and
      {!Rendered_element.execute_pending_effects} from your event
      loop to drive incremental updates.
    - {!add_stale_tree_handler} is how the event loop learns that
      a hook fired a state setter — register a wakeup closure to
      schedule the flush triad above.
    - {!Hooks} carries the per-component-instance state model
      (state, reducer, ref, use_effect, use_latest). See the
      [docs/hooks-idioms.md] companion for the patterns.

    Consumers should use the PPX sugar — direct calls to
    {!component} / {!native_component} are deprecated in favour
    of [let%component] / [let%nativeComponent], which expand to
    the {!Expert} variants underneath. *)

(** Process-wide state, mainly for tests. *)
module GlobalState : sig
  val reset : unit -> unit
  (** Resets global state including the component-key counter.
      Useful in tests that need deterministic key allocation
      across runs; not normally called from app code. *)
end

type handler = unit -> unit
(** A no-arg callback. Used by {!add_stale_tree_handler} for the
    "tree became stale; please re-flush" notification. *)

type unregisterF = handler
(** A no-arg callback that, when invoked, removes a previously
    registered handler. Returned by {!add_stale_tree_handler}. *)

val add_stale_tree_handler : handler -> unregisterF
(** Register a handler to be called whenever the rendered tree
    becomes stale (a hook fired [set_state] or equivalent). Returns
    a thunk that removes the handler when called. Drives the
    integration with a host event loop: typically the handler wakes
    up the run loop, which then calls
    {!Rendered_element.flush_pending_updates} +
    {!Rendered_element.execute_host_view_updates} +
    {!Rendered_element.execute_pending_effects}. *)

val addStaleTreeHandler : handler -> unregisterF
[@@ocaml.deprecated "Use [add_stale_tree_handler] instead."]

(** Component keys — identity tokens used by {!list_to_element} to
    match children across renders. *)
module Key : sig
  type t
  (** Abstract type of the component key. *)

  val create : unit -> t
  (** Mint a fresh unique key. Each call returns a key distinct
      from every other [create ()] result for the lifetime of the
      process. *)

  val none : t
  (** The "no key" sentinel. Components passed [~key:none] are
      matched positionally against their previous-render
      counterparts by {!list_to_element}. *)
end

type 'node element
(** An element of the brisk tree, parametrised by the host node
    type [\'node] it eventually renders to (e.g. an AppKit view,
    a DOM node). Construct via {!component} / {!native_component}
    (or, more idiomatically, the [brisk_ppx]'s [let%component] +
    [<jsx />] sugar); render via {!Rendered_element.render}. *)

(** A host-node-producing element — what the PPX-generated
    [let%nativeComponent] returns. Wraps a [make] / [configureInstance]
    pair (the host's "construct" and "reconcile" hooks) plus the
    four mutation callbacks the reconciler invokes on the host
    tree.

    The standard pattern: the host adapter defines [insertNode] /
    [deleteNode] / [moveNode] once, and every [let%nativeComponent]
    plugs them into this record alongside its own [make] /
    [configureInstance]. *)
type ('node, 'childNode) hostNodeElement =
  { make : unit -> 'node
        (** Build a fresh host-node instance. Called once when
            the reconciler decides a new instance is needed
            (initial mount, or a swap when the component
            identity changes). *)
  ; configureInstance : isFirstRender:bool -> 'node -> 'node
        (** Apply the current render's props to an existing
            host-node instance. Called on every render that
            keeps the same instance (i.e. [shouldRerender] is
            true on the component but the reconciler matched
            the previous instance via component equality).
            Returns the (possibly mutated) node. [isFirstRender]
            is [true] only on the call immediately following
            [make]. *)
  ; children : 'childNode element
        (** The sub-tree to mount under this node. [empty] for
            leaves; a [list_to_element [...]] (or
            [Expert.jsx_list]) for containers. *)
  ; insertNode : parent:'node -> child:'childNode -> position:int -> 'node
        (** Insert [child] under [parent] at the given position.
            Returns the (possibly new) parent — most hosts
            return the same parent unchanged. *)
  ; deleteNode : parent:'node -> child:'childNode -> position:int -> 'node
        (** Detach [child] from [parent]. *)
  ; moveNode : parent:'node -> child:'childNode -> from:int -> to_:int -> 'node
        (** Reorder [child] under [parent] from index [~from] to
            [~to_]. *)
  }

val list_to_element : 'a element list -> 'a element
(** Converts a list of elements to a single element. Lists use a mix
    of positional and key-based matching to decide whether a child
    is inserted, removed, or updated.

    For two lists with the same length, a component with a key is
    matched to the same-keyed component from the previous render
    (if any); components without a key are matched by position.
    Lists with different lengths are matched only by key. *)

val listToElement : 'a element list -> 'a element
[@@ocaml.deprecated "Use [list_to_element] instead."]

val empty : 'a element
(** The "no element" sentinel. Use as the leaf {!hostNodeElement.children}
    when a component renders nothing, or as one branch of a
    conditional render ([if visible then <panel /> else empty]). *)

(** Rendered-element runtime. Hold a {!t} between render passes;
    feed it through the {!flush_pending_updates} →
    {!execute_host_view_updates} → {!execute_pending_effects}
    triad on every wakeup driven by a stale-tree handler. *)
module Rendered_element : sig
  type ('node, 'childNode) t
  (** A snapshot of the rendered tree plus the bookkeeping needed
      to flush state updates, apply view updates, and dispatch
      pending effects. Carried between renders by the host
      runtime. *)

  type ('node, 'childNode) root =
    { node : 'node
          (** The host node the reconciler treats as the tree
              root — typically a container view the host
              adapter provides. *)
    ; insertNode : parent:'node -> child:'childNode -> position:int -> 'node
          (** Insert [child] under [parent] at [position]. *)
    ; deleteNode : parent:'node -> child:'childNode -> position:int -> 'node
          (** Detach [child] from [parent]. *)
    ; moveNode :
        parent:'node -> child:'childNode -> from:int -> to_:int -> 'node
          (** Reorder [child] under [parent] from [~from] to [~to_]. *)
    }
  (** Root descriptor passed to {!render}. Bundles the root
      host-node with the same four mutation callbacks that
      {!hostNodeElement} uses, so the reconciler can attach the
      initial subtree under it. *)

  val render :
     ('node, 'childNode) root
    -> 'childNode element
    -> ('node, 'childNode) t
  (** Render an element under a fresh root, allocating new
      host-node instances for every component in the tree.

      Call once at startup. For subsequent updates driven by
      hook state changes, use {!flush_pending_updates} —
      [render] always builds a from-scratch tree and won't
      reuse any prior instances. *)

  val update :
     previousElement:'node element
    -> renderedElement:('parentNode, 'node) t
    -> 'node element
    -> ('parentNode, 'node) t
  (** Reconcile a freshly-constructed element against a
      previously-rendered tree. Used when the {b shape} of the
      tree might change (different top-level element returned by
      the user's render function), as distinct from
      {!flush_pending_updates} which processes pending state
      mutations against an unchanged top-level element. Most
      host integrations only need [flush_pending_updates]. *)

  val flush_pending_updates : ('parentNode, 'node) t -> ('parentNode, 'node) t
  (** Flush pending state updates (and possibly add new ones). *)

  val flushPendingUpdates : ('parentNode, 'node) t -> ('parentNode, 'node) t
  [@@ocaml.deprecated "Use [flush_pending_updates] instead."]

  val execute_host_view_updates : ('parentNode, _) t -> 'parentNode
  (** Execute pending updates to the OutputTree and return the new
      output tree. The return value can be ignored for mutable
      OutputTrees. *)

  val executeHostViewUpdates : ('parentNode, _) t -> 'parentNode
  [@@ocaml.deprecated "Use [execute_host_view_updates] instead."]

  val execute_pending_effects : ('a, 'b) t -> ('a, 'b) t
  (** Executes pending effects from hooks.
      Note: effects won't run unless this function is a part of
      your runloop. *)

  val executePendingEffects : ('a, 'b) t -> ('a, 'b) t
  [@@ocaml.deprecated "Use [execute_pending_effects] instead."]
end

module RenderedElement = Rendered_element
[@@ocaml.deprecated "Use [Rendered_element] instead."]

val component :
   ?useDynamicKey:bool
  -> string
  -> ?key:Key.t
  -> (('a, 'a) Hooks.t -> (Hooks.nil, 'a) Hooks.t * 'node element)
  -> 'node element
[@@ocaml.deprecated "Use let%component instead."]

val native_component :
   ?useDynamicKey:bool
  -> string
  -> ?key:Key.t
  -> (('a, 'a) Hooks.t
      -> (Hooks.nil, 'a) Hooks.t * ('node, 'childNode) hostNodeElement)
  -> 'node element
[@@ocaml.deprecated "Use let%nativeComponent instead."]

val nativeComponent :
   ?useDynamicKey:bool
  -> string
  -> ?key:Key.t
  -> (('a, 'a) Hooks.t
      -> (Hooks.nil, 'a) Hooks.t * ('node, 'childNode) hostNodeElement)
  -> 'node element
[@@ocaml.deprecated "Use [native_component] (or let%nativeComponent)."]

(** The component constructors the [brisk_ppx]'s [let%component] /
    [let%nativeComponent] PPX expands into. Direct callers can use
    these if they need to bypass the PPX (e.g. for custom JSX-like
    wrappers). The hook continuation comes second in the body's
    return tuple (the deprecated top-level {!component} /
    {!native_component} have it first). *)
module Expert : sig
  val jsx_list : 'node element list -> 'node element
  (** Build a list element from a list of children. Emitted by
      the brisk PPX when expanding adjacent JSX siblings
      ([<a /><b /><c />] becomes [jsx_list \[a (); b (); c ()\]]).
      Equivalent to {!list_to_element} but kept as a separate
      name so the PPX's emitted code is greppable. *)

  val component :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t -> 'node element * (Hooks.nil, 'a) Hooks.t)
    -> 'node element
  (** Create a component — a function that retains state over time
      via {!Hooks}. The body should be pure and side-effect-free;
      put effects through {!Hooks.use_effect}. The string argument
      is a debug name (the PPX defaults it to [Module.function_name]).

      [?useDynamicKey] — when [true], each instance of the
      component is given a fresh {!Key.t} on every render. Used
      for components in lists where positional matching would do
      the wrong thing (e.g. a header that may sit either above or
      below a body and shouldn't be reused when the position
      flips). Defaults to [false].

      [?key] — caller-supplied {!Key.t} to pin identity across
      renders. The reconciler matches keyed components against
      their same-keyed predecessor in the previous render; without
      a key, matching is positional within the parent's
      {!list_to_element} sequence. Defaults to {!Key.none}. *)

  val native_component :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t
        -> ('node, _) hostNodeElement * (Hooks.nil, 'a) Hooks.t)
    -> 'node element
  (** Like {!component} but the body returns a {!hostNodeElement}
      instead of an {!element} — so the reconciler knows to
      allocate a host-node for this component via the
      [hostNodeElement.make] callback. Same [?useDynamicKey] /
      [?key] semantics as {!component}. *)

  val nativeComponent :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t
        -> ('node, _) hostNodeElement * (Hooks.nil, 'a) Hooks.t)
    -> 'node element
  [@@ocaml.deprecated "Use [native_component] instead."]
end

module Hooks = Hooks
(** Per-component-instance state, effects, refs.
    See {!Brisk_reconciler.Hooks} for the API and the
    [docs/hooks-idioms.md] companion in the brisk-reconciler
    repo for the patterns. *)

module Remote_action = Remote_action
(** Lightweight publish/subscribe primitive for the
    "stale-tree-handler fires; my event loop processes the
    queued action on the next tick" pattern. The lambda-term
    example uses it as the bridge between brisk's stale-tree
    callback and Lwt's event loop; UI hosts whose event loop
    can call the flush triad synchronously from the stale-tree
    handler don't need it. *)

(* For backward compatibility *)
module RemoteAction = Remote_action
[@@ocaml.deprecated "Use [Remote_action] instead."]
