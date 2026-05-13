(** Global state, mainly useful for internal testing *)
module GlobalState : sig
  val reset : unit -> unit
  (** Resets global state including keys. *)
end

type handler = unit -> unit
type unregisterF = handler

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

(** Component keys *)
module Key : sig
  type t
  (** Abstract type of the component key. It prevents duplicate key issues *)

  val create : unit -> t
  (** Create a unique component key. *)

  val none : t
  (** Default key *)
end

type 'node element
(** Type of element returned from render *)

(** Type of element that renders OutputTree.node *)
type ('node, 'childNode) hostNodeElement =
  { make : unit -> 'node
  ; configureInstance : isFirstRender:bool -> 'node -> 'node
  ; children : 'childNode element
  ; insertNode : parent:'node -> child:'childNode -> position:int -> 'node
  ; deleteNode : parent:'node -> child:'childNode -> position:int -> 'node
  ; moveNode : parent:'node -> child:'childNode -> from:int -> to_:int -> 'node
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
(** Empty synthetic element. *)

module Rendered_element : sig
  type ('node, 'childNode) t
  (** Type of a react element after rendering *)

  type ('node, 'childNode) root =
    { node : 'node
    ; insertNode : parent:'node -> child:'childNode -> position:int -> 'node
    ; deleteNode : parent:'node -> child:'childNode -> position:int -> 'node
    ; moveNode :
        parent:'node -> child:'childNode -> from:int -> to_:int -> 'node }

  (** Render one element by creating new instances. *)
  val render :
     ('node, 'childNode) root
    -> 'childNode element
    -> ('node, 'childNode) t

  (** Update a rendered element when a new react element is received. *)
  val update :
     previousElement:'node element
    -> renderedElement:('parentNode, 'node) t
    -> 'node element
    -> ('parentNode, 'node) t

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

(** Creates a component. Components are functions which
    retain state over time via Hooks. The function you pass to
    component should be pure and all side effects should be
    handled using Hooks.use_effect *)
val component :
   ?useDynamicKey:bool
  -> string
  -> ?key:Key.t
  -> (('a, 'a) Hooks.t -> (Hooks.nil, 'a) Hooks.t * 'node element)
  -> 'node element
[@@ocaml.deprecated "Use let%component instead."]

(** Creates a component which renders an OutputTree node. *)
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

module Expert : sig
  val jsx_list : 'node element list -> 'node element

  val component :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t -> 'node element * (Hooks.nil, 'a) Hooks.t)
    -> 'node element

  val native_component :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t
        -> ('node, _) hostNodeElement * (Hooks.nil, 'a) Hooks.t)
    -> 'node element

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
module Remote_action = Remote_action

(* For backward compatibility *)
module RemoteAction = Remote_action
[@@ocaml.deprecated "Use [Remote_action] instead."]
