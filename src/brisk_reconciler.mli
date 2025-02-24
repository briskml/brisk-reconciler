(** Global state, mainly useful for internal testing *)
module GlobalState : sig
  val reset : unit -> unit
  (** Resets global state including keys. *)
end

type handler = unit -> unit
type unregisterF = handler

val addStaleTreeHandler : handler -> unregisterF

(** Component keys *)
module Key : sig
  type t
  (** Abstract type of the component key. It prevents duplicate key issues *)

  val create : unit -> t
  (** Create unique a component key. *)

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

val listToElement : 'a element list -> 'a element
(** Converts a list of elements to a single syntheticElement.
    Lists use a mix of positoinal and keys to determine if a component should be
    inserted, removed, or updated.
    For two lists with the same length (i.e. if chicldren have the same length
    before and after update) if a component has a key, it'll be updated if
    there was a component with the same key on the previous render.
    Components without a key are matched based on position.
    Lists with different lengths are updated only based on keys. *)

val empty : 'a element
(** Empty synthetic element. *)

module RenderedElement : sig
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

  val flushPendingUpdates : ('parentNode, 'node) t -> ('parentNode, 'node) t
  (** Flush pending state updates (and possibly add new ones). *)

  val executeHostViewUpdates : ('parentNode, _) t -> 'parentNode
  (** Execute pending updates to the OutputTree and return the new
      output tree. The return value can be ignored for mutable
      OutputTrees. *)

  val executePendingEffects : ('a, 'b) t -> ('a, 'b) t
  (** Executes pending effects from hooks.
      Note: effects won't run unless this function is a part of
      your runloop. *)
end

(** Creates a component. Components are a functions which
    retain state over time via Hooks. The function you pass to
    component should be pure and all side effects should be
    handled using Hooks.effect *)
val component :
   ?useDynamicKey:bool
  -> string
  -> ?key:Key.t
  -> (('a, 'a) Hooks.t -> (Hooks.nil, 'a) Hooks.t * 'node element)
  -> 'node element
[@@ocaml.deprecated "Use let%component instead."]

(** Creates a component which renders an OutputTree node. *)
val nativeComponent :
   ?useDynamicKey:bool
  -> string
  -> ?key:Key.t
  -> (('a, 'a) Hooks.t
      -> (Hooks.nil, 'a) Hooks.t * ('node, 'childNode) hostNodeElement)
  -> 'node element
[@@ocaml.deprecated "Use let%nativeComponent instead."]

module Expert : sig
  val jsx_list : 'node element list -> 'node element

  val component :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t -> 'node element * (Hooks.nil, 'a) Hooks.t)
    -> 'node element

  val nativeComponent :
     ?useDynamicKey:bool
    -> string
    -> ?key:Key.t
    -> (('a, 'a) Hooks.t
        -> ('node, _) hostNodeElement * (Hooks.nil, 'a) Hooks.t)
    -> 'node element
end

module Hooks = Hooks
module Remote_action = Remote_action
