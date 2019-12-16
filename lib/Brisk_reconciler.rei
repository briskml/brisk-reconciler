/**
  * Global state, mainly useful for internal testing
  */
module GlobalState: {
  /**
    * Resets global state including keys.
    */
  let reset: unit => unit;
};

type handler = unit => unit;
type unregisterF = handler;
let addStaleTreeHandler: handler => unregisterF;

/**
  * Component keys
  */
module Key: {
  /**
    * Abstract type of the component key. It prevents duplicate key issues
    */
  type t;
  /**
    * Create unique a component key.
    */
  let create: unit => t;

  /**
    * Default key
    */
  let none: t;
};

/** Type of element returned from render */
type element('node);

/** Type of element that renders OutputTree.node */
type hostNodeElement('node, 'childNode) = {
  make: unit => 'node,
  configureInstance: (~isFirstRender: bool, 'node) => 'node,
  children: element('childNode),
  insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  moveNode:
    (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
};

/**
  * Converts a list of elements to a single syntheticElement.
  * Lists use a mix of positoinal and keys to determine if a component should be
  * inserted, removed, or updated.
  * For two lists with the same length (i.e. if chicldren have the same length
  * before and after update) if a component has a key, it'll be updated if there was a component
  * with the same key on the previous render.
  * Components without a key are matched based on position.
  * Lists with different lengths are updated only based on keys.
  */
let listToElement: list(element('a)) => element('a);

/**
  * Empty synthetic element.
  */
let empty: element('a);

module RenderedElement: {
  /** Type of a react element after rendering  */
  type t('node, 'childNode);
  type root('node, 'childNode) = {
    node: 'node,
    insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
    deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
    moveNode:
      (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
  };

  /** Render one element by creating new instances. */
  let render:
    (root('node, 'childNode), element('childNode)) => t('node, 'childNode);

  /** Update a rendered element when a new react element is received. */
  let update:
    (
      ~renderedElement: t('parentNode, 'node),
      element('node)
    ) =>
    t('parentNode, 'node);

  /** Flush pending state updates (and possibly add new ones). */
  let flushPendingUpdates: t('parentNode, 'node) => t('parentNode, 'node);

  /** Execute pending updates to the OutputTree and return the new
    * output tree. The return value can be ignored for mutable
    * OutputTrees.
    */
  let executeHostViewUpdates: t('parentNode, _) => 'parentNode;

  /**
    * Executes pending effects from hooks.
    * Note: effects won't run unless this function is a part of
    * your runloop.
    */
  let executePendingEffects: t('a, 'b) => t('a, 'b);
};

/**
  * Creates a component. Components are a functions which
  * retain state over time via Hooks. The function you pass to
  * component should be pure and all side effects should be
  * handled using Hooks.effect
  */
[@ocaml.deprecated "Use let%component instead."]
let component:
  (
    ~useDynamicKey: bool=?,
    string,
    ~key: Key.t=?,
    Hooks.t('a, 'a) => (Hooks.t(Hooks.nil, 'a), element('node))
  ) =>
  element('node);

/**
  * Creates a component which renders an OutputTree node.
  */
[@ocaml.deprecated "Use let%nativeComponent instead."]
let nativeComponent:
  (
    ~useDynamicKey: bool=?,
    string,
    ~key: Key.t=?,
    Hooks.t('a, 'a) =>
    (Hooks.t(Hooks.nil, 'a), hostNodeElement('node, 'childNode))
  ) =>
  element('node);

module Expert: {
  /* Create a constant list of element */
  let jsx_list: list(element('node)) => element('node);
  let component:
    (
      ~useDynamicKey: bool=?,
      string,
      ~key: Key.t=?,
      Hooks.t('a, 'a) => (element('node), Hooks.t(Hooks.nil, 'a))
    ) =>
    element('node);
  let nativeComponent:
    (
      ~useDynamicKey: bool=?,
      string,
      ~key: Key.t=?,
      Hooks.t('a, 'a) =>
      (hostNodeElement('node, _), Hooks.t(Hooks.nil, 'a))
    ) =>
    element('node);
};

module Hooks = Hooks;
module RemoteAction = RemoteAction;
