type handler = unit => unit;
type unregisterF = handler;
let addStaleTreeHandler: handler => unregisterF;

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
    (~renderedElement: t('parentNode, 'node), element('node)) =>
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

module Expert: {
  /* Create a constant list of element */
  let jsx_list: list(element('node)) => element('node);
  let component:
    (
      string,
      Hooks.t('a, 'a) => (element('node), Hooks.t(Hooks.nil, 'a))
    ) =>
    element('node);
  let nativeComponent:
    (
      string,
      Hooks.t('a, 'a) =>
      (hostNodeElement('node, _), Hooks.t(Hooks.nil, 'a))
    ) =>
    element('node);
};

type movableStateContainerState('node);

let movableStateContainer:
  (
    ~children: element('node),
    unit,
    Hooks.t(movableStateContainerState('node) => 'c, 'd),
  ) =>
  (unit => element('node), Hooks.t('c, 'd));

module Hooks = Hooks;
module RemoteAction = RemoteAction;
