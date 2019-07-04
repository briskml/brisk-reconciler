/**
  * The description of the output tree such as DOM, native view hierarchy
  * or navigation screens. The output tree can be both mutable and immutable.
  */
module type OutputTree = {
  /**
    * The type of a single output tree node. In the case of DOM, it could be a DOM node.
    * It's more convenient to work with this type if it remains abstract. If we have
    * strongly typed OutputTree, it's hard to avoid the usage of Obj.magic or asserts.
    * This should be addressed in the future versions.
    */
  type node;

  /**
    * This function is called when there's a new pending state update. An example of such
    * event is calling setState.
    */
  let markAsStale: unit => unit;

  /**
    * Insert child as a child of parent at position. The position is important for
    * z axis ordering.
    */
  let insertNode: (~parent: node, ~child: node, ~position: int) => node;
  /**
    * Delete child from parent.
    */
  let deleteNode: (~parent: node, ~child: node, ~position: int) => node;
  /**
    * Move child in parent from the from index to to_ index.
    */
  let moveNode: (~parent: node, ~child: node, ~from: int, ~to_: int) => node;
};

/**
  * Creates a reconciler specialized for a given OutputTree
  */
module Make:
  (OutputTree: OutputTree) =>
   {
    /**
      * Global state, mainly useful for internal testing
      */
    module GlobalState: {
      /**
        * If true, additional debug information is printed.
        */
      let debug: ref(bool);
      /**
        * Resets global state including keys.
        */
      let reset: unit => unit;

      /**
       * Use physical equality to recognize that an element was added to the list of children.
       * Note: this currently does not check for pending updates on components in the list.
       */
      let useTailHack: ref(bool);
    };

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
    };

    /** Type of element returned from render */
    type syntheticElement;

    /** Type of element that renders OutputTree.node */
    type outputTreeElement = {
      make: unit => OutputTree.node,
      configureInstance:
        (~isFirstRender: bool, OutputTree.node) => OutputTree.node,
      children: syntheticElement,
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
    let listToElement: list(syntheticElement) => syntheticElement;

    /**
      * Empty synthetic element.
      */
    let empty: syntheticElement;

    module RenderedElement: {
      /** Type of a react element after rendering  */
      type t;

      /** Render one element by creating new instances. */
      let render: (OutputTree.node, syntheticElement) => t;

      /** Update a rendered element when a new react element is received. */
      let update:
        (
          ~previousElement: syntheticElement,
          ~renderedElement: t,
          syntheticElement
        ) =>
        t;

      /** Flush pending state updates (and possibly add new ones). */
      let flushPendingUpdates: t => t;

      /** Execute pending updates to the OutputTree and return the new
        * output tree. The return value can be ignored for mutable
        * OutputTrees.
        */
      let executeHostViewUpdates: t => OutputTree.node;

      /**
        * Executes pending effects from hooks.
        * Note: effects won't run unless this function is a part of
        * your runloop.
        */
      let executePendingEffects: t => t;
    };

    /**
      * Creates a component. Components are a functions which
      * retain state over time via Hooks. The function you pass to
      * component should be pure and all side effects should be
      * handled using Hooks.effect
      */
    let component:
      (
        ~useDynamicKey: bool=?,
        string,
        ~key: Key.t=?,
        Hooks.t('a, 'a) =>
        (Hooks.t(Hooks.nil, 'a), syntheticElement)
      ) =>
      syntheticElement;

    /**
      * Creates a component which renders an OutputTree node.
      */
    let nativeComponent:
      (
        ~useDynamicKey: bool=?,
        string,
        ~key: Key.t=?,
        Hooks.t('a, 'a) =>
        (Hooks.t(Hooks.nil, 'a), outputTreeElement)
      ) =>
      syntheticElement;

    module Hooks = Hooks;
    module RemoteAction = RemoteAction;
  };

module Hooks = Hooks;
module RemoteAction = RemoteAction;
