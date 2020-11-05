module GlobalState = {
  let componentKeyCounter = ref(0);
  let reset = () => {
    componentKeyCounter := 0;
  };
};

type handler = unit => unit;
type unregisterF = handler;

let staleHandlers = ref([]);
let addStaleTreeHandler = (handler: handler) => {
  staleHandlers := [handler, ...staleHandlers^];
  () => {
    staleHandlers := List.filter(f => f === handler, staleHandlers^);
  };
};
let callStaleHandlers = () => List.iter(f => f(), staleHandlers^);

module Key = {
  type t = int;

  let equal = (==);
  let none = (-1);
  let dynamicKeyMagicNumber = 0;
  let create = () => {
    incr(GlobalState.componentKeyCounter);
    GlobalState.componentKeyCounter^;
  };
};

type hostNode('a) =
  | Node('a)
  | UpdatedNode('a, 'a);

type lazyHostNode('a) = Lazy.t(hostNode('a));

type lazyHostNodeList('a) = list(lazyHostNode('a));

type componentId('a) = ..;

type element('node) =
  | Flat(opaqueComponent('node))
  | Nested(list(element('node)))
and component('a) = {
  debugName: string,
  key: int,
  id: componentId(instance('a)),
  childrenType: childrenType('viewSpec),
  eq:
    'other.
    ('other, componentId('other), componentId(instance('a))) =>
    option(instance('a)),

  render:
    Hooks.t('hooks, 'hooks) => ('children, Hooks.t(Hooks.nil, 'hooks)),
}
constraint 'viewSpec = ('node, 'children, 'childNode, 'wrappedNode)
constraint 'a = ('hooks, 'viewSpec)
and instance('a) = {
  hooks: Hooks.state('hooks),
  opaqueComponent: opaqueComponent('node),
  component: component('a),
  children_: 'children,
  childInstances: instanceForest('childNode),
  wrappedHostNode: 'wrappedNode,
}
constraint 'viewSpec = ('node, 'children, 'childNode, 'wrappedNode)
constraint 'a = ('hooks, 'viewSpec)
and opaqueComponent('node) =
  | OpaqueComponent(component((_, ('node, _, _, _))))
    : opaqueComponent('node)
and instanceForest('node) =
  | IFlat(opaqueInstance('node))
  | INested(list(instanceForest('node)), int /*subtree size*/)
and opaqueInstance('node) =
  | Instance(instance((_, ('node, _, _, _)))): opaqueInstance('node)
and hostNodeElement('node, 'childNode) = {
  make: unit => 'node,
  configureInstance: (~isFirstRender: bool, 'node) => 'node,
  children: element('childNode),
  insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  moveNode:
    (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
}
and childrenType('viewSpec) =
  | Host: childrenType(
            (
              'node,
              hostNodeElement('node, 'childNode),
              'childNode,
              lazyHostNode('node),
            ),
          )
  | React
      : childrenType(
          ('node, element('node), 'node, lazyHostNodeList('node)),
        );

type renderedElement('node, 'childNode) = {
  nearestHostNode: lazyHostNode('node),
  nodeElement: hostNodeElement('node, 'childNode),
  instanceForest: instanceForest('childNode),
  enqueuedEffects: EffectSequence.t,
};

type opaqueInstanceUpdate('node, 'childNode) = {
  nearestHostNode: lazyHostNode('node),
  nodeElement: hostNodeElement('node, 'childNode),
  opaqueInstance: opaqueInstance('childNode),
  enqueuedEffects: EffectSequence.t,
};

module InstanceForest = {
  let getSubtreeSize: type node. instanceForest(node) => int =
    fun
    | INested(_, x) => x
    | IFlat(Instance({wrappedHostNode, component: {childrenType}})) =>
      switch (childrenType) {
      | React => List.length(wrappedHostNode)
      | Host => 1
      };

  let rec flatten =
    fun
    | IFlat(l) => [l]
    | INested(l, _) => ListTR.concat(ListTR.map(flatten, l));

  let outputTreeNodes:
    type node. instanceForest(node) => lazyHostNodeList(node) =
    subtree =>
      flatten(subtree)
      |> List.fold_left(
           (
             acc: lazyHostNodeList(node),
             Instance(
               (
                 {component: {childrenType}, wrappedHostNode}:
                   instance((_, (node, _, _, _)))
               ),
             ):
               opaqueInstance(node),
           ) =>
             switch (childrenType) {
             | React => List.append(wrappedHostNode, acc)
             | Host => [wrappedHostNode, ...acc]
             },
           [],
         )
      |> List.rev;

  let pendingEffects = (~lifecycle, acc, instanceForest) => {
    let f = (acc, Instance({hooks})) =>
      EffectSequence.chain(
        Hooks.pendingEffects(~lifecycle, Some(hooks)),
        acc,
      );
    let rec fold:
      type any. (EffectSequence.t, instanceForest(any)) => EffectSequence.t =
      (acc, instanceForest) => {
        switch (instanceForest) {
        | IFlat(Instance({childInstances}) as opaqueInstance) =>
          f(fold(acc, childInstances), opaqueInstance)
        | INested(l, _) =>
          List.fold_left(
            (acc, instanceForest) => fold(acc, instanceForest),
            acc,
            l,
          )
        };
      };
    fold(acc, instanceForest);
  };
};

module Element = {
  let rec map = (f, syntheticElement) =>
    switch (syntheticElement) {
    | Flat(l) =>
      let (opaqueInstance, enqueuedMountEffects) = f(l);
      (IFlat(opaqueInstance), enqueuedMountEffects);
    | Nested(l) =>
      let instanceSubForestAndEffects = ListTR.map(map(f), l);
      let subForest = ListTR.map(fst, instanceSubForestAndEffects);
      let effects = ListTR.map(snd, instanceSubForestAndEffects);
      (
        INested(
          subForest,
          subForest
          |> List.map(InstanceForest.getSubtreeSize)
          |> List.fold_left((+), 0),
        ),
        List.fold_left(EffectSequence.chain, EffectSequence.noop, effects),
      );
    };

  let rec fold = (f, renderedElement, nearestHostNode, nodeElement) =>
    switch (renderedElement) {
    | Flat(e) =>
      let {nearestHostNode, nodeElement, opaqueInstance, enqueuedEffects} =
        f(~nearestHostNode, ~nodeElement, e);
      {
        nearestHostNode,
        instanceForest: IFlat(opaqueInstance),
        nodeElement,
        enqueuedEffects,
      };
    | Nested(l) =>
      let (nextL, nearestHostNode, enqueuedEffects, nodeElement) =
        List.fold_left(
          ((acc, nearestHostNode, enqueuedEffects, nodeElement), element) => {
            let {
              nearestHostNode,
              instanceForest,
              enqueuedEffects: nextEffects,
              nodeElement,
            } =
              fold(f, element, nearestHostNode, nodeElement);
            (
              [instanceForest, ...acc],
              nearestHostNode,
              EffectSequence.chain(enqueuedEffects, nextEffects),
              nodeElement,
            );
          },
          ([], nearestHostNode, EffectSequence.noop, nodeElement),
          List.rev(l),
        );
      {
        nearestHostNode,
        nodeElement,
        instanceForest:
          INested(
            nextL,
            List.map(InstanceForest.getSubtreeSize, nextL)
            |> List.fold_left((+), 0),
          ),
        enqueuedEffects,
      };
    };
};

module HostNode = {
  let make =
      (
        type node,
        type children,
        type wrappedNode,
        type childNode,
        childrenType: childrenType((node, children, childNode, wrappedNode)),
        children: children,
        childInstances: instanceForest(childNode),
      )
      : wrappedNode =>
    switch (childrenType) {
    | Host =>
      lazy({
        let instance =
          children.make() |> children.configureInstance(~isFirstRender=true);
        Node(
          List.fold_left(
            ((position, parent), child) =>
              (
                position + 1,
                {
                  let Node(child) | UpdatedNode(_, child) =
                    Lazy.force(child);
                  children.insertNode(~parent, ~child, ~position);
                },
              ),
            (0, instance),
            InstanceForest.outputTreeNodes(childInstances),
          )
          |> snd,
        );
      })
    | React => InstanceForest.outputTreeNodes(childInstances)
    };
};

module SubtreeChange = {
  let insertNodes =
      (
        ~nodeElement,
        ~parent as parentWrapper,
        ~children,
        ~position as initialPosition: int,
      ) => {
    let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
    let newParent =
      List.fold_left(
        ((position, parent), child) =>
          (
            position + 1,
            {
              let Node(child) | UpdatedNode(_, child) = Lazy.force(child);
              nodeElement.insertNode(~parent, ~child, ~position);
            },
          ),
        (initialPosition, oldParent),
        children,
      )
      |> snd;
    newParent === oldParent
      ? parentWrapper : UpdatedNode(oldParent, newParent);
  };
  let deleteNodes =
      (
        ~nodeElement,
        ~parent as parentWrapper,
        ~children,
        ~position as initialPosition: int,
      ) => {
    let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
    let newParent =
      List.fold_left(
        ((position, parent), child) =>
          (
            position + 1,
            {
              let Node(child) | UpdatedNode(_, child) = Lazy.force(child);
              nodeElement.deleteNode(~parent, ~child, ~position);
            },
          ),
        (initialPosition, oldParent),
        children,
      )
      |> snd;
    newParent === oldParent
      ? parentWrapper : UpdatedNode(oldParent, newParent);
  };

  let insertElement = (~nodeElement, ~parent, ~children, ~position) =>
    lazy(
      insertNodes(
        ~nodeElement,
        ~parent=Lazy.force(parent),
        ~children,
        ~position,
      )
    );

  let replaceSubtree =
      (
        ~nodeElement,
        ~parent,
        ~prevChildren,
        ~nextChildren,
        ~absoluteSubtreeIndex: int,
      ) =>
    lazy(
      {
        insertNodes(
          ~nodeElement,
          ~parent=
            deleteNodes(
              ~nodeElement,
              ~parent=Lazy.force(parent),
              ~children=prevChildren,
              ~position=absoluteSubtreeIndex,
            ),
          ~children=nextChildren,
          ~position=absoluteSubtreeIndex,
        );
      }
    );

  let reorderNode =
      (~nodeElement, ~child, ~parent, ~indexShift: int, ~from: int, ~to_: int) => {
    let isVal = Lazy.is_val(child);
    switch (Lazy.force(child)) {
    | Node(child) =>
      from === to_ - indexShift
        ? parent : nodeElement.moveNode(~parent, ~child, ~from, ~to_)
    | UpdatedNode(prevChild, child) when !isVal =>
      nodeElement.insertNode(
        ~parent=
          nodeElement.deleteNode(~parent, ~child=prevChild, ~position=from),
        ~child,
        ~position=to_,
      )
    | UpdatedNode(_prevChild, child) =>
      from === to_ - indexShift
        ? parent : nodeElement.moveNode(~parent, ~child, ~from, ~to_)
    };
  };

  let reorder =
      (
        type node,
        type childNode,
        ~nodeElement,
        ~parent: lazyHostNode(node),
        ~instance as
          Instance({wrappedHostNode, component: {childrenType}}):
            opaqueInstance(childNode),
        ~indexShift,
        ~from,
        ~to_,
      ) =>
    switch (childrenType) {
    | Host =>
      lazy({
        let parentWrapper = Lazy.force(parent);
        let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
        let newParent =
          reorderNode(
            ~nodeElement,
            ~parent=oldParent,
            ~child=wrappedHostNode,
            ~indexShift,
            ~from,
            ~to_,
          );
        newParent === oldParent
          ? parentWrapper : UpdatedNode(oldParent, newParent);
      })
    | React =>
      lazy({
        let parentWrapper = Lazy.force(parent);
        let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
        let newParent =
          List.fold_left(
            ((index, parent), child) =>
              (
                index + 1,
                reorderNode(
                  ~nodeElement,
                  ~parent,
                  ~child,
                  ~indexShift,
                  ~from=from + index,
                  ~to_=to_ + index,
                ),
              ),
            (0, oldParent),
            wrappedHostNode,
          )
          |> snd;
        newParent === oldParent
          ? parentWrapper : UpdatedNode(oldParent, newParent);
      })
    };

  let updateNodes =
      (~nodeElement, ~parent, ~instanceForest, ~position as initialPosition) =>
    lazy({
      let parentWrapper = Lazy.force(parent);
      let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
      let newParent =
        List.fold_left(
          ((position, instance), x) =>
            (
              position + 1,
              switch (Lazy.force(x)) {
              | Node(_child) => instance
              | UpdatedNode(oldNode, newNode) =>
                nodeElement.insertNode(
                  ~parent=
                    nodeElement.deleteNode(
                      ~parent=instance,
                      ~child=oldNode,
                      ~position,
                    ),
                  ~child=newNode,
                  ~position,
                )
              },
            ),
          (initialPosition, oldParent),
          InstanceForest.outputTreeNodes(instanceForest),
        )
        |> snd;
      newParent === oldParent
        ? parentWrapper : UpdatedNode(oldParent, newParent);
    });
};

module OpaqueInstanceHash = {
  type t('node) = lazy_t(Hashtbl.t(int, (opaqueInstance('node), int)));
  let addOpaqueInstance = (idTable, index, opaqueInstance) => {
    let Instance({component: {key}}) = opaqueInstance;
    key == Key.none
      ? () : Hashtbl.add(idTable, key, (opaqueInstance, index));
  };
  let addRenderedElement = (idTable, renderedElement, index) => {
    let rec aux = index =>
      fun
      | IFlat(l) => addOpaqueInstance(idTable, index, l)
      | INested(l, _) => List.iteri((i, x) => aux(i, x), l);
    aux(index, renderedElement);
  };
  let createKeyTable = renderedElement =>
    lazy({
      let keyTable = Hashtbl.create(1);
      addRenderedElement(keyTable, renderedElement, 0);
      keyTable;
    });
  let lookupKey = (table, key) => {
    let keyTable = Lazy.force(table);
    try(Some(Hashtbl.find(keyTable, key))) {
    | Not_found => None
    };
  };
};

module Instance = {

  let rec ofComponent:
    type hooks node children childNode wrappedHostNode.
      (
        opaqueComponent(node),
        component((hooks, (node, children, childNode, wrappedHostNode)))
      ) =>
      (opaqueInstance(node), EffectSequence.t) =
    (opaqueComponent, component) => (
      {
        let (children_, hooks) =
          component.render(
            Hooks.ofState(None, ~onStateDidChange=callStaleHandlers),
          );
        let hooks = Hooks.toState(hooks);
        let childElements =
          switch (component.childrenType) {
          | React => (children_: element(childNode))
          | Host => children_.children
          };
        let (childInstances, mountEffects) = ofList(childElements);
        (
          Instance({
            hooks,
            opaqueComponent,
            component,
            children_,
            childInstances,
            wrappedHostNode:
              HostNode.make(
                component.childrenType,
                children_,
                childInstances,
              ),
          }),
          EffectSequence.chain(
            Hooks.pendingEffects(~lifecycle=Hooks.Effect.Mount, Some(hooks)),
            mountEffects,
          ),
        );
      }: (
        opaqueInstance(node),
        EffectSequence.t,
      )
    )

  and ofOpaqueComponent:
    type node.
      opaqueComponent(node) => (opaqueInstance(node), EffectSequence.t) =
    (OpaqueComponent(component) as opaqueComponent) =>
      ofComponent(opaqueComponent, component)

  and ofList:
    type node. element(node) => (instanceForest(node), EffectSequence.t) =
    syntheticElement => Element.map(ofOpaqueComponent, syntheticElement);

  let pendingEffects =
      (~lifecycle, ~nextEffects, ~instance as {childInstances, hooks}) => {
    InstanceForest.pendingEffects(
      ~lifecycle,
      EffectSequence.chain(
        Hooks.pendingEffects(~lifecycle, Some(hooks)),
        nextEffects,
      ),
      childInstances,
    );
  };
};

module Render = {
  let getOpaqueInstance = (~useKeyTable, OpaqueComponent({key})) =>
    switch (useKeyTable) {
    | None => None
    | Some(keyTable) => OpaqueInstanceHash.lookupKey(keyTable, key)
    };

  type childElementUpdate('node, 'childNode) = {
    updatedRenderedElement: renderedElement('node, 'childNode),
    /* This represents the way previously rendered elements have been shifted due to moves */
    indexShift: int,
  };

  module UpdateContext = {
    type t('node, 'childNode) = {
      shouldExecutePendingUpdates: bool,
      useKeyTable: option(OpaqueInstanceHash.t('childNode)),
      /* This is a unique index of an element within a subtree,
        * thanks to tracking it we can efficiently manage moves of within a subtree
       */
      nearestHostNode: lazyHostNode('node),
      nodeElement: hostNodeElement('node, 'childNode),
      absoluteSubtreeIndex: int,
    };
  };


  /**
      * Initial render of an Element. Recurses to produce the entire tree of
      * instances.
      */
  let rec renderElement:
    type parentNode node.
      (
        ~nearestHostNode: lazyHostNode(parentNode),
        ~nodeElement: hostNodeElement(parentNode, node),
        ~useKeyTable: OpaqueInstanceHash.t(node)=?,
        opaqueComponent(node)
      ) =>
      opaqueInstanceUpdate(parentNode, node) =
    (~nearestHostNode, ~nodeElement, ~useKeyTable=?, opaqueComponent) =>
      switch (getOpaqueInstance(~useKeyTable, opaqueComponent)) {
      | Some((opaqueInstance, _)) =>
        updateOpaqueInstance(
          ~updateContext=
            UpdateContext.{
              nearestHostNode,
              nodeElement,
              absoluteSubtreeIndex: 0,
              useKeyTable,
              shouldExecutePendingUpdates: false,
            },
          opaqueInstance,
          opaqueComponent,
        )
      | None =>
        let (opaqueInstance, enqueuedEffects) =
          Instance.ofOpaqueComponent(opaqueComponent);

        {nearestHostNode, nodeElement, opaqueInstance, enqueuedEffects};
      }

  and renderReactElement:
    type parentNode node.
      (
        ~useKeyTable: OpaqueInstanceHash.t(node)=?,
        lazyHostNode(parentNode),
        element(node),
        hostNodeElement(parentNode, node)
      ) =>
      renderedElement(parentNode, node) =
    (~useKeyTable=?, neareastHostNode, element, nodeElement) =>
      Element.fold(
        renderElement(~useKeyTable?),
        element,
        neareastHostNode,
        nodeElement,
      )

  /**
      * Update a previously rendered instance tree according to a new Element.
      *
      * Here's where the magic happens:
      * -------------------------------
      *
      * We perform a dynamic check that two types are statically equal by way of
      * a type witness! We have a value of type `instance` and another of type
      * `element`, where each has their own `component 'x` for potentially
      * different 'x. We need to see if the they are the same and if so safely "cast"
      * one's `component` to the others. We do this by using a type witness that is
      * able to prove type equality to the compiler through the use of an extensible
      * GADT as a kind of dynamic type.
      *
      * For details, see:
      *   https://discuss.ocaml.org/t/types-as-first-class-citizens-in-ocaml/2030/3
      *   https://alan.petitepomme.net/cwn/2015.03.24.html#1
      *
      * The UpdateLog:
      * ---------------------
      * The updates happen depth first and so the update log contains the deepes
      * changes first.
      * A change at depth N in the tree, causes all nodes from 0 to N generate an
      * update. It's because the render tree is an immutable data structure.
      * A change deep within a tree, means that the subtree of its parent has
      * changed and it propagates to the root of a tree.
      */
  and updateOpaqueInstance:
    type node parentNode.
      (
        ~updateContext: UpdateContext.t(parentNode, node),
        opaqueInstance(node),
        opaqueComponent(node)
      ) =>
      opaqueInstanceUpdate(parentNode, node) =
    (
      ~updateContext,
      Instance(instance) as originalOpaqueInstance,
      OpaqueComponent(nextComponent) as nextOpaqueComponent,
    ) => {
      let nextState =
        updateContext.shouldExecutePendingUpdates
          ? Hooks.flushPendingStateUpdates(instance.hooks) : instance.hooks;
      let stateChanged = nextState !== instance.hooks;

      let bailOut =
        !stateChanged && instance.opaqueComponent === nextOpaqueComponent;

      if (bailOut && !updateContext.shouldExecutePendingUpdates) {
        {
          nearestHostNode: updateContext.nearestHostNode,
          nodeElement: updateContext.nodeElement,
          opaqueInstance: originalOpaqueInstance,
          enqueuedEffects: EffectSequence.noop,
        };
      } else {
        let {component} = instance;
        switch (
          nextComponent.eq(
            {...instance, hooks: nextState},
            component.id,
            nextComponent.id,
          )
        ) {
        /*
         * Case A: The next element *is* of the same component class.
         */
        | Some(handedInstance) =>
          let {
                nearestHostNode,
                nodeElement,
                opaqueInstance: newOpaqueInstance,
                enqueuedEffects,
              } as ret =
            updateInstance(
              ~originalOpaqueInstance,
              ~updateContext,
              ~nextComponent,
              ~nextOpaqueComponent,
              ~stateChanged,
              handedInstance,
            );
          newOpaqueInstance === originalOpaqueInstance
            ? ret
            : {
              nodeElement,
              nearestHostNode:
                SubtreeChange.updateNodes(
                  ~nodeElement,
                  ~parent=nearestHostNode,
                  ~instanceForest=IFlat(newOpaqueInstance),
                  ~position=updateContext.absoluteSubtreeIndex,
                ),
              opaqueInstance: newOpaqueInstance,
              enqueuedEffects,
            };
        /*
         * Case B: The next element is *not* of the same component class. We know
         * because otherwise we would have observed the mutation on
         * `nextComponentClass`.
         */
        | None =>
          /**
            * ** Switching component type **
            */
          let (opaqueInstance, mountEffects) =
            Instance.ofOpaqueComponent(nextOpaqueComponent);
          let enqueuedEffects =
            Instance.pendingEffects(
              ~lifecycle=Hooks.Effect.Unmount,
              ~nextEffects=mountEffects,
              ~instance,
            );
          {
            nodeElement: updateContext.nodeElement,
            nearestHostNode:
              SubtreeChange.replaceSubtree(
                ~nodeElement=updateContext.nodeElement,
                ~parent=updateContext.nearestHostNode,
                ~prevChildren=
                  InstanceForest.outputTreeNodes(
                    IFlat(originalOpaqueInstance),
                  ),
                ~nextChildren=
                  InstanceForest.outputTreeNodes(IFlat(opaqueInstance)),
                ~absoluteSubtreeIndex=updateContext.absoluteSubtreeIndex,
              ),
            opaqueInstance,
            enqueuedEffects,
          };
        };
      };
    }

  and updateInstance:
    type hooks node children childNode wrappedHostNode parentNode.
      (
        ~originalOpaqueInstance: opaqueInstance(node),
        ~updateContext: UpdateContext.t(parentNode, node),
        ~nextComponent: component(
                          (
                            hooks,
                            (node, children, childNode, wrappedHostNode),
                          ),
                        ),
        ~nextOpaqueComponent: opaqueComponent(node),
        ~stateChanged: bool,
        instance((hooks, (node, children, childNode, wrappedHostNode)))
      ) =>
      opaqueInstanceUpdate(parentNode, node) =
    (
      ~originalOpaqueInstance,
      ~updateContext,
      ~nextComponent,
      ~nextOpaqueComponent,
      ~stateChanged,
      instance,
    ) => {
      let updatedInstanceWithNewElement = {
        ...instance,
        component: nextComponent,
        opaqueComponent: nextOpaqueComponent,
      };

      let shouldRerender =
        stateChanged || nextOpaqueComponent !== instance.opaqueComponent;

      let (nextSubElements, initialHooks) =
        if (shouldRerender) {
          let (nextElement, initialHooks) =
            nextComponent.render(
              Hooks.ofState(
                Some(updatedInstanceWithNewElement.hooks),
                ~onStateDidChange=callStaleHandlers,
              ),
            );
          (nextElement, Hooks.toState(initialHooks));
        } else {
          (instance.children_, instance.hooks);
        };

      let updatedInstanceWithNewState = {
        ...updatedInstanceWithNewElement,
        hooks: initialHooks,
      };

      let {children_, childInstances} = updatedInstanceWithNewState;
      let (
        nearestHostNode: lazyHostNode(parentNode),
        updatedInstanceWithNewSubtree,
        enqueuedEffects,
        nodeElement: hostNodeElement(parentNode, node),
      ) =
        switch (nextComponent.childrenType) {
        | React =>
          let {
            nearestHostNode,
            nodeElement,
            instanceForest: nextInstanceSubForest,
            enqueuedEffects,
          } =
            updateInstanceSubtree(
              ~updateContext,
              ~oldInstanceForest=childInstances,
              ~oldReactElement=children_,
              ~nextReactElement=nextSubElements,
              (),
            );
          nextInstanceSubForest !== childInstances
            ? (
              nearestHostNode,
              {
                ...updatedInstanceWithNewState,
                children_: nextSubElements,
                childInstances: nextInstanceSubForest,
              }:
                instance(
                  (hooks, (node, children, childNode, wrappedHostNode)),
                ),
              enqueuedEffects,
              nodeElement,
            )
            : (
              nearestHostNode,
              updatedInstanceWithNewState,
              enqueuedEffects,
              nodeElement,
            );
        | Host =>
          let instanceWithNewHostView:
            instance((hooks, (node, children, childNode, wrappedHostNode))) =
            shouldRerender
              ? {
                ...updatedInstanceWithNewState,
                wrappedHostNode:
                  lazy({
                    let instance =
                      Lazy.force(updatedInstanceWithNewState.wrappedHostNode);
                    let Node(beforeUpdate) | UpdatedNode(_, beforeUpdate) = instance;
                    let afterUpdate =
                      nextSubElements.configureInstance(
                        ~isFirstRender=false,
                        beforeUpdate,
                      );
                    afterUpdate === beforeUpdate
                      ? instance : UpdatedNode(beforeUpdate, afterUpdate);
                  }),
              }
              : updatedInstanceWithNewState;

          let {
            nearestHostNode: wrappedHostNode,
            instanceForest: nextInstanceSubForest,
            enqueuedEffects,
          } = {
            updateInstanceSubtree(
              ~updateContext={
                UpdateContext.shouldExecutePendingUpdates:
                  updateContext.shouldExecutePendingUpdates,
                useKeyTable: None,
                absoluteSubtreeIndex: 0,
                nearestHostNode: (
                  instanceWithNewHostView.wrappedHostNode: lazyHostNode(node)
                ),
                nodeElement: (
                  instanceWithNewHostView.children_:
                    hostNodeElement(node, childNode)
                ),
              },
              ~oldInstanceForest=childInstances,
              ~oldReactElement=children_.children,
              ~nextReactElement=nextSubElements.children,
              (),
            );
          };
          if (nextInstanceSubForest !== instanceWithNewHostView.childInstances) {
            (
              updateContext.nearestHostNode,
              {
                ...instanceWithNewHostView,
                childInstances: nextInstanceSubForest,
                children_: nextSubElements,
                wrappedHostNode,
              }:
                instance(
                  (hooks, (node, children, childNode, wrappedHostNode)),
                ),
              enqueuedEffects,
              updateContext.nodeElement,
            );
          } else {
            (
              updateContext.nearestHostNode,
              instanceWithNewHostView,
              enqueuedEffects,
              updateContext.nodeElement,
            );
          };
        };
      if (updatedInstanceWithNewSubtree === updatedInstanceWithNewElement
          && !stateChanged) {
        {
          nodeElement,
          nearestHostNode,
          opaqueInstance: originalOpaqueInstance,
          enqueuedEffects,
        };
      } else {
        {
          nodeElement,
          nearestHostNode,
          opaqueInstance: Instance(updatedInstanceWithNewSubtree),
          enqueuedEffects:
            EffectSequence.chain(
              Hooks.pendingEffects(
                ~lifecycle=Hooks.Effect.Update,
                Some(updatedInstanceWithNewSubtree.hooks),
              ),
              enqueuedEffects,
            ),
        };
      };
    }

  /**
      * updateRenderedElement recurses through the syntheticElement tree as long as
      * the oldReactElement and nextReactElement have the same shape.
      *
      * The base case is either an empty list - Nested([]) or a Flat element.
      *
      * syntheticElement is a recursive tree like data structure. The tree doesn't
      * contain children of the syntheticElements returned from children, it only
      * contains the "immediate" children so to speak including all nested lists.
      *
      * `keyTable` is a hash table containing all keys in the syntheticElement tree.
      */
  and updateInstanceSubtree:
    type parentNode node.
      (
        ~updateContext: UpdateContext.t(parentNode, node),
        ~oldInstanceForest: instanceForest(node),
        ~oldReactElement: element(node),
        ~nextReactElement: element(node),
        unit
      ) =>
      renderedElement(parentNode, node) =
    (
      ~updateContext,
      ~oldInstanceForest,
      ~oldReactElement,
      ~nextReactElement,
      (),
    ) =>
      switch (oldInstanceForest, oldReactElement, nextReactElement) {
      | (
          INested(instanceSubTrees, subtreeSize),
          Nested(oldReactElements),
          Nested([nextReactElement, ...nextReactElements]),
        )
          when nextReactElements === oldReactElements =>
        /* Detected that nextReactElement was obtained by adding one element */
        let {
          nearestHostNode,
          nodeElement,
          instanceForest: addedElement,
          enqueuedEffects,
        } =
          renderReactElement(
            updateContext.nearestHostNode,
            nextReactElement,
            updateContext.nodeElement,
          );
        {
          nodeElement,
          nearestHostNode:
            SubtreeChange.insertElement(
              ~nodeElement,
              ~parent=nearestHostNode,
              ~children=InstanceForest.outputTreeNodes(addedElement),
              ~position=updateContext.absoluteSubtreeIndex,
            ),
          /*** Prepend element */
          instanceForest:
            INested(
              [addedElement, ...instanceSubTrees],
              subtreeSize + InstanceForest.getSubtreeSize(addedElement),
            ),
          enqueuedEffects,
        };
      | (
          INested(oldInstanceForests, _),
          Nested(oldReactElements),
          Nested(nextReactElements),
        )
          when
            List.length(nextReactElements)
            === List.length(oldInstanceForests) =>
        let keyTable = OpaqueInstanceHash.createKeyTable(oldInstanceForest);
        let (
          nearestHostNode,
          newInstanceForests,
          subtreeSize,
          _indexShift,
          enqueuedEffects,
          nodeElement,
        ) =
          ListTR.fold3(
            (
              (
                nearestHostNode,
                renderedElements,
                prevSubtreeSize,
                indexShift,
                enqueuedEffectsAcc,
                nodeElement,
              ),
              oldInstanceForest,
              oldReactElement,
              nextReactElement,
            ) => {
              let {
                indexShift,
                updatedRenderedElement: {
                  nearestHostNode,
                  instanceForest,
                  enqueuedEffects,
                  nodeElement,
                },
              } =
                updateChildRenderedElement(
                  ~updateContext={
                    ...updateContext,
                    nearestHostNode,
                    useKeyTable: Some(keyTable),
                    absoluteSubtreeIndex: prevSubtreeSize,
                    nodeElement,
                  },
                  ~indexShift,
                  ~oldInstanceForest,
                  ~oldReactElement,
                  ~nextReactElement,
                  (),
                );
              (
                nearestHostNode,
                [instanceForest, ...renderedElements],
                prevSubtreeSize
                + InstanceForest.getSubtreeSize(instanceForest),
                indexShift,
                EffectSequence.chain(enqueuedEffects, enqueuedEffectsAcc),
                nodeElement,
              );
            },
            oldInstanceForests,
            oldReactElements,
            nextReactElements,
            (
              updateContext.nearestHostNode,
              [],
              updateContext.absoluteSubtreeIndex,
              0,
              EffectSequence.noop,
              updateContext.nodeElement,
            ),
          );
        let newInstanceForests = List.rev(newInstanceForests);
        {
          nodeElement,
          nearestHostNode,
          instanceForest: INested(newInstanceForests, subtreeSize),
          enqueuedEffects,
        };
      /*
       * Key Policy for syntheticElement.
       * Nested elements determine shape: if the shape is not identical, re-render.
       * Flat elements use a positional match by default, where components at
       * the same position (from left) are matched for updates.
       * If the component has an explicit key, match the instance with the same key.
       * Note: components are matched for key across the entire syntheticElement structure.
       */
      | (
          IFlat(Instance(oldInstance) as oldOpaqueInstance),
          Flat(OpaqueComponent({key: oldKey})),
          Flat(OpaqueComponent({key: nextKey}) as nextReactElement),
        ) =>
        if (nextKey !== oldKey) {
          /* Not found: render a new instance */
          let {
            nearestHostNode,
            opaqueInstance: newOpaqueInstance,
            enqueuedEffects: mountEffects,
            nodeElement,
          } =
            renderElement(
              nextReactElement,
              ~nearestHostNode=updateContext.nearestHostNode,
              ~nodeElement=updateContext.nodeElement,
            );
          let enqueuedEffects =
            Instance.pendingEffects(
              ~lifecycle=Unmount,
              ~nextEffects=mountEffects,
              ~instance=oldInstance,
            );
          let newInstanceForest = IFlat(newOpaqueInstance);
          {
            nodeElement,
            nearestHostNode:
              SubtreeChange.replaceSubtree(
                ~nodeElement,
                ~parent=nearestHostNode,
                ~prevChildren=
                  InstanceForest.outputTreeNodes(oldInstanceForest),
                ~nextChildren=
                  InstanceForest.outputTreeNodes(newInstanceForest),
                /* hard-coded zero since we will only ever have one child */
                ~absoluteSubtreeIndex=0,
              ),
            instanceForest: newInstanceForest,
            enqueuedEffects,
          };
        } else {
          let {
            nearestHostNode,
            opaqueInstance: newOpaqueInstance,
            enqueuedEffects,
            nodeElement,
          } =
            updateOpaqueInstance(
              ~updateContext={...updateContext, useKeyTable: None},
              oldOpaqueInstance,
              nextReactElement,
            );
          {
            nodeElement,
            nearestHostNode,
            instanceForest:
              oldOpaqueInstance !== newOpaqueInstance
                ? IFlat(newOpaqueInstance) : oldInstanceForest,
            enqueuedEffects,
          };
        }
      | (oldInstanceForest, _, _) =>
        /* Notice that all elements which are queried successfully
         *  from the hash table must have been here in the previous render
         * No, it's not true. What if the key is the same but element type changes
         */
        let keyTable =
          switch (updateContext.useKeyTable) {
          | None => OpaqueInstanceHash.createKeyTable(oldInstanceForest)
          | Some(keyTable) => keyTable
          };
        let {
          nearestHostNode,
          instanceForest: newInstanceForest,
          enqueuedEffects: mountEffects,
          nodeElement,
        } =
          renderReactElement(
            ~useKeyTable=keyTable,
            updateContext.nearestHostNode,
            nextReactElement,
            updateContext.nodeElement,
          );

        let enqueuedEffects =
          InstanceForest.pendingEffects(
            ~lifecycle=Hooks.Effect.Unmount,
            mountEffects,
            oldInstanceForest,
          );
        {
          nodeElement,
          nearestHostNode:
            SubtreeChange.replaceSubtree(
              ~nodeElement,
              ~parent=nearestHostNode,
              ~prevChildren=InstanceForest.outputTreeNodes(oldInstanceForest),
              ~nextChildren=InstanceForest.outputTreeNodes(newInstanceForest),
              ~absoluteSubtreeIndex=updateContext.absoluteSubtreeIndex,
            ),
          instanceForest: newInstanceForest,
          enqueuedEffects,
        };
      }

  and updateChildRenderedElement:
    type parentNode node.
      (
        ~updateContext: UpdateContext.t(parentNode, node),
        ~indexShift: int,
        ~oldInstanceForest: instanceForest(node),
        ~oldReactElement: element(node),
        ~nextReactElement: element(node),
        unit
      ) =>
      childElementUpdate(parentNode, node) =
    (
      ~updateContext as {
        UpdateContext.shouldExecutePendingUpdates,
        useKeyTable,
        nearestHostNode,
        nodeElement,
        absoluteSubtreeIndex,
      },
      ~indexShift,
      ~oldInstanceForest,
      ~oldReactElement,
      ~nextReactElement,
      (),
    ) =>
      switch (oldInstanceForest, oldReactElement, nextReactElement) {
      /*
       * Key Policy for syntheticElement.
       * Nested elements determine shape: if the shape is not identical, re-render.
       * Flat elements use a positional match by default, where components at
       * the same position (from left) are matched for updates.
       * If the component has an explicit key, match the instance with the same key.
       * Note: components are matched for key across the entire syntheticElement structure.
       */
      | (
          IFlat(oldOpaqueInstance),
          Flat(OpaqueComponent({key: oldKey})),
          Flat(OpaqueComponent({key: nextKey}) as nextReactElement),
        ) =>
        let keyTable =
          switch (useKeyTable) {
          | None =>
            OpaqueInstanceHash.createKeyTable(IFlat(oldOpaqueInstance))
          | Some(keyTable) => keyTable
          };
        let (nearestHostNode, update, newOpaqueInstance, enqueuedEffects) = {
          let OpaqueComponent(component) = nextReactElement;
          if (component.key !== Key.none) {
            switch (OpaqueInstanceHash.lookupKey(keyTable, component.key)) {
            | Some((subOpaqueInstance, previousIndex)) =>
              /* Instance tree with the same component key */
              let {
                nearestHostNode,
                opaqueInstance: updatedOpaqueInstance,
                enqueuedEffects,
              } =
                updateOpaqueInstance(
                  ~updateContext=
                    UpdateContext.{
                      useKeyTable,
                      shouldExecutePendingUpdates,
                      nearestHostNode,
                      absoluteSubtreeIndex: previousIndex + indexShift,
                      nodeElement,
                    },
                  subOpaqueInstance,
                  nextReactElement,
                );
              (
                nearestHostNode,
                `NoChangeOrNested(previousIndex),
                updatedOpaqueInstance,
                enqueuedEffects,
              );
            | None =>
              /* Not found: render a new instance */
              let {
                nearestHostNode,
                opaqueInstance: newOpaqueInstance,
                enqueuedEffects,
              } =
                renderElement(
                  ~nearestHostNode,
                  nextReactElement,
                  ~nodeElement,
                );
              (
                nearestHostNode,
                `NewElement,
                newOpaqueInstance,
                enqueuedEffects,
              );
            };
          } else {
            let {
              nearestHostNode,
              opaqueInstance: updatedOpaqueInstance,
              enqueuedEffects,
            } =
              updateOpaqueInstance(
                ~updateContext=
                  UpdateContext.{
                    nodeElement,
                    shouldExecutePendingUpdates,
                    nearestHostNode,
                    absoluteSubtreeIndex,
                    useKeyTable,
                  },
                oldOpaqueInstance,
                nextReactElement,
              );
            (
              nearestHostNode,
              `NoChangeOrNested(absoluteSubtreeIndex),
              updatedOpaqueInstance,
              enqueuedEffects,
            );
          };
        };
        switch (update) {
        | `NewElement =>
          let newInstanceForest = IFlat(newOpaqueInstance);
          {
            updatedRenderedElement: {
              nearestHostNode:
                SubtreeChange.replaceSubtree(
                  ~nodeElement,
                  ~parent=nearestHostNode,
                  ~prevChildren=
                    InstanceForest.outputTreeNodes(oldInstanceForest),
                  ~nextChildren=
                    InstanceForest.outputTreeNodes(newInstanceForest),
                  ~absoluteSubtreeIndex,
                ),
              instanceForest: newInstanceForest,
              enqueuedEffects,
              nodeElement,
            },
            indexShift: 0,
          };
        | `NoChangeOrNested(previousIndex) =>
          let changed = oldOpaqueInstance !== newOpaqueInstance;
          let element =
            changed ? IFlat(newOpaqueInstance) : oldInstanceForest;
          if (oldKey != nextKey) {
            {
              updatedRenderedElement: {
                nearestHostNode:
                  SubtreeChange.reorder(
                    ~nodeElement,
                    ~parent=nearestHostNode,
                    ~instance=newOpaqueInstance,
                    ~indexShift,
                    ~from=previousIndex,
                    ~to_=absoluteSubtreeIndex,
                  ),
                instanceForest: element,
                enqueuedEffects,
                nodeElement,
              },
              indexShift: InstanceForest.getSubtreeSize(element),
            };
          } else {
            {
              updatedRenderedElement: {
                nearestHostNode,
                instanceForest: element,
                enqueuedEffects,
                nodeElement,
              },
              indexShift: 0,
            };
          };
        };
      | (_, _, _) => {
          updatedRenderedElement:
            updateInstanceSubtree(
              ~updateContext={
                UpdateContext.absoluteSubtreeIndex,
                nodeElement,
                shouldExecutePendingUpdates,
                nearestHostNode,
                /* Not sure about this */
                useKeyTable,
              },
              ~oldInstanceForest,
              ~oldReactElement,
              ~nextReactElement,
              (),
            ),
          indexShift: 0,
        }
      };

  /**
      * Execute the pending updates at the top level of an instance tree.
      * If no state change is performed, the argument is returned unchanged.
      */
  let flushPendingUpdates = (opaqueInstance, nearestHostNode, nodeElement) => {
    let Instance({opaqueComponent}) = opaqueInstance;
    updateOpaqueInstance(
      ~updateContext=
        UpdateContext.{
          useKeyTable: None,
          shouldExecutePendingUpdates: true,
          nearestHostNode,
          nodeElement,
          absoluteSubtreeIndex: 0,
        },
      opaqueInstance,
      opaqueComponent,
    );
  };
};

module RenderedElement = {
  /**
      * Rendering produces a list of instance trees.
      */
  type t('node, 'childNode) = renderedElement('node, 'childNode);
  type root('node, 'childNode) = {
    node: 'node,
    insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
    deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
    moveNode:
      (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
  };

  let listToRenderedElement = renderedElements =>
    INested(
      renderedElements,
      renderedElements
      |> List.fold_left(
           (acc, e) => acc + InstanceForest.getSubtreeSize(e),
           0,
         ),
    );
  let render = (root, children) => {
    let (instanceForest, mountEffects) = Instance.ofList(children);
    {
      nodeElement: {
        make: () => root.node,
        configureInstance: (~isFirstRender as _, i) => i,
        children,
        insertNode: root.insertNode,
        deleteNode: root.deleteNode,
        moveNode: root.moveNode,
      },
      instanceForest,
      nearestHostNode:
        lazy(
          Node(
            InstanceForest.outputTreeNodes(instanceForest)
            |> List.fold_left(
                 ((position, parent), child) =>
                   (
                     position + 1,
                     {
                       let Node(child) | UpdatedNode(_, child) =
                         Lazy.force(child);
                       let parent =
                         root.insertNode(~parent, ~child, ~position);
                       parent;
                     },
                   ),
                 (0, root.node),
               )
            |> snd,
          )
        ),
      enqueuedEffects: mountEffects,
    };
  };
  let update =
      (
        ~previousElement,
        ~renderedElement as {instanceForest, nearestHostNode, nodeElement},
        nextReactElement,
      ) =>
    Render.updateInstanceSubtree(
      ~updateContext=
        Render.UpdateContext.{
          nodeElement,
          nearestHostNode,
          absoluteSubtreeIndex: 0,
          useKeyTable: None,
          shouldExecutePendingUpdates: false,
        },
      ~oldInstanceForest=instanceForest,
      ~oldReactElement=previousElement,
      ~nextReactElement,
      (),
    );

  let rec map = (f, renderedElement, nearestHostNode, nodeElement) =>
    switch (renderedElement) {
    | IFlat(e) =>
      let {nearestHostNode, opaqueInstance, enqueuedEffects, nodeElement} =
        f(e, nearestHostNode, nodeElement);
      let unchanged = e === opaqueInstance;

      {
        nodeElement,
        nearestHostNode,
        instanceForest: unchanged ? renderedElement : IFlat(opaqueInstance),
        enqueuedEffects,
      };
    | INested(l, _) =>
      let (nextL, nearestHostNode, effects, nodeElement) =
        List.fold_left(
          ((acc, nearestHostNode, effectsAcc, nodeElement), renderedElement) => {
            let {
              nearestHostNode,
              instanceForest: next,
              enqueuedEffects,
              nodeElement,
            } =
              map(f, renderedElement, nearestHostNode, nodeElement);
            (
              [next, ...acc],
              nearestHostNode,
              EffectSequence.chain(effectsAcc, enqueuedEffects),
              nodeElement,
            );
          },
          ([], nearestHostNode, EffectSequence.noop, nodeElement),
          List.rev(l),
        );
      let unchanged = List.for_all2((===), l, nextL);

      {
        nodeElement,
        nearestHostNode,
        instanceForest:
          unchanged
            ? renderedElement
            : INested(
                nextL,
                List.fold_left(
                  (acc, elem) => InstanceForest.getSubtreeSize(elem) + acc,
                  0,
                  nextL,
                ),
              ),
        enqueuedEffects: effects,
      };
    };

  /**
      * Flush the pending updates in an instance tree.
      */
  let flushPendingUpdates =
      ({instanceForest, nearestHostNode, enqueuedEffects, nodeElement}) => {
    let {
      nearestHostNode,
      instanceForest: newInstanceForest,
      enqueuedEffects: nextEnqueuedEffects,
      nodeElement,
    } =
      map(
        Render.flushPendingUpdates,
        instanceForest,
        nearestHostNode,
        nodeElement,
      );
    {
      nodeElement,
      instanceForest: newInstanceForest,
      nearestHostNode,
      enqueuedEffects:
        EffectSequence.chain(nextEnqueuedEffects, enqueuedEffects),
    };
  };

  let executeHostViewUpdates = ({nearestHostNode}: t(_, _)) => {
    let Node(hostView) | UpdatedNode(_, hostView) =
      Lazy.force(nearestHostNode);
    hostView;
  };

  let executePendingEffects =
      ({enqueuedEffects} as renderedElement: t(_, _)) => {
    enqueuedEffects();
    {...renderedElement, enqueuedEffects: EffectSequence.noop};
  };
};

let element = (~key as argumentKey=Key.none, component) => {
  let key =
    argumentKey != Key.none
      ? argumentKey
      : {
        let isDynamicKey = component.key == Key.dynamicKeyMagicNumber;
        isDynamicKey ? Key.create() : Key.none;
      };
  let componentWithKey =
    key != component.key ? {...component, key} : component;
  Flat(OpaqueComponent(componentWithKey));
};

let listToElement = l => Nested(l);
let empty = Nested([]);

module Hooks = Hooks;
module RemoteAction = RemoteAction;

module Expert = {
  let jsx_list = listToElement;
  let component:
    type a node.
      (
        ~useDynamicKey: bool=?,
        string,
        ~key: Key.t=?,
        Hooks.t(a, a) => (element(node), Hooks.t(Hooks.nil, a))
      ) =>
      element(node) =
    (~useDynamicKey=false, debugName) => {
      module Component = {
        type componentId('a) +=
          | Id: componentId(
                  instance(
                    (
                      a,
                      (node, element(node), node, lazyHostNodeList(node)),
                    ),
                  ),
                );

        let eq:
          type c.
            (
              c,
              componentId(c),
              componentId(
                instance(
                  (a, (node, element(node), node, lazyHostNodeList(node))),
                ),
              )
            ) =>
            option(
              instance(
                (a, (node, element(node), node, lazyHostNodeList(node))),
              ),
            ) =
          (instance, id1, id2) => {
            switch (id1, id2) {
            | (Id, Id) => Some(instance)
            | (_, _) => None
            };
          };
      };
      (~key=?, render) =>
        element(
          ~key?,
          {
            debugName,
            childrenType: React,
            key: useDynamicKey ? Key.dynamicKeyMagicNumber : Key.none,
            id: Component.Id,
            eq: Component.eq,
            render,
          },
        );
    };

  let nativeComponent:
    type a node childNode.
      (
        ~useDynamicKey: bool=?,
        string,
        ~key: Key.t=?,
        Hooks.t(a, a) =>
        (hostNodeElement(node, childNode), Hooks.t(Hooks.nil, a))
      ) =>
      element(node) =
    (~useDynamicKey=false, debugName) => {
      module Component = {
        type componentId('a) +=
          | Id: componentId(
                  instance(
                    (
                      a,
                      (
                        node,
                        hostNodeElement(node, childNode),
                        childNode,
                        lazyHostNode(node),
                      ),
                    ),
                  ),
                );

        let eq:
          type c.
            (
              c,
              componentId(c),
              componentId(
                instance(
                  (
                    a,
                    (
                      node,
                      hostNodeElement(node, childNode),
                      childNode,
                      lazyHostNode(node),
                    ),
                  ),
                ),
              )
            ) =>
            option(
              instance(
                (
                  a,
                  (
                    node,
                    hostNodeElement(node, childNode),
                    childNode,
                    lazyHostNode(node),
                  ),
                ),
              ),
            ) =
          (instance, id1, id2) => {
            switch (id1, id2) {
            | (Id, Id) => Some(instance)
            | (_, _) => None
            };
          };
      };
      (~key=?, render) =>
        element(
          ~key?,
          {
            debugName,
            childrenType: Host,
            key: useDynamicKey ? Key.dynamicKeyMagicNumber : Key.none,
            id: Component.Id,
            eq: Component.eq,
            render,
          },
        );
    };
};
let component = (~useDynamicKey=?, debugName) => {
  let c = Expert.component(~useDynamicKey?, debugName);
  (~key=?, render) => {
    c(
      ~key?,
      hooks => {
        let (hooks, e) = render(hooks);
        (e, hooks);
      },
    );
  };
};
let nativeComponent = (~useDynamicKey=?, debugName) => {
  let c = Expert.nativeComponent(~useDynamicKey?, debugName);
  (~key=?, render) => {
    c(
      ~key?,
      hooks => {
        let (hooks, e) = render(hooks);
        (e, hooks);
      },
    );
  };
};
