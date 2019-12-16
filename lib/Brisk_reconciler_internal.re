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
let callStaleHanlders = () => List.iter(f => f(), staleHandlers^);

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

type opaqueInstanceHash('node) =
  Lazy.t(Hashtbl.t(int, (opaqueInstance('node), int)));

module Update = {
  type hostTreeUpdate('node, 'childNode) = {
    nearestHostNode: lazyHostNode('node),
    nodeElement: hostNodeElement('node, 'childNode),
    /* This is a unique index of an element within a subtree,
      * thanks to tracking it we can efficiently manage moves of within a subtree
     */
    absoluteSubtreeIndex: int,
  };
  type t('node, 'childNode, 'payload) = {
    hostTreeUpdate: hostTreeUpdate('node, 'childNode),
    enqueuedEffects: EffectSequence.t,
    payload: 'payload,
  };
  type context('node, 'childNode) = {
    shouldExecutePendingUpdates: bool,
    useKeyTable: option(opaqueInstanceHash('childNode)),
    hostTreeUpdate: hostTreeUpdate('node, 'childNode),
  };
  let map = (f, update) => {
    let {payload, hostTreeUpdate, enqueuedEffects} = update;
    {hostTreeUpdate, enqueuedEffects, payload: f(payload)};
  };
};

type renderedElement('node, 'childNode) =
  Update.t('node, 'childNode, instanceForest('childNode));

type opaqueInstanceUpdate('node, 'childNode) =
  Update.t('node, 'childNode, opaqueInstance('childNode));

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

  let rec fold = (~f, ~init, renderedElement) =>
    switch (renderedElement) {
    | Flat(e) =>
      f(~hostTreeUpdate=init, ~element=e)
      |> Update.map(instance => IFlat(instance))
    | Nested(l) =>
      List.fold_left(
        (update: Update.t(_, _, _), element) => {
          let {Update.hostTreeUpdate, payload, enqueuedEffects: nextEffects} =
            fold(~f, ~init=update.Update.hostTreeUpdate, element);
          {
            Update.payload: [payload, ...update.Update.payload],
            enqueuedEffects:
              EffectSequence.chain(
                update.Update.enqueuedEffects,
                nextEffects,
              ),
            hostTreeUpdate,
          };
        },
        {
          Update.payload: [],
          enqueuedEffects: EffectSequence.noop,
          hostTreeUpdate: init,
        },
        List.rev(l),
      )
      |> Update.map(payload =>
           INested(
             payload,
             List.map(InstanceForest.getSubtreeSize, payload)
             |> List.fold_left((+), 0),
           )
         )
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
  type t('node) = opaqueInstanceHash('node);
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
            Hooks.ofState(None, ~onStateDidChange=callStaleHanlders),
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

  type subtreeUpdate('node) =
    | MatchingSubtree(list(subtreeUpdate('node)))
    | Update(opaqueInstance('node), opaqueComponent('node))
    | ReRender(element('node));

  let rec prepareUpdate = (~oldInstanceForest, ~nextElement) => {
    switch (oldInstanceForest, nextElement) {
    | (IFlat(instance), Flat(component)) => Update(instance, component)
    | (INested(instances, _), Nested(elements))
        when List.length(instances) == List.length(elements) =>
      MatchingSubtree(
        List.map2(
          (inst, elem) =>
            prepareUpdate(~oldInstanceForest=inst, ~nextElement=elem),
          instances,
          elements,
        ),
      )
    | _ => ReRender(nextElement)
    };
  };


  /**
      * Initial render of an Element. Recurses to produce the entire tree of
      * instances.
      */
  let rec renderElement:
    type parentNode node.
      (
        ~updateContext: Update.context(parentNode, node),
        opaqueComponent(node)
      ) =>
      opaqueInstanceUpdate(parentNode, node) =
    (~updateContext, opaqueComponent) =>
      switch (
        getOpaqueInstance(
          ~useKeyTable=updateContext.useKeyTable,
          opaqueComponent,
        )
      ) {
      | Some((opaqueInstance, _)) =>
        updateOpaqueInstance(~updateContext, opaqueInstance, opaqueComponent)
      | None =>
        let (opaqueInstance, enqueuedEffects) =
          Instance.ofOpaqueComponent(opaqueComponent);
        // This is a bug; the nearestHostNode and nodeElement are not updated.
        {
          hostTreeUpdate: updateContext.hostTreeUpdate,
          enqueuedEffects,
          payload: opaqueInstance,
        };
      }

  and renderReactElement:
    type parentNode node.
      (~updateContext: Update.context(parentNode, node), element(node)) =>
      renderedElement(parentNode, node) =
    (~updateContext, element) =>
      Element.fold(
        ~f=
          (~hostTreeUpdate, ~element) => {
            renderElement(
              ~updateContext={...updateContext, hostTreeUpdate},
              element,
            )
          },
        ~init=updateContext.hostTreeUpdate,
        element,
      )

  and updateOpaqueInstance:
    type node parentNode.
      (
        ~updateContext: Update.context(parentNode, node),
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
          hostTreeUpdate: updateContext.hostTreeUpdate,
          payload: originalOpaqueInstance,
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
                Update.hostTreeUpdate,
                payload: newOpaqueInstance,
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
              hostTreeUpdate: {
                nodeElement: hostTreeUpdate.nodeElement,
                nearestHostNode:
                  SubtreeChange.updateNodes(
                    ~nodeElement=hostTreeUpdate.nodeElement,
                    ~parent=hostTreeUpdate.nearestHostNode,
                    ~instanceForest=IFlat(newOpaqueInstance),
                    ~position=hostTreeUpdate.absoluteSubtreeIndex,
                  ),
                absoluteSubtreeIndex: hostTreeUpdate.absoluteSubtreeIndex,
              },
              payload: newOpaqueInstance,
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
          let hostTreeUpdate = updateContext.hostTreeUpdate;
          {
            hostTreeUpdate: {
              nodeElement: hostTreeUpdate.nodeElement,
              nearestHostNode:
                SubtreeChange.replaceSubtree(
                  ~nodeElement=hostTreeUpdate.nodeElement,
                  ~parent=hostTreeUpdate.nearestHostNode,
                  ~prevChildren=
                    InstanceForest.outputTreeNodes(
                      IFlat(originalOpaqueInstance),
                    ),
                  ~nextChildren=
                    InstanceForest.outputTreeNodes(IFlat(opaqueInstance)),
                  ~absoluteSubtreeIndex=
                    updateContext.hostTreeUpdate.absoluteSubtreeIndex,
                ),
              absoluteSubtreeIndex: 0,
            },
            payload: opaqueInstance,
            enqueuedEffects,
          };
        };
      };
    }

  and updateInstance:
    type hooks node children childNode wrappedHostNode parentNode.
      (
        ~originalOpaqueInstance: opaqueInstance(node),
        ~updateContext: Update.context(parentNode, node),
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
                ~onStateDidChange=callStaleHanlders,
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

      let {childInstances} = updatedInstanceWithNewState;
      let (
        nearestHostNode: lazyHostNode(parentNode),
        updatedInstanceWithNewSubtree,
        enqueuedEffects,
        nodeElement: hostNodeElement(parentNode, node),
      ) =
        switch (nextComponent.childrenType) {
        | React =>
          let {
            Update.payload: nextInstanceSubForest,
            enqueuedEffects,
          } =
            updateInstanceSubtree(
              ~updateContext,
              ~oldInstanceForest=childInstances,
              ~nextElement=nextSubElements,
              (),
            );
          nextInstanceSubForest !== childInstances
            ? (
              updateContext.hostTreeUpdate.nearestHostNode,
              {
                ...updatedInstanceWithNewState,
                children_: nextSubElements,
                childInstances: nextInstanceSubForest,
              }:
                instance(
                  (hooks, (node, children, childNode, wrappedHostNode)),
                ),
              enqueuedEffects,
              updateContext.hostTreeUpdate.nodeElement,
            )
            : (
              updateContext.hostTreeUpdate.nearestHostNode,
              updatedInstanceWithNewState,
              enqueuedEffects,
              updateContext.hostTreeUpdate.nodeElement,
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
            Update.hostTreeUpdate,
            payload: nextInstanceSubForest,
            enqueuedEffects,
          } = {
            updateInstanceSubtree(
              ~updateContext={
                Update.shouldExecutePendingUpdates:
                  updateContext.shouldExecutePendingUpdates,
                useKeyTable: None,
                hostTreeUpdate: {
                  absoluteSubtreeIndex: 0,
                  nearestHostNode: (
                    instanceWithNewHostView.wrappedHostNode:
                      lazyHostNode(node)
                  ),
                  nodeElement: (
                    instanceWithNewHostView.children_:
                      hostNodeElement(node, childNode)
                  ),
                },
              },
              ~oldInstanceForest=childInstances,
              ~nextElement=nextSubElements.children,
              (),
            );
          };
          if (nextInstanceSubForest !== instanceWithNewHostView.childInstances) {
            (
              updateContext.hostTreeUpdate.nearestHostNode,
              {
                ...instanceWithNewHostView,
                childInstances: nextInstanceSubForest,
                children_: nextSubElements,
                wrappedHostNode: hostTreeUpdate.nearestHostNode,
              }:
                instance(
                  (hooks, (node, children, childNode, wrappedHostNode)),
                ),
              enqueuedEffects,
              updateContext.hostTreeUpdate.nodeElement,
            );
          } else {
            (
              updateContext.hostTreeUpdate.nearestHostNode,
              instanceWithNewHostView,
              enqueuedEffects,
              updateContext.hostTreeUpdate.nodeElement,
            );
          };
        };
      if (updatedInstanceWithNewSubtree === updatedInstanceWithNewElement
          && !stateChanged) {
        {
          hostTreeUpdate: {
            nodeElement,
            nearestHostNode,
            absoluteSubtreeIndex: 0,
          },
          payload: originalOpaqueInstance,
          enqueuedEffects,
        };
      } else {
        {
          hostTreeUpdate: {
            nodeElement,
            nearestHostNode,
            absoluteSubtreeIndex: 0,
          },
          payload: Instance(updatedInstanceWithNewSubtree),
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

  and applyUpdate:
    type node childNode.
      (
        ~updateContext: Update.context(node, childNode),
        subtreeUpdate(childNode)
      ) =>
      Update.t(node, childNode, instanceForest(childNode)) =
    (~updateContext, update) => {
      switch (update) {
      | MatchingSubtree(updates) =>
        let update =
          List.fold_left(
            (updateAcc: Update.t(_, _, _), subtreeUpdate) => {
              let update =
                applyUpdate(
                  ~updateContext={
                    Update.hostTreeUpdate: updateAcc.Update.hostTreeUpdate,
                    shouldExecutePendingUpdates:
                      updateContext.shouldExecutePendingUpdates,
                    useKeyTable: updateContext.useKeyTable,
                  },
                  subtreeUpdate,
                );
              // Don't forget about effects...
              update
              |> Update.map(updatedInstanceForest =>
                   [updatedInstanceForest, ...updateAcc.payload]
                 );
            },
            {
              Update.payload: [],
              hostTreeUpdate: updateContext.hostTreeUpdate,
              enqueuedEffects: EffectSequence.noop,
            },
            List.rev(updates),
          );
        update
        |> Update.map(instances =>
             INested(
               instances,
               update.Update.hostTreeUpdate.absoluteSubtreeIndex
               - updateContext.hostTreeUpdate.absoluteSubtreeIndex,
             )
           );
      | Update(instance, newComponent) =>
        updateOpaqueInstance(~updateContext, instance, newComponent)
        |> Update.map(instance => IFlat(instance))
      | ReRender(element) => renderReactElement(~updateContext, element)
      };
    }

  and updateInstanceSubtree:
    type parentNode node.
      (
        ~updateContext: Update.context(parentNode, node),
        ~oldInstanceForest: instanceForest(node),
        ~nextElement: element(node),
        unit
      ) =>
      renderedElement(parentNode, node) =
    (~updateContext, ~oldInstanceForest, ~nextElement, ()) => {
      applyUpdate(
        ~updateContext,
        prepareUpdate(~oldInstanceForest, ~nextElement),
      );
    };

  /**
      * Execute the pending updates at the top level of an instance tree.
      * If no state change is performed, the argument is returned unchanged.
      */
  let flushPendingUpdates = (opaqueInstance, nearestHostNode, nodeElement) => {
    let Instance({opaqueComponent}) = opaqueInstance;
    updateOpaqueInstance(
      ~updateContext=
        Update.{
          useKeyTable: None,
          shouldExecutePendingUpdates: true,
          hostTreeUpdate: {
            nearestHostNode,
            nodeElement,
            absoluteSubtreeIndex: 0,
          },
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
      Update.hostTreeUpdate: {
        nodeElement: {
          make: () => root.node,
          configureInstance: (~isFirstRender as _, i) => i,
          children,
          insertNode: root.insertNode,
          deleteNode: root.deleteNode,
          moveNode: root.moveNode,
        },
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
        absoluteSubtreeIndex: 0,
      },
      payload: instanceForest,
      enqueuedEffects: mountEffects,
    };
  };
  let update =
      (~renderedElement as {Update.payload, hostTreeUpdate}, nextElement) =>
    Render.updateInstanceSubtree(
      ~updateContext=
        Update.{
          hostTreeUpdate,
          useKeyTable: None,
          shouldExecutePendingUpdates: false,
        },
      ~oldInstanceForest=payload,
      ~nextElement,
      (),
    );

  let rec map = (f, renderedElement, nearestHostNode, nodeElement) =>
    switch (renderedElement) {
    | IFlat(e) =>
      let {Update.hostTreeUpdate, payload: opaqueInstance, enqueuedEffects} =
        f(e, nearestHostNode, nodeElement);
      let unchanged = e === opaqueInstance;

      {
        Update.hostTreeUpdate,
        payload: unchanged ? renderedElement : IFlat(opaqueInstance),
        enqueuedEffects,
      };
    | INested(l, _) =>
      let update =
        List.fold_left(
          (acc, renderedElement) => {
            let update =
              map(f, renderedElement, nearestHostNode, nodeElement);
            {
              ...update |> Update.map(next => [next, ...acc.Update.payload]),
              enqueuedEffects:
                EffectSequence.chain(
                  acc.enqueuedEffects,
                  update.enqueuedEffects,
                ),
            };
          },
          {
            Update.payload: [],
            hostTreeUpdate: {
              nearestHostNode,
              nodeElement,
              absoluteSubtreeIndex: 0,
            },
            enqueuedEffects: EffectSequence.noop,
          },
          List.rev(l),
        );
      let unchanged = List.for_all2((===), l, update.payload);

      update
      |> Update.map(nextL =>
           unchanged
             ? renderedElement
             : INested(
                 nextL,
                 List.fold_left(
                   (acc, elem) => InstanceForest.getSubtreeSize(elem) + acc,
                   0,
                   nextL,
                 ),
               )
         );
    };

  /**
      * Flush the pending updates in an instance tree.
      */
  let flushPendingUpdates =
      ({Update.payload: instanceForest, hostTreeUpdate, enqueuedEffects}) => {
    let update =
      map(
        Render.flushPendingUpdates,
        instanceForest,
        hostTreeUpdate.nearestHostNode,
        hostTreeUpdate.nodeElement,
      );
    {
      ...update,
      enqueuedEffects:
        EffectSequence.chain(update.enqueuedEffects, enqueuedEffects),
    };
  };

  let executeHostViewUpdates =
      ({Update.hostTreeUpdate: {nearestHostNode}}: t(_, _)) => {
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
