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

type hostNode('a) =
  | Node('a)
  | UpdatedNode('a, 'a);

type lazyHostNode('a) = Lazy.t(hostNode('a));

type lazyHostNodeSeq('a) = Seq.t(lazyHostNode('a));

type componentId('a) = ..;

type subtreeSize = int;

type diffItem('left, 'right) =
  | Added('right)
  | Removed('left)
  | Updated('left, 'right);

type dynamicElement('node, 'typ) = {
  diff: 'a. dynamicElement('node, 'a) => Seq.t(diffItem('typ, 'a)),
  map: 'a. (~f: 'typ => 'a) => dynamicElement('node, 'a),
  toSeq: unit => Seq.t('typ),
  insert: 'typ => dynamicElement('node, 'typ),
  empty: 'any. unit => dynamicElement('node, 'any),
};

type element('node) =
  | Leaf(opaqueComponent('node))
  | StaticList(list(element('node)))
  | DiffableSequence(dynamicElement('node, element('node)))
  | Movable(element('node), ref(option(instanceForest('node))))
and component('a) = {
  debugName: string,
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
  | INested(list(instanceForest('node)), subtreeSize)
  | IDiffableSequence(
      dynamicElement('node, instanceForest('node)),
      subtreeSize,
    )
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
          ('node, element('node), 'node, lazyHostNodeSeq('node)),
        );

type opaqueInstanceHash('node) =
  Lazy.t(Hashtbl.t(int, (opaqueInstance('node), int)));

module Update = {
  type hostTreeState('node, 'childNode) = {
    nearestHostNode: lazyHostNode('node),
    nodeElement: hostNodeElement('node, 'childNode),
    /* This is a unique index of an element within a subtree,
      * thanks to tracking it we can efficiently manage moves of within a subtree
     */
    absoluteSubtreeIndex: int,
  };
  type t('node, 'childNode, 'payload) = {
    hostTreeUpdate: hostTreeState('node, 'childNode),
    enqueuedEffects: EffectSequence.t,
    payload: 'payload,
    childNodes: lazyHostNodeSeq('childNode),
  };
  type context('node, 'childNode) = {
    shouldExecutePendingUpdates: bool,
    useKeyTable: option(opaqueInstanceHash('childNode)),
    hostTreeState: hostTreeState('node, 'childNode),
  };
  let map = (f, update) => {
    let {payload, hostTreeUpdate, enqueuedEffects, childNodes} = update;
    {hostTreeUpdate, enqueuedEffects, payload: f(payload), childNodes};
  };
  let mapEffects = (f, update) => {
    let {payload, hostTreeUpdate, enqueuedEffects, childNodes} = update;
    {
      hostTreeUpdate,
      enqueuedEffects: f(enqueuedEffects),
      payload,
      childNodes,
    };
  };
};

type renderedElement('node, 'childNode) =
  Update.t('node, 'childNode, instanceForest('childNode));

type opaqueInstanceUpdate('node, 'childNode) =
  Update.t('node, 'childNode, opaqueInstance('childNode));

module InstanceForest = {
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
        | IDiffableSequence(l, _) =>
          Seq.fold_left(
            (acc, instanceForest) => fold(acc, instanceForest),
            acc,
            l.toSeq(),
          )
        };
      };
    fold(acc, instanceForest);
  };
};

module Element = {
  let rec fold = (~f, ~init, renderedElement) => {
    let foldSequence = (seq, container, add, wrapResult) => {
      let update =
        Seq.fold_left(
          (update: Update.t(_, _, _), element) => {
            // Append child nodes here
            fold(~f, ~init=update.Update.hostTreeUpdate, element)
            |> Update.map(payload => add(update.Update.payload, payload))
            |> Update.mapEffects(nextEffects =>
                 EffectSequence.chain(
                   update.Update.enqueuedEffects,
                   nextEffects,
                 )
               )
          },
          {
            Update.payload: container,
            enqueuedEffects: EffectSequence.noop,
            hostTreeUpdate: init,
            childNodes: Seq.empty,
          },
          seq,
        );
      update
      |> Update.map(payload =>
           wrapResult(
             payload,
             update.hostTreeUpdate.absoluteSubtreeIndex
             - init.absoluteSubtreeIndex,
           )
         );
    };
    switch (renderedElement) {
    | Leaf(c) =>
      f(~hostTreeState=init, ~component=c)
      |> Update.map(instance => IFlat(instance))
    | DiffableSequence(seq) =>
      foldSequence(
        seq.toSeq(),
        seq.empty(),
        (instances, instance) => instances.insert(instance),
        (seq, length) => IDiffableSequence(seq, length),
      )
    | StaticList(l) =>
      foldSequence(
        List.rev(l) |> List.to_seq,
        [],
        (rest, h) => [h, ...rest],
        (list, length) => INested(list, length),
      )

    | Movable(l, _) => fold(~f, ~init, l)
    };
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
      Seq.fold_left(
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
      Seq.fold_left(
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
          Seq.fold_left(
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
      (~nodeElement, ~parent, ~children, ~position as initialPosition) =>
    lazy({
      let parentWrapper = Lazy.force(parent);
      let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
      let newParent =
        Seq.fold_left(
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
          children,
        )
        |> snd;
      newParent === oldParent
        ? parentWrapper : UpdatedNode(oldParent, newParent);
    });
};

module Instance = {

  let rec ofComponent:
    type parentNode hooks node children childNode wrappedHostNode.
      (
        ~hostTreeState: Update.hostTreeState(parentNode, node),
        opaqueComponent(node),
        component((hooks, (node, children, childNode, wrappedHostNode)))
      ) =>
      opaqueInstanceUpdate(parentNode, node) =
    (~hostTreeState, opaqueComponent, component) => {
      let (children_, hooks) =
        component.render(
          Hooks.ofState(None, ~onStateDidChange=callStaleHanlders),
        );
      let hooks = Hooks.toState(hooks);
      let addMountEffects = u =>
        u
        |> Update.mapEffects(e =>
             EffectSequence.chain(
               Hooks.pendingEffects(
                 ~lifecycle=Hooks.Effect.Mount,
                 Some(hooks),
               ),
               e,
             )
           );
      switch (component.childrenType) {
      | React =>
        let update = ofList(~hostTreeState, children_: element(childNode));
        update
        |> addMountEffects
        |> Update.map(childInstances =>
             Instance({
               hooks,
               opaqueComponent,
               component,
               children_,
               childInstances,
               wrappedHostNode: update.childNodes,
             })
           );
      | Host =>
        let node =
          lazy(
            Node(
              children_.make()
              |> children_.configureInstance(~isFirstRender=true),
            )
          );
        let update =
          ofList(
            ~hostTreeState={
              nearestHostNode: node,
              nodeElement: children_,
              absoluteSubtreeIndex: 0,
            },
            children_.children,
          )
          |> addMountEffects
          |> Update.map(childInstances =>
               Instance({
                 hooks,
                 opaqueComponent,
                 component,
                 children_,
                 childInstances,
                 wrappedHostNode: node,
               })
             );
        let hostTreeUpdate = {
          ...hostTreeState,
          nearestHostNode:
            lazy(
              SubtreeChange.insertNodes(
                ~nodeElement=hostTreeState.nodeElement,
                ~parent=Lazy.force(hostTreeState.nearestHostNode),
                ~children=[node] |> List.to_seq,
                ~position=hostTreeState.absoluteSubtreeIndex,
              )
            ),
          absoluteSubtreeIndex: hostTreeState.absoluteSubtreeIndex + 1,
        };
        {
          hostTreeUpdate,
          enqueuedEffects: update.enqueuedEffects,
          payload: update.payload,
          childNodes: Seq.((() => Cons(node, () => Nil))),
        };
      };
    }

  and ofOpaqueComponent:
    type parentNode node.
      (
        ~hostTreeState: Update.hostTreeState(parentNode, node),
        ~component: opaqueComponent(node)
      ) =>
      opaqueInstanceUpdate(parentNode, node) =
    (
      ~hostTreeState,
      ~component as OpaqueComponent(component) as opaqueComponent,
    ) =>
      ofComponent(~hostTreeState, opaqueComponent, component)

  and ofList:
    type parentNode node.
      (
        ~hostTreeState: Update.hostTreeState(parentNode, node),
        element(node)
      ) =>
      renderedElement(parentNode, node) =
    (~hostTreeState, syntheticElement) =>
      Element.fold(
        ~f=ofOpaqueComponent,
        ~init=hostTreeState,
        syntheticElement,
      );

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

  let outputTreeNodes:
    type node. opaqueInstance(node) => lazyHostNodeSeq(node) =
    (Instance(instance)) => {
      switch (instance.component.childrenType) {
      | React => instance.wrappedHostNode
      | Host => Seq.((() => Cons(instance.wrappedHostNode, () => Nil)))
      };
    };
};

module Render = {
  let getOpaqueInstance = (~useKeyTable, OpaqueComponent(_)) =>
    switch (useKeyTable) {
    | None => None
    | Some(_keyTable) => None
    };

  type subtreeUpdate('node) =
    | MatchingSubtree(list(subtreeUpdate('node)))
    | Update(opaqueInstance('node), opaqueComponent('node))
    | ReRender(element('node))
    | UpdateSequence(
        dynamicElement('node, instanceForest('node)),
        dynamicElement('node, element('node)),
      );

  let rec prepareUpdate = (~oldInstanceForest, ~nextElement) => {
    switch (oldInstanceForest, nextElement) {
    | (IFlat(instance), Leaf(component)) => Update(instance, component)
    | (INested(instances, _), StaticList(elements))
        when List.length(instances) == List.length(elements) =>
      MatchingSubtree(
        List.map2(
          (inst, elem) =>
            prepareUpdate(~oldInstanceForest=inst, ~nextElement=elem),
          instances,
          elements,
        ),
      )
    | (IDiffableSequence(instances, _), DiffableSequence(elements)) =>
      UpdateSequence(instances, elements)
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
        Instance.ofOpaqueComponent(
          ~hostTreeState=updateContext.hostTreeState,
          ~component=opaqueComponent,
        )
      }

  and renderReactElement:
    type parentNode node.
      (~updateContext: Update.context(parentNode, node), element(node)) =>
      renderedElement(parentNode, node) =
    (~updateContext, element) =>
      Element.fold(
        ~f=
          (~hostTreeState, ~component) => {
            renderElement(
              ~updateContext={...updateContext, hostTreeState},
              component,
            )
          },
        ~init=updateContext.hostTreeState,
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
          hostTreeUpdate: updateContext.hostTreeState,
          payload: originalOpaqueInstance,
          enqueuedEffects: EffectSequence.noop,
          childNodes: Instance.outputTreeNodes(originalOpaqueInstance),
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
                    ~children=Instance.outputTreeNodes(newOpaqueInstance),
                    ~position=hostTreeUpdate.absoluteSubtreeIndex,
                  ),
                absoluteSubtreeIndex: hostTreeUpdate.absoluteSubtreeIndex,
              },
              payload: newOpaqueInstance,
              enqueuedEffects,
              childNodes: [] |> List.to_seq,
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
          let update =
            Instance.ofOpaqueComponent(
              ~hostTreeState=updateContext.hostTreeState,
              ~component=nextOpaqueComponent,
            )
            |> Update.mapEffects(mountEffects =>
                 Instance.pendingEffects(
                   ~lifecycle=Hooks.Effect.Unmount,
                   ~nextEffects=mountEffects,
                   ~instance,
                 )
               );
          let childNodes = Instance.outputTreeNodes(update.payload);
          let {Update.hostTreeState} = updateContext;
          {
            hostTreeUpdate: {
              nodeElement: hostTreeState.nodeElement,
              nearestHostNode:
                SubtreeChange.replaceSubtree(
                  ~nodeElement=hostTreeState.nodeElement,
                  ~parent=hostTreeState.nearestHostNode,
                  ~prevChildren=
                    Instance.outputTreeNodes(originalOpaqueInstance),
                  ~nextChildren=childNodes,
                  ~absoluteSubtreeIndex=
                    updateContext.hostTreeState.absoluteSubtreeIndex,
                ),
              absoluteSubtreeIndex: 0,
            },
            payload: update.payload,
            enqueuedEffects: update.enqueuedEffects,
            childNodes,
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
          let {Update.payload: nextInstanceSubForest, enqueuedEffects} =
            updateInstanceSubtree(
              ~updateContext,
              ~oldInstanceForest=childInstances,
              ~nextElement=nextSubElements,
              (),
            );
          nextInstanceSubForest !== childInstances
            ? (
              updateContext.hostTreeState.nearestHostNode,
              {
                ...updatedInstanceWithNewState,
                children_: nextSubElements,
                childInstances: nextInstanceSubForest,
              }:
                instance(
                  (hooks, (node, children, childNode, wrappedHostNode)),
                ),
              enqueuedEffects,
              updateContext.hostTreeState.nodeElement,
            )
            : (
              updateContext.hostTreeState.nearestHostNode,
              updatedInstanceWithNewState,
              enqueuedEffects,
              updateContext.hostTreeState.nodeElement,
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
                hostTreeState: {
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
              updateContext.hostTreeState.nearestHostNode,
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
              updateContext.hostTreeState.nodeElement,
            );
          } else {
            (
              updateContext.hostTreeState.nearestHostNode,
              instanceWithNewHostView,
              enqueuedEffects,
              updateContext.hostTreeState.nodeElement,
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
          childNodes: Seq.empty,
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
          childNodes: Seq.empty,
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
                    Update.hostTreeState: updateAcc.Update.hostTreeUpdate,
                    shouldExecutePendingUpdates:
                      updateContext.shouldExecutePendingUpdates,
                    useKeyTable: updateContext.useKeyTable,
                  },
                  subtreeUpdate,
                );
              // TODO: Don't forget about effects...
              update
              |> Update.map(updatedInstanceForest =>
                   [updatedInstanceForest, ...updateAcc.payload]
                 );
            },
            {
              Update.payload: [],
              hostTreeUpdate: updateContext.hostTreeState,
              enqueuedEffects: EffectSequence.noop,
              childNodes: Seq.empty,
            },
            List.rev(updates),
          );
        update
        |> Update.map(instances =>
             INested(
               instances,
               update.Update.hostTreeUpdate.absoluteSubtreeIndex
               - updateContext.hostTreeState.absoluteSubtreeIndex,
             )
           );
      | Update(instance, newComponent) =>
        updateOpaqueInstance(~updateContext, instance, newComponent)
        |> Update.map(instance => IFlat(instance))
      | UpdateSequence(instances, elements) =>
        instances.diff(elements)
        |> Seq.fold_left(
             (acc, change) =>
               switch (change) {
               | Updated(oldInstanceForest, nextElement) =>
                 applyUpdate(
                   ~updateContext={
                     Update.hostTreeState: acc.Update.hostTreeUpdate,
                     shouldExecutePendingUpdates:
                       updateContext.shouldExecutePendingUpdates,
                     useKeyTable: updateContext.useKeyTable,
                   },
                   prepareUpdate(~oldInstanceForest, ~nextElement),
                 )
                 |> Update.map(instanceForest =>
                      acc.payload.insert(instanceForest)
                    )
               | Added(element) =>
                 renderReactElement(~updateContext, element)
                 |> Update.map(instanceForest =>
                      acc.payload.insert(instanceForest)
                    )
               | Removed(_instance) => acc
               },
             {
               Update.payload: instances,
               hostTreeUpdate: updateContext.hostTreeState,
               enqueuedEffects: EffectSequence.noop,
               childNodes: Seq.empty,
             },
           )
        |> Update.map(instances => IDiffableSequence(instances, 0))
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
          hostTreeState: {
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

  let render = (root, children) => {
    let hostTreeState = {
      Update.nodeElement: {
        make: () => root.node,
        configureInstance: (~isFirstRender as _, i) => i,
        children,
        insertNode: root.insertNode,
        deleteNode: root.deleteNode,
        moveNode: root.moveNode,
      },
      nearestHostNode: lazy(Node(root.node)),
      absoluteSubtreeIndex: 0,
    };
    Instance.ofList(~hostTreeState, children);
  };
  let update =
      (~renderedElement as {Update.payload, hostTreeUpdate}, nextElement) =>
    Render.updateInstanceSubtree(
      ~updateContext=
        Update.{
          hostTreeState: hostTreeUpdate,
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
        childNodes: Seq.empty,
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
            childNodes: Seq.empty,
          },
          List.rev(l),
        );
      let unchanged = List.for_all2((===), l, update.payload);

      update
      |> Update.map(nextL =>
           unchanged
             ? renderedElement
             : INested(nextL, update.hostTreeUpdate.absoluteSubtreeIndex)
         );
    | IDiffableSequence(instances, _l) =>
      instances.toSeq()
      |> Seq.fold_left(
           (acc, _instance) => {acc},
           {
             Update.payload: instances,
             hostTreeUpdate: {
               nearestHostNode,
               nodeElement,
               absoluteSubtreeIndex: 0,
             },
             enqueuedEffects: EffectSequence.noop,
             childNodes: Seq.empty,
           },
         )
      |> Update.map(instances => IDiffableSequence(instances, 0))
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

let element = component => {
  Leaf(OpaqueComponent(component));
};

let listToElement = l => StaticList(l);
let empty = StaticList([]);

module Hooks = Hooks;
module RemoteAction = RemoteAction;

module Expert = {
  let jsx_list = l => StaticList(l);
  let component:
    type a node.
      (string, Hooks.t(a, a) => (element(node), Hooks.t(Hooks.nil, a))) =>
      element(node) =
    debugName => {
      module Component = {
        type componentId('a) +=
          | Id: componentId(
                  instance(
                    (
                      a,
                      (node, element(node), node, lazyHostNodeSeq(node)),
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
                  (a, (node, element(node), node, lazyHostNodeSeq(node))),
                ),
              )
            ) =>
            option(
              instance(
                (a, (node, element(node), node, lazyHostNodeSeq(node))),
              ),
            ) =
          (instance, id1, id2) => {
            switch (id1, id2) {
            | (Id, Id) => Some(instance)
            | (_, _) => None
            };
          };
      };
      render =>
        element({
          debugName,
          childrenType: React,
          id: Component.Id,
          eq: Component.eq,
          render,
        });
    };

  let nativeComponent:
    type a node childNode.
      (
        string,
        Hooks.t(a, a) =>
        (hostNodeElement(node, childNode), Hooks.t(Hooks.nil, a))
      ) =>
      element(node) =
    debugName => {
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
      render =>
        element({
          debugName,
          childrenType: Host,
          id: Component.Id,
          eq: Component.eq,
          render,
        });
    };
};

type movableStateContainerState('node) = ref(option(instanceForest('node)));

let movableStateContainer = (~children, (), hooks) => {
  let (instanceRef, hooks) = Hooks.ref(None, hooks);
  (() => Movable(children, instanceRef), hooks);
};
