open CoreTypes;

module MockImplementationValues = {
  let zero = 0;
  let calculateIndex = (~oldIndex) => oldIndex;
  let noChildren = Seq.empty;
};

type subtreeUpdate('node) =
  | MatchingSubtree(list(subtreeUpdate('node)))
  | Update(opaqueInstance('node), opaqueLeafElement('node))
  | ReplaceOrMove({
      prevForest: instanceForest('node),
      nextElement: element('node),
    })
  | UpdateSequence(
      dynamicElement('node, instanceForest('node)),
      dynamicElement('node, element('node)),
    )
  | UpdateMovable(
      subtreeUpdate('node),
      ref(option(movableElementState('node))),
    );

type context('node, 'childNode) = {
  shouldExecutePendingUpdates: bool,
  hostTreeState: Update.hostTreeState('node, 'childNode),
  calculateIndex: (~oldIndex: int) => int,
};

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
  | (
      prevForest,
      Movable(element, {contents: Some({instanceForest})} as ref),
    ) =>
    if (instanceForest === prevForest) {
      UpdateMovable(
        prepareUpdate(~oldInstanceForest, ~nextElement=element),
        ref,
      );
    } else {
      ReplaceOrMove({prevForest, nextElement});
    }
  | (prevForest, nextElement) => ReplaceOrMove({prevForest, nextElement})
  };
};

/**
      * Initial render of an Element. Recurses to produce the entire tree of
      * instances.
      */
let renderElement:
  type parentNode node.
    (~updateContext: context(parentNode, node), opaqueLeafElement(node)) =>
    Instance.opaqueInstanceUpdate(parentNode, node) =
  (~updateContext, opaqueLeafElement) =>
    Instance.ofOpaqueLeafElement(
      ~hostTreeState=updateContext.hostTreeState,
      ~component=opaqueLeafElement,
    );

let renderReactElement:
  type parentNode node.
    (~updateContext: context(parentNode, node), element(node)) =>
    Instance.renderedElement(parentNode, node) =
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
    );

let replaceInstanceForest = (~updateContext, ~oldInstanceForest, ~nextElement) => {
  let {Update.nodeElement, nearestHostNode, absoluteSubtreeIndex} =
    updateContext.hostTreeState;
  memoizeTheIndexShiftForOldInstanceForestSomewhereSoThatWeCanGoBackToItIfItWasAMovableAndMoveIt(
    renderReactElement(
      ~updateContext={
        ...updateContext,
        hostTreeState: {
          nodeElement,
          nearestHostNode /* lazy(   SubtreeChange.deleteNodes(     ~nodeElement,     ~parent=Lazy.force(nearestHostNode),     ~children=Instance.Forest.childNodes(oldInstanceForest),     ~position=absoluteSubtreeIndex,   ) ) */,

          absoluteSubtreeIndex: MockImplementationValues.zero,
        },
      },
      nextElement,
    ),
  )
  |> Update.mapEffects(mountEffects =>
       EffectSequence.chain(
         Instance.Forest.pendingEffects(
           ~lifecycle=Hooks.Effect.Unmount,
           oldInstanceForest,
         ),
         mountEffects,
       )
     );
};


let rec updateOpaqueInstance:
  type node parentNode.
    (
      ~updateContext: context(parentNode, node),
      opaqueInstance(node),
      opaqueLeafElement(node)
    ) =>
    Instance.opaqueInstanceUpdate(parentNode, node) =
  (
    ~updateContext,
    Instance(instance) as originalOpaqueInstance,
    OpaqueLeafElement(nextLeafElement) as nextOpaqueLeafElement,
  ) => {
    let nextState =
      updateContext.shouldExecutePendingUpdates
        ? Hooks.flushPendingStateUpdates(instance.hooks) : instance.hooks;
    let stateChanged = nextState !== instance.hooks;

    let bailOut =
      !stateChanged && instance.opaqueLeafElement === nextOpaqueLeafElement;

    if (bailOut && !updateContext.shouldExecutePendingUpdates) {
      {
        hostTreeUpdate: updateContext.hostTreeState,
        payload: originalOpaqueInstance,
        enqueuedEffects: EffectSequence.noop,
        childNodes: Instance.outputTreeNodes(originalOpaqueInstance),
      };
    } else {
      let {leafElement} = instance;
      switch (
        nextLeafElement.eq(
          {...instance, hooks: nextState},
          leafElement.id,
          nextLeafElement.id,
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
            ~nextLeafElement,
            ~nextOpaqueLeafElement,
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
          Instance.ofOpaqueLeafElement(
            ~hostTreeState=updateContext.hostTreeState,
            ~component=nextOpaqueLeafElement,
          )
          |> Update.mapEffects(mountEffects =>
               Instance.pendingEffects(
                 ~lifecycle=Hooks.Effect.Unmount,
                 ~nextEffects=mountEffects,
                 ~instance,
               )
             );
        let childNodes = Instance.outputTreeNodes(update.payload);
        let {hostTreeState} = updateContext;
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
            absoluteSubtreeIndex: MockImplementationValues.zero,
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
      ~updateContext: context(parentNode, node),
      ~nextLeafElement: leafElement(
                          (
                            hooks,
                            (node, children, childNode, wrappedHostNode),
                          ),
                        ),
      ~nextOpaqueLeafElement: opaqueLeafElement(node),
      ~stateChanged: bool,
      instance((hooks, (node, children, childNode, wrappedHostNode)))
    ) =>
    Instance.opaqueInstanceUpdate(parentNode, node) =
  (
    ~originalOpaqueInstance,
    ~updateContext,
    ~nextLeafElement,
    ~nextOpaqueLeafElement,
    ~stateChanged,
    instance,
  ) => {
    let updatedInstanceWithNewElement = {
      ...instance,
      leafElement: nextLeafElement,
      opaqueLeafElement: nextOpaqueLeafElement,
    };

    let shouldRerender =
      stateChanged || nextOpaqueLeafElement !== instance.opaqueLeafElement;

    let (nextSubElements, initialHooks) =
      if (shouldRerender) {
        let (nextElement, initialHooks) =
          nextLeafElement.render(
            Hooks.ofState(
              Some(updatedInstanceWithNewElement.hooks),
              ~onStateDidChange=GlobalState.callStaleHandlers,
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
      switch (nextLeafElement.childrenType) {
      | React =>
        let {Update.payload: nextInstanceSubForest, enqueuedEffects} =
          reconcile(
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
          reconcile(
            ~updateContext={
              calculateIndex: MockImplementationValues.calculateIndex,
              shouldExecutePendingUpdates:
                updateContext.shouldExecutePendingUpdates,
              hostTreeState: {
                absoluteSubtreeIndex: 0,
                nearestHostNode: (
                  instanceWithNewHostView.wrappedHostNode: lazyHostNode(node)
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
    let hostTreeUpdate = {
      Update.nodeElement,
      nearestHostNode,
      absoluteSubtreeIndex: 0,
    };
    if (updatedInstanceWithNewSubtree === updatedInstanceWithNewElement
        && !stateChanged) {
      {
        hostTreeUpdate,
        payload: originalOpaqueInstance,
        enqueuedEffects,
        childNodes: MockImplementationValues.noChildren,
      };
    } else {
      {
        hostTreeUpdate,
        payload: Instance(updatedInstanceWithNewSubtree),
        enqueuedEffects:
          EffectSequence.chain(
            Hooks.pendingEffects(
              ~lifecycle=Hooks.Effect.Update,
              Some(updatedInstanceWithNewSubtree.hooks),
            ),
            enqueuedEffects,
          ),
        childNodes: MockImplementationValues.noChildren,
      };
    };
  }

and applyUpdate:
  type node childNode.
    (~updateContext: context(node, childNode), subtreeUpdate(childNode)) =>
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
                  calculateIndex: MockImplementationValues.calculateIndex,
                  hostTreeState: updateAcc.Update.hostTreeUpdate,
                  shouldExecutePendingUpdates:
                    updateContext.shouldExecutePendingUpdates,
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
                   calculateIndex: MockImplementationValues.calculateIndex,
                   hostTreeState: acc.Update.hostTreeUpdate,
                   shouldExecutePendingUpdates:
                     updateContext.shouldExecutePendingUpdates,
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
      |> Update.map(instances =>
           IDiffableSequence(instances, MockImplementationValues.zero)
         )
    | UpdateMovable(update, ref) =>
      let updated = applyUpdate(~updateContext, update);
      ref := Some(updated.payload);
      updated;
    | ReplaceOrMove({prevForest, nextElement}) =>
      /* At this point we know the index in the tree */
      /* If we've already gone through the element we wanted to move it might already be unmounted */
      /* We should unmount all elements at the end */
      /* Index of nextElement if it's movable for moves */
      /* Procedure on every element:
          1. Memoize prevForest in toRemove field (don't remove them yet)
          2. Mount nextElement
            - If it's movable on a bigger index - move it from there (take current shift into account)
            - If it's moveable on a smaller index - move it from there and remove from toRemove (adjust current shift)
            - Else simply render and adjust current shift
         */
      replaceInstanceForest(
        ~updateContext,
        ~oldInstanceForest=prevForest,
        ~nextElement,
      )
    };
  }

and reconcile:
  type parentNode node.
    (
      ~updateContext: context(parentNode, node),
      ~oldInstanceForest: instanceForest(node),
      ~nextElement: element(node),
      unit
    ) =>
    Instance.renderedElement(parentNode, node) =
  (~updateContext, ~oldInstanceForest, ~nextElement, ()) => {
    /* Call cleanup which will remove all elements from toRemove */
    applyUpdate(
      ~updateContext,
      prepareUpdate(~oldInstanceForest, ~nextElement),
    );
  };

let reconcile =
    (
      ~shouldExecutePendingUpdates,
      ~hostTreeState,
      ~oldInstanceForest,
      ~nextElement,
      unit,
    ) =>
  reconcile(
    ~updateContext={
      shouldExecutePendingUpdates,
      hostTreeState,
      calculateIndex: MockImplementationValues.calculateIndex,
    },
    ~oldInstanceForest,
    ~nextElement,
    (),
  );

let flushPendingUpdates =
    (~parentHostNode, ~parentHostNodeElement, ~rootInstance) => {
  let Instance({opaqueLeafElement}) = rootInstance;
  updateOpaqueInstance(
    ~updateContext={
      calculateIndex: MockImplementationValues.calculateIndex,
      shouldExecutePendingUpdates: true,
      hostTreeState: {
        nearestHostNode: parentHostNode,
        nodeElement: parentHostNodeElement,
        absoluteSubtreeIndex: 0,
      },
    },
    rootInstance,
    opaqueLeafElement,
  );
};
