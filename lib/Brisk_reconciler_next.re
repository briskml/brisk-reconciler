module type OutputTree = {
  type node;

  let markAsStale: unit => unit;

  let insertNode: (~parent: node, ~child: node, ~position: int) => node;
  let deleteNode: (~parent: node, ~child: node, ~position: int) => node;
  let moveNode: (~parent: node, ~child: node, ~from: int, ~to_: int) => node;
};

module EffectSequence: {
  type t;

  let noop: t;
  let flatten: list(t) => t;
  let chain: (t, t) => t;
} = {
  type t = unit => unit;

  let noop = () => ();

  let flatten = (l, ()) => List.iter(f => f(), l);

  let chain = (f', f, ()) => {
    f'();
    f();
  };
};

module ElementGroup: {
  type t('a);
  type key;
  /* fold two element groups in ascending order */
  let fold2:
    (('acc, key, option('a), option('b)) => 'acc, 'acc, t('a), t('b)) =>
    'acc;
} = {};

module Make = (OutputTree: OutputTree) => {
  module GlobalState = {
    let debug = ref(true);
    let setDebug = value => {
      debug := value;
    };
  };

  type internalOutputNode =
    | Node(OutputTree.node)
    | UpdatedNode(OutputTree.node, OutputTree.node);
  type outputNodeContainer = Lazy.t(internalOutputNode);
  type outputNodeGroup = Seq.t(outputNodeContainer);
  type id('a) = ..;
  type instance('hooks, 'initialHooks, 'elementType, 'outputNode) = {
    hooks: Hooks.state('hooks, unit),
    component: component('hooks, 'initialHooks, 'elementType, 'outputNode),
    opaqueComponent,
    instanceSubForest: instanceForest,
    subElements: 'elementType,
    hostInstance: 'outputNode,
  }
  [@unboxed]
  and opaqueComponent =
    | OpaqueComponent(
        component('hooks, 'initialHooks, 'elementType, 'outputNode),
      )
      : opaqueComponent
  and element =
    | Component(opaqueComponent)
    | StaticFragment(list(element))
    | DynamicKeyedFragment(ElementGroup.t(element))
  and outputTreeElement = {
    make: unit => OutputTree.node,
    configureInstance:
      (~isFirstRender: bool, OutputTree.node) => OutputTree.node,
    children: element,
  }
  and elementType('elementType, 'outputNode) =
    | Host: elementType(outputTreeElement, outputNodeContainer)
    | React: elementType(element, outputNodeGroup)
  and instanceForest =
    | IComponent(opaqueInstance)
    | IStaticFragment(list(instanceForest), element)
    | IDynamicKeyedFragment(
        ElementGroup.t(instanceForest),
        element,
        int /*number of host instances in the whole instance forest*/,
      )
  and component('hooks, 'initialHooks, 'elementType, 'outputNode) = {
    debugName: string,
    elementType: elementType('elementType, 'outputNode),
    id: id(instance('hooks, 'initialHooks, 'elementType, 'outputNode)),
    eq:
      'a.
      (
        'a,
        id('a),
        id(instance('hooks, 'initialHooks, 'elementType, 'outputNode))
      ) =>
      option(instance('hooks, 'initialHooks, 'elementType, 'outputNode)),

    render:
      Hooks.t('hooks, unit, 'initialHooks, 'initialHooks) =>
      (Hooks.t(unit, unit, 'hooks, unit), 'elementType),
  }
  [@unboxed]
  and opaqueInstance =
    | Instance(instance('hooks, 'initialHooks, 'elementType, 'outputNode))
      : opaqueInstance;

  type update('a) = {
    updated: 'a,
    enqueuedEffects: EffectSequence.t,
    nearestHostOutputNode: outputNodeContainer,
  };

  type opaqueInstanceUpdate = update(opaqueInstance);
  type renderedElement = update(instanceForest);

  type instanceWithEffects = {
    opaqueInstance,
    enqueuedEffects: EffectSequence.t,
  };

  module FoldMap = {
    let rec list2 = (f, acc, l1, l2) =>
      switch (l1, l2) {
      | ([], []) => (acc, [])
      | ([h1, ...t1], [h2, ...t2]) =>
        let (acc, t) = list2(f, acc, t1, t2);
        let (nextAcc, h) = f(acc, h1, h2);
        (nextAcc, [h, ...t]);
      | _ =>
        raise(
          Invalid_argument("FoldMap.list2: Lists have different lengths"),
        )
      };
  };

  module Instance: {
    type t('a, 'b, 'c, 'd) = instance('a, 'b, 'c, 'd);
    let pendingEffects:
      (
        ~lifecycle: Hooks.Effect.lifecycle,
        ~instance: instance('a, 'b, 'c, 'd)
      ) =>
      EffectSequence.t;
  } = {};

  module OpaqueInstance: {
    type t = opaqueInstance;
    let ofComponent: opaqueComponent => t;
    let pendingEffects:
      (~lifecycle: Hooks.Effect.lifecycle, ~opaqueInstance: opaqueInstance) =>
      EffectSequence.t;
    let outputTreeNodes: opaqueInstance => Seq.t(outputNodeContainer);
  } = {};

  module SubtreeChange: {
    let insertNodes:
      (
        ~parent: Lazy.t(internalOutputNode),
        ~children: Seq.t(Lazy.t(internalOutputNode)),
        ~position: int
      ) =>
      Lazy.t(internalOutputNode);
    let deleteNodes:
      (
        ~parent: Lazy.t(internalOutputNode),
        ~children: Seq.t(Lazy.t(internalOutputNode)),
        ~position: int
      ) =>
      Lazy.t(internalOutputNode);
    let replaceSubtree:
      (
        ~parent: Lazy.t(internalOutputNode),
        ~prevChildren: Seq.t(Lazy.t(internalOutputNode)),
        ~nextChildren: Seq.t(Lazy.t(internalOutputNode)),
        ~position: int
      ) =>
      Lazy.t(internalOutputNode);
    let updateNode:
      (
        ~parent: Lazy.t(internalOutputNode),
        ~opaqueInstance: opaqueInstance,
        ~position: int
      ) =>
      Lazy.t(internalOutputNode);
  } = {};

  module InstanceForest: {
    type t = instanceForest;
    let outputTreeNodes: t => Seq.t(outputNodeContainer);
    let compareTypeToElement: (t, element) => bool;
    let pendingEffects:
      (~lifecycle: Hooks.Effect.lifecycle, ~instanceForest: t) =>
      EffectSequence.t;
  } = {};

  module Element: {
    type newInstanceForest = {
      instanceForest,
      mountEffects: EffectSequence.t,
    };
    let toInstanceForest: element => newInstanceForest;
  } = {};

  module UpdateContext = {
    type t = {
      shouldExecutePendingUpdates: bool,
      /* This is a unique index of an element within a subtree,
        * thanks to tracking it we can efficiently manage moves of within a subtree
       */
      nearestHostOutputNode: outputNodeContainer,
      absoluteSubtreeIndex: int,
    };
  };

  module DynamicFragmentUpdateContext: {
    type t = {
      updateContext: UpdateContext.t,
      instanceGroup: ElementGroup.t(instanceForest),
    };

    let toRenderedElement: t => renderedElement;
  } = {};

  module DynamicFragment: {
    let insertElement:
      (
        ~updateContext: DynamicFragmentUpdateContext.t,
        ~element: element,
        ~key: ElementGroup.key
      ) =>
      DynamicFragmentUpdateContext.t;

    let updateInstanceForest:
      (
        ~updateContext: DynamicFragmentUpdateContext.t,
        ~key: ElementGroup.key,
        ~instanceForest: instanceForest,
        ~updatedElement: element
      ) =>
      DynamicFragmentUpdateContext.t;

    let removeInstanceForest:
      (
        ~updateContext: DynamicFragmentUpdateContext.t,
        ~instanceForest: instanceForest,
        ~key: ElementGroup.key
      ) =>
      DynamicFragmentUpdateContext.t;
  } = {};

  let shouldUpdateStaticFragment =
    List.for_all2(InstanceForest.compareTypeToElement);

  let replaceInstanceForest =
      (~updateContext, ~oldInstanceForest, ~nextElement) => {
    let {Element.instanceForest: newInstanceForest, mountEffects} =
      Element.toInstanceForest(nextElement);
    let enqueuedEffects =
      EffectSequence.chain(
        InstanceForest.pendingEffects(
          ~lifecycle=Hooks.Effect.Unmount,
          ~instanceForest=oldInstanceForest,
        ),
        mountEffects,
      );
    {
      nearestHostOutputNode:
        SubtreeChange.replaceSubtree(
          ~parent=updateContext.UpdateContext.nearestHostOutputNode,
          ~prevChildren=InstanceForest.outputTreeNodes(oldInstanceForest),
          ~nextChildren=InstanceForest.outputTreeNodes(newInstanceForest),
          ~position=updateContext.absoluteSubtreeIndex,
        ),
      updated: newInstanceForest,
      enqueuedEffects,
    };
  };

  let rec updateOpaqueInstance =
          (
            ~updateContext: UpdateContext.t,
            Instance(instance) as originalOpaqueInstance,
            OpaqueComponent(nextComponent) as nextOpaqueComponent,
          )
          : opaqueInstanceUpdate => {
    let nextState =
      updateContext.shouldExecutePendingUpdates
        ? Hooks.flushPendingStateUpdates(instance.hooks) : instance.hooks;
    let stateChanged = nextState !== instance.hooks;

    let bailOut =
      !stateChanged && instance.opaqueComponent === nextOpaqueComponent;

    if (bailOut && !updateContext.shouldExecutePendingUpdates) {
      {
        nearestHostOutputNode: updateContext.nearestHostOutputNode,
        updated: originalOpaqueInstance,
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
              nearestHostOutputNode,
              updated: newOpaqueInstance,
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
            nearestHostOutputNode:
              SubtreeChange.updateNode(
                ~parent=nearestHostOutputNode,
                ~opaqueInstance=newOpaqueInstance,
                ~position=updateContext.absoluteSubtreeIndex,
              ),
            updated: newOpaqueInstance,
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
        let opaqueInstance = OpaqueInstance.ofComponent(nextOpaqueComponent);
        let enqueuedEffects =
          EffectSequence.chain(
            Instance.pendingEffects(
              ~lifecycle=Hooks.Effect.Unmount,
              ~instance,
            ),
            OpaqueInstance.pendingEffects(
              ~lifecycle=Hooks.Effect.Mount,
              ~opaqueInstance,
            ),
          );
        {
          nearestHostOutputNode:
            SubtreeChange.replaceSubtree(
              ~parent=updateContext.nearestHostOutputNode,
              ~prevChildren=
                OpaqueInstance.outputTreeNodes(originalOpaqueInstance),
              ~nextChildren=OpaqueInstance.outputTreeNodes(opaqueInstance),
              ~position=updateContext.absoluteSubtreeIndex,
            ),
          updated: opaqueInstance,
          enqueuedEffects,
        };
      };
    };
  }
  and updateInstance:
    type hooks initialHooks elementType outputNodeType.
      (
        ~originalOpaqueInstance: opaqueInstance,
        ~updateContext: UpdateContext.t,
        ~nextComponent: component(
                          hooks,
                          initialHooks,
                          elementType,
                          outputNodeType,
                        ),
        ~nextOpaqueComponent: opaqueComponent,
        ~stateChanged: bool,
        instance(hooks, initialHooks, elementType, outputNodeType)
      ) =>
      opaqueInstanceUpdate =
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

      let (initialHooks, nextSubElements) =
        if (shouldRerender) {
          let (initialHooks, nextOpaqueComponent) =
            nextComponent.render(
              Hooks.ofState(
                updatedInstanceWithNewElement.hooks,
                ~onStateDidChange=OutputTree.markAsStale,
              ),
            );
          (Hooks.toState(initialHooks), nextOpaqueComponent);
        } else {
          (instance.hooks, instance.subElements);
        };

      let updatedInstanceWithNewState = {
        ...updatedInstanceWithNewElement,
        hooks: initialHooks,
      };

      let {subElements, instanceSubForest} = updatedInstanceWithNewState;
      let (
        nearestHostOutputNode,
        updatedInstanceWithNewSubtree,
        enqueuedEffects,
      ) =
        switch (nextComponent.elementType) {
        | React =>
          let {
            nearestHostOutputNode,
            updated: nextInstanceSubForest,
            enqueuedEffects,
          } =
            updateInstanceForest(
              ~updateContext,
              ~oldInstanceForest=instanceSubForest,
              ~nextElement=nextSubElements: element,
              (),
            );
          nextInstanceSubForest !== instanceSubForest
            ? (
              nearestHostOutputNode,
              {
                ...updatedInstanceWithNewState,
                subElements: nextSubElements,
                instanceSubForest: nextInstanceSubForest,
              },
              enqueuedEffects,
            )
            : (
              nearestHostOutputNode,
              updatedInstanceWithNewState,
              enqueuedEffects,
            );
        | Host =>
          let instanceWithNewHostView =
            shouldRerender
              ? {
                ...updatedInstanceWithNewState,
                hostInstance:
                  lazy {
                    let instance =
                      Lazy.force(updatedInstanceWithNewState.hostInstance);
                    let Node(beforeUpdate) | UpdatedNode(_, beforeUpdate) = instance;
                    let afterUpdate =
                      nextSubElements.configureInstance(
                        ~isFirstRender=false,
                        beforeUpdate,
                      );
                    afterUpdate === beforeUpdate
                      ? instance : UpdatedNode(beforeUpdate, afterUpdate);
                  },
              }
              : updatedInstanceWithNewState;

          let {
            nearestHostOutputNode: hostInstance,
            updated: nextInstanceSubForest,
            enqueuedEffects,
          } =
            updateInstanceForest(
              ~updateContext={
                ...updateContext,
                absoluteSubtreeIndex: 0,
                nearestHostOutputNode: (
                  instanceWithNewHostView.hostInstance: outputNodeContainer
                ),
              },
              ~oldInstanceForest=instanceSubForest,
              ~nextElement=nextSubElements.children,
              (),
            );
          if (nextInstanceSubForest
              !== instanceWithNewHostView.instanceSubForest) {
            (
              updateContext.nearestHostOutputNode,
              {
                ...instanceWithNewHostView,
                instanceSubForest: nextInstanceSubForest,
                subElements: nextSubElements,
                hostInstance,
              }:
                instance(hooks, initialHooks, elementType, outputNodeType),
              enqueuedEffects,
            );
          } else {
            (
              updateContext.nearestHostOutputNode,
              instanceWithNewHostView,
              enqueuedEffects,
            );
          };
        };
      if (updatedInstanceWithNewSubtree === updatedInstanceWithNewElement
          && !stateChanged) {
        {
          nearestHostOutputNode,
          updated: originalOpaqueInstance,
          enqueuedEffects,
        };
      } else {
        {
          nearestHostOutputNode,
          updated: Instance(updatedInstanceWithNewSubtree),
          enqueuedEffects:
            EffectSequence.chain(
              enqueuedEffects,
              Instance.pendingEffects(
                ~lifecycle=Hooks.Effect.Update,
                ~instance,
              ),
            ),
        };
      };
    }

  and updateInstanceForest =
      (
        ~updateContext,
        ~oldInstanceForest: instanceForest,
        ~nextElement: element,
        (),
      )
      : renderedElement =>
    switch (oldInstanceForest, nextElement) {
    | (IComponent(componentInstance), Component(nextOpaqueComponent)) =>
      let {nearestHostOutputNode, updated, enqueuedEffects} =
        updateOpaqueInstance(
          ~updateContext,
          componentInstance,
          nextOpaqueComponent,
        );
      {nearestHostOutputNode, updated: IComponent(updated), enqueuedEffects};
    | (
        IStaticFragment(
          oldOpaqueInstanceList,
          StaticFragment(oldElementList),
        ),
        StaticFragment(nextElementList),
      )
        when
          shouldUpdateStaticFragment(oldOpaqueInstanceList, nextElementList) =>
      let {nearestHostOutputNode, updated, enqueuedEffects} =
        updateStaticFragment(
          ~updateContext,
          ~oldOpaqueInstanceList,
          ~nextElementList,
        );
      {
        nearestHostOutputNode,
        updated: IStaticFragment(updated, nextElement),
        enqueuedEffects,
      };
    | (
        IDynamicKeyedFragment(
          instanceForestMap,
          DynamicKeyedFragment(prevElementMap),
          _subtreeSize,
        ),
        DynamicKeyedFragment(nextOpaqueComponentMap),
      ) =>
      updateDynamicKeyedFragment(
        ~updateContext,
        instanceForestMap,
        prevElementMap,
        nextOpaqueComponentMap,
      )
    | (_, _) =>
      replaceInstanceForest(~updateContext, ~oldInstanceForest, ~nextElement)
    }
  and updateDynamicKeyedFragment:
    (
      ~updateContext: UpdateContext.t,
      ElementGroup.t(instanceForest),
      ElementGroup.t(element),
      ElementGroup.t(element)
    ) =>
    renderedElement =
    (
      ~updateContext as fragmentUpdateContext,
      initialInstanceGroup,
      prevElementGroup,
      nextOpaqueComponentGroup,
    ) => {
      ElementGroup.fold2(
        (updateContext, key, instance, nextOpaqueComponent) =>
          switch (instance, nextOpaqueComponent) {
          | (None, Some(element)) =>
            DynamicFragment.insertElement(~key, ~updateContext, ~element)
          | (Some(instanceForest), Some(updatedElement)) =>
            DynamicFragment.updateInstanceForest(
              ~updateContext,
              ~key,
              ~instanceForest,
              ~updatedElement,
            )
          | (Some(instanceForest), None) =>
            DynamicFragment.removeInstanceForest(
              ~updateContext,
              ~key,
              ~instanceForest,
            )
          | (None, None) => /* Unreachable */ updateContext
          },
        DynamicFragmentUpdateContext.{
          updateContext: fragmentUpdateContext,
          instanceGroup: initialInstanceGroup,
        },
        initialInstanceGroup,
        nextOpaqueComponentGroup,
      )
      |> DynamicFragmentUpdateContext.toRenderedElement;
    }
  and updateStaticFragment =
      (~updateContext, ~oldOpaqueInstanceList, ~nextElementList) => {
    let (updateContext, updates) =
      FoldMap.list2(
        (updateContext, oldInstanceForest, nextElement) => {
          let {nearestHostOutputNode, enqueuedEffects, updated} =
            updateInstanceForest(
              ~updateContext,
              ~oldInstanceForest,
              ~nextElement,
              (),
            );
          (
            {...updateContext, nearestHostOutputNode},
            (updated, enqueuedEffects),
          );
        },
        updateContext,
        oldOpaqueInstanceList,
        nextElementList,
      );
    {
      nearestHostOutputNode: updateContext.nearestHostOutputNode,
      updated: List.map(fst, updates),
      enqueuedEffects: EffectSequence.flatten(List.map(snd, updates)),
    };
  };
};
