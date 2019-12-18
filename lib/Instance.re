open CoreTypes;

type opaque('node) = CoreTypes.opaqueInstance('node);
type forest('node) = CoreTypes.instanceForest('node);

type opaqueInstanceUpdate('node, 'childNode) =
  Update.t('node, 'childNode, opaque('childNode));

type renderedElement('node, 'childNode) =
  Update.t('node, 'childNode, instanceForest('childNode));

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
        Hooks.ofState(None, ~onStateDidChange=GlobalState.callStaleHandlers),
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
    Element.fold(~f=ofOpaqueComponent, ~init=hostTreeState, syntheticElement);

let pendingEffects =
    (~lifecycle, ~nextEffects, ~instance as {childInstances, hooks}) => {
  EffectSequence.chain(
    InstanceForest.pendingEffects(~lifecycle, childInstances)
    |> EffectSequence.chain(Hooks.pendingEffects(~lifecycle, Some(hooks))),
    nextEffects,
  );
};

let outputTreeNodes: type node. opaqueInstance(node) => lazyHostNodeSeq(node) =
  (Instance(instance)) => {
    switch (instance.component.childrenType) {
    | React => instance.wrappedHostNode
    | Host => Seq.((() => Cons(instance.wrappedHostNode, () => Nil)))
    };
  };
