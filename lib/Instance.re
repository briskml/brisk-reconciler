open CoreTypes;

let pendingEffectsInForest = (~lifecycle, instanceForest) => {
  let f = (acc, CoreTypes.Instance({hooks})) =>
    EffectSequence.chain(
      Hooks.pendingEffects(~lifecycle, Some(hooks)),
      acc,
    );
  let rec fold:
    type any.
      (EffectSequence.t, CoreTypes.instanceForest(any)) => EffectSequence.t =
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
  fold(EffectSequence.noop, instanceForest);
};

type opaque('node) = CoreTypes.opaqueInstance('node);

type opaqueInstanceUpdate('node, 'childNode) =
  Update.t('node, 'childNode, opaque('childNode));

type renderedElement('node, 'childNode) =
  Update.t('node, 'childNode, instanceForest('childNode));


let rec ofComponent:
  type parentNode hooks node children childNode wrappedHostNode.
    (
      ~hostTreeState: Update.hostTreeState(parentNode, node),
      opaqueLeafElement(node),
      leafElement((hooks, (node, children, childNode, wrappedHostNode)))
    ) =>
    opaqueInstanceUpdate(parentNode, node) =
  (~hostTreeState, opaqueLeafElement, leafElement) => {
    let (children_, hooks) =
      leafElement.render(
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
    switch (leafElement.childrenType) {
    | React =>
      let update = ofList(~hostTreeState, children_: element(childNode));
      update
      |> addMountEffects
      |> Update.map(childInstances =>
           Instance({
             hooks,
             opaqueLeafElement,
             leafElement,
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
      let {Update.hostTreeUpdate, enqueuedEffects, payload} =
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
               opaqueLeafElement,
               leafElement,
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
              ~children=[hostTreeUpdate.nearestHostNode] |> List.to_seq,
              ~position=hostTreeState.absoluteSubtreeIndex,
            )
          ),
        absoluteSubtreeIndex: hostTreeState.absoluteSubtreeIndex + 1,
      };
      {
        hostTreeUpdate,
        enqueuedEffects,
        payload,
        childNodes: Seq.((() => Cons(node, () => Nil))),
      };
    };
  }

and ofOpaqueLeafElement:
  type parentNode node.
    (
      ~hostTreeState: Update.hostTreeState(parentNode, node),
      ~component: opaqueLeafElement(node)
    ) =>
    opaqueInstanceUpdate(parentNode, node) =
  (
    ~hostTreeState,
    ~component as OpaqueLeafElement(component) as opaqueComponent,
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
      ~f=ofOpaqueLeafElement,
      ~init=hostTreeState,
      syntheticElement,
    );

let pendingEffects =
    (~lifecycle, ~nextEffects, ~instance as {childInstances, hooks}) => {
  EffectSequence.chain(
    pendingEffectsInForest(~lifecycle, childInstances)
    |> EffectSequence.chain(Hooks.pendingEffects(~lifecycle, Some(hooks))),
    nextEffects,
  );
};

let outputTreeNodes: type node. opaqueInstance(node) => lazyHostNodeSeq(node) =
  (Instance(instance)) => {
    switch (instance.leafElement.childrenType) {
    | React => instance.wrappedHostNode
    | Host => Seq.((() => Cons(instance.wrappedHostNode, () => Nil)))
    };
  };

module Forest = {
  let pendingEffects = pendingEffectsInForest;

  let childNodes = instanceForest => {
    let rec inner = (acc, instanceForest) =>
      switch (instanceForest) {
      | CoreTypes.IFlat(opaqueInstance) =>
        let nodes = outputTreeNodes(opaqueInstance) |> List.of_seq;
        [nodes, ...acc];
      | INested(l, _) => List.fold_left(inner, acc, l)
      | IDiffableSequence(l, _) => Seq.fold_left(inner, acc, l.toSeq())
      };

    inner([], instanceForest) |> List.flatten |> List.to_seq;
  };
};
