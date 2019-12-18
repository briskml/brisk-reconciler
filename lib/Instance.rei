type opaqueInstanceUpdate('node, 'childNode) =
  Update.t(
    'node,
    'childNode,
    CoreTypes.opaqueInstance('childNode),
  );

type renderedElement('node, 'childNode) =
  Update.t(
    'node,
    'childNode,
    CoreTypes.instanceForest('childNode),
  );

let ofOpaqueComponent:
  (
    ~hostTreeState: Update.hostTreeState('parentNode, 'node),
    ~component: CoreTypes.opaqueComponent('node)
  ) =>
  opaqueInstanceUpdate('parentNode, 'node);

let ofList:
  (
    ~hostTreeState: Update.hostTreeState('parentNode, 'node),
    Element.t('node)
  ) =>
  renderedElement('parentNode, 'node);

let pendingEffects:
  (
    ~lifecycle: Hooks.Effect.lifecycle,
    ~nextEffects: EffectSequence.t,
    ~instance: CoreTypes.instance(_)
  ) =>
  EffectSequence.t;

let outputTreeNodes:
  CoreTypes.opaqueInstance('node) => CoreTypes.lazyHostNodeSeq('node);
