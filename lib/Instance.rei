type opaqueInstanceUpdate('node, 'childNode) =
  Update.t('node, 'childNode, CoreTypes.opaqueInstance('childNode));

type renderedElement('node, 'childNode) =
  Update.t('node, 'childNode, CoreTypes.instanceForest('childNode));

let ofOpaqueLeafElement:
  (
    ~hostTreeState: Update.hostTreeState('parentNode, 'node),
    ~component: CoreTypes.opaqueLeafElement('node)
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

module Forest: {
  let pendingEffects:
    (~lifecycle: Hooks.Effect.lifecycle, CoreTypes.instanceForest(_)) =>
    EffectSequence.t;

  let childNodes:
    CoreTypes.instanceForest('childNode) =>
    CoreTypes.lazyHostNodeSeq('childNode);
};
