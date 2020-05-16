open CoreTypes;

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
let map = (f, update) => {
  let {payload, hostTreeUpdate, enqueuedEffects, childNodes} = update;
  {hostTreeUpdate, enqueuedEffects, payload: f(payload), childNodes};
};
let mapEffects = (f, update) => {
  let {payload, hostTreeUpdate, enqueuedEffects, childNodes} = update;
  {hostTreeUpdate, enqueuedEffects: f(enqueuedEffects), payload, childNodes};
};
