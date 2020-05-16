let reconcile:
  (
    ~shouldExecutePendingUpdates: bool,
    ~hostTreeState: Update.hostTreeState('parentNode, 'node),
    ~oldInstanceForest: CoreTypes.instanceForest('node),
    ~nextElement: CoreTypes.element('node),
    unit
  ) =>
  Instance.renderedElement('parentNode, 'node);

/**
  * Execute the pending updates at the top level of an instance tree.
  * If no state change is performed, the argument is returned unchanged.
  */
let flushPendingUpdates:
  (
    ~parentHostNode: CoreTypes.lazyHostNode('parentNode),
    ~parentHostNodeElement: CoreTypes.hostNodeElement('parentNode, 'node),
    ~rootInstance: CoreTypes.opaqueInstance('node)
  ) =>
  Update.t('parentNode, 'node, CoreTypes.opaqueInstance('node));
