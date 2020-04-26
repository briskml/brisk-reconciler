type t('node) = CoreTypes.element('node);

let fold:
  (
    ~f: (
          ~hostTreeState: Update.hostTreeState('a, 'b),
          ~component: CoreTypes.opaqueLeafElement('c)
        ) =>
        Update.t('a, 'b, CoreTypes.opaqueInstance('c)),
    ~init: Update.hostTreeState('a, 'b),
    t('c)
  ) =>
  Update.t('a, 'b, CoreTypes.instanceForest('c));
