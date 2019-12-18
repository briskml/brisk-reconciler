let insertNodes:
  (
    ~nodeElement: CoreTypes.hostNodeElement('node, 'children),
    ~parent: CoreTypes.hostNode('node),
    ~children: CoreTypes.lazyHostNodeSeq('children),
    ~position: int
  ) =>
  CoreTypes.hostNode('node);

let updateNodes:
  (
    ~nodeElement: CoreTypes.hostNodeElement('node, 'children),
    ~parent: CoreTypes.lazyHostNode('node),
    ~children: CoreTypes.lazyHostNodeSeq('children),
    ~position: int
  ) =>
  CoreTypes.lazyHostNode('node);

let replaceSubtree:
  (
    ~nodeElement: CoreTypes.hostNodeElement('node, 'children),
    ~parent: CoreTypes.lazyHostNode('node),
    ~prevChildren: CoreTypes.lazyHostNodeSeq('children),
    ~nextChildren: CoreTypes.lazyHostNodeSeq('children),
    ~absoluteSubtreeIndex: int
  ) =>
  CoreTypes.lazyHostNode('node);

let reorder:
  (
    ~nodeElement: CoreTypes.hostNodeElement('node, 'children),
    ~parent: CoreTypes.lazyHostNode('node),
    ~instance: CoreTypes.opaqueInstance('children),
    ~indexShift: int,
    ~from: int,
    ~to_: int
  ) =>
  CoreTypes.lazyHostNode('node);
