type hostNode('a) =
  | Node('a)
  | UpdatedNode('a, 'a);

type lazyHostNode('a) = Lazy.t(hostNode('a));

type lazyHostNodeSeq('a) = Seq.t(lazyHostNode('a));

type componentId('a) = ..;

type subtreeSize = int;

type diffItem('left, 'right) =
  | Added('right)
  | Removed('left)
  | Updated('left, 'right);

type dynamicElement('node, 'typ) = {
  diff: 'a. dynamicElement('node, 'a) => Seq.t(diffItem('typ, 'a)),
  map: 'a. (~f: 'typ => 'a) => dynamicElement('node, 'a),
  toSeq: unit => Seq.t('typ),
  insert: 'typ => dynamicElement('node, 'typ),
  empty: 'any. unit => dynamicElement('node, 'any),
};

type element('node) =
  | Leaf(opaqueLeafElement('node))
  | StaticList(list(element('node)))
  | DiffableSequence(dynamicElement('node, element('node)))
  | Movable(element('node), ref(option(instanceForest('node))))
and leafElement('a) = {
  debugName: string,
  id: componentId(instance('a)),
  childrenType: childrenType('viewSpec),
  eq:
    'other.
    ('other, componentId('other), componentId(instance('a))) =>
    option(instance('a)),

  render:
    Hooks.t('hooks, 'hooks) => ('children, Hooks.t(Hooks.nil, 'hooks)),
}
constraint 'viewSpec = ('node, 'children, 'childNode, 'wrappedNode)
constraint 'a = ('hooks, 'viewSpec)
and instance('a) = {
  hooks: Hooks.state('hooks),
  opaqueLeafElement: opaqueLeafElement('node),
  leafElement: leafElement('a),
  children_: 'children,
  childInstances: instanceForest('childNode),
  wrappedHostNode: 'wrappedNode,
}
constraint 'viewSpec = ('node, 'children, 'childNode, 'wrappedNode)
constraint 'a = ('hooks, 'viewSpec)
and opaqueLeafElement('node) =
  | OpaqueLeafElement(leafElement((_, ('node, _, _, _))))
    : opaqueLeafElement('node)
and instanceForest('node) =
  | IFlat(opaqueInstance('node))
  | INested(list(instanceForest('node)), subtreeSize)
  | IDiffableSequence(
      dynamicElement('node, instanceForest('node)),
      subtreeSize,
    )
and opaqueInstance('node) =
  | Instance(instance((_, ('node, _, _, _)))): opaqueInstance('node)
and hostNodeElement('node, 'childNode) = {
  make: unit => 'node,
  configureInstance: (~isFirstRender: bool, 'node) => 'node,
  children: element('childNode),
  insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  moveNode:
    (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
}
and childrenType('viewSpec) =
  | Host: childrenType(
            (
              'node,
              hostNodeElement('node, 'childNode),
              'childNode,
              lazyHostNode('node),
            ),
          )
  | React
      : childrenType(
          ('node, element('node), 'node, lazyHostNodeSeq('node)),
        );

type opaqueInstanceHash('node) =
  Lazy.t(Hashtbl.t(int, (opaqueInstance('node), int)));
