type t('node, 'childNode) = Instance.renderedElement('node, 'childNode);
type root('node, 'childNode) = {
  node: 'node,
  insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  moveNode:
    (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
};

let render:
  (root('node, 'childNode), Element.t('childNode)) => t('node, 'childNode);
let update:
  (~renderedElement: t('rootNode, 'node), Element.t('node)) =>
  t('rootNode, 'node);
let executePendingEffects: t('a, 'b) => t('a, 'b);
let executeHostViewUpdates: t('node, _) => 'node;
let flushPendingUpdates: t('a, 'b) => t('a, 'b);
