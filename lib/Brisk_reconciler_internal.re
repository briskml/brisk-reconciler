open CoreTypes;

type handler = GlobalState.handler;
type unregisterF = GlobalState.unregisterF;
let addStaleTreeHandler = GlobalState.addStaleTreeHandler;

type element('node) = CoreTypes.element('node);
type hostNodeElement('node, 'childNode) =
  CoreTypes.hostNodeElement('node, 'childNode) = {
    make: unit => 'node,
    configureInstance: (~isFirstRender: bool, 'node) => 'node,
    children: element('childNode),
    insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
    deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
    moveNode:
      (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
  };

let element = component => {
  Leaf(OpaqueComponent(component));
};

let listToElement = l => StaticList(l);
let empty = StaticList([]);

module Hooks = Hooks;
module RemoteAction = RemoteAction;
module RenderedElement = RenderedElement;

module Expert = {
  let jsx_list = l => StaticList(l);
  let component:
    type a node.
      (string, Hooks.t(a, a) => (element(node), Hooks.t(Hooks.nil, a))) =>
      element(node) =
    debugName => {
      module Component = {
        type componentId('a) +=
          | Id: componentId(
                  instance(
                    (
                      a,
                      (node, element(node), node, lazyHostNodeSeq(node)),
                    ),
                  ),
                );

        let eq:
          type c.
            (
              c,
              componentId(c),
              componentId(
                instance(
                  (a, (node, element(node), node, lazyHostNodeSeq(node))),
                ),
              )
            ) =>
            option(
              instance(
                (a, (node, element(node), node, lazyHostNodeSeq(node))),
              ),
            ) =
          (instance, id1, id2) => {
            switch (id1, id2) {
            | (Id, Id) => Some(instance)
            | (_, _) => None
            };
          };
      };
      render =>
        element({
          debugName,
          childrenType: React,
          id: Component.Id,
          eq: Component.eq,
          render,
        });
    };

  let nativeComponent:
    type a node childNode.
      (
        string,
        Hooks.t(a, a) =>
        (hostNodeElement(node, childNode), Hooks.t(Hooks.nil, a))
      ) =>
      element(node) =
    debugName => {
      module Component = {
        type componentId('a) +=
          | Id: componentId(
                  instance(
                    (
                      a,
                      (
                        node,
                        hostNodeElement(node, childNode),
                        childNode,
                        lazyHostNode(node),
                      ),
                    ),
                  ),
                );

        let eq:
          type c.
            (
              c,
              componentId(c),
              componentId(
                instance(
                  (
                    a,
                    (
                      node,
                      hostNodeElement(node, childNode),
                      childNode,
                      lazyHostNode(node),
                    ),
                  ),
                ),
              )
            ) =>
            option(
              instance(
                (
                  a,
                  (
                    node,
                    hostNodeElement(node, childNode),
                    childNode,
                    lazyHostNode(node),
                  ),
                ),
              ),
            ) =
          (instance, id1, id2) => {
            switch (id1, id2) {
            | (Id, Id) => Some(instance)
            | (_, _) => None
            };
          };
      };
      render =>
        element({
          debugName,
          childrenType: Host,
          id: Component.Id,
          eq: Component.eq,
          render,
        });
    };
};

type movableStateContainerState('node) =
  ref(option(instanceForest('node)));

let movableStateContainer = (~children, (), hooks) => {
  let (instanceRef, hooks) = Hooks.ref(None, hooks);
  (() => Movable(children, instanceRef), hooks);
};
