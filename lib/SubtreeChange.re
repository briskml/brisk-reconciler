open CoreTypes;

let insertNodes =
    (
      ~nodeElement,
      ~parent as parentWrapper,
      ~children,
      ~position as initialPosition: int,
    ) => {
  let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
  let newParent =
    Seq.fold_left(
      ((position, parent), child) =>
        (
          position + 1,
          {
            let Node(child) | UpdatedNode(_, child) = Lazy.force(child);
            nodeElement.insertNode(~parent, ~child, ~position);
          },
        ),
      (initialPosition, oldParent),
      children,
    )
    |> snd;
  newParent === oldParent ? parentWrapper : UpdatedNode(oldParent, newParent);
};
let deleteNodes =
    (
      ~nodeElement,
      ~parent as parentWrapper,
      ~children,
      ~position as initialPosition: int,
    ) => {
  let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
  let newParent =
    Seq.fold_left(
      ((position, parent), child) =>
        (
          position + 1,
          {
            let Node(child) | UpdatedNode(_, child) = Lazy.force(child);
            nodeElement.deleteNode(~parent, ~child, ~position);
          },
        ),
      (initialPosition, oldParent),
      children,
    )
    |> snd;
  newParent === oldParent ? parentWrapper : UpdatedNode(oldParent, newParent);
};

let replaceSubtree =
    (
      ~nodeElement,
      ~parent,
      ~prevChildren,
      ~nextChildren,
      ~absoluteSubtreeIndex: int,
    ) =>
  lazy(
    {
      insertNodes(
        ~nodeElement,
        ~parent=
          deleteNodes(
            ~nodeElement,
            ~parent=Lazy.force(parent),
            ~children=prevChildren,
            ~position=absoluteSubtreeIndex,
          ),
        ~children=nextChildren,
        ~position=absoluteSubtreeIndex,
      );
    }
  );

let reorderNode =
    (~nodeElement, ~child, ~parent, ~indexShift: int, ~from: int, ~to_: int) => {
  let isVal = Lazy.is_val(child);
  switch (Lazy.force(child)) {
  | Node(child) =>
    from === to_ - indexShift
      ? parent : nodeElement.moveNode(~parent, ~child, ~from, ~to_)
  | UpdatedNode(prevChild, child) when !isVal =>
    nodeElement.insertNode(
      ~parent=
        nodeElement.deleteNode(~parent, ~child=prevChild, ~position=from),
      ~child,
      ~position=to_,
    )
  | UpdatedNode(_prevChild, child) =>
    from === to_ - indexShift
      ? parent : nodeElement.moveNode(~parent, ~child, ~from, ~to_)
  };
};

let reorder =
    (
      type node,
      type childNode,
      ~nodeElement,
      ~parent: lazyHostNode(node),
      ~instance as
        Instance({wrappedHostNode, component: {childrenType}}):
          opaqueInstance(childNode),
      ~indexShift,
      ~from,
      ~to_,
    ) =>
  switch (childrenType) {
  | Host =>
    lazy({
      let parentWrapper = Lazy.force(parent);
      let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
      let newParent =
        reorderNode(
          ~nodeElement,
          ~parent=oldParent,
          ~child=wrappedHostNode,
          ~indexShift,
          ~from,
          ~to_,
        );
      newParent === oldParent
        ? parentWrapper : UpdatedNode(oldParent, newParent);
    })
  | React =>
    lazy({
      let parentWrapper = Lazy.force(parent);
      let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
      let newParent =
        Seq.fold_left(
          ((index, parent), child) =>
            (
              index + 1,
              reorderNode(
                ~nodeElement,
                ~parent,
                ~child,
                ~indexShift,
                ~from=from + index,
                ~to_=to_ + index,
              ),
            ),
          (0, oldParent),
          wrappedHostNode,
        )
        |> snd;
      newParent === oldParent
        ? parentWrapper : UpdatedNode(oldParent, newParent);
    })
  };
let updateNodes =
    (~nodeElement, ~parent, ~children, ~position as initialPosition) =>
  lazy({
    let parentWrapper = Lazy.force(parent);
    let Node(oldParent) | UpdatedNode(_, oldParent) = parentWrapper;
    let newParent =
      Seq.fold_left(
        ((position, instance), x) =>
          (
            position + 1,
            switch (Lazy.force(x)) {
            | Node(_child) => instance
            | UpdatedNode(oldNode, newNode) =>
              nodeElement.insertNode(
                ~parent=
                  nodeElement.deleteNode(
                    ~parent=instance,
                    ~child=oldNode,
                    ~position,
                  ),
                ~child=newNode,
                ~position,
              )
            },
          ),
        (initialPosition, oldParent),
        children,
      )
      |> snd;
    newParent === oldParent
      ? parentWrapper : UpdatedNode(oldParent, newParent);
  });
