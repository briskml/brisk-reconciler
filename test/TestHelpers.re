open TestReconciler;
open Brisk_reconciler__Brisk_reconciler_internal;

type testState = {
  element: element(node),
  renderedElement: RenderedElement.t(node, node),
};

let render = (root, element) => {
  element,
  renderedElement:
    RenderedElement.render(
      {node: root, insertNode, deleteNode, moveNode},
      element,
    ),
};

let reset = x => {
  TestReconciler.mountLog := [];
  x;
};

let update = (nextReactElement, {renderedElement}) => {
  element: nextReactElement,
  renderedElement:
    RenderedElement.update(
      ~renderedElement,
      nextReactElement,
    ),
};

let flushPendingUpdates = ({renderedElement, element} as testState) =>
  isDirty^
    ? {
      isDirty := false;
      {
        element,
        renderedElement: RenderedElement.flushPendingUpdates(renderedElement),
      };
    }
    : testState;

let executeSideEffects = ({renderedElement} as testState) => {
  RenderedElement.executeHostViewUpdates(renderedElement) |> ignore;

  {
    ...testState,
    renderedElement: RenderedElement.executePendingEffects(renderedElement),
  };
};

let act = (~action, rAction, testState) => {
  RemoteAction.send(rAction, ~action);
  testState;
};

let getMountLogAndReset = _state => {
  let mountLog = List.rev(TestReconciler.mountLog^);
  TestReconciler.mountLog := [];

  mountLog;
};
