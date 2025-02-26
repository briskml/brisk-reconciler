open Reconciler
open Brisk_reconciler__Brisk_reconciler_internal

type testState =
  {element : node element; renderedElement : (node, node) RenderedElement.t}

let render root element =
  { element
  ; renderedElement =
      RenderedElement.render
        {node = root; insertNode; deleteNode; moveNode}
        element }

let reset x =
  Reconciler.mountLog := [];
  x

let update nextReactElement {element = previousElement; renderedElement} =
  { element = nextReactElement
  ; renderedElement =
      RenderedElement.update ~previousElement ~renderedElement nextReactElement
  }

let flushPendingUpdates ({renderedElement; element} as testState) =
  match !isDirty with
  | true ->
      isDirty := false;
      { element
      ; renderedElement = RenderedElement.flushPendingUpdates renderedElement }
  | false -> testState

let executeSideEffects ({renderedElement} as testState) =
  RenderedElement.executeHostViewUpdates renderedElement |> ignore;
  { testState with
    renderedElement = RenderedElement.executePendingEffects renderedElement }

let act ~action rAction testState =
  Remote_action.send rAction ~action;
  testState

let getMountLogAndReset _state =
  let mountLog = List.rev !Reconciler.mountLog in
  Reconciler.mountLog := [];
  mountLog
