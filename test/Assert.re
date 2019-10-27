open Alcotest;
open TestReconciler;
open Brisk_reconciler__Brisk_reconciler_internal;

type mountElement = RenderedElement.t(node, node);

type mount = list(testMountEntry);

type testHostItem('a) =
  | MountElement(mountElement): testHostItem(mount);

let mountLog =
  Alcotest.testable(
    (formatter, t) => TestPrinter.printMountLog(formatter, t),
    equal_testMountLog,
  );

let diffOutput = (expected, actual) => {
  let gray = string => "\027[90m" ++ string ++ "\027[39m";
  let red = string => "\027[31m" ++ string ++ "\027[39m";
  let green = string => "\027[32m" ++ string ++ "\027[39m";
  module Diff = Patience_diff_lib.Patience_diff;
  module StringDiff = Patience_diff_lib.Patience_diff.String;
  let diff =
    StringDiff.get_hunks(
      ~transform=x => x,
      ~big_enough=?None,
      ~context=-1,
      ~mine=expected,
      ~other=actual,
    );
  diff
  |> Diff.Hunks.ranges
  |> List.map(
       fun
       | Diff.Range.Same(lines) =>
         gray(
           String.concat(
             "\n",
             Array.map(((line, _)) => " " ++ line, lines) |> Array.to_list,
           ),
         )
       | Old(lines) =>
         red(
           String.concat(
             "\n",
             lines |> Array.map(line => "-" ++ line) |> Array.to_list,
           ),
         )
       | New(lines) =>
         green(
           String.concat(
             "\n",
             lines |> Array.map(line => "+" ++ line) |> Array.to_list,
           ),
         )
       | Replace(expected, actual) =>
         red(
           String.concat(
             "\n",
             actual |> Array.map(line => "-" ++ line) |> Array.to_list,
           ),
         )
         ++ "\n"
         ++ green(
              String.concat(
                "\n",
                expected |> Array.map(line => "+" ++ line) |> Array.to_list,
              ),
            )
       | Unified(_) => failwith("UNEXPECTED FAILURE"),
     )
  |> String.concat("\n");
};

let line = (ppf, c) => {
  let line = Astring.String.v(~len=80, _ => c);
  Fmt.pf(ppf, "%a\n%!", Fmt.(styled(`Yellow, string)), line);
};

let check = (t, msg, x, y) =>
  if (!equal(t, x, y)) {
    line(Fmt.stderr, '-');
    let expected =
      Fmt.strf("%a", pp(t), x)
      |> String.split_on_char('\n')
      |> Array.of_list;
    let actual =
      Fmt.strf("%a", pp(t), y)
      |> String.split_on_char('\n')
      |> Array.of_list;
    let diff = diffOutput(expected, actual);
    Fmt.strf("%s:\n\n%s\n", msg, diff) |> failwith;
  };

let assertMountLog = (~label="", expected, actual) => {
  TestReconciler.mountLog := [];
  check(mountLog, label, expected, List.rev(actual));
};

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

let update = (nextReactElement, {element: previousElement, renderedElement}) => {
  element: nextReactElement,
  renderedElement:
    RenderedElement.update(
      ~previousElement,
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

let expect = (~label=?, expected, testState) => {
  let mountLog = TestReconciler.mountLog^;
  assertMountLog(~label?, expected, mountLog);
  reset(testState);
};

let expectInt = (~label, expected, actual) => {
  Alcotest.(check(int))(label, expected, actual);
};

let act = (~action, rAction, testState) => {
  RemoteAction.send(rAction, ~action);
  testState;
};
