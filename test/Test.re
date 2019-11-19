open TestFramework;
open TestReconciler;
open TestHelpers;
open Brisk_reconciler__Brisk_reconciler_internal;

let root = {name: "root", element: View};
let div = {name: "Div", element: View};
let singleChildDiv = {name: "SingleChildDiv", element: View};
let text = t => {name: "Text", element: Text(t)};
let box = t => {name: "Box", element: Text(t)};

let render = render(root);

describe("Test initial render", ({test}) => {
  test("It correctly inserts nodes", ({expect}) => {
    let mountLog =
      render(<Components.BoxWrapper />)
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(div, box("ImABox"), 0),
      MountChild(root, div, 0),
    ]);
  })
});

describe("Test rendering list children", ({test}) => {
  test("It inserts two boxes in a div", ({expect}) => {
    let mountLog =
      render(
        Components.(
          <Div> <Box title="ImABox1" /> <Box title="ImABox2" /> </Div>
        ),
      )
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(div, box("ImABox1"), 0),
      MountChild(div, box("ImABox2"), 1),
      MountChild(root, div, 0),
    ]);
  })
});

describe("Test replacing subtree", ({test}) => {
  test("It replaces the subtree", ({expect}) => {
    let mountLog =
      render(
        Components.(
          <Div> <Box title="ImABox1" /> <Box title="ImABox2" /> </Div>
        ),
      )
      |> executeSideEffects
      |> reset
      |> update(Components.(<Div> <Box title="ImABox3" /> </Div>))
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(div, box("ImABox1")),
      UnmountChild(div, box("ImABox2")),
      MountChild(div, box("ImABox3"), 0),
    ]);
  })
});

describe("Test top level reorder", ({test}) => {
  let key1 = Key.create();
  let key2 = Key.create();

  test("It reorders only one element", ({expect}) => {
    let state =
      render(
        listToElement(
          Components.[
            <Text key=key1 title="x" />,
            <Text key=key2 title="y" />,
          ],
        ),
      )
      |> executeSideEffects;

    let mountLog = getMountLogAndReset(state);

    // "It correctly constructs initial tree"
    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
      ChangeText("y", "y"),
      MountChild(root, text("y"), 1),
    ]);

    let mountLog =
      update(
        listToElement(
          Components.[
            <Text key=key2 title="y" />,
            <Text key=key1 title="x" />,
          ],
        ),
        state,
      )
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([RemountChild(root, text("y"), 1, 0)]);
  });
});

describe("Test top level replace elements", ({test}) => {
  test("It replaces text(x) with text(y)", ({expect}) => {
    let key1 = Key.create();
    let key2 = Key.create();

    let state =
      render(<Components.Text key=key1 title="x" />) |> executeSideEffects;

    let mountLog = state |> getMountLogAndReset;

    // "It constructs initial tree"
    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
    ]);

    let mountLog =
      state
      |> update(<Components.Text key=key2 title="y" />)
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(root, text("x")),
      ChangeText("y", "y"),
      MountChild(root, text("y"), 0),
    ]);
  })
});
