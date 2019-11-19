open TestFramework;
open TestReconciler;
open TestHelpers;
// open Brisk_reconciler__Brisk_reconciler_internal;

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
      render(Components.(
        <Div> <Box title="ImABox1" /> <Box title="ImABox2" /> </Div>
      ))
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(div, box("ImABox1"), 0),
      MountChild(div, box("ImABox2"), 1),
      MountChild(root, div, 0),
    ]);
  })
});
