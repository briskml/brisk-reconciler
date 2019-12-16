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

  let state =
    ref(
      render(
        listToElement(
          Components.[
            <Text key=key1 title="x" />,
            <Text key=key2 title="y" />,
          ],
        ),
      ),
    );

  test("It correctly constructs initial tree", ({expect}) => {
    state := state^ |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
      ChangeText("y", "y"),
      MountChild(root, text("y"), 1),
    ]);
  });

  test("It reorders only one element", ({expect}) => {
    let mountLog =
      state^
      |> update(
           listToElement(
             Components.[
               <Text key=key2 title="y" />,
               <Text key=key1 title="x" />,
             ],
           ),
         )
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([RemountChild(root, text("y"), 1, 0)]);
  });
});

describe("Test top level replace elements", ({test}) => {
  let key1 = Key.create();
  let key2 = Key.create();

  let state = ref(render(<Components.Text key=key1 title="x" />));

  test("It constructs initial tree", ({expect}) => {
    state := state^ |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;
    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
    ]);
  });

  test("It replaces text(x) with text(y)", ({expect}) => {
    let mountLog =
      state^
      |> update(<Components.Text key=key2 title="y" />)
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(root, text("x")),
      ChangeText("y", "y"),
      MountChild(root, text("y"), 0),
    ]);
  });
});

describe("Test subtree replace elements (not at top-level)", ({test}) => {
  let rAction = RemoteAction.create();
  let well = text("well");

  let state =
    ref(render(Components.(<Div> <ToggleClicks rAction /> </Div>)));

  test("It constructs initial tree", ({expect}) => {
    state := state^ |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("well", "well"),
      MountChild(div, well, 0),
      MountChild(div, div, 0),
      MountChild(root, div, 0),
    ]);
  });

  test("It replaces text(well) with text(cell1) and text(cell2)", ({expect}) => {
    RemoteAction.send(~action=Components.ToggleClicks.Click, rAction);
    let cell1 = text("cell1");
    let cell2 = text("cell2");

    let mountLog =
      state^
      |> flushPendingUpdates
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(div, well),
      ChangeText("cell1", "cell1"),
      MountChild(div, cell1, 0),
      ChangeText("cell2", "cell2"),
      MountChild(div, cell2, 1),
    ]);
  });
});

describe("Test subtree replace elements", ({test}) => {
  let rAction = RemoteAction.create();
  let well = text("well");

  let state = ref(render(<Components.ToggleClicks rAction />));

  test("It replaces text(well) with text(cell1) and text(cell2)", ({expect}) => {
    state := state^ |> executeSideEffects;

    let mountLog = state |> getMountLogAndReset;

    /* "It constructs initial tree" */
    expect.list(mountLog).toEqual([
      ChangeText("well", "well"),
      MountChild(div, well, 0),
      MountChild(root, div, 0),
    ]);
  });

  test("It replaces text(well) with text(cell1) and text(cell2)", ({expect}) => {
    RemoteAction.send(~action=Components.ToggleClicks.Click, rAction);
    let cell1 = text("cell1");
    let cell2 = text("cell2");

    let mountLog =
      state^
      |> flushPendingUpdates
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(div, well),
      ChangeText("cell1", "cell1"),
      MountChild(div, cell1, 0),
      ChangeText("cell2", "cell2"),
      MountChild(div, cell2, 1),
    ]);
  });
});

describe("Test top level prepend", ({test}) => {
  let key1 = Key.create();
  let key2 = Key.create();
  let commonElement = [<Components.Text key=key1 title="x" />];

  let state = ref(render(listToElement(commonElement)));

  test("It constructs initial tree", ({expect}) => {
    state := state^ |> executeSideEffects;

    let mountLog = state |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
    ]);
  });

  test("It correctly mounts prepend topLevelUpdate", ({expect}) => {
    let mountLog =
      state^
      |> update(
           listToElement([
             <Components.Text key=key2 title="y" />,
             ...commonElement,
           ]),
         )
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("y", "y"),
      MountChild(root, text("y"), 0),
    ]);
  });
});

describe("Test simple subtree change", ({test}) => {
  let state = ref(render(<Components.BoxWrapper />));

  test("It renders one Box inside a Div", ({expect}) => {
    state := state^ |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(div, box("ImABox"), 0),
      MountChild(root, div, 0),
    ]);
  });

  test("It replaces one box with two boxes", ({expect}) => {
    let mountLog =
      state^
      |> update(<Components.BoxWrapper twoBoxes=true />)
      |> executeSideEffects
      |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(div, box("ImABox")),
      MountChild(div, box("ImABox"), 0),
      MountChild(div, box("ImABox"), 1),
    ]);
  });
});

describe("Test changing components", ({test}) => {
  let state =
    ref(render(<Components.EmptyComponent />) |> executeSideEffects);

  test("It renders nothing", ({expect}) => {
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([]);
  });

  test(
    "It changes from EmptyComponent to ButtonWrapperWrapper",
    ({expect}) => {
    state :=
      state^
      |> update(
           <Components.ButtonWrapperWrapper wrappedText="initial text" />,
         )
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("initial text", "initial text"),
      MountChild(div, text("initial text"), 0),
      MountChild(div, div, 1),
      MountChild(root, div, 0),
    ]);
  });

  test("It updates text in the ButtonWrapper", ({expect}) => {
    state :=
      state^
      |> update(
           <Components.ButtonWrapperWrapper wrappedText="updated text" />,
         )
      |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("initial text", "updated text"),
    ]);
  });
});

describe("Test BoxList with dynamic keys", ({test}) => {
  let rAction = RemoteAction.create();
  let state =
    ref(
      render(<Components.BoxList useDynamicKeys=true rAction />)
      |> executeSideEffects,
    );

  test("It renders an empty list", ({expect}) => {
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([]);
  });

  test("It inserts one item", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.BoxList.Create("Hello"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("Hello", "Hello"),
      MountChild(root, text("Hello"), 0),
    ]);
  });

  test("It prepends one more BoxItem and then flushes", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.BoxList.Create("World"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("World", "World"),
      MountChild(root, text("World"), 0),
    ]);
  });

  test("It reverses the items list in the BoxList", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.BoxList.Reverse, rAction)
      |> flushPendingUpdates
      |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      RemountChild(root, text("Hello"), 1, 0),
    ]);
  });
});

describe(
  "Test BoxList without dynamic keys", ({test}) => {
  let rAction = RemoteAction.create();
  let state =
    ref(render(<Components.BoxList rAction />) |> executeSideEffects);

  test("It renders an empty list", ({expect}) => {
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([]);
  });

  test("It adds a new Box", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.BoxList.Create("Hello"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([MountChild(root, box("Hello"), 0)]);
  });

  test("It prepends one more Box", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.BoxList.Create("World"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([MountChild(root, box("World"), 0)]);
  });

  test("It reverses the boxes list in the BoxList", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.BoxList.Reverse, rAction)
      |> flushPendingUpdates
      |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(root, box("World")),
      MountChild(root, box("Hello"), 0),
      UnmountChild(root, box("Hello")),
      MountChild(root, box("World"), 1),
    ]);
  });
});

describe("Test BoxItemDynamic memoizing during deep move", ({test}) => {
  let box = <Components.BoxItemDynamic title="box to move" />;

  let state = ref(render(box));

  let beforeUpdate = ref(None);
  let afterUpdate = ref(None);

  test("It renders the initial BoxItemDynamic", ({expect}) => {
    state := state^ |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    beforeUpdate := Some(state^.renderedElement.payload);

    expect.list(mountLog).toEqual([
      ChangeText("box to move", "box to move"),
      MountChild(root, text("box to move"), 0),
    ]);
  });

  test(
    "It adds new element before BoxItemDynamic (it replaces the whole tree)",
    ({expect}) => {
    state :=
      state^
      |> update(
           listToElement([
             Components.stringToElement("before"),
             listToElement([box]),
           ]),
         )
      |> executeSideEffects;

    afterUpdate := Some(state^.renderedElement.payload);

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(root, text("box to move")),
      ChangeText("before", "before"),
      MountChild(root, text("before"), 0),
      MountChild(root, text("box to move"), 1),
    ]);
  });

  test("It memoized the nested BoxItemDynamic", ({expect}) => {
    expect.bool(
      switch (beforeUpdate^, afterUpdate^) {
      | (Some(IFlat(x)), Some(INested([_, INested([IFlat(y)], _)], _))) =>
        x === y
      | _ => false
      },
    ).
      toBeTrue()
  });
});

describe("Test list updates with static keys", ({test}) => {
  let key1 = Key.create();
  let key2 = Key.create();

  let state =
    ref(
      render(
        listToElement([
          <Components.Box key=key1 title="Box1unchanged" />,
          <Components.Box key=key2 title="Box2unchanged" />,
        ]),
      ),
    );

  test("It renders the initial Boxes list", ({expect}) => {
    state := state^ |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(root, box("Box1unchanged"), 0),
      MountChild(root, box("Box2unchanged"), 1),
    ]);
  });

  test("It reorders the list", ({expect}) => {
    state :=
      state^
      |> update(
           listToElement([
             <Components.Box key=key2 title="Box2changed" />,
             <Components.Box key=key1 title="Box1changed" />,
           ]),
         )
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(root, box("Box2unchanged")),
      MountChild(root, box("Box2changed"), 1),
      RemountChild(root, box("Box2changed"), 1, 0),
      UnmountChild(root, box("Box1unchanged")),
      MountChild(root, box("Box1changed"), 1),
    ]);
  });
});

describe("Test conditional updating by leveraging refs", ({test}) => {
  let rAction = RemoteAction.create();

  let state = ref(render(<Components.UpdateAlternateClicks rAction />));

  test("It renders UpdateAlternateClicks element", ({expect}) => {
    state := state^ |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("0", "0"),
      MountChild(root, text("0"), 0),
    ]);
  });

  test("It only changes state on first click", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([]);
  });

  test("It changes both state and contents on second click", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([ChangeText("0", "2")]);
  });

  test("It only changes state on third click", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([]);
  });

  test("It changes both state and contents on fourth click", ({expect}) => {
    state :=
      state^
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([ChangeText("2", "4")]);
  });
});

describe("Test updating with identical element", ({test}) => {
  let key1 = Key.create();
  let key2 = Key.create();

  let state =
    ref(
      render(
        listToElement([
          <Components.Text key=key1 title="x" />,
          <Components.Text key=key2 title="y" />,
        ]),
      ),
    );

  test("It renders list with Text elements", ({expect}) => {
    state := state^ |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
      ChangeText("y", "y"),
      MountChild(root, text("y"), 1),
    ]);
  });

  test(
    "It updates the state with a new instance of (same) string", ({expect}) => {
    state :=
      state^
      |> update(
           listToElement(
             Components.[
               <Text key=key1 title="x" />,
               <Text key=key2 title="y" />,
             ],
           ),
         )
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([]);
  });

  test("It reorders the list", ({expect}) => {
    state :=
      state^
      |> update(
           listToElement(
             Components.[
               <Text key=key2 title="y" />,
               <Text key=key1 title="x" />,
             ],
           ),
         )
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([RemountChild(root, text("y"), 1, 0)]);
  });
});

describe("Test prepending new element", ({test}) => {
  let key1 = Key.create();
  let key2 = Key.create();
  let commonElement = [<Components.Text key=key1 title="x" />];

  let state = ref(render(listToElement(commonElement)));

  test("It renders a new Text element", ({expect}) => {
    state := state^ |> executeSideEffects;
    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("x", "x"),
      MountChild(root, text("x"), 0),
    ]);
  });

  test("It prepends a new Text element to the list", ({expect}) => {
    state :=
      state^
      |> update(
           listToElement([
             <Components.Text key=key2 title="y" />,
             ...commonElement,
           ]),
         )
      |> executeSideEffects;

    let mountLog = state^ |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      ChangeText("y", "y"),
      MountChild(root, text("y"), 0),
    ]);
  });
});

describe("Test 'Always' effect", ({test}) => {
  let effectCallCount = ref(0);
  let effectDisposeCallCount = ref(0);

  let onEffect = () => effectCallCount := effectCallCount^ + 1;
  let onEffectDispose = () =>
    effectDisposeCallCount := effectDisposeCallCount^ + 1;

  let state =
    ref(
      render(
        <Components.EmptyComponentWithAlwaysEffect onEffect onEffectDispose />,
      ),
    );

  test("The effect should've been run", ({expect}) => {
    state := state^ |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test("The dispose should not have been run yet", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(0)
  });

  test("It prepends a new Text element to the list", ({expect}) => {
    state :=
      state^
      |> update(
           <Components.EmptyComponentWithAlwaysEffect
             onEffect
             onEffectDispose
           />,
         )
      |> executeSideEffects;

    expect.int(effectCallCount^).toBe(2);
  });

  test("The effect dispose callback should have been run", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(1)
  });
});

describe("Test 'Always' effect in a nested component", ({test}) => {
  let effectCallCount = ref(0);
  let effectDisposeCallCount = ref(0);

  let onEffect = () => effectCallCount := effectCallCount^ + 1;
  let onEffectDispose = () =>
    effectDisposeCallCount := effectDisposeCallCount^ + 1;

  let state =
    ref(
      render(
        Components.(
          <Div>
            <EmptyComponentWithAlwaysEffect onEffect onEffectDispose />
          </Div>
        ),
      ),
    );

  test("The effect should've been run", ({expect}) => {
    state := state^ |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test("The dispose should not have been run yet", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(0)
  });
});

describe("Test 'OnMount' effect", ({test}) => {
  let effectCallCount = ref(0);
  let effectDisposeCallCount = ref(0);

  let onEffect = () => effectCallCount := effectCallCount^ + 1;
  let onEffectDispose = () =>
    effectDisposeCallCount := effectDisposeCallCount^ + 1;

  let state =
    ref(
      render(
        <Components.EmptyComponentWithOnMountEffect
          onEffect
          onEffectDispose
        />,
      ),
    );

  test("The effect should've been run", ({expect}) => {
    state := state^ |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test("The dispose should not have been run yet", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(0)
  });

  test("The effect should not have been run again", ({expect}) => {
    state :=
      state^
      |> update(
           <Components.EmptyComponentWithOnMountEffect
             onEffect
             onEffectDispose
           />,
         )
      |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test(
    "The effect dispose callback should not have been run yet", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(0)
  });

  test("The effect should not have been run again", ({expect}) => {
    state :=
      state^ |> update(<Components.EmptyComponent />) |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test(
    "The effect dispose callback should have been called since the component was un-mounted.",
    ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(1)
  });
});

describe("Test 'OnMount' effect in nested component", ({test}) => {
  let effectCallCount = ref(0);
  let effectDisposeCallCount = ref(0);

  let onEffect = () => effectCallCount := effectCallCount^ + 1;
  let onEffectDispose = () =>
    effectDisposeCallCount := effectDisposeCallCount^ + 1;

  let state =
    ref(
      render(
        Components.(
          <Div>
            <EmptyComponentWithOnMountEffect onEffect onEffectDispose />
          </Div>
        ),
      ),
    );

  test("The effect should've been run", ({expect}) => {
    state := state^ |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test("The dispose should not have been run yet", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(0)
  });

  test("The effect should not have been run again", ({expect}) => {
    state := state^ |> update(Components.(<Div />)) |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test(
    "The effect dispose callback should have been called since the component was un-mounted.",
    ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(1)
  });
});

describe("Test 'OnMount' effect in extra-nested component", ({test}) => {
  let effectCallCount = ref(0);
  let effectDisposeCallCount = ref(0);

  let onEffect = () => incr(effectCallCount);
  let onEffectDispose = () => incr(effectDisposeCallCount);

  let state =
    ref(
      render(
        Components.(
          <Div>
            <Div>
              <EmptyComponentWithOnMountEffect onEffect onEffectDispose />
            </Div>
          </Div>
        ),
      ),
    );

  /*
   * When a parent-of-a-parent of a component with an OnMountEffect is removed,
   * the OnMount effect doesn't get disposed on removal
   */
  test("The effect should've been run", ({expect}) => {
    state := state^ |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test("The dispose should not have been run yet", ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(0)
  });

  test("The effect should not have been run again", ({expect}) => {
    state := state^ |> update(Components.(<Div />)) |> executeSideEffects;

    expect.int(effectCallCount^).toBe(1);
  });

  test(
    "The effect dispose callback should have been called since the component was un-mounted.",
    ({expect}) => {
    expect.int(effectDisposeCallCount^).toBe(1)
  });
});

describe("Test transition from empty list to non-empty list", ({test}) => {
  test("It mounts IAmBox0+1", ({expect}) => {
    let state =
      render(
        Components.(<Div> {listToElement([])} <Box title="ImABox1" /> </Div>),
      )
      |> executeSideEffects
      |> reset
      |> update(
           Components.(
             <Div>
               {listToElement([<Box title="ImABox0" />])}
               <Box title="ImABox1" />
             </Div>
           ),
         )
      |> executeSideEffects;
    let mountLog = state |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(div, box("ImABox0"), 0),
      UnmountChild(div, box("ImABox1")),
      MountChild(div, box("ImABox1"), 1),
    ]);
  });

  test("It mounts IAmBox0+1", ({expect}) => {
    let state =
      render(
        Components.(<Div> <Box title="ImABox0" /> {listToElement([])} </Div>),
      )
      |> executeSideEffects
      |> reset
      |> update(
           Components.(
             <Div>
               <Box title="ImABox0" />
               {listToElement([<Box title="ImABox1" />])}
             </Div>
           ),
         )
      |> executeSideEffects;

    let mountLog = state |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(div, box("ImABox0")),
      MountChild(div, box("ImABox0"), 0),
      MountChild(div, box("ImABox1"), 1),
    ]);
  });
});

describe(
  "Test transition from empty list to non-empty list & <Box key> becomes <Div key>",
  ({test}) => {
  test("IAmBox", ({expect}) => {
    let key = Key.create();
    let state =
      render(
        Components.(
          <Div> {listToElement([])} <Box key title="ImABoxA" /> </Div>
        ),
      )
      |> executeSideEffects
      |> reset
      |> update(
           Components.(
             <Div>
               {listToElement([<Box title="ImABoxB" />])}
               <Div key />
             </Div>
           ),
         )
      |> executeSideEffects;
    let mountLog = state |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      MountChild(div, box("ImABoxB"), 0),
      UnmountChild(div, box("ImABoxA")),
      MountChild(div, div, 1),
    ]);
  })
});

describe(
  "Test a <SingleChildDiv> with single Flat child, with a changing key",
  ({test}) => {
  test("It re-mounts the node with the new key", ({expect}) => {
    let key1 = Key.create();
    let key2 = Key.create();
    let state =
      render(
        Components.(<SingleChildDiv> ...<Div key=key1 /> </SingleChildDiv>),
      )
      |> executeSideEffects
      |> reset
      |> update(
           Components.(<SingleChildDiv> ...<Div key=key2 /> </SingleChildDiv>),
         )
      |> executeSideEffects;
    let mountLog = state |> getMountLogAndReset;

    expect.list(mountLog).toEqual([
      UnmountChild(singleChildDiv, div),
      MountChild(singleChildDiv, div, 0),
    ]);
  })
});
