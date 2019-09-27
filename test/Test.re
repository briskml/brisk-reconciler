open TestReconciler;
open Assert;

let root = Implementation.{name: "root", element: View};
let div = Implementation.{name: "Div", element: View};
let singleChildDiv = Implementation.{name: "SingleChildDiv", element: View};
let text = t => Implementation.{name: "Text", element: Text(t)};
let box = t => Implementation.{name: "Box", element: Text(t)};

let render = render(root);

let core = [
  (
    "Test initial render",
    `Quick,
    () =>
      render(<Components.BoxWrapper />)
      |> executeSideEffects
      |> expect(
           ~label="It correctly inserts nodes",
           [MountChild(div, box("ImABox"), 0), MountChild(root, div, 0)],
         )
      |> ignore,
  ),
  (
    "Test rendering list children",
    `Quick,
    () =>
      render(
        Components.(
          <Div> <Box title="ImABox1" /> <Box title="ImABox2" /> </Div>
        ),
      )
      |> executeSideEffects
      |> expect(
           ~label="It inserts two boxes in a div",
           [
             MountChild(div, box("ImABox1"), 0),
             MountChild(div, box("ImABox2"), 1),
             MountChild(root, div, 0),
           ],
         )
      |> ignore,
  ),
  (
    "Test replacing subtree",
    `Quick,
    () =>
      render(
        Components.(
          <Div> <Box title="ImABox1" /> <Box title="ImABox2" /> </Div>
        ),
      )
      |> executeSideEffects
      |> reset
      |> update(Components.(<Div> <Box title="ImABox3" /> </Div>))
      |> executeSideEffects
      |> expect(
           ~label="It replaces the subtree",
           [
             UnmountChild(div, box("ImABox1")),
             UnmountChild(div, box("ImABox2")),
             MountChild(div, box("ImABox3"), 0),
           ],
         )
      |> ignore,
  ),
  (
    "Test top level reorder",
    `Quick,
    () => {
      GlobalState.useTailHack := true;

      let key1 = Key.create();
      let key2 = Key.create();

      render(
        listToElement(
          Components.[
            <Text key=key1 title="x" />,
            <Text key=key2 title="y" />,
          ],
        ),
      )
      |> executeSideEffects
      |> expect(
           ~label="It correctly constructs initial tree",
           [
             ChangeText("x", "x"),
             MountChild(root, text("x"), 0),
             ChangeText("y", "y"),
             MountChild(root, text("y"), 1),
           ],
         )
      |> update(
           listToElement(
             Components.[
               <Text key=key2 title="y" />,
               <Text key=key1 title="x" />,
             ],
           ),
         )
      |> executeSideEffects
      |> expect(
           ~label="It reorders only one element",
           [RemountChild(root, text("y"), 1, 0)],
         )
      |> ignore;
    },
  ),
  (
    "Test top level replace elements",
    `Quick,
    () => {
      let key1 = Key.create();
      let key2 = Key.create();
      render(<Components.Text key=key1 title="x" />)
      |> executeSideEffects
      |> expect(
           ~label="It constructs initial tree",
           [ChangeText("x", "x"), MountChild(root, text("x"), 0)],
         )
      |> update(<Components.Text key=key2 title="y" />)
      |> executeSideEffects
      |> expect(
           ~label="It replaces text(x) with text(y)",
           [
             UnmountChild(root, text("x")),
             ChangeText("y", "y"),
             MountChild(root, text("y"), 0),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test subtree replace elements (not at top-level)",
    `Quick,
    () => {
      let rAction = RemoteAction.create();

      let well = text("well");

      let testState =
        render(Components.(<Div> <ToggleClicks rAction /> </Div>))
        |> executeSideEffects
        |> expect(
             ~label="It constructs the initial tree",
             [
               ChangeText("well", "well"),
               MountChild(div, well, 0),
               MountChild(div, div, 0),
               MountChild(root, div, 0),
             ],
           );

      RemoteAction.send(~action=Components.ToggleClicks.Click, rAction);
      let cell1 = text("cell1");
      let cell2 = text("cell2");

      testState
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It replaces text(well) with text(cell1) and text(cell2)",
           [
             UnmountChild(div, well),
             ChangeText("cell1", "cell1"),
             MountChild(div, cell1, 0),
             ChangeText("cell2", "cell2"),
             MountChild(div, cell2, 1),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test subtree replace elements",
    `Quick,
    () => {
      let rAction = RemoteAction.create();

      let well = text("well");

      let testState =
        render(Components.(<ToggleClicks rAction />))
        |> executeSideEffects
        |> expect(
             ~label="It constructs the initial tree",
             [
               ChangeText("well", "well"),
               MountChild(div, well, 0),
               MountChild(root, div, 0),
             ],
           );

      RemoteAction.send(~action=Components.ToggleClicks.Click, rAction);
      let cell1 = text("cell1");
      let cell2 = text("cell2");

      testState
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It replaces text(well) with text(cell1) and text(cell2)",
           [
             UnmountChild(div, well),
             ChangeText("cell1", "cell1"),
             MountChild(div, cell1, 0),
             ChangeText("cell2", "cell2"),
             MountChild(div, cell2, 1),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test top level prepend",
    `Quick,
    () => {
      GlobalState.useTailHack := true;
      let key1 = Key.create();
      let key2 = Key.create();
      let commonElement = [<Components.Text key=key1 title="x" />];

      render(listToElement(commonElement))
      |> executeSideEffects
      |> expect(
           ~label="It constructs the initial tree",
           [ChangeText("x", "x"), MountChild(root, text("x"), 0)],
         )
      |> update(
           listToElement([
             <Components.Text key=key2 title="y" />,
             ...commonElement,
           ]),
         )
      |> executeSideEffects
      |> expect(
           ~label="It correctly mounts prepend topLevelUpdate",
           [ChangeText("y", "y"), MountChild(root, text("y"), 0)],
         )
      |> ignore;
    },
  ),
  (
    "Test simple subtree change",
    `Quick,
    () =>
      render(<Components.BoxWrapper />)
      |> executeSideEffects
      |> expect(
           ~label="It renders one Box inside a Div",
           [MountChild(div, box("ImABox"), 0), MountChild(root, div, 0)],
         )
      |> update(<Components.BoxWrapper twoBoxes=true />)
      |> executeSideEffects
      |> expect(
           ~label="It replaces one box with two boxes",
           [
             UnmountChild(div, box("ImABox")),
             MountChild(div, box("ImABox"), 0),
             MountChild(div, box("ImABox"), 1),
           ],
         )
      |> ignore,
  ),
  (
    "Test changing components",
    `Quick,
    () =>
      render(<Components.EmptyComponent />)
      |> executeSideEffects
      |> expect(~label="It renders ChangeCounter component", [])
      |> update(
           <Components.ButtonWrapperWrapper wrappedText="initial text" />,
         )
      |> executeSideEffects
      |> expect(
           ~label=
             "It changes components from ChangeCounter to ButtonWrapperWrapper",
           Implementation.[
             ChangeText("initial text", "initial text"),
             MountChild(div, text("initial text"), 0),
             MountChild(div, div, 1),
             MountChild(root, div, 0),
           ],
         )
      |> update(
           <Components.ButtonWrapperWrapper wrappedText="updated text" />,
         )
      |> executeSideEffects
      |> expect(
           ~label="It updates text in the ButtonWrapper",
           Implementation.[ChangeText("initial text", "updated text")],
         )
      |> ignore,
  ),
  (
    "Test BoxList with dynamic keys",
    `Quick,
    () => {
      let rAction = RemoteAction.create();
      render(<Components.BoxList useDynamicKeys=true rAction />)
      |> executeSideEffects
      |> expect(~label="It renders initial BoxList", [])
      |> act(~action=Components.BoxList.Create("Hello"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It adds a new BoxItem and then flushes",
           Implementation.[
             ChangeText("Hello", "Hello"),
             MountChild(root, text("Hello"), 0),
           ],
         )
      |> act(~action=Components.BoxList.Create("World"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It prepends one more BoxItem and then flushes",
           Implementation.[
             ChangeText("World", "World"),
             MountChild(root, text("World"), 0),
           ],
         )
      |> act(~action=Components.BoxList.Reverse, rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It reverses the items list in the BoxList",
           Implementation.[RemountChild(root, text("Hello"), 1, 0)],
         )
      |> ignore;
    },
  ),
  (
    "Test BoxList without dynamic keys",
    `Quick,
    () => {
      let rAction = RemoteAction.create();
      render(<Components.BoxList rAction />)
      |> executeSideEffects
      |> expect(~label="It renders BoxList", [])
      |> act(~action=Components.BoxList.Create("Hello"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It adds a new Box",
           Implementation.[MountChild(root, box("Hello"), 0)],
         )
      |> act(~action=Components.BoxList.Create("World"), rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It prepends one more Box",
           Implementation.[MountChild(root, box("World"), 0)],
         )
      |> act(~action=Components.BoxList.Reverse, rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It reverses the boxes list in the BoxList",
           Implementation.[
             UnmountChild(root, box("World")),
             MountChild(root, box("Hello"), 0),
             UnmountChild(root, box("Hello")),
             MountChild(root, box("World"), 1),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test BoxItemDynamic memoizing during deep move",
    `Quick,
    () => {
      let box = <Components.BoxItemDynamic title="box to move" />;
      let {renderedElement: {instanceForest: beforeUpdate}} as testState =
        render(box)
        |> executeSideEffects
        |> expect(
             ~label="It renders the initial BoxItemDynamic",
             Implementation.[
               ChangeText("box to move", "box to move"),
               MountChild(root, text("box to move"), 0),
             ],
           );
      let {renderedElement: {instanceForest: afterUpdate}} =
        testState
        |> update(
             listToElement([
               Components.stringToElement("before"),
               listToElement([box]),
             ]),
           )
        |> executeSideEffects
        |> expect(
             ~label=
               "It adds new element before BoxItemDynamic (it replaces the whole tree)",
             Implementation.[
               UnmountChild(root, text("box to move")),
               ChangeText("before", "before"),
               MountChild(root, text("before"), 0),
               MountChild(root, text("box to move"), 1),
             ],
           );
      check(
        Alcotest.bool,
        "It memoized the nested BoxItemDynamic",
        true,
        switch (beforeUpdate, afterUpdate) {
        | (IFlat(x), INested([_, INested([IFlat(y)], _)], _)) => x === y
        | _ => false
        },
      );
    },
  ),
  (
    "Test list updates with static keys",
    `Quick,
    () => {
      let key1 = Key.create();
      let key2 = Key.create();
      render(
        listToElement([
          <Components.Box key=key1 title="Box1unchanged" />,
          <Components.Box key=key2 title="Box2unchanged" />,
        ]),
      )
      |> executeSideEffects
      |> expect(
           ~label="It renders the initial Boxes list",
           Implementation.[
             MountChild(root, box("Box1unchanged"), 0),
             MountChild(root, box("Box2unchanged"), 1),
           ],
         )
      |> update(
           listToElement([
             <Components.Box key=key2 title="Box2changed" />,
             <Components.Box key=key1 title="Box1changed" />,
           ]),
         )
      |> executeSideEffects
      |> expect(
           ~label="It reorders the list",
           Implementation.[
             UnmountChild(root, box("Box2unchanged")),
             MountChild(root, box("Box2changed"), 1),
             RemountChild(root, box("Box2changed"), 1, 0),
             UnmountChild(root, box("Box1unchanged")),
             MountChild(root, box("Box1changed"), 1),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test conditional updating by leveraging refs",
    `Quick,
    () => {
      let rAction = RemoteAction.create();
      render(<Components.UpdateAlternateClicks rAction />)
      |> executeSideEffects
      |> expect(
           ~label="It renders UpdateAlternateClicks element",
           Implementation.[
             ChangeText("0", "0"),
             MountChild(root, text("0"), 0),
           ],
         )
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(~label="It only changes state on first click", [])
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It changes both state and contents on second click",
           Implementation.[ChangeText("0", "2")],
         )
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(~label="It only changes state on third click", [])
      |> act(~action=Components.UpdateAlternateClicks.Click, rAction)
      |> flushPendingUpdates
      |> executeSideEffects
      |> expect(
           ~label="It changes both state and contents on fourth click",
           Implementation.[ChangeText("2", "4")],
         )
      |> ignore;
    },
  ),
  (
    "Test updating with identical element",
    `Quick,
    () => {
      let key1 = Key.create();
      let key2 = Key.create();
      render(
        listToElement([
          <Components.Text key=key1 title="x" />,
          <Components.Text key=key2 title="y" />,
        ]),
      )
      |> executeSideEffects
      |> expect(
           ~label="It renders list with Text elements",
           Implementation.[
             ChangeText("x", "x"),
             MountChild(root, text("x"), 0),
             ChangeText("y", "y"),
             MountChild(root, text("y"), 1),
           ],
         )
      |> update(
           listToElement(
             Components.[
               <Text key=key1 title="x" />,
               <Text key=key2 title="y" />,
             ],
           ),
         )
      |> executeSideEffects
      |> expect(
           ~label="It updates the state with a new instance of (same) string",
           [],
         )
      |> update(
           listToElement(
             Components.[
               <Text key=key2 title="y" />,
               <Text key=key1 title="x" />,
             ],
           ),
         )
      |> executeSideEffects
      |> expect(
           ~label="it reorders the list",
           Implementation.[RemountChild(root, text("y"), 1, 0)],
         )
      |> ignore;
    },
  ),
  (
    "Test prepending new element",
    `Quick,
    () => {
      GlobalState.useTailHack := true;
      let key1 = Key.create();
      let key2 = Key.create();
      let commonElement = [<Components.Text key=key1 title="x" />];
      render(listToElement(commonElement))
      |> executeSideEffects
      |> expect(
           ~label="It renders a new Text element",
           Implementation.[
             ChangeText("x", "x"),
             MountChild(root, text("x"), 0),
           ],
         )
      |> update(
           listToElement([
             <Components.Text key=key2 title="y" />,
             ...commonElement,
           ]),
         )
      |> executeSideEffects
      |> expect(
           ~label="It prepends a new Text element to the list",
           Implementation.[
             ChangeText("y", "y"),
             MountChild(root, text("y"), 0),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test 'Always' effect",
    `Quick,
    () => {
      let effectCallCount = ref(0);
      let effectDisposeCallCount = ref(0);

      let onEffect = () => effectCallCount := effectCallCount^ + 1;
      let onEffectDispose = () =>
        effectDisposeCallCount := effectDisposeCallCount^ + 1;

      let testState =
        render(
          <Components.EmptyComponentWithAlwaysEffect
            onEffect
            onEffectDispose
          />,
        )
        |> executeSideEffects;

      expectInt(~label="The effect should've been run", 1, effectCallCount^);

      expectInt(
        ~label="The dispose should not have been run yet",
        0,
        effectDisposeCallCount^,
      );

      testState
      |> update(
           <Components.EmptyComponentWithAlwaysEffect
             onEffect
             onEffectDispose
           />,
         )
      |> executeSideEffects
      |> ignore;

      expectInt(
        ~label="The effect should've been run again",
        2,
        effectCallCount^,
      );

      expectInt(
        ~label="The effect dispose callback should have been run",
        1,
        effectDisposeCallCount^,
      );
    },
  ),
  (
    "Test 'Always' effect in a nested component",
    `Quick,
    () => {
      let effectCallCount = ref(0);
      let effectDisposeCallCount = ref(0);
      let onEffect = () => effectCallCount := effectCallCount^ + 1;
      let onEffectDispose = () =>
        effectDisposeCallCount := effectDisposeCallCount^ + 1;

      render(
        Components.(
          <Div>
            <EmptyComponentWithAlwaysEffect onEffect onEffectDispose />
          </Div>
        ),
      )
      |> executeSideEffects
      |> ignore;

      expectInt(~label="The effect should've been run", 1, effectCallCount^);

      expectInt(
        ~label="The dispose should not have been run yet",
        0,
        effectDisposeCallCount^,
      );
    },
  ),
  (
    "Test 'OnMount' effect",
    `Quick,
    () => {
      let effectCallCount = ref(0);
      let effectDisposeCallCount = ref(0);
      let onEffect = () => effectCallCount := effectCallCount^ + 1;
      let onEffectDispose = () =>
        effectDisposeCallCount := effectDisposeCallCount^ + 1;

      let testState =
        render(
          <Components.EmptyComponentWithOnMountEffect
            onEffect
            onEffectDispose
          />,
        )
        |> executeSideEffects;

      expectInt(~label="The effect should've been run", 1, effectCallCount^);

      expectInt(
        ~label="The dispose should not have been run yet",
        0,
        effectDisposeCallCount^,
      );

      let testState =
        testState
        |> update(
             <Components.EmptyComponentWithOnMountEffect
               onEffect
               onEffectDispose
             />,
           )
        |> executeSideEffects;

      expectInt(
        ~label="The effect should not have been run again",
        1,
        effectCallCount^,
      );

      expectInt(
        ~label="The effect dispose callback should not have been run yet",
        0,
        effectDisposeCallCount^,
      );

      testState
      |> update(<Components.EmptyComponent />)
      |> executeSideEffects
      |> ignore;

      expectInt(
        ~label="The effect should not have been run again",
        1,
        effectCallCount^,
      );

      expectInt(
        ~label=
          "The effect dispose callback should have been called since the component was un-mounted.",
        1,
        effectDisposeCallCount^,
      );
    },
  ),
  (
    "Test 'OnMount' effect in nested component",
    `Quick,
    () => {
      let effectCallCount = ref(0);
      let effectDisposeCallCount = ref(0);
      let onEffect = () => effectCallCount := effectCallCount^ + 1;
      let onEffectDispose = () =>
        effectDisposeCallCount := effectDisposeCallCount^ + 1;

      let testState =
        render(
          Components.(
            <Div>
              <EmptyComponentWithOnMountEffect onEffect onEffectDispose />
            </Div>
          ),
        )
        |> executeSideEffects;

      expectInt(~label="The effect should've been run", 1, effectCallCount^);

      expectInt(
        ~label="The dispose should not have been run yet",
        0,
        effectDisposeCallCount^,
      );

      testState
      |> update(Components.(<Div />))
      |> executeSideEffects
      |> ignore;

      expectInt(
        ~label=
          "The effect dispose callback should have been called since the component was un-mounted.",
        1,
        effectDisposeCallCount^,
      );
    },
  ),
  (
    "Test 'OnMount' effect in extra-nested component",
    `Quick,
    () => {
      let effectCallCount = ref(0);
      let effectDisposeCallCount = ref(0);
      let onEffect = () => incr(effectCallCount);
      let onEffectDispose = () => incr(effectDisposeCallCount);

      /*
       * When a parent-of-a-parent of a component with an OnMountEffect is removed,
       * the OnMount effect doesn't get disposed on removal
       */
      let testState =
        render(
          Components.(
            <Div>
              <Div>
                <EmptyComponentWithOnMountEffect onEffect onEffectDispose />
              </Div>
            </Div>
          ),
        )
        |> executeSideEffects;

      expectInt(~label="The effect should've been run", 1, effectCallCount^);

      expectInt(
        ~label="The dispose should not have been run yet",
        0,
        effectDisposeCallCount^,
      );

      testState
      |> update(Components.(<Div />))
      |> executeSideEffects
      |> ignore;

      expectInt(
        ~label=
          "The effect dispose callback should have been called since the component was un-mounted.",
        1,
        effectDisposeCallCount^,
      );
    },
  ),
  (
    "Test transition from empty list to non-empty list",
    `Quick,
    () => {
      GlobalState.useTailHack := false;
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
      |> executeSideEffects
      |> expect(
           ~label="It mounts IAmBox0+1",
           [
             MountChild(div, box("ImABox0"), 0),
             UnmountChild(div, box("ImABox1")),
             MountChild(div, box("ImABox1"), 1),
           ],
         )
      |> ignore;

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
      |> executeSideEffects
      |> expect(
           ~label="It mounts IAmBox0+1",
           [
             UnmountChild(div, box("ImABox0")),
             MountChild(div, box("ImABox0"), 0),
             MountChild(div, box("ImABox1"), 1),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test transition from empty list to non-empty list & <Box key> becomes <Div key>",
    `Quick,
    () => {
      let key = Key.create();
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
      |> executeSideEffects
      |> expect(
           ~label="IAmBox",
           [
             MountChild(div, box("ImABoxB"), 0),
             UnmountChild(div, box("ImABoxA")),
             MountChild(div, div, 1),
           ],
         )
      |> ignore;
    },
  ),
  (
    "Test a <SingleChildDiv> with single Flat child, with a changing key",
    `Quick,
    () => {
      render(
        Components.(
          <SingleChildDiv> ...<Div key={Key.create()} /> </SingleChildDiv>
        ),
      )
      |> executeSideEffects
      |> reset
      |> update(
           Components.(
             <SingleChildDiv> ...<Div key={Key.create()} /> </SingleChildDiv>
           ),
         )
      |> executeSideEffects
      |> expect(
           ~label="it re-mounts the node with the new key",
           [
             UnmountChild(singleChildDiv, div),
             MountChild(singleChildDiv, div, 0),
           ],
         )
      |> ignore;
    },
  ),
];

/** Annoying dune progress */
print_endline("");

Alcotest.run(~argv=[|"--verbose --color"|], "Brisk", [("Core", core)]);
