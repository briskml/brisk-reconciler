open TestReconciler;

/**
 * The simplest component. Composes nothing!
 */
module Box = {
  let component = nativeComponent("Box");
  let createElement =
      (~key=?, ~title="ImABox", ~onClick as _=?, ~children as _children, ()) =>
    component(~key?, h =>
      (
        h,
        {
          children: listToElement([]),
          make: () => Implementation.{name: "Box", element: Text(title)},
          configureInstance: (~isFirstRender, instance) =>
            isFirstRender
              ? instance : Implementation.{name: "Box", element: Text(title)},
        },
      )
    );
};

module Div = {
  let component = nativeComponent("Div");
  let createElement = (~key=?, ~children, ()) =>
    component(~key?, h =>
      (
        h,
        {
          make: () => Implementation.{name: "Div", element: View},
          configureInstance: (~isFirstRender as _, d) => d,
          children: listToElement(children),
        },
      )
    );
};

module Text = {
  type state = {
    current: string,
    prev: string,
  };
  let component = nativeComponent("Text");
  let createElement = (~key=?, ~title="ImABox", ~children as _children, ()) =>
    component(
      ~key?,
      hooks => {
        let (prevTitle, setTitle, hooks) = Hooks.ref(title, hooks);
        let hooks =
          Hooks.effect(
            Always,
            () => {
              setTitle(title);
              None;
            },
            hooks,
          );
        (
          hooks,
          {
            make: () => Implementation.{name: "Text", element: Text(title)},
            configureInstance: (~isFirstRender, t) => {
              if (prevTitle != title || isFirstRender) {
                Implementation.mountLog :=
                  [
                    Implementation.ChangeText(prevTitle, title),
                    ...Implementation.mountLog^,
                  ];
              };
              t;
            },
            children: listToElement([]),
          },
        );
      },
    );
};

let stringToElement = string => <Text title=string />;

module BoxWrapper = {
  let component = component("BoxWrapper");
  let createElement =
      (
        ~key=?,
        ~title="ImABox",
        ~twoBoxes=false,
        ~onClick as _=?,
        ~children as _,
        (),
      ) =>
    component(~key?, h =>
      (
        h,
        twoBoxes
          ? <Div> <Box title /> <Box title /> </Div>
          : <Div> <Box title /> </Div>,
      )
    );
};

/**
 * Box with dynamic keys.
 */
module BoxItemDynamic = {
  let component = component(~useDynamicKey=true, "BoxItemDynamic");
  let createElement = (~title="ImABox", ~children as _, ()) =>
    component(h => (h, stringToElement(title)));
};

module BoxList = {
  type action =
    | Create(string)
    | Reverse;
  let component = component("BoxList");
  let createElement = (~rAction, ~useDynamicKeys=false, ~children as _, ()) =>
    component(hooks => {
      let (state, dispatch, hooks) =
        Hooks.reducer(
          ~initialState=[],
          (action, state) =>
            switch (action) {
            | Create(title) => [
                useDynamicKeys ? <BoxItemDynamic title /> : <Box title />,
                ...state,
              ]
            | Reverse => List.rev(state)
            },
          hooks,
        );
      let hooks =
        Hooks.effect(
          OnMount,
          () => Some(RemoteAction.subscribe(~handler=dispatch, rAction)),
          hooks,
        );

      (hooks, listToElement(state));
    });
};

module StatelessButton = {
  let component = component("StatelessButton");
  let createElement =
      (
        ~initialClickCount as _="noclicks",
        ~test as _="default",
        ~children as _,
        (),
      ) =>
    component(h => (h, <Div />));
};

let testWrapper = (~wrappedText="default", _children) =>
  component("TestWrapper", hooks =>
    (
      hooks,
      <StatelessButton
        initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
      />,
    )
  );

module ButtonWrapper = {
  let component = component("ButtonWrapper");
  let createElement = (~wrappedText="default", ~children as _, ()) =>
    component(hooks =>
      (
        hooks,
        <StatelessButton
          initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
        />,
      )
    );
};

module ButtonWrapperWrapper = {
  let buttonWrapperJsx = <ButtonWrapper wrappedText="TestButtonUpdated!!!" />;
  let component = component("ButtonWrapperWrapper");
  let createElement = (~wrappedText="default", ~children as _, ()) =>
    component(hooks =>
      (hooks, <Div> {stringToElement(wrappedText)} buttonWrapperJsx </Div>)
    );
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  let component = component("UpdateAlternateClicks");
  let createElement = (~rAction, ~children as _, ()) =>
    component(hooks => {
      let (state, dispatch, hooks) =
        Hooks.reducer(
          ~initialState=ref(0),
          (Click, state) =>
            /* FIXME: make this pure */
            state^ mod 2 === 0
              ? {
                state := state^ + 1;
                state;
              }
              : ref(state^ + 1),
          hooks,
        );
      let hooks =
        Hooks.effect(
          OnMount,
          () => Some(RemoteAction.subscribe(~handler=dispatch, rAction)),
          hooks,
        );
      (hooks, stringToElement(string_of_int(state^)));
    });
};

module ToggleClicks = {
  type action =
    | Click;
  let component = component("ToggleClicks");
  let createElement = (~rAction, ~children as _, ()) =>
    component(hooks => {
      let (state, dispatch, hooks) =
        Hooks.reducer(~initialState=false, (Click, state) => !state, hooks);
      let hooks =
        Hooks.effect(
          OnMount,
          () =>
            Some(
              RemoteAction.subscribe(~handler=a => dispatch(a), rAction),
            ),
          hooks,
        );
      (
        hooks,
        if (state) {
          <Div> <Text title="cell1" /> <Text title="cell2" /> </Div>;
        } else {
          <Div> <Text title="well" /> </Div>;
        },
      );
    });
};

module EmptyComponent = {
  let component = component("Box");
  let createElement = (~key=?, ~children as _children, ()) =>
    component(~key?, hooks => (hooks, listToElement([])));
};

module EmptyComponentWithAlwaysEffect = {
  let component = component("Box");
  let createElement =
      (~children as _children, ~onEffect, ~onEffectDispose, ()) =>
    component(hooks => {
      let hooks =
        Hooks.effect(
          Always,
          () => {
            onEffect();
            Some(onEffectDispose);
          },
          hooks,
        );
      (hooks, listToElement([]));
    });
};

module EmptyComponentWithOnMountEffect = {
  let component = component("Box");
  let createElement =
      (~children as _children, ~onEffect, ~onEffectDispose, ()) =>
    component(hooks => {
      let hooks =
        Hooks.effect(
          OnMount,
          () => {
            onEffect();
            Some(onEffectDispose);
          },
          hooks,
        );
      (hooks, listToElement([]));
    });
};
