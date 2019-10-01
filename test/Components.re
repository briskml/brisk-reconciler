open TestReconciler;

/**
 * The simplest component. Composes nothing!
 */
module Box = {
  [@nativeComponent]
  let make = (~title="ImABox", (), h) => (
    h,
    {
      children: empty,
      make: () => Implementation.{name: "Box", element: Text(title)},
      configureInstance: (~isFirstRender, instance) =>
        isFirstRender
          ? instance : Implementation.{name: "Box", element: Text(title)},
    },
  );
};

module Div = {
  [@nativeComponent]
  let make = (~children=empty, (), h) => (
    h,
    {
      make: () => Implementation.{name: "Div", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children,
    },
  );
};

module SingleChildDiv = {
  [@nativeComponent]
  let make = (~children, (), h) => (
    h,
    {
      make: () => Implementation.{name: "SingleChildDiv", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children,
    },
  );
};

module Text = {
  type state = {
    current: string,
    prev: string,
  };
  [@nativeComponent]
  let make = (~title="ImABox", (), hooks) => {
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
        children: empty,
      },
    );
  };
};

let stringToElement = string => <Text title=string />;

module BoxWrapper = {
  [@component]
  let make = (~title="ImABox", ~twoBoxes=false, (), h) => (
    h,
    twoBoxes
      ? <Div> <Box title /> <Box title /> </Div> : <Div> <Box title /> </Div>,
  );
};

/**
 * Box with dynamic keys.
 */
module BoxItemDynamic = {
  [@component useDynamicKey]
  let make = (~title="ImABox", (), h) => (h, stringToElement(title));
};

module BoxList = {
  type action =
    | Create(string)
    | Reverse;
  [@component]
  let make = (~rAction, ~useDynamicKeys=false, (), hooks) => {
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
  };
};

module StatelessButton = {
  [@component]
  let make = (~initialClickCount as _="noclicks", ~test as _="default", (), h) => (
    h,
    <Div />,
  );
};

module ButtonWrapper = {
  [@component]
  let make = (~wrappedText="default", (), hooks) => (
    hooks,
    <StatelessButton
      initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
    />,
  );
};

module ButtonWrapperWrapper = {
  let buttonWrapperJsx = <ButtonWrapper wrappedText="TestButtonUpdated!!!" />;
  [@component]
  let make = (~wrappedText="default", (), hooks) => (
    hooks,
    <Div> {stringToElement(wrappedText)} buttonWrapperJsx </Div>,
  );
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  [@component]
  let make = (~rAction, (), hooks) => {
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
  };
};

module ToggleClicks = {
  type action =
    | Click;
  [@component]
  let make = (~rAction, (), hooks) => {
    let (state, setState, hooks) = Hooks.state(false, hooks);
    let hooks =
      Hooks.effect(
        Always,
        () =>
          Some(
            RemoteAction.subscribe(
              ~handler=(_: action) => setState(!state),
              rAction,
            ),
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
  };
};

module EmptyComponent = {
  [@component]
  let make = ((), hooks) => (hooks, empty);
};

module EmptyComponentWithAlwaysEffect = {
  [@component]
  let make = (~onEffect, ~onEffectDispose, (), hooks) => {
    let hooks =
      Hooks.effect(
        Always,
        () => {
          onEffect();
          Some(onEffectDispose);
        },
        hooks,
      );
    (hooks, empty);
  };
};

module EmptyComponentWithOnMountEffect = {
  [@component]
  let make = (~onEffect, ~onEffectDispose, (), hooks) => {
    let hooks =
      Hooks.effect(
        OnMount,
        () => {
          onEffect();
          Some(onEffectDispose);
        },
        hooks,
      );
    (hooks, empty);
  };
};
