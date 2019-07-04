open TestReconciler;

module Box = {
  [@nativeComponent]
  let make =
      (~title="ImABox", ~onClick as _: option(unit => unit)=?, (), hooks) => {
    (
      hooks,
      {
        children: listToElement([]),
        make: () => Implementation.{name: "Box", element: Text(title)},
        configureInstance: (~isFirstRender, instance) =>
          isFirstRender
            ? instance : Implementation.{name: "Box", element: Text(title)},
      },
    );
  };
};

module Div = {
  [@nativeComponent]
  let make = (~children=[], (), h) => (
    h,
    {
      make: () => Implementation.{name: "Div", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children: listToElement(children),
    },
  );
};

module SingleChildDiv = {
  [@nativeComponent]
  let make = (~children as child: syntheticElement, (), h) => (
    h,
    {
      make: () => Implementation.{name: "SingleChildDiv", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children: child,
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
        children: listToElement([]),
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
 */;

module BoxItemDynamic = {
  [@component]
  let make = (~title="ImABox", (), h) => (h, stringToElement(title));
};

module BoxList = {
  type boxListAction =
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

[@component]
let statelessButton = (~initialClickCount as _, (), h) => (h, <Div />);

[@component]
let buttonWrapper = (~wrappedText="default", (), hooks) => (
  hooks,
  <statelessButton
    initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
  />,
);

let buttonWrapperJsx = <buttonWrapper wrappedText="TestButtonUpdated!!!" />;
[@component]
let buttonWrapperWrapper = (~wrappedText="default", (), hooks) => (
  hooks,
  <Div> {stringToElement(wrappedText)} buttonWrapperJsx </Div>,
);

type updateAlternateClicksAction =
  | Click;
[@component]
let updateAlternateClicks = (~rAction, (), hooks) => {
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

type toggleClicksAction =
  | Click;
[@component]
let toggleClicks = (~rAction, (), hooks) => {
  let (state, setState, hooks) = Hooks.state(false, hooks);
  let hooks =
    Hooks.effect(
      Always,
      () =>
        Some(
          RemoteAction.subscribe(
            ~handler=(Click) => setState(!state),
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

[@component]
let emptyComponent = ((), hooks) => (hooks, listToElement([]));

[@component]
let emptyComponentWithAlwaysEffect =
    (
      ~children as _children: list(syntheticElement),
      ~onEffect,
      ~onEffectDispose,
      (),
      hooks,
    ) => {
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
};

[@component]
let emptyComponentWithOnMountEffect = (~onEffect, ~onEffectDispose, (), hooks) => {
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
};
