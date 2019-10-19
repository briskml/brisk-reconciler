open TestReconciler;

/**
 * The simplest component. Composes nothing!
 */
module Box = {
  let%nativeComponent make = (~title="ImABox", (), h) => (
    {
      children: empty,
      make: () => Implementation.{name: "Box", element: Text(title)},
      configureInstance: (~isFirstRender, instance) =>
        isFirstRender
          ? instance : Implementation.{name: "Box", element: Text(title)},
    },
    h,
  );
};

module Div = {
  let%nativeComponent make = (~children=empty, (), h) => (
    {
      make: () => Implementation.{name: "Div", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children,
    },
    h,
  );
};

module SingleChildDiv = {
  let%nativeComponent make = (~children, (), h) => (
    {
      make: () => Implementation.{name: "SingleChildDiv", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children,
    },
    h,
  );
};

module Text = {
  type state = {
    current: string,
    prev: string,
  };
  let%nativeComponent make = (~title="ImABox", ()) => {
    let%hook (prevTitle, setTitle) = Hooks.ref(title);
    let%hook () =
      Hooks.effect(
        Always,
        () => {
          setTitle(title);
          None;
        },
      );
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
    };
  };
};

let stringToElement = string => <Text title=string />;

module BoxWrapper = {
  let%component make = (~title="ImABox", ~twoBoxes=false, (), h) => (
    twoBoxes
      ? <Div> <Box title /> <Box title /> </Div> : <Div> <Box title /> </Div>,
    h,
  );
};

/**
 * Box with dynamic keys.
 */
module BoxItemDynamic = {
  [@component useDynamicKey]
  let make = (~title="ImABox", (), h) => (stringToElement(title), h);
};

module BoxList = {
  type action =
    | Create(string)
    | Reverse;
  let%component make = (~rAction, ~useDynamicKeys=false, ()) => {
    let%hook (state, dispatch) =
      Hooks.reducer(~initialState=[], (action, state) =>
        switch (action) {
        | Create(title) => [
            useDynamicKeys ? <BoxItemDynamic title /> : <Box title />,
            ...state,
          ]
        | Reverse => List.rev(state)
        }
      );
    let%hook () =
      Hooks.effect(OnMount, () =>
        Some(RemoteAction.subscribe(~handler=dispatch, rAction))
      );

    listToElement(state);
  };
};

module StatelessButton = {
  let%component make =
                (
                  ~initialClickCount as _="noclicks",
                  ~test as _="default",
                  (),
                  h,
                ) => (
    <Div />,
    h,
  );
};

module ButtonWrapper = {
  let%component make = (~wrappedText="default", (), hooks) => (
    <StatelessButton
      initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
    />,
    hooks,
  );
};

module ButtonWrapperWrapper = {
  let buttonWrapperJsx = <ButtonWrapper wrappedText="TestButtonUpdated!!!" />;
  let%component make = (~wrappedText="default", (), hooks) => (
    <Div> {stringToElement(wrappedText)} buttonWrapperJsx </Div>,
    hooks,
  );
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  let%component make = (~rAction, ()) => {
    let%hook (state, dispatch) =
      Hooks.reducer(~initialState=ref(0), (Click, state)
        /* FIXME: make this pure */
        =>
          state^ mod 2 === 0
            ? {
              state := state^ + 1;
              state;
            }
            : ref(state^ + 1)
        );
    let%hook () =
      Hooks.effect(OnMount, () =>
        Some(RemoteAction.subscribe(~handler=dispatch, rAction))
      );
    stringToElement(string_of_int(state^));
  };
};

module ToggleClicks = {
  type action =
    | Click;
  let customHook = rAction => {
    let%hook (state, setState) = Hooks.state(false);
    let%hook () =
      Hooks.effect(Always, () =>
        Some(
          RemoteAction.subscribe(
            ~handler=(_: action) => setState(!state),
            rAction,
          ),
        )
      );
    state;
  };
  let%component make = (~rAction, ()) => {
    let%hook state = customHook(rAction);
    if (state) {
      <Div> <Text title="cell1" /> <Text title="cell2" /> </Div>;
    } else {
      <Div> <Text title="well" /> </Div>;
    };
  };
};

module EmptyComponent = {
  let%component make = ((), hooks) => (empty, hooks);
};

module EmptyComponentWithAlwaysEffect = {
  let%component make = (~onEffect, ~onEffectDispose, ()) => {
    let%hook () =
      Hooks.effect(
        Always,
        () => {
          onEffect();
          Some(onEffectDispose);
        },
      );
    empty;
  };
};

module EmptyComponentWithOnMountEffect = {
  let%component make = (~onEffect, ~onEffectDispose, ()) => {
    let%hook () =
      Hooks.effect(
        OnMount,
        () => {
          onEffect();
          Some(onEffectDispose);
        },
      );
    empty;
  };
};
