open TestReconciler;
module Brisk_reconciler = Brisk_reconciler__Brisk_reconciler_internal;
open Brisk_reconciler;

/**
 * The simplest component. Composes nothing!
 */
module Box = {
  let%nativeComponent make = (~title="ImABox", (), h) => (
    {
      children: Brisk_reconciler.empty,
      make: () => {name: "Box", element: Text(title)},
      configureInstance: (~isFirstRender, instance) =>
        isFirstRender ? instance : {name: "Box", element: Text(title)},
      insertNode,
      moveNode,
      deleteNode,
    },
    h,
  );
};

module Div = {
  let%nativeComponent make = (~children=empty, (), h) => (
    {
      make: () => {name: "Div", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children,
      insertNode,
      moveNode,
      deleteNode,
    },
    h,
  );
};

module SingleChildDiv = {
  let%nativeComponent make = (~children, (), h) => (
    {
      make: () => {name: "SingleChildDiv", element: View},
      configureInstance: (~isFirstRender as _, d) => d,
      children,
      insertNode,
      moveNode,
      deleteNode,
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
    let%hook prevTitle = Hooks.ref(title);
    let%hook () =
      Hooks.effect(
        Always,
        () => {
          prevTitle := title;
          None;
        },
      );
    {
      make: () => {name: "Text", element: Text(title)},
      configureInstance: (~isFirstRender, t) => {
        if (prevTitle^ != title || isFirstRender) {
          mountLog := [ChangeText(prevTitle^, title), ...mountLog^];
        };
        t;
      },
      children: empty,
      insertNode,
      moveNode,
      deleteNode,
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
            ~handler=(_: action) => setState(state => !state),
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

let empty: element(node) = empty;

module EmptyComponent = {
  let%component make = ((), hooks) => (empty, hooks);
};

module EmptyComponentWithAlwaysEffect = {
  let%component make =
                (~onEffect: unit => unit, ~onEffectDispose: unit => unit, ()) => {
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
  let%component make =
                (~onEffect: unit => unit, ~onEffectDispose: unit => unit, ()) => {
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

module ShouldAllowComponentProp = {
  let%component make = (~component, (), hooks) => (
    <Div> component </Div>,
    hooks,
  );
};

module LocallyAbstractType: {
  let make:
    (~key: Key.t=?, ~foo: int, ~onA: int => bool, ~onB: bool => unit, unit) =>
    element(node);
} = {
  let%component make =
                (
                  type a,
                  ~foo: a,
                  type b,
                  ~onA: a => b,
                  ~onB: b => unit,
                  (),
                  hooks,
                ) => {
    let b = onA(foo);
    onB(b);
    (empty, hooks);
  };
};
