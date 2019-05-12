open TestReconciler;

/**
 * The simplest component. Composes nothing!
 */
module Box = {
  let make =
    nativeComponent(
      "Box",
      (
        ~hooks as h,
        ~title="ImABox",
        ~onClick as _: option(unit => unit)=?,
        ~children as _children: list(syntheticElement),
        (),
      ) =>
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
  let createElement = (~key=?, ~title="ImABox", ~onClick=?, ~children, ()) => {
    element(make(~key?, ~title, ~onClick?, ~children, ()));
  };
};

module Div = {
  let make =
    nativeComponent("Div", (~hooks as h, ~children, ()) =>
      (
        h,
        {
          make: () => Implementation.{name: "Div", element: View},
          configureInstance: (~isFirstRender as _, d) => d,
          children: listToElement(children),
        },
      )
    );
  let createElement = (~key=?, ~children, ()) => element(make(~key?, ~children, ()));
};

module SingleChildDiv = {
  let make =
    nativeComponent(
      "SingleChildDiv",
      (~hooks as h, ~children as child: syntheticElement, ()) =>
      (
        h,
        {
          make: () => Implementation.{name: "SingleChildDiv", element: View},
          configureInstance: (~isFirstRender as _, d) => d,
          children: child,
        },
      )
    );
  let createElement = (~children, ()) => element(make(~children, ()));
};

module Text = {
  type state = {
    current: string,
    prev: string,
  };
  let make =
    nativeComponent(
      "Text", (~hooks, ~title="ImABox", ~children as _children: list(syntheticElement), ()) => {
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
    });
  let createElement = (~key=?, ~title=?, ~children, ()) =>
    element(make(~key?, ~title?, ~children, ()));
};

let stringToElement = string => <Text title=string />;

module BoxWrapper = {
  let make =
    component(
      "BoxWrapper",
      (~hooks as h, ~title="ImABox", ~twoBoxes=false, ~children as _, ()) =>
      (
        h,
        twoBoxes
          ? <Div> <Box title /> <Box title /> </Div>
          : <Div> <Box title /> </Div>,
      )
    );
  let createElement =
      (
        ~title=?,
        ~twoBoxes=?,
        ~onClick as _: option(unit => unit)=?,
        ~children: list(syntheticElement),
        (),
      ) =>
    element(make(~title?, ~twoBoxes?, ~children, ()));
};

/**
 * Box with dynamic keys.
 */
module BoxItemDynamic = {
  let make =
    component(
      ~useDynamicKey=true,
      "BoxItemDynamic",
      (~hooks as h, ~title="ImABox", ~children as _: list(syntheticElement), ()) =>
      (h, stringToElement(title))
    );
  let createElement = (~title=?, ~children, ()) =>
    element(make(~title?, ~children, ()));
};

module BoxList = {
  type action =
    | Create(string)
    | Reverse;
  let make =
    component(
      "BoxList", (~hooks, ~rAction, ~useDynamicKeys=false, ~children as _: list(syntheticElement), ()) => {
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
  let createElement = (~rAction, ~useDynamicKeys=?, ~children, ()) =>
    element(make(~rAction, ~useDynamicKeys?, ~children, ()));
};

module StatelessButton = {
  let make =
    component(
      "StatelessButton",
      (~hooks as h, ~initialClickCount as _, ~children as _: list(syntheticElement), ()) =>
      (h, <Div />)
    );
  let createElement = (~children, ~initialClickCount, ()) =>
    element(make(~children, ~initialClickCount, ()));
};

let testWrapper = {
  let make =
    component("TestWrapper", (~hooks, ~wrappedText="default", _children) =>
      (
        hooks,
        <StatelessButton
          initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
        />,
      )
    );
  (~wrappedText=?, ()) => element(make(~wrappedText?, ()));
};

module ButtonWrapper = {
  let make =
    component(
      "ButtonWrapper", (~hooks, ~wrappedText="default", ~children as _: list(syntheticElement), ()) =>
      (
        hooks,
        <StatelessButton
          initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
        />,
      )
    );
  let createElement = (~wrappedText=?, ~children, ()) =>
    element(make(~wrappedText?, ~children, ()));
};

module ButtonWrapperWrapper = {
  let buttonWrapperJsx = <ButtonWrapper wrappedText="TestButtonUpdated!!!" />;
  let make =
    component(
      "ButtonWrapperWrapper",
      (~hooks, ~wrappedText="default", ~children as _: list(syntheticElement), ()) =>
      (hooks, <Div> {stringToElement(wrappedText)} buttonWrapperJsx </Div>)
    );
  let createElement = (~wrappedText=?, ~children, ()) =>
    element(make(~wrappedText?, ~children, ()));
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  let make =
    component("UpdateAlternateClicks", (~hooks, ~rAction, ~children as _: list(syntheticElement), ()) => {
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
  let createElement = (~rAction, ~children, ()) =>
    element(make(~rAction, ~children, ()));
};

module ToggleClicks = {
  type action =
    | Click;
  let make =
    component("ToggleClicks", (~hooks, ~rAction, ~children as _: list(syntheticElement), ()) => {
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
    });
  let createElement = (~rAction, ~children, ()) =>
    element(make(~rAction, ~children, ()));
};

module EmptyComponent = {
  let make =
    component("Box", (~hooks, ~children as _children: list(syntheticElement), ()) =>
      (hooks, listToElement([]))
    );
  let createElement = (~children, ()) => element(make(~children, ()));
};

module EmptyComponentWithAlwaysEffect = {
  let make =
    component(
      "Box", (~hooks, ~children as _children: list(syntheticElement), ~onEffect, ~onEffectDispose, ()) => {
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
  let createElement = (~children, ~onEffect, ~onEffectDispose, ()) =>
    element(make(~children, ~onEffect, ~onEffectDispose, ()));
};

module EmptyComponentWithOnMountEffect = {
  let make =
    component(
      "Box", (~hooks, ~children as _children: list(syntheticElement), ~onEffect, ~onEffectDispose, ()) => {
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
  let createElement = (~children, ~onEffect, ~onEffectDispose, ()) =>
    element(make(~children, ~onEffect, ~onEffectDispose, ()));
};
