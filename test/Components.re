open TestReconciler;

/**
 * The simplest component. Composes nothing!
 */
module Box = {
  let createElement =
    nativeComponent(
      "Box",
      (
        ~hooks as h,
        ~title="ImABox",
        ~onClick as _=?,
        ~children as _children,
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
};

module Div = {
  let createElement =
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
};

module SingleChildDiv = {
  let createElement =
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
};

module Text = {
  type state = {
    current: string,
    prev: string,
  };
  let createElement =
    nativeComponent(
      "Text", (~hooks, ~title="ImABox", ~children as _children, ()) => {
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
};

let stringToElement = string => <Text title=string />;

module BoxWrapper = {
  let createElement =
    component(
      "BoxWrapper",
      (
        ~hooks as h,
        ~title="ImABox",
        ~twoBoxes=false,
        ~onClick as _=?,
        ~children as _,
        (),
      ) =>
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
  let createElement =
    component(
      ~useDynamicKey=true,
      "BoxItemDynamic",
      (~hooks as h, ~title="ImABox", ~children as _, ()) =>
      (h, stringToElement(title))
    );
};

module BoxList = {
  type action =
    | Create(string)
    | Reverse;
  let createElement =
    component(
      "BoxList", (~hooks, ~rAction, ~useDynamicKeys=false, ~children as _, ()) => {
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
  let createElement =
    component(
      "StatelessButton",
      (
        ~hooks as h,
        ~initialClickCount as _="noclicks",
        ~test as _="default",
        ~children as _,
        (),
      ) =>
      (h, <Div />)
    );
};

let testWrapper =
  component("TestWrapper", (~hooks, ~wrappedText="default", _children) =>
    (
      hooks,
      <StatelessButton
        initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
      />,
    )
  );

module ButtonWrapper = {
  let createElement =
    component(
      "ButtonWrapper", (~hooks, ~wrappedText="default", ~children as _, ()) =>
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
  let createElement =
    component(
      "ButtonWrapperWrapper",
      (~hooks, ~wrappedText="default", ~children as _, ()) =>
      (hooks, <Div> {stringToElement(wrappedText)} buttonWrapperJsx </Div>)
    );
};

module UpdateAlternateClicks = {
  type action =
    | Click;
  let createElement =
    component("UpdateAlternateClicks", (~hooks, ~rAction, ~children as _, ()) => {
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
  let createElement =
    component("ToggleClicks", (~hooks, ~rAction, ~children as _, ()) => {
      let (state, setState, hooks) = Hooks.state(false, hooks);
      let hooks =
        Hooks.effect(
          Always,
          () =>
            Some(
              RemoteAction.subscribe(
                ~handler=_ => setState(!state),
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
};

module EmptyComponent = {
  let createElement =
    component("Box", (~hooks, ~children as _children, ()) =>
      (hooks, listToElement([]))
    );
};

module EmptyComponentWithAlwaysEffect = {
  let createElement =
    component(
      "Box", (~hooks, ~children as _children, ~onEffect, ~onEffectDispose, ()) => {
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
  let createElement =
    component(
      "Box", (~hooks, ~children as _children, ~onEffect, ~onEffectDispose, ()) => {
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
