open TestReconciler;

/**
 * The simplest component. Composes nothing!
 */
let box =
  nativeComponent(
    (~hooks as h, ~title="ImABox", ~onClick as _: option(unit => unit)=?, ()) =>
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

let div =
  nativeComponent((~hooks as h, ~children=[], ()) =>
    (
      h,
      {
        make: () => Implementation.{name: "Div", element: View},
        configureInstance: (~isFirstRender as _, d) => d,
        children: listToElement(children),
      },
    )
  );

let singleChildDiv =
  nativeComponent(
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

type state = {
  current: string,
  prev: string,
};
let text =
  nativeComponent((~hooks, ~title="ImABox", ()) => {
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

let stringToElement = string => <text title=string />;

let boxWrapper =
  component(
    (~hooks as h, ~title="ImABox", ~twoBoxes=false, ~children as _, ()) =>
    (
      h,
      twoBoxes
        ? <div> <box title /> <box title /> </div> : <div> <box title /> </div>,
    )
  );

/**
 * Box with dynamic keys.
 */
let boxItemDynamic =
  component(~useDynamicKey=true, (~hooks as h, ~title="ImABox", ()) =>
    (h, stringToElement(title))
  );

type boxListAction =
  | Create(string)
  | Reverse;

let boxList =
  component((~hooks, ~rAction, ~useDynamicKeys=false, ()) => {
    let (state, dispatch, hooks) =
      Hooks.reducer(
        ~initialState=[],
        (action, state) =>
          switch (action) {
          | Create(title) => [
              useDynamicKeys ? <boxItemDynamic title /> : <box title />,
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

let statelessButton =
  component((~hooks as h, ~initialClickCount as _, ()) =>
    (h, <div />)
  );

let testWrapper = {
  let make =
    component((~hooks, ~wrappedText) =>
      (
        hooks,
        <statelessButton
          initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
        />,
      )
    );
  (~key=?, ~wrappedText="", ()) =>
    element(~key?, make.render(~wrappedText), make);
};

let buttonWrapper =
  component((~hooks, ~wrappedText="default", ()) =>
    (
      hooks,
      <statelessButton
        initialClickCount={"wrapped:" ++ wrappedText ++ ":wrapped"}
      />,
    )
  );

let buttonWrapperJsx = <buttonWrapper wrappedText="TestButtonUpdated!!!" />;
let buttonWrapperWrapper =
  component(
    (~hooks, ~wrappedText="default", ()) =>
    (hooks, <div> {stringToElement(wrappedText)} buttonWrapperJsx </div>)
  );

type updateAlternateClicksAction =
  | Click;
let updateAlternateClicks =
  component(
    (~hooks, ~rAction, ()) => {
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

type toggleClicksAction =
  | Click;
let toggleClicks =
  component(
    (~hooks, ~rAction, ()) => {
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
        <div> <text title="cell1" /> <text title="cell2" /> </div>;
      } else {
        <div> <text title="well" /> </div>;
      },
    );
  });

let emptyComponent =
  component((~hooks, ()) => (hooks, listToElement([])));

let emptyComponentWithAlwaysEffect =
  component(
    (
      ~hooks,
      ~children as _children: list(syntheticElement),
      ~onEffect,
      ~onEffectDispose,
      (),
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
  });

let emptyComponentWithOnMountEffect =
  component(
    (~hooks, ~onEffect, ~onEffectDispose, ()) => {
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
