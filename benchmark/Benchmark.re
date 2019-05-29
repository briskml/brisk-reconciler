open Core_bench.Std;

let numberOfChildren = 4;
let totalNodesAtDepth = depth => Core.Int.(numberOfChildren ** depth);

module Update = {
  type t = {
    depth: int,
    index: int,
    switchComponent: bool,
  };

  let random = (~depth, ~switchComponent) => {
    {
      depth,
      index: Core.Random.int(totalNodesAtDepth(depth)),
      switchComponent,
    };
  };

  let remoteAction: Brisk.RemoteAction.t(t) = Brisk.RemoteAction.create();
};

module Unit = {
  open Brisk;

  let make =
    nativeComponent("B", (hooks, ~depth as _, ~children) =>
      (
        hooks,
        {
          make: () => (),
          configureInstance: (~isFirstRender as _, ()) => (),
          children: listToElement(children),
        },
      )
    );
  let createElement = (~depth, ~children, ()) =>
    element(make.render(~depth, ~children), make);
};
module A = {
  open Brisk;
  open Update;

  module ComponentType = {
    type t =
      | A
      | A'
      | B
      | B';

    let toggle =
      fun
      | A
      | A' => B
      | B
      | B' => A;

    let dummyUpdate =
      fun
      | A => A'

      | A' => A
      | B => B'
      | B' => B;
  };

  let list4MapIndexExn = (~f, ~index, l) =>
    switch (l) {
    | [a, b, c, d] =>
      switch (index) {
      | 0 => [f(a), b, c, d]
      | 1 => [a, f(b), c, d]
      | 2 => [a, b, f(c), d]
      | 3 => [a, b, c, f(d)]
      | _ => raise(Invalid_argument("Index too big"))
      }
    | _ => raise(Invalid_argument("List should have 4 elements"))
    };

  let rec componentDefinition = (hooks, ~make, ~depth, ~index) => {
    let childrenIndexOffset = index * 4;
    let childrenDepth = depth + 1;
    let (children, dispatch, hooks) =
      Hooks.reducer(
        ComponentType.[A, A, A, A],
        ({depth, index, switchComponent}, state) => {
          let mappedIndex = index - childrenIndexOffset;
          if (childrenDepth == depth && mappedIndex >= 0 && mappedIndex <= 3) {
            if (switchComponent) {
              list4MapIndexExn(
                ~f=ComponentType.toggle,
                ~index=mappedIndex,
                state,
              );
            } else {
              list4MapIndexExn(
                ~f=ComponentType.dummyUpdate,
                ~index=mappedIndex,
                state,
              );
            };
          } else {
            state;
          };
        },
        hooks,
      );
    let hooks =
      Hooks.effect(
        OnMount,
        () =>
          Some(
            Brisk.RemoteAction.subscribe(~handler=dispatch, remoteAction),
          ),
        hooks,
      );

    if (depth < 5) {
      (
        hooks,
        Brisk.listToElement(
          List.mapi(
            (i, c) =>
              make(c, ~depth=depth + 1, ~index=i + childrenIndexOffset, []),
            children,
          ),
        ),
      );
    } else {
      (hooks, <Unit depth={depth + 1} />);
    };
  };

  let componentA = component("A", componentDefinition);
  let componentB = component("B", componentDefinition);

  let rec make = (componentType, ~depth, ~index, _children: list(unit)) =>
    switch (componentType) {
    | ComponentType.A
    | A' => element(componentA.render(~make, ~depth, ~index), componentA)
    | B
    | B' => element(componentB.render(~make, ~depth, ~index), componentB)
    };

  let makeA = make(ComponentType.A);
  let makeB = make(ComponentType.B);

  let render = () => {
    Brisk.RenderedElement.render((), makeA(~depth=0, ~index=0, []));
  };
};

let benchUpdateAtDepth = (depth, ~switchComponent) =>
  Bench.Test.create_with_initialization(
    ~name="Depth: " ++ string_of_int(depth),
    (`init) => {
      let renderedElement =
        A.render() |> Brisk.RenderedElement.executePendingEffects;
      Brisk.RemoteAction.send(
        ~action=Update.random(~depth, ~switchComponent),
        Update.remoteAction,
      );
      () => {
        Brisk.RenderedElement.flushPendingUpdates(renderedElement);
      };
    },
  );

let main = () => {
  Random.self_init();
  Core.Command.run(
    Bench.make_command([
      Bench.Test.create(~name="First render", A.render),
      Bench.Test.create_with_initialization(
        ~name="Execute mount effects", (`init) => {
        let renderedElement = A.render();
        () => Brisk.RenderedElement.executePendingEffects(renderedElement);
      }),
      Bench.Test.create_with_initialization(
        ~name="Execute host view updates", (`init) => {
        let renderedElement =
          A.render() |> Brisk.RenderedElement.executePendingEffects;
        () => Brisk.RenderedElement.executeHostViewUpdates(renderedElement);
      }),
      Bench.Test.create_with_initialization(
        ~name="noop flushPendingUpdates", (`init) => {
        let renderedElement = A.render();
        () => {
          Brisk.RenderedElement.flushPendingUpdates(renderedElement);
        };
      }),
      Bench.Test.create_group(
        ~name="State update without subtree change",
        List.map(
          benchUpdateAtDepth(~switchComponent=false),
          [0, 1, 2, 3, 4, 5],
        ),
      ),
      Bench.Test.create_group(
        ~name="State update with subtree change",
        List.map(
          benchUpdateAtDepth(~switchComponent=true),
          [0, 1, 2, 3, 4, 5],
        ),
      ),
    ]),
  );
};

let () = main();
