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
};

let remoteAction = Brisk.RemoteAction.create();

module B = {
  open Brisk;
  let component = nativeComponent("B");

  let createElement = (~depth as _, ~children, ()) => {
    element(
      component((_: Hooks.empty) =>
        {
          make: () => (),
          configureInstance: (~isFirstRender as _, ()) => (),
          children: listToElement(children),
        }
      ),
    );
  };
};
module A = {
  open Brisk;
  open Update;

  let component = component("A");
  let rec make = (~depth, ~index, children) =>
    component(hooks => {
      let (_, dispatch, hooks) =
        Hooks.reducer(
          true,
          ({depth: uDepth, index: uIndex, switchComponent}, state) =>
            if (depth == uDepth && index == uIndex && !switchComponent) {
              !state;
            } else {
              state;
            },
          hooks,
        );
      let _: Hooks.empty =
        Hooks.effect(
          OnMount,
          () =>
            Some(
              Brisk.RemoteAction.subscribe(~handler=dispatch, remoteAction),
            ),
          hooks,
        );

      if (depth < 5) {
        Brisk.listToElement([
          element(make(~depth=depth + 1, ~index=0 + index * 4, [])),
          element(make(~depth=depth + 1, ~index=1 + index * 4, [])),
          element(make(~depth=depth + 1, ~index=2 + index * 4, [])),
          element(make(~depth=depth + 1, ~index=3 + index * 4, [])),
        ]);
      } else {
        <B depth={depth + 1} />;
      };
    });

  let createElement = (~depth, ~index, ~children as _, ()) => {
    element(make(~depth, ~index, []));
  };

  let render = () => {
    Brisk.RenderedElement.render(
      (),
      createElement(~depth=0, ~index=0, ~children=(), ()),
    );
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
        remoteAction,
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
        let renderedElement = A.render() |> Brisk.RenderedElement.executePendingEffects;
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
          [0, 1, 2, 3, 4, 5, 6],
        ),
      ),
    ]),
  );
};

let () = main();
