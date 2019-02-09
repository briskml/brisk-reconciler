open Lwt;
open LTerm_widget;

/**
  * This is an implementation of a reconciler for the Lambda_term widget library:
  * https://github.com/ocaml-community/lambda-term
  *
  * This is just an example but you could use this to create interesting
  * CLI apps, with a react-like functional API!
*/
/*
   Step 1: Define the reconciler template
 */
module Reconciler = {
  type hostElement =
    | Label(LTerm_widget.label)
    | Button(LTerm_widget.button)
    | Container(LTerm_widget.box);
  type node = hostElement;

  let onStale: EventLambda.t(unit) = EventLambda.create();

  let insertNode = (~parent: node, ~child: node, ~position as _) => {
    switch (parent, child) {
    | (Container(box), Label(child)) => box#add(child)
    | (Container(box), Button(child)) => box#add(child)
    | (Container(box), Container(child)) => box#add(child)
    | _ => ()
    };
    parent;
  };

  let deleteNode = (~parent: node, ~child: node) => {
    switch (parent, child) {
    | (Container(box), Label(child)) => box#remove(child)
    | (Container(box), Button(child)) => box#remove(child)
    | (Container(box), Container(child)) => box#remove(child)
    | _ => ()
    };
    parent;
  };

  let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
    parent;
  };

  let markAsStale = () => EventLambda.dispatch(onStale, ());

  let beginChanges = () => ();
  let commitChanges = () => ();
};

/*
    Step 2: Create the actual reconciler using Brisk_reconciler.Make
 */
module LambdaReact = Brisk_reconciler.Make(Reconciler);

/*
   Step 3: Define some native components (aka primitives)
 */
let hbox = {
  let component = LambdaReact.nativeComponent("Hbox");

  (~children, ()) => {
    component(hooks =>
      (
        hooks,
        {
          make: () => {
            let node = new LTerm_widget.hbox;
            Container(node);
          },
          configureInstance: (~isFirstRender as _, node) => {
            node;
          },
          children: LambdaReact.listToElement(children),
        },
      )
    );
  };
};

let vbox = {
  let component = LambdaReact.nativeComponent("Vbox");
  (~children, ()) => {
    component(hooks =>
      (
        hooks,
        {
          make: () => {
            let node = new LTerm_widget.vbox;
            Container(node);
          },
          configureInstance: (~isFirstRender as _, node) => {
            node;
          },
          children: LambdaReact.listToElement(children),
        },
      )
    );
  };
};

let label = {
  let component = LambdaReact.nativeComponent("Label");

  (~text, ~children, ()) => {
    component(hooks =>
      (
        hooks,
        {
          make: () => {
            let node = (new LTerm_widget.label)(text);
            Label(node);
          },
          configureInstance: (~isFirstRender as _, node) => {
            switch (node) {
            | Label(n) => n#set_text(text)
            | _ => () /* Should never happen */
            };
            node;
          },
          children: LambdaReact.listToElement(children),
        },
      )
    );
  };
};

let button = {
  let component = LambdaReact.nativeComponent("Button");

  (~text, ~onClick, ~children, ()) => {
    component(hooks =>
      (
        hooks,
        {
          make: () => {
            let button = (new button)(text);
            button#on_click(onClick);
            Button(button);
          },
          configureInstance: (~isFirstRender as _, node) => {
            switch (node) {
            | Button(n) =>
              n#set_label(text);
              n#on_click(onClick);
            | _ => () /* Should never happen */
            };
            node;
          },
          children: LambdaReact.listToElement(children),
        },
      )
    );
  };
};

/*
    Step 4: Use the reconciler + native elements to create something great!
 */

/*
    CounterButtons

    This shows how you can use reducers with a callback from a primitive.
 */
type action =
  | Increment
  | Decrement;

let reducer = (action, state) =>
  switch (action) {
  | Increment => state + 1
  | Decrement => state - 1
  };

let counterButtons = {
  let component = LambdaReact.component("CounterButtons");

  (~children as _: list(unit), ()) =>
    component(hooks => {
      let (count, dispatch, hooks) =
        LambdaReact.Hooks.reducer(~initialState=0, reducer, hooks);
      (
        hooks,
        <hbox>
          <button text="Decrement" onClick={() => dispatch(Decrement)} />
          <label text={"Counter: " ++ string_of_int(count)} />
          <button text="Increment" onClick={() => dispatch(Increment)} />
        </hbox>,
      );
    });
};

/*
    Clock

    Custom clock component to show the time. Demonstrates
    use of `useEffect` and `setState` together.
 */

let clock = {
  let component = LambdaReact.component("Clock");
  (~children as _: list(unit), ()) =>
    component(hooks => {
      let (time, setTime, hooks) = LambdaReact.Hooks.state(0., hooks);
      let hooks =
        LambdaReact.Hooks.effect(
          LambdaReact.Hooks.Effect.Always,
          () => {
            let evt =
              Lwt_engine.on_timer(1.0, true, _ => setTime(Unix.time()));
            Some(() => Lwt_engine.stop_event(evt));
          },
          hooks,
        );
      (hooks, <label text={"Time: " ++ string_of_float(time)} />);
    });
};

/*
    Step 5: Make the first render
 */
let main = () => {
  let (waiter, wakener) = wait();

  let quit = () => wakeup(wakener, ());

  /* Let's finally put our UI to use! */
  let render = () =>
    <vbox>
      <label text="Hello World!" />
      <clock />
      <counterButtons />
      <button onClick=quit text="Quit" />
    </vbox>;

  /* Create a container for our UI */
  let body = new vbox;
  let root = Reconciler.Container(body);
  let rendered = ref(LambdaReact.RenderedElement.render(root, render()));
  LambdaReact.RenderedElement.executeHostViewUpdates(rendered^) |> ignore;

  let _unsubscribe = EventLambda.subscribe(
    Reconciler.onStale,
    () => {
      let nextElement =
        LambdaReact.RenderedElement.flushPendingUpdates(rendered^);
      LambdaReact.RenderedElement.executeHostViewUpdates(rendered^) |> ignore;
      rendered := nextElement;
    },
  );
  LambdaReact.RenderedElement.executeHostViewUpdates(rendered^) |> ignore;

  Lazy.force(LTerm.stdout)
  >>= (
    term =>
      LTerm.enable_mouse(term)
      >>= (
        () =>
          Lwt.finalize(
            () => run(term, body, waiter),
            () => LTerm.disable_mouse(term),
          )
      )
  );
};

let () = Lwt_main.run(main());