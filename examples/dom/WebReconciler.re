/**
  * This is an implementation of a reconciler for DOM elements via js_of_ocaml :
  * http://ocsigen.org/js_of_ocaml/3.1.0/api/Dom_html
  *
  * This is just an example but you could use this to create interesting
  * CLI apps, with a react-like functional API!
*/

exception InvalidNodePrimitiveMatchInUpdateInstance;

let str = string_of_int;

module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;

/*
   Step 1: Define the reconciler template
 */
module Reconciler = {
  type hostElement = Js.t(Dom_html.element);
  type node = Js.t(Dom_html.element);

  let onStale: Event.t(unit) = Event.create();

  let insertNode = (~parent: node, ~child: node, ~position as _) => {
    Dom.appendChild(parent, child);
    parent;
  };

  let deleteNode = (~parent: node, ~child: node) => {
    Dom.removeChild(parent, child);
    parent;
  };

  let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
    parent;
  };

  let markAsStale = () => Event.dispatch(onStale, ());

  let beginChanges = () => ();
  let commitChanges = () => ();
};

/*
    Step 2: Create the actual reconciler using Brisk_reconciler.Make
 */
module JsooReact = Brisk_reconciler.Make(Reconciler);

/*
   Step 3: Define some native components (aka primitives)
 */
let document = Dom_html.window##.document;

module View = {
  let component = JsooReact.nativeComponent("View");

  let make = children => {
    component((_: JsooReact.Hooks.empty) =>
      {
        make: () => {
          let node = Dom_html.createDiv(document);
          node;
        },
        configureInstance: (~isFirstRender as _, node) => {
          node;
        },
        children,
      }
    );
  };

  let createElement = (~children, ()) => {
    JsooReact.element(make(JsooReact.listToElement(children)));
  };
};

module Image = {
  let component = JsooReact.nativeComponent("Image");

  let make = (~src, children) => {
    component((_: JsooReact.Hooks.empty) =>
      {
        make: () => {
          let node = Dom_html.createImg(document);
          node##.src := Js.string(src);
          node |> Dom_html.element;
        },
        configureInstance: (~isFirstRender as _, n) => {
          /* TODO: Proper way to downcast? */
          let node: Js.t(Dom_html.imageElement) = Obj.magic(n);
          node##.src := Js.string(src);
          node |> Dom_html.element;
        },
        children,
      }
    );
  };

  let createElement = (~src, ~children, ()) => {
    JsooReact.element(make(~src, JsooReact.listToElement(children)));
  };
};

module Text = {
  let component = JsooReact.nativeComponent("Text");

  let make = (~text, children) => {
    component((_: JsooReact.Hooks.empty) =>
      {
        make: () => {
          let node = Dom_html.createSpan(document);
          node##.innerHTML := Js.string(text);
          node |> Dom_html.element;
        },
        configureInstance: (~isFirstRender as _, n) => {
          /* TODO: Proper way to downcast? */
          let node: Js.t(Dom_html.imageElement) = Obj.magic(n);
          node##.innerHTML := Js.string(text);
          node |> Dom_html.element;
        },
        children,
      }
    );
  };

  let createElement = (~text, ~children, ()) => {
    JsooReact.element(make(~text, JsooReact.listToElement(children)));
  };
};

module Button = {
  let component = JsooReact.nativeComponent("Button");

  let make = (~onPress, ~title, children) => {
    component((_: JsooReact.Hooks.empty) =>
      {
        make: () => {
          let node =
            Dom_html.createButton(~_type=Js.string("button"), document);
          let t = Js.string(title);
          node##.title := t;
          node##.innerHTML := t;
          node##.onclick :=
            Dom_html.handler(_e => {
              onPress();
              Js.bool(false);
            });
          node |> Dom_html.element;
        },
        configureInstance: (~isFirstRender as _, n) => {
          /* TODO: Proper way to downcast? */
          let node: Js.t(Dom_html.imageElement) = Obj.magic(n);
          let t = Js.string(title);
          node##.title := t;
          node##.innerHTML := t;
          node##.onclick :=
            Dom_html.handler(_e => {
              onPress();
              Js.bool(false);
            });
          node |> Dom_html.element;
        },
        children,
      }
    );
  };

  let createElement = (~onPress, ~title, ~children, ()) => {
    JsooReact.element(
      make(~onPress, ~title, JsooReact.listToElement(children)),
    );
  };
};

/*
    Step 4: Use the reconciler + native elements to create something great!
 */
type action =
  | Increment
  | Decrement;

let reducer = (action, state) =>
  switch (action) {
  | Increment => state + 1
  | Decrement => state - 1
  };

module CounterButtons = {
  let component = JsooReact.component("Calculator");

  let make = () =>
    component(slots => {
      let (count, dispatch, _slots: JsooReact.Hooks.empty) =
        JsooReact.Hooks.reducer(~initialState=0, reducer, slots);
      <View>
        <Button title="Decrement" onPress={() => dispatch(Decrement)} />
        <Text text={"Counter: " ++ str(count)} />
        <Button title="Increment" onPress={() => dispatch(Increment)} />
      </View>;
    });

  let createElement = (~children as _, ()) => JsooReact.element(make());
};

let render = () =>
  <View> <Text text="Hello World" /> <CounterButtons /> </View>;

/*
    Step 5: Make the first render
 */
let rendered =
  JsooReact.RenderedElement.render(
    Dom_html.getElementById_exn("app"),
    render(),
  );
JsooReact.RenderedElement.executeHostViewUpdates(rendered) |> ignore;