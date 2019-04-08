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
module RemoteAction = Brisk_reconciler.RemoteAction;

/*
   Step 1: Define the reconciler template
 */
module Reconciler = {
  type hostElement = Js.t(Dom_html.element);
  type node = Js.t(Dom_html.element);

  let onStale: RemoteAction.t(unit) = RemoteAction.create();

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

  let markAsStale = () => RemoteAction.send(~action=(), onStale);

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

let view = {
  let component = JsooReact.nativeComponent("View");
  (~children, ()) => {
    component(hooks =>
      (
        hooks,
        {
          make: () => {
            let node = Dom_html.createDiv(document);
            node;
          },
          configureInstance: (~isFirstRender as _, node) => {
            node;
          },
          children: JsooReact.listToElement(children),
        },
      )
    );
  };
};

let image = {
  let component = JsooReact.nativeComponent("Image");
  (~src, ~children, ()) => {
    component(hooks =>
      (
        hooks,
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
        },
      )
    );
  };
};

let text = {
  let component = JsooReact.nativeComponent("Text");

  (~text, ~children, ()) => {
    component(hooks =>
      (
        hooks,
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
          children: JsooReact.listToElement(children),
        },
      )
    );
  };
};

let button = {
  let component = JsooReact.nativeComponent("Button");
  (~onPress, ~title, ~children, ()) => {
    component(hooks =>
      (
        hooks,
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
          children: JsooReact.listToElement(children),
        },
      )
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

let action = RemoteAction.create();

let counterButtons = (~children as _, ()) => {
  let component = JsooReact.component("CounterButtons");
  component(hooks => {
    let (count, setCount, hooks) =
      JsooReact.Hooks.state(0, hooks);
    let hooks =
      JsooReact.Hooks.subscribe(~handler=setCount, action, hooks);
    (
      hooks,
      <view>
        <button title="Decrement" onPress={() => setCount(count - 1)} />
        <text text={"Counter: " ++ str(count)} />
        <button
          title="Reset (Race condition)"
          onPress={() =>
            ignore(Js_of_ocaml.Dom_html.setTimeout(() => RemoteAction.send(0, action), 3000.))
          }
        />
        <button title="Increment" onPress={() => setCount(count + 1)} />
      </view>,
    );
  });
};

let render = () =>
  <view> <text text="Hello World" /> <counterButtons /> </view>;

/*
    Step 5: Make the first render
 */
let rendered =
  ref(
    JsooReact.RenderedElement.render(
      Dom_html.getElementById_exn("app"),
      render(),
    ),
  );
JsooReact.RenderedElement.executeHostViewUpdates(rendered^) |> ignore;

RemoteAction.subscribe(
  ~handler=
    () => {
      let nextElement =
        JsooReact.RenderedElement.flushPendingUpdates(rendered^);
      JsooReact.RenderedElement.executeHostViewUpdates(nextElement) |> ignore;
      rendered := nextElement;
    },
  Reconciler.onStale,
);
