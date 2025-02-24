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

open Brisk_reconciler;

/*
   Step 1: Define the reconciler template
 */
type hostElement = Js.t(Dom_html.element);
type node = Js.t(Dom_html.element);

let onStale: RemoteAction.t(unit) = RemoteAction.create();

let insertNode = (~parent: node, ~child: node, ~position as _) => {
  Dom.appendChild(parent, child);
  parent;
};

let deleteNode = (~parent: node, ~child: node, ~position as _) => {
  Dom.removeChild(parent, child);
  parent;
};

let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
  parent;
};

Brisk_reconciler.addStaleTreeHandler(() =>
  RemoteAction.send(~action=(), onStale)
);

/*
   Step 2: Define some native components (aka primitives)
 */
let document = Dom_html.window##.document;

let%nativeComponent view = (~children, (), hooks) => (
  {
    make: () => {
      let node = Dom_html.createDiv(document);
      node;
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent image = (~src, ~children, (), hooks) => (
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
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent text = (~text, (), hooks) => (
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
    children: empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent button = (~onPress, ~title, (), hooks) => (
  {
    make: () => {
      let node = Dom_html.createButton(~_type=Js.string("button"), document);
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
    children: empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

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

let%component counterButtons = () => {
  let%hook (count, setCount) = Hooks.state(0);
  RemoteAction.subscribe(
    ~handler=n => setCount(_ => n), action, ()) |> ignore;
  <view>
    <button title="Decrement" onPress={() => setCount(count => count - 1)} />
    <text text={"Counter: " ++ str(count)} />
    <button
      title="Reset (Race condition)"
      onPress={() =>
        ignore(
          Js_of_ocaml.Dom_html.setTimeout(
            () => RemoteAction.send(~action=0, action),
            3000.,
          ),
        )
      }
    />
    <button title="Increment" onPress={() => setCount(count => count + 1)} />
  </view>;
};

let render = () =>
  <view> <text text="Hello World" /> <counterButtons /> </view>;

/*
    Step 5: Make the first render
 */
let rendered =
  ref(
    RenderedElement.render(
      {
        node: Dom_html.getElementById_exn("app"),
        insertNode,
        deleteNode,
        moveNode,
      },
      render(),
    ),
  );
RenderedElement.executeHostViewUpdates(rendered^) |> ignore;

RemoteAction.subscribe(
  ~handler=
    () => {
      let nextElement = RenderedElement.flushPendingUpdates(rendered^);
      RenderedElement.executeHostViewUpdates(nextElement) |> ignore;
      rendered := nextElement;
    },
  onStale,
);
