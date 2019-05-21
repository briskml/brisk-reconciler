[![Build Status](https://dev.azure.com/briskml/brisk-reconciler/_apis/build/status/briskml.brisk-reconciler?branchName=master)](https://dev.azure.com/briskml/brisk-reconciler/_build/latest?definitionId=2&branchName=master)

# brisk-reconciler

### An easy way to model any `tree-shaped state` with simple `stateful functions`

#### Definitions:
- `tree-shaped state`: Any tree shaped-state like the DOM tree, app navigation state, or even rich text document!
- `stateful functions`: Functions that maintain state over time. Imagine that you can take any variable in your function and manage its value over the function's inocation. Now, imagine that any function invocation really creates its own "instance" of the function which will track this state separately from other invocations of this function.

## Why?

We want to provide expressive and powerful abstractions to build and update trees of dynamic data. We built it with UIs in mind but this library is generally applicable.

We encourage to experiment with this abstraction for different types of outputs. Thanks to `OutputTree`-agnostic Hooks (the mechanism which makes functions stateful), different types of outputs can use the same internal logic for updates or side effects. 

One example of this is Brisk and Revery, where we have two vastly different approaches to building UIs. However, thanks to the shared layer we envision that it'll be very simple to use the two at the same time.

We are building it in Reason. It's a very fast, scalable, and extremely expressive language with an amazing FFI. It has an incredibly powerful type system. Reason is a bridge between the world of mainstream programming and great ideas born in academia. It is an alternative syntax to OCaml so you can use (or even convert between) the two at the same time. 

## Projects which use brisk-reconciler

- [Brisk](https://github.com/briskml/brisk)
- [Revery](https://github.com/revery-ui/revery)

### Examples

- [Lambda_term](examples/lambda-term/Lambda_term.re)
  - `cd examples/lambda-term`
  - `esy`
  - `esy run`

- [DOM](examples/dom/WebReconciler.re)
  - `cd examples/dom`
  - `esy`
  - `open index.html`

## Contributing

### Install [esy](https://esy.sh/)

```
npm install -g esy
```

### Installation using opam

> We currently don't have an opam workflow ready. We're open to contributions!

### Development workflow

Take a look at our [issues](https://github.com/briskml/brisk-reconciler/issues) if you'd like to get an idea where to start. 

- build: `esy`
- test: `esy test`

### Community

Join us on `reason native ui` discord server [here](https://discord.gg/5ANq4EZ).

## License

This project is provided under the [MIT License](LICENSE).
