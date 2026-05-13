[![CI](https://github.com/briskml/brisk-reconciler/actions/workflows/build.yml/badge.svg?branch=master)](https://github.com/briskml/brisk-reconciler/actions/workflows/build.yml)

# brisk-reconciler

### An easy way to model any `tree-shaped state` with simple `stateful functions`

#### Definitions:
- `tree-shaped state`: Any tree shaped-state like the DOM tree, app navigation state, or even rich text document!
- `stateful functions`: Functions that maintain state over time. Imagine that you can take any variable in your function and manage its value over the function's invocation. Now, imagine that any function invocation really creates its own "instance" of the function which will track this state separately from other invocations of this function.

## Why?

We want to provide expressive and powerful abstractions to build and update trees of dynamic data. We built it with UIs in mind but this library is generally applicable.

We encourage you to experiment with this abstraction for different types of outputs. Thanks to `OutputTree`-agnostic Hooks (the mechanism which makes functions stateful), different types of outputs can use the same internal logic for updates or side effects.

One example of this is Brisk and Revery, where we have two vastly different approaches to building UIs. However, thanks to the shared layer, we envision that it'll be very simple to use the two at the same time.

## Projects which use brisk-reconciler

- [Brisk](https://github.com/briskml/brisk)
- [Revery](https://github.com/revery-ui/revery)

### Examples

- [Lambda_term](example/lambda-term/lambda_term.mlx)
  - `make run-lambda-term`

- [Web_reconciler](example/web-reconciler/web_reconciler.mlx)
  - `make`
  - `open _build/default/example/web-reconciler/index.html`

## Contributing

### Installation using opam

Create a local opam switch (optional):

```
opam switch create ./ 5.4.0 --deps-only
```

Install dependencies (add `--with-dev-setup` to also pull in formatters, LSP, and benchmark tools):

```
opam install . --with-test --with-doc
```

### Development workflow

Take a look at our [issues](https://github.com/briskml/brisk-reconciler/issues) if you'd like to get an idea where to start.

- build: `make`
- test: `make test`
- bench (requires `--with-dev-setup`): `make bench`

### Community

Join us on `reason native ui` discord server [here](https://discord.gg/5ANq4EZ).

## License

This project is provided under the [MIT License](LICENSE).
