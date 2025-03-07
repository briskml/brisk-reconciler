(lang dune 3.16)

(name brisk-reconciler)

(source
 (github briskml/brisk-reconciler))

(license MIT)

(generate_opam_files true)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (format
   (run ocamlformat-mlx %{input-file}))
  (preprocess
   (run mlx-pp %{input-file}))))

(package
 (name brisk-reconciler)
 (synopsis "A lightweight library for modeling tree-shaped state with stateful functions")
 (description "\
Brisk Reconciler provides an expressive and powerful abstraction for managing **tree-shaped state** using **stateful functions**. This makes it ideal for modeling dynamic structures like the **DOM tree**, **app navigation state**, or even a **rich text document**.

### ✨ **Key Features**
- **Tree-shaped state modeling**: Represent and update hierarchical structures efficiently.
- **OutputTree-agnostic hooks**: Support different output representations.
- **UI-oriented, but flexible**: Designed with UIs in mind but applicable to other domains.")
 (depends
  ;; OCaml 5.3 reserves the keyword `effect` used in the library
  (ocaml (and (>= 4.8.0) (< 5.3.0)))
  (ppxlib (and (>= 0.12.0) (< 0.36.0)))
  (mlx :with-test)
  (ocamlformat-mlx :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)
  (alcotest :with-test)
  (ppx_deriving :with-test)
  (js_of_ocaml :with-test)
  (js_of_ocaml-ppx :with-test)
  (lwt :with-test)
  (lambda-term :with-test)
  (core_bench :with-test)
  (core_unix :with-test)))
