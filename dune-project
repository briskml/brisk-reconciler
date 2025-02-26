(lang dune 3.16)

(name brisk-reconciler)

(source
 (github briskml/brisk-reconciler))

(license MIT)

(version 1.0.0-alpha.1)

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
 (depends
  (ocaml (>= 4.8.0))
  ppxlib
  (mlx :with-test)
  (ocamlformat-mlx :with-dev)
  (ocamlformat :with-dev)
  (ocaml-lsp-server :with-dev)
  (alcotest :with-test)
  (ppx_deriving :with-test)
  (js_of_ocaml :with-test)
  (js_of_ocaml-ppx :with-test)
  (lwt :with-test)
  (lambda-term :with-test)
  (core_bench :with-test)))
