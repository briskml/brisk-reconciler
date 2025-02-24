(lang dune 3.16)

(name brisk-reconciler)

(source
 (github brisk/brisk-reconciler))

(license MIT)

(version 1.0.0-alpha.0)

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
  (ocaml
   (>= 4.8.0))
  (reason
   (>= 3.4.0))
  ppxlib))

(package
 (name brisk-reconciler-ocaml)
 (depends
  (alcotest :with-test)
  (mlx :with-dev)
  (ocamlformat-mlx :with-dev)))
