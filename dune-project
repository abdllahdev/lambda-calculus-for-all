(lang dune 3.10)

(name simply-lambdas)

(generate_opam_files true)

(source
 (github abdllahdev/simply-lambdas))

(authors "Abdullah Fathy Elsayed (@abdllahdev)")

(maintainers "Abdullah Fathy Elsayed (@abdllahdev)")

(license MIT)

(package
 (name simply-lambdas)
 (synopsis "Lambda calculus tutorials")
 (description "A series of tutorials for lambda calculus")
 (depends ocaml dune core fmt)
 (tags
  ("ocaml" "type theory" "lambda calculus")))
