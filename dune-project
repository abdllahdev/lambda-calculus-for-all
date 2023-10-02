(lang dune 3.10)

(name simply-lambda)

(generate_opam_files true)

(source
 (github abdllahdev/simply-lambda))

(authors "Abdllah Fathy Elsayed (@abdllahdev)")

(maintainers "Abdllah Fathy Elsayed (@abdllahdev)")

(license MIT)

(package
 (name simply-lambda)
 (synopsis "Lambda calculus tutorials")
 (description "A series of tutorials for lambda calculus")
 (depends ocaml dune core fmt)
 (tags
  ("ocaml" "type theory" "lambda calculus")))
