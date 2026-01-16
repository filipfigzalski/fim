(lang dune 3.20)

(name fim)

(generate_opam_files true)

(source
 (github filipfigzalski/fim))

(authors "Filip Figzał <filip@figzal.com>")

(maintainers "Filip Figzał <filip@figzal.com>")

(license MIT)

(package
 (name fim)
 (synopsis "Functional vim clone.")
 (depends ocaml)
 (tags
  ("vim")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
