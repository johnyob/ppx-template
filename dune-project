(lang dune 3.6)

(generate_opam_files true)

(name ppx_template)

(source
 (github johnyob/ppx-template))

(license MIT)

(authors "Alistair O'Brien")

(maintainers "ajob410@gmail.com")

(package
 (name ppx_template)
 (depends
  ocaml
  (dune
   (>= 3.6))
  ppxlib
  core))
