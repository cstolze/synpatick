(lang dune 3.20)

(name synpa)

(generate_opam_files true)

(source
 (github cstolze/synpatick))

(authors "Claude Stolze")

(maintainers "Claude Stolze")

(license GPL-3.0-only)

(documentation https://url/to/documentation)

(package
 (name synpa)
 (synopsis "SynPaTick")
 (description "Proof-of-concept implementation of a process algebra with clocks and priorities")
 (depends (ocaml (>= 4.14)))
 (tags
  (CCS clocks priorities)))
