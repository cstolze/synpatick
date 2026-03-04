# synpatick
Implementation of the LTS of a process algebra with priorities and clocks. OCaml code which compiles to JavaScript.

## How-To
Go to the `bin` folder, and type `dune build ./synpa_js.bc.js`. The generated JavaScript file should be found in `_build/default/bin/synpa_js.bc.js`.

## Dependencies
This code relies on the `dune` ecosystem and the following OCaml libraries and tools (to be installed through the `opam` package manager):
- `angstrom`
- `js_of_ocaml`
- `js_of_ocaml-ppx`
- `js_of_ocaml-compiler`
