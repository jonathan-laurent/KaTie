# Kappa Trace Query Language

Implementation of the _Trace Query Language_, as
introduced in [this
paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf)
and documented on [this
page](https://www.cs.cmu.edu/~jlaurent/software/katql-documentation.html).

## Build Instructions

- Install [`opam`](https://opam.ocaml.org/doc/Install.html).
- Install the [`Kappa Tools`](https://github.com/Kappa-Dev/KappaTools): `opam pin add https://github.com/Kappa-Dev/KappaTools.git`.
- Install other dependencies: `opam install ANSITerminal`.
- Build the tool by running `dune build` at the root of this repo.