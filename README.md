# Kappa Trace Query Language

Implementation of the _Trace Query Language_, as
introduced in [this
paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf)
and documented on [this
page](https://www.cs.cmu.edu/~jlaurent/software/katql-documentation.html).

## Build Instructions

- Install [`opam`](https://opam.ocaml.org/doc/Install.html).
- Install the [`Kappa Tools`](https://github.com/Kappa-Dev/KappaTools): `opam pin add https://github.com/Kappa-Dev/KappaTools.git`.
- Install the other dependencies: `dune build @install; opam install --deps-only .`
- Build and install the tool by running `dune build; dune install` at the root of this repo.
- The tool can then be used as `KaTie -t <trace_file> -q <query_file> [options]`.

## Testing Instructions

The `tests` directory contains a collection of example models and queries. A subdirectory is considered as defining a valid test if it features a `model.ka` and a `query.katie` file.

- To run the full test suite, use `python runtests run`.
- To run a specific test (e.g `catphos`), use `python runtests run catphos`.
- To clean all output files, use `python runtests clean`.