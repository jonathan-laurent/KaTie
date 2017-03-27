Kappa Trace Query Engine

---

In order to compile, you need to `opam pin --dev add KaSim` [1] and
you can then `make`. This will generate a `query` executable, which
can be used as follows:
`./query -t <your JSON trace file> -q <your query file>`

---

[1] KaSim requires Ocaml >= 4.02.3. Opam will take care of installing
other dependencies.