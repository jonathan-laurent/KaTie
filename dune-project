(lang dune 3.3)
(using menhir 2.1)

(generate_opam_files true)

(authors "Jonathan Laurent")
(maintainers "jonathan.laurent@cs.cmu.edu")

(package
 (name kappa-trace-queries)
 (allow_empty)
 (synopsis "The Kappa Trace Query Language")
 (source (github "jonathan-laurent/Kappa-TQL"))
 (description
  "\| A unified language for querying causal patterns
  "\| in simulation traces of Kappa models
 )
 (depends
  (ANSITerminal (>= 0.8.5))
  (menhir (>= 20220210))
 ))
