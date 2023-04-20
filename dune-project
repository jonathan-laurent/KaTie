(lang dune 3.3)
(using menhir 2.1)

(generate_opam_files true)

(name kappa-trace-queries)
(authors "Jonathan Laurent")
(maintainers "jonathan.laurent@cs.cmu.edu")

(package
 (name kappa-trace-queries)
 (version 0.3)
 (allow_empty)
 (synopsis "The Kappa Trace Query Language")
 (source (github "jonathan-laurent/KaTie"))
 (description
  "\| A unified language for querying causal patterns
  "\| in simulation traces of Kappa models
 )
 (depends
  (ANSITerminal (>= 0.8.5))
  (fmt (>= 0.9.0))
  (ppx_yojson_conv_lib (>= 0.15.0))
  (ppx_yojson_conv (>= 0.15.1))
  (ppx_deriving (>= 5.2.1))
  (menhir (>= 20220210))
 ))
