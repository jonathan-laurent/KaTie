(lang dune 3.3)
(using menhir 2.1)

(generate_opam_files true)

(authors "Jonathan Laurent")
(maintainers "jonathan.laurent@cs.cmu.edu")

(package
 (name kappa-tql)
 (allow_empty)
 (synopsis "The Kappa Trace Query Language")
 (source (github "jonathan-laurent/Kappa-TQL"))
 (description
  "\| A utility to examine and query trace files generated
  "\| by the Kappa simulator.
 )
 (depends
  (ANSITerminal (>= 0.8.5))
  (menhir (>= 20220210))
 ))
