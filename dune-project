(lang dune 2.8)
(name dhl)
(version 0.0.0)
(generate_opam_files true)

(source (github sano-jin/dhl-alpha))
(license MIT)
(authors "sano<sano@ueda.info.waseda.ac.jp>")
(maintainers "sano<sano@ueda.info.waseda.ac.jp>")

(package
 (name dhl)
 (synopsis "Directed HyperLMNtal compiler and runtime")
 (description "A minimal compiler and runtime for a language based on graph rewriting")
 (depends
  ; (ounit :with-test)
  (menhir (>= 20210419))
  ; (patdiff (>= 0.14.0))
  (dune (> 1.5))))
(using menhir 2.1)
