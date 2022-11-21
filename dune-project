(lang dune 3.4)

(name panda)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Sheng JianG && Zhizhen Zhu")

(maintainers "Sheng JianG && Zhizhen Zhu")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name panda)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
