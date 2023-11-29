(lang dune 3.11)

(name sliphub)

(generate_opam_files true)

(source
 (github pangled/sliphub))

(authors "Paul-Elliot Anglès d'Auriac")

(maintainers "Paul-Elliot Anglès d'Auriac")

(license GPL-3.0-or-later)

(package
 (name sliphub)
 (synopsis "A website to live edit your slipshow presentations")
 (description
  "A website to live edit your slipshow presentations. Can also be used to remotely store your presentations.")
 (depends ocaml dune js_of_ocaml dream camlot caqti-driver-sqlite3 lwt dream petrol crunch)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
