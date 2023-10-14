(lang dune 3.11)

(name sitegen)

(generate_opam_files true)

(source
 (github keyle/site-gen-ocaml))

(authors "keyle")

(maintainers "keyle")

(license LICENSE)

(documentation https://github.com/keyle/site-gen-ocaml/blob/master/README.md)

(package
 (name sitegen)
 (synopsis "Site generator")
 (description "Static website generator that takes a bunch of markdown files 
     and replaces them in situe with html files, including formatting, 
     blog capabilities (static, no comments), date handling, metadata, sitemap.xml, and rss feed")
 (depends ocaml dune)
 (tags
  ("site generator" "markdown to html" "static site generator" "static blog generator")))
