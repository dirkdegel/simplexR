usethis::use_pkgdown()
devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
pkgdown::build_site()
pkgdown::build_reference_index()


pkgdown::build_reference()

navbar:
  type: default
  left:
  - text: Intro
    href: articles/introduction.html
  - text: Reference
    href: reference.html
  - text: Articles
