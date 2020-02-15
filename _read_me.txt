usethis::use_pkgdown()
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
