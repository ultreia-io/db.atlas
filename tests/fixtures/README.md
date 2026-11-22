# Offline fixture coverage

The vignettes and pkgdown use the survey snapshot in `inst/examples/survey/metadata/`.

`tests/testthat/helper-fixtures.R` provides in-memory metadata for focused unit tests.

The unit tests cover key order, cross-schema dependencies, isolated tables, and optional descriptions.
