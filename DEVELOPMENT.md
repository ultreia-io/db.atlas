# Maintainer guide

Work on `develop`. `main` contains releases.

## Code organization

Each R file groups one area of the package, with public functions above the internal helpers.

| File                  | Responsibility                                        |
|-----------------------|-------------------------------------------------------|
| `metadata.R`          | PostgreSQL extraction, snapshots, and comments.       |
| `schema-model.R`      | Schema models, table accessors, and dependency trees. |
| `reports.R`           | Report configuration, HTML rendering, and indexes.    |
| `dependency-graphs.R` | Graph data and interactive dependency graphs.         |
| `db.atlas-package.R`  | Package documentation and shared resource lookup.     |

## Example metadata

The runnable guides use the survey snapshot in `inst/examples/survey/metadata/`.

The PostgreSQL queries and report templates are under `inst/sql/` and `inst/templates/`.

## Local verification

Run from the package root with development dependencies, Pandoc and LaTeX installed.

```sh
Rscript tools/check.R
```

This generates documentation, runs tests, checks the archive with `--as-cran` and builds the website.

Local and CI checks share the CRAN mirror settings in `tools/check.Renviron`.

The **Build** workflow checks Linux, macOS and Windows and uploads Linux release coverage to Codecov.

Archives and check output stay in `build/`.

## Website

Website builds use `pkgdown` and `zip`, declared in `Config/Needs/website`.

Run these commands in R from the package root to build the website by itself.

```r
# Prepare downloadable files from the package sources.
source("tools/prepare-site-assets.R")
# Build the website with pkgdown.
pkgdown::build_site()
```

## GitHub release

1. Set the release version in `DESCRIPTION` and finalize the matching `NEWS.md` heading.
2. Run the local checks, then commit and push the release changes on `develop`.
3. Run **Actions → Start Release** on `develop` to open a release pull request.
4. Wait for its checks to pass, then merge the pull request into `main`.

**Publish Release** checks the merged commit, uploads coverage and publishes the GitHub archive and checksum.

The website workflow runs from `develop` and builds the same release commit.

## License

LGPL-2.1. See [LICENSE](LICENSE).
