
<!-- README.md is generated from README.Rmd. Please edit README.Rmd. -->

# db.atlas

[![Build](https://github.com/ultreia-io/db.atlas/actions/workflows/Build.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/db.atlas/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Test](https://github.com/ultreia-io/db.atlas/actions/workflows/Test.yaml/badge.svg?branch=develop)](https://github.com/ultreia-io/db.atlas/actions/workflows/Test.yaml?query=branch%3Adevelop)
[![Website](https://github.com/ultreia-io/db.atlas/actions/workflows/Website.yaml/badge.svg)](https://github.com/ultreia-io/db.atlas/actions/workflows/Website.yaml)
[![Codecov](https://codecov.io/gh/ultreia-io/db.atlas/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/ultreia-io/db.atlas/tree/develop)
[![Latest
release](https://img.shields.io/github/v/release/ultreia-io/db.atlas?display_name=tag&sort=semver)](https://github.com/ultreia-io/db.atlas/releases/latest)
[![License](https://img.shields.io/github/license/ultreia-io/db.atlas)](https://github.com/ultreia-io/db.atlas/blob/main/LICENSE)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

`db.atlas` extracts PostgreSQL catalog metadata, saves portable
snapshots, builds dependency models, and creates neutral offline HTML
schema reports. Applications provide the database connection and
optional branding.

Package website: <https://ultreia-io.github.io/db.atlas/>

## Features

- Ordered simple and composite primary and foreign keys.
- Cross-schema dependencies and reverse usages.
- Portable CSV snapshots for offline documentation.
- Dependency trees and interactive graphs.
- Configurable titles, schemas, colors, links, logos, analytics,
  sections, and templates.
- No database driver, credentials, or application branding in the
  package.

## Installation

``` r
install.packages("pak")
pak::pak("ultreia-io/db.atlas")
```

The package declares `db.tools` through `Remotes`, so GitHub Actions and
local package installers can resolve it without absolute paths.

## Offline usage

``` r
library(db.atlas)

fixture <- system.file("examples", "survey", "metadata", package = "db.atlas")
metadata <- read_metadata(fixture)
model <- build_schema_model(metadata)
model_dependencies(model)
```

    ##    schema        table               foreign_key
    ##    <char>       <char>                    <char>
    ## 1: survey     metadata      metadata_contacts_fk
    ## 2: survey observations  observations_stations_fk
    ## 3: survey observations observations_observers_fk
    ## 4: survey      samples   samples_observations_fk
    ## 5: survey measurements   measurements_samples_fk
    ## 6: survey        notes     notes_observations_fk
    ##                             columns target_schema target_table
    ##                              <list>        <char>       <char>
    ## 1:                    contact_email     catalogue     contacts
    ## 2:                          station     catalogue     stations
    ## 3:                         observer     catalogue    observers
    ## 4:           station,observation_id        survey observations
    ## 5: station,observation_id,sample_no        survey      samples
    ## 6:           station,observation_id        survey observations
    ##                      target_columns            table_id     target_table_id
    ##                              <list>              <char>              <char>
    ## 1:                            email     survey.metadata  catalogue.contacts
    ## 2:                             code survey.observations  catalogue.stations
    ## 3:                             code survey.observations catalogue.observers
    ## 4:           station,observation_id      survey.samples survey.observations
    ## 5: station,observation_id,sample_no survey.measurements      survey.samples
    ## 6:           station,observation_id        survey.notes survey.observations

For live extraction, provide a zero-argument DBI connection provider:

``` r
provider <- function() DBI::dbConnect(RPostgres::Postgres(), dbname = "example")
metadata <- extract_schema_metadata(c("survey", "catalogue"), provider)
write_metadata(metadata, "metadata-snapshot")
```

Render an offline report to a caller-selected directory:

``` r
config <- atlas_report_config(
  title = "Example database",
  output_directory = "schema-report",
  schema_colors = c(survey = "#3B82F6", catalogue = "#F59E0B")
)
render_schema_report(model, config)
generate_report_indexes(config$output_directory)
```

## `db.tools` dependency

The package delegates query execution to `db.tools::db_query()`,
connection ownership to `db.tools::with_db_connection()`, and packaged
SQL loading to `db.tools::read_sql_file()`. It does not duplicate those
helpers and imports no database driver.

## Development

``` r
devtools::load_all()
devtools::build_readme()
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()
```

See [CONTRIBUTING.md](CONTRIBUTING.md) and
[DEVELOPMENT.md](DEVELOPMENT.md).

## License

GNU Lesser General Public License, version 2.1 (LGPL-2.1), matching
`db.tools`. See [LICENSE](LICENSE).
