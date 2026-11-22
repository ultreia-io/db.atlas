# db.atlas

[![Build](https://img.shields.io/github/actions/workflow/status/ultreia-io/db.atlas/Build.yaml?branch=develop&label=Build&logo=githubactions&logoColor=white)](https://github.com/ultreia-io/db.atlas/actions/workflows/Build.yaml?query=branch%3Adevelop)
[![Website](https://img.shields.io/badge/website-documentation-blue?logo=readthedocs&logoColor=white)](https://ultreia-io.github.io/db.atlas/index.html)
[![License](https://img.shields.io/badge/license-LGPL--2.1-blue?logo=gnu&logoColor=white)](https://github.com/ultreia-io/db.atlas/blob/main/LICENSE)

[![Latest release](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fapi.github.com%2Frepos%2Fultreia-io%2Fdb.atlas%2Freleases%2Flatest&query=%24.tag_name&label=Latest%20release&color=blue&logo=github&logoColor=white)](https://github.com/ultreia-io/db.atlas/releases/latest)
![CRAN release](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fcrandb.r-pkg.org%2Fdb.atlas&query=%24.Version&label=CRAN%20release&color=brightgreen&logo=r&logoColor=white)

[![Coverage develop](https://img.shields.io/codecov/c/github/ultreia-io/db.atlas/develop?label=coverage%20develop&logo=codecov)](https://app.codecov.io/gh/ultreia-io/db.atlas/tree/develop)
[![Coverage main](https://img.shields.io/codecov/c/github/ultreia-io/db.atlas/main?label=coverage%20main&logo=codecov)](https://app.codecov.io/gh/ultreia-io/db.atlas/tree/main)

## About this project

> `db.atlas` turns PostgreSQL schema metadata into dependency models and offline HTML documentation.

Read the [Terminology][terminology] for the concepts used throughout this documentation.

Applications supply a database connection provider for extraction and can save the metadata as a CSV snapshot.

The snapshot carries the schema, table and column descriptions, primary keys and foreign keys.

## Features

**Metadata extraction and snapshots**

- Extract PostgreSQL metadata using an application-supplied DBI connection provider.
- Preserve ordered primary and foreign key columns, including composite keys.
- Save and load portable CSV snapshots for offline use.

**Dependency models**

- Inspect schemas, tables, columns, dependencies and reverse usages.
- Follow relationships across schemas through dependency trees and interactive graphs.

**Reports and descriptions**

- Render HTML reports with configurable schema groups, colors, links and templates.
- Parse multilingual descriptions and generate SQL to apply stored descriptions to PostgreSQL.

## Related packages

- [db.tools](https://github.com/ultreia-io/db.tools) provides connection handling and SQL execution.

## Installation

Once available on CRAN, install the latest version:

``` r
# Install db.atlas from CRAN.
install.packages("db.atlas")
```

Alternatively, install the latest GitHub release:

``` r
# Install the package installer once.
install.packages("pak")
# Install the latest released db.atlas package.
pak::pak("ultreia-io/db.atlas@*release")
```

`pak` downloads the package and installs its dependencies automatically.

For the development version:

``` r
# Install db.atlas from the development branch.
pak::pak("ultreia-io/db.atlas@develop")
```

## Getting started

Follow [Getting started][getting-started] for the runnable example using a bundled metadata snapshot.

[terminology]: https://ultreia-io.github.io/db.atlas/articles/terminology.html
[getting-started]: https://ultreia-io.github.io/db.atlas/articles/atlas.html

## Guides

- [Schema reports](https://ultreia-io.github.io/db.atlas/articles/reports.html)
- [Package resources](https://ultreia-io.github.io/db.atlas/articles/resources.html)
- [API reference](https://ultreia-io.github.io/db.atlas/reference/index.html)
- [Contributing][contributing] and [maintenance][maintenance]

[contributing]: https://ultreia-io.github.io/db.atlas/CONTRIBUTING.html
[maintenance]: https://ultreia-io.github.io/db.atlas/DEVELOPMENT.html
