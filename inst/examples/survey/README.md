# Survey example

The `metadata/` directory contains a PostgreSQL schema snapshot based on the shared `form.*` survey example.

- The `survey` schema models the sheets in the `form.model` survey form.
- The `catalogue` schema models the station, observer, and contact tables from `form.references`.

Samples reference observations by station and observation ID. Measurements reference samples by their composite key.

The CSV files describe schemas, tables, columns, primary keys, and foreign keys.

Use `db.atlas::read_metadata()` on the `metadata/` directory to load the snapshot.
