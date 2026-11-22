#' Extract PostgreSQL schema metadata
#'
#' Reads schema, table, column and key metadata from the PostgreSQL catalogues.
#' It opens one connection through the provider and closes it when the call
#' exits, including after a query error.
#'
#' @param schemas Non-empty character vector of schema names.
#' @param connection_provider Zero-argument function returning a DBI connection.
#' @return A [dbschema_metadata-class] object.
#' @seealso [write_metadata()] to save a snapshot for offline use.
#' @export
extract_schema_metadata <- function(schemas, connection_provider) {
  schemas <- validate_schemas(schemas)
  extract_schema_metadata_impl(
    schemas,
    connection_provider,
    with_connection = db.tools::with_db_connection,
    query = db.tools::db_query,
    sql_reader = read_atlas_sql
  )
}

#' Load a metadata snapshot
#'
#' Reads the required CSV files and restores ordered key columns as list columns.
#' It also loads `dependencies.csv` when present. See [dbschema_metadata-class]
#' for the file names and format.
#'
#' @param path Directory created by [write_metadata()].
#' @return A [dbschema_metadata-class] object.
#' @export
read_metadata <- function(path) {
  values <- lapply(metadata_components, function(name) read_component(path, name))
  names(values) <- metadata_components
  values$tables_foreign_keys[, columns := split_list_column(columns)]
  values$tables_foreign_keys[, target_columns := split_list_column(target_columns)]
  values$tables_primary_keys[, primary_key_columns := split_list_column(primary_key_columns)]
  dependency_file <- file.path(path, "dependencies.csv")
  if (file.exists(dependency_file)) {
    values$dependencies <- data.table::fread(dependency_file, na.strings = "NA", encoding = "UTF-8")
  }
  validate_metadata(values)
}

#' Save a metadata snapshot
#'
#' Creates the destination directory if needed and writes the CSV files listed
#' in [dbschema_metadata-class]. Existing files with those names are overwritten
#' when written. The result can be loaded with [read_metadata()].
#'
#' @param metadata A metadata list, such as a [dbschema_metadata-class] object.
#' @param path Output directory.
#' @return `path`, invisibly.
#' @export
write_metadata <- function(metadata, path) {
  metadata <- validate_metadata(metadata)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty directory path.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (name in metadata_components) {
    value <- data.table::copy(metadata[[name]])
    list_columns <- names(value)[vapply(value, is.list, logical(1L))]
    for (column in list_columns) {
      data.table::set(value, j = column, value = flatten_list_column(value[[column]]))
    }
    data.table::fwrite(value, file.path(path, paste0(name, ".csv")), bom = TRUE, na = "NA")
  }
  if (!is.null(metadata$dependencies)) {
    data.table::fwrite(metadata$dependencies, file.path(path, "dependencies.csv"), bom = TRUE, na = "NA")
  }
  invisible(path)
}

#' Parse language-tagged descriptions
#'
#' A block starts on a new line with a two-letter uppercase tag, such as
#' `[EN]` or `EN:`. A block can span multiple lines.
#'
#' @param x One character string containing the description.
#' @return A named list of trimmed blocks keyed by language. Untagged text uses
#'   `EN`. Missing input returns an empty list.
#' @export
parse_description <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(list())
  pattern <- "(?m)^[ \t]*(?:\\[[A-Z]{2}\\][ \t]*:?[ \t]*|[A-Z]{2}:[ \t]*)"
  starts <- gregexpr(pattern, x, perl = TRUE)[[1L]]
  if (starts[[1L]] < 0L) return(list(EN = trimws(x)))
  lengths <- attr(starts, "match.length")
  headers <- substring(x, starts, starts + lengths - 1L)
  languages <- sub("^[ \t]*\\[?([A-Z]{2})\\]?.*$", "\\1", headers, perl = TRUE)
  ends <- c(starts[-1L] - 1L, nchar(x))
  text <- trimws(substring(x, starts + lengths, ends))
  stats::setNames(as.list(text), languages)
}

#' Apply stored descriptions to a database
#'
#' Builds PostgreSQL `COMMENT ON` statements for non-empty schema, table and
#' column descriptions. Both preview and execution open a connection for DBI
#' quoting. The connection closes when the call exits.
#'
#' @param metadata A metadata list or a snapshot directory.
#' @param connection_provider Zero-argument function returning a DBI connection.
#' @param apply Use `FALSE` to preview the SQL. Use `TRUE` to execute the
#'   statements in one transaction.
#' @return Character vector of generated statements.
#' @export
apply_metadata_comments <- function(metadata, connection_provider, apply = FALSE) {
  if (is.character(metadata) && length(metadata) == 1L) metadata <- read_metadata(metadata)
  metadata <- validate_metadata(metadata)
  db.tools::with_db_connection(connection_provider, function(connection) {
    statements <- comment_statements(metadata, connection)
    if (isTRUE(apply) && length(statements)) {
      DBI::dbWithTransaction(connection, lapply(statements, function(statement) DBI::dbExecute(connection, statement)))
    }
    statements
  })
}

#' Database schema metadata
#'
#' An S3 list of `data.table` objects returned by [extract_schema_metadata()]
#' and [read_metadata()]. Pass it to [write_metadata()] or [build_schema_model()].
#'
#' - `schemas_description`, `tables_description` and `tables_columns` describe
#'   the extracted schemas, tables and columns.
#' - `tables_primary_keys` holds the ordered key columns for each table.
#' - `tables_foreign_keys` holds the ordered source and target columns for each foreign key.
#'
#' Key column sequences are stored in list columns, including single-column keys.
#'
#' **Snapshot format**
#'
#' Each required component has a UTF-8 CSV with the same name as the component,
#' such as `tables_columns.csv`. Key sequences use `|` between columns and keep
#' their original order. The writer represents missing values as `NA`.
#' An optional `dependencies.csv` carries the `dependencies` component.
#' @name dbschema_metadata-class
NULL

# Internal helpers

#' PostgreSQL catalogue query files keyed by metadata component
#' @keywords internal
metadata_queries <- c(
  schemas_description = "get_schemas_description.sql",
  tables_description = "get_tables_description.sql",
  tables_columns = "get_tables_columns.sql",
  tables_foreign_keys = "get_tables_foreign_keys.sql",
  tables_primary_keys = "get_tables_primary_keys.sql"
)

#' Required components of a metadata snapshot in their storage order
#' @keywords internal
metadata_components <- c(
  "schemas_description", "tables_description", "tables_columns",
  "tables_foreign_keys", "tables_primary_keys"
)

#' Read a bundled PostgreSQL catalogue query
#'
#' @param name SQL filename under the installed package's `sql/` directory.
#' @return The query as a character string.
#' @keywords internal
read_atlas_sql <- function(name) {
  db.tools::read_sql_file(atlas_resource("sql", name))
}

#' Run and normalize the catalogue queries on one connection
#'
#' @param schemas Validated schema names, bound as one vector parameter per query.
#' @param connection_provider Zero-argument function returning a DBI connection.
#' @param with_connection Function accepting the provider and a connection callback.
#' @param query Function accepting the connection, SQL and named `params` argument.
#' @param sql_reader Function taking a bundled SQL filename and returning its query.
#' @return A validated `dbschema_metadata` object.
#' @keywords internal
extract_schema_metadata_impl <- function(schemas, connection_provider, with_connection, query, sql_reader) {
  with_connection(connection_provider, function(connection) {
    raw <- lapply(metadata_queries, function(resource) {
      query(connection, sql_reader(resource), params = list(schemas))
    })
    normalize_extracted_metadata(raw)
  })
}

#' Validate and deduplicate a schema selection
#'
#' @param schemas Non-empty character vector of non-empty schema names.
#' @return Unique schema names in first-occurrence order.
#' @keywords internal
validate_schemas <- function(schemas) {
  if (!is.character(schemas) || !length(schemas) || anyNA(schemas) || any(!nzchar(schemas))) {
    stop("`schemas` must be a non-empty character vector without missing values.", call. = FALSE)
  }
  unique(schemas)
}

#' Rename catalogue columns present in a query result
#'
#' @param x Tabular query result.
#' @param mapping Named character vector mapping current column names to new names.
#' @return A data table with the matching columns renamed.
#' @keywords internal
rename_columns <- function(x, mapping) {
  x <- data.table::as.data.table(x)
  present <- intersect(names(mapping), names(x))
  if (length(present)) {
    data.table::setnames(x, present, unname(mapping[present]))
  }
  x
}

#' Convert catalogue rows into snapshot tables
#'
#' Uses catalogue position fields to order composite keys when available.
#' For earlier query results, foreign keys fall back to column-name order and
#' primary keys keep input order.
#'
#' @param raw Named list of results from the queries in `metadata_queries`.
#' @return Validated metadata with one row per key and ordered key list columns.
#' @keywords internal
normalize_extracted_metadata <- function(raw) {
  schemas <- rename_columns(raw$schemas_description, c(schema_name = "schema"))
  tables <- rename_columns(raw$tables_description, c(schema_name = "schema", table_name = "table"))
  columns <- rename_columns(raw$tables_columns, c(schema_name = "schema", table_name = "table", column_name = "column"))
  foreign_keys <- rename_columns(raw$tables_foreign_keys, c(schema_name = "schema", table_name = "table"))
  primary_keys <- rename_columns(raw$tables_primary_keys, c(schema_name = "schema", table_name = "table"))

  if (nrow(foreign_keys)) {
    order_column <- if ("column_order" %in% names(foreign_keys)) "column_order" else "column_name"
    data.table::setorderv(foreign_keys, c("schema", "table", "foreign_key", order_column))
    foreign_keys <- foreign_keys[, list(
      columns = list(.SD[["column_name"]]),
      target_schema = .SD[["target_schema"]][1L],
      target_table = .SD[["target_table"]][1L],
      target_columns = list(.SD[["target_column"]])
    ), by = c("schema", "table", "foreign_key")]
  } else {
    foreign_keys <- empty_foreign_keys()
  }

  if (nrow(primary_keys)) {
    if ("position" %in% names(primary_keys)) {
      data.table::setorderv(primary_keys, c("schema", "table", "position"))
    }
    primary_keys <- primary_keys[, list(primary_key_columns = list(.SD[["column_name"]])), by = c("schema", "table")]
  } else {
    primary_keys <- empty_primary_keys()
  }

  validate_metadata(list(
    schemas_description = schemas[, intersect(c("schema", "description"), names(schemas)), with = FALSE],
    tables_description = tables[, intersect(c("schema", "table", "description"), names(tables)), with = FALSE],
    tables_columns = columns[, intersect(c("schema", "table", "column", "type", "mandatory", "description"), names(columns)), with = FALSE],
    tables_foreign_keys = foreign_keys,
    tables_primary_keys = primary_keys
  ))
}

#' Create the foreign-key component for schemas without foreign keys
#'
#' @return A zero-row data table with source and target key list columns.
#' @keywords internal
empty_foreign_keys <- function() {
  data.table::data.table(
    schema = character(), table = character(), foreign_key = character(),
    columns = list(), target_schema = character(), target_table = character(),
    target_columns = list()
  )
}

#' Create the primary-key component for schemas without primary keys
#'
#' @return A zero-row data table with a primary-key list column.
#' @keywords internal
empty_primary_keys <- function() {
  data.table::data.table(schema = character(), table = character(), primary_key_columns = list())
}

#' Validate the snapshot structure and copy its tables
#'
#' @param metadata Named list of metadata components with the required columns.
#' @return A `dbschema_metadata` object containing the required components and
#'   the optional `dependencies` table when supplied.
#' @keywords internal
validate_metadata <- function(metadata) {
  if (!is.list(metadata)) {
    stop("`metadata` must be a named list.", call. = FALSE)
  }
  missing <- setdiff(metadata_components, names(metadata))
  if (length(missing)) {
    stop("Missing metadata component(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  required <- list(
    schemas_description = c("schema", "description"),
    tables_description = c("schema", "table", "description"),
    tables_columns = c("schema", "table", "column", "type", "mandatory", "description"),
    tables_foreign_keys = c("schema", "table", "foreign_key", "columns", "target_schema", "target_table", "target_columns"),
    tables_primary_keys = c("schema", "table", "primary_key_columns")
  )
  result <- lapply(metadata_components, function(name) {
    value <- data.table::as.data.table(metadata[[name]])
    absent <- setdiff(required[[name]], names(value))
    if (length(absent)) {
      stop("Component `", name, "` is missing column(s): ", paste(absent, collapse = ", "), call. = FALSE)
    }
    data.table::copy(value)
  })
  names(result) <- metadata_components
  if (!is.null(metadata$dependencies)) {
    result$dependencies <- data.table::as.data.table(metadata$dependencies)
  }
  class(result) <- c("dbschema_metadata", "list")
  result
}

#' Read one required snapshot CSV
#'
#' @param path Snapshot directory.
#' @param name Component name without the `.csv` suffix.
#' @return The component as a data table. Missing files raise an error.
#' @keywords internal
read_component <- function(path, name) {
  file <- file.path(path, paste0(name, ".csv"))
  if (!file.exists(file)) {
    stop("Metadata file not found: ", file, call. = FALSE)
  }
  data.table::fread(file, na.strings = "NA", encoding = "UTF-8")
}

#' Restore ordered key columns from snapshot cells
#'
#' @param x Vector of cells with names separated by `|`.
#' @return A list of character vectors. Missing and empty cells yield `character()`.
#' @keywords internal
split_list_column <- function(x) {
  lapply(x, function(value) {
    if (is.na(value) || !nzchar(value)) character() else strsplit(value, "|", fixed = TRUE)[[1L]]
  })
}

#' Encode list columns for snapshot CSVs and report tables
#'
#' @param x List column containing ordered values for each row.
#' @return A character vector with values in each row joined by `|` in order.
#' @keywords internal
flatten_list_column <- function(x) {
  vapply(x, paste, collapse = "|", FUN.VALUE = character(1L))
}

#' Quote a qualified PostgreSQL identifier
#'
#' @param connection Open DBI connection used for driver-specific quoting.
#' @param ... Identifier parts in qualification order, such as schema and table.
#' @return A character string with individually quoted parts joined by dots.
#' @keywords internal
quote_identifier <- function(connection, ...) {
  parts <- list(...)
  paste(vapply(parts, function(part) as.character(DBI::dbQuoteIdentifier(connection, part)), character(1L)), collapse = ".")
}

#' Generate SQL for populated descriptions
#'
#' @param metadata Validated metadata containing schema, table and column descriptions.
#' @param connection Open DBI connection used to quote identifiers and descriptions.
#' @return A character vector of `COMMENT ON` statements in schema, table and column order.
#' @keywords internal
comment_statements <- function(metadata, connection) {
  statements <- character()
  schemas <- metadata$schemas_description[!is.na(description) & nzchar(description)]
  if (nrow(schemas)) statements <- c(statements, mapply(function(schema, description) {
    sprintf("COMMENT ON SCHEMA %s IS %s;", quote_identifier(connection, schema), DBI::dbQuoteString(connection, description))
  }, schemas$schema, schemas$description, USE.NAMES = FALSE))
  tables <- metadata$tables_description[!is.na(description) & nzchar(description)]
  if (nrow(tables)) statements <- c(statements, mapply(function(schema, table, description) {
    sprintf("COMMENT ON TABLE %s IS %s;", quote_identifier(connection, schema, table), DBI::dbQuoteString(connection, description))
  }, tables$schema, tables$table, tables$description, USE.NAMES = FALSE))
  columns <- metadata$tables_columns[!is.na(description) & nzchar(description)]
  if (nrow(columns)) statements <- c(statements, mapply(function(schema, table, column, description) {
    sprintf("COMMENT ON COLUMN %s IS %s;", quote_identifier(connection, schema, table, column), DBI::dbQuoteString(connection, description))
  }, columns$schema, columns$table, columns$column, columns$description, USE.NAMES = FALSE))
  statements
}
