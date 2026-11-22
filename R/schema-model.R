#' Build a schema model
#'
#' Builds table and column flags, forward dependencies, and reverse usages from
#' a metadata snapshot. Composite keys remain ordered list columns.
#'
#' @param metadata A metadata list, such as a [dbschema_metadata-class] object,
#'   or a snapshot directory.
#' @return A [dbschema_model-class] object.
#' @export
build_schema_model <- function(metadata) {
  if (is.character(metadata) && length(metadata) == 1L) metadata <- read_metadata(metadata)
  metadata <- validate_metadata(metadata)
  schemas <- data.table::copy(metadata$schemas_description)
  tables <- data.table::copy(metadata$tables_description)
  columns <- data.table::copy(metadata$tables_columns)
  primary_keys <- data.table::copy(metadata$tables_primary_keys)
  foreign_keys <- data.table::copy(metadata$tables_foreign_keys)

  columns[, mandatory := as_flag(mandatory)]
  columns[, primary_key := FALSE]
  columns[, foreign_key := FALSE]
  if (nrow(primary_keys)) {
    pk <- primary_keys[, list(column = unlist(primary_key_columns)), by = c("schema", "table")]
    columns[pk, primary_key := TRUE, on = c("schema", "table", "column")]
  }
  if (nrow(foreign_keys)) {
    fk <- foreign_keys[, list(column = unlist(columns)), by = c("schema", "table")]
    columns[unique(fk), foreign_key := TRUE, on = c("schema", "table", "column")]
  }

  dependencies <- data.table::copy(foreign_keys)
  if (nrow(dependencies)) {
    dependencies[, table_id := table_id(schema, table)]
    dependencies[, target_table_id := table_id(target_schema, target_table)]
    usages <- dependencies[, list(
      schema = target_schema,
      table = target_table,
      columns = target_columns,
      table_id = target_table_id,
      usage_schema = schema,
      usage_table = table,
      usage_columns = columns,
      usage_table_id = table_id,
      usage_foreign_key = foreign_key
    )]
  } else {
    dependencies[, `:=`(table_id = character(), target_table_id = character())]
    usages <- data.table::data.table(
      schema = character(), table = character(), columns = list(), table_id = character(),
      usage_schema = character(), usage_table = character(), usage_columns = list(),
      usage_table_id = character(), usage_foreign_key = character()
    )
  }

  tables[, table_id := table_id(schema, table)]
  result <- list(
    schemas = schemas,
    tables = tables,
    columns = columns,
    primary_keys = primary_keys,
    foreign_keys = foreign_keys,
    dependencies = dependencies,
    usages = usages
  )
  class(result) <- c("dbschema_model", "list")
  result
}

#' Access schema-model tables
#'
#' Use the accessors to inspect schemas, tables and columns with their key flags.
#' Dependencies describe foreign keys from the source table to the target table.
#' Usages describe those same foreign keys from the target table's perspective.
#'
#' @seealso [model_schemas()], [model_tables()], [model_columns()],
#'   [model_dependencies()], [model_usages()]
#' @name model_accessors
NULL

#' List the schemas in a model
#'
#' @param model A [dbschema_model-class] object from [build_schema_model()].
#' @return A copy of the schemas data table with schema names and descriptions.
#' @seealso [model_tables()]
#' @export
model_schemas <- function(model) data.table::copy(validate_model(model)$schemas)

#' List the tables in a model
#'
#' @param model A [dbschema_model-class] object from [build_schema_model()].
#' @return A copy of the tables data table with schema names, table names,
#'   descriptions and fully qualified `table_id` values.
#' @seealso [model_columns()]
#' @export
model_tables <- function(model) data.table::copy(validate_model(model)$tables)

#' List modeled columns with their key and nullability flags
#'
#' @param model A [dbschema_model-class] object from [build_schema_model()].
#' @return A copy of the columns data table with names, types and descriptions.
#'   The logical columns `mandatory`, `primary_key` and `foreign_key` describe
#'   whether a column requires a value and whether it belongs to either kind of key.
#' @seealso [model_tables()]
#' @export
model_columns <- function(model) data.table::copy(validate_model(model)$columns)

#' List forward foreign-key dependencies
#'
#' @param model A [dbschema_model-class] object from [build_schema_model()].
#' @return A copy of the dependencies data table with one row per foreign key.
#'   `table_id` identifies the source table and `target_table_id` identifies the
#'   referenced table. `columns` and `target_columns` are ordered list columns.
#' @seealso [model_usages()], [dependency_tree()]
#' @export
model_dependencies <- function(model) data.table::copy(validate_model(model)$dependencies)

#' List the tables that reference each modeled table
#'
#' @param model A [dbschema_model-class] object from [build_schema_model()].
#' @return A copy of the usages data table with one row per foreign key.
#'   `table_id` and `columns` identify the referenced table and key columns.
#'   The `usage_*` fields describe the referencing table, columns and foreign key.
#' @seealso [model_dependencies()], [dependency_tree()]
#' @export
model_usages <- function(model) data.table::copy(validate_model(model)$usages)

#' Build a dependency tree
#'
#' Starts at the entry point's table and follows dependencies and reverse usages
#' breadth-first. Each table appears once, via the first route that reaches it.
#'
#' @param model A [dbschema_model-class] object.
#' @param entry_point A `schema.table` or `schema.table.column` identifier. Both forms
#'   start at the table.
#' @param max_depth Maximum traversal depth. Use `0` for the root alone or `Inf`
#'   for unlimited depth.
#' @return A `data.table` with one row per visited table. It records the level,
#'   parent table, direction and foreign key. The `path` column describes the
#'   step from the parent. The root is at level zero.
#' @export
dependency_tree <- function(model, entry_point, max_depth = Inf) {
  model <- validate_model(model)
  root <- normalize_entry_point(entry_point)
  if (!root %in% model$tables$table_id) stop("Unknown entry point table: ", root, call. = FALSE)
  if (!is.numeric(max_depth) || length(max_depth) != 1L || is.na(max_depth) || max_depth < 0) {
    stop("`max_depth` must be a non-negative number.", call. = FALSE)
  }
  result <- data.table::data.table(
    level = 0L, parent_table = NA_character_, table = root,
    direction = NA_character_, foreign_key = NA_character_, path = root
  )
  visited <- root
  frontier <- root
  level <- 0L
  while (length(frontier) && level < max_depth) {
    level <- level + 1L
    additions <- list()
    for (current in frontier) {
      forward <- model$dependencies[table_id == current]
      if (nrow(forward)) {
        additions[[length(additions) + 1L]] <- data.table::data.table(
          level = level, parent_table = current, table = forward$target_table_id,
          direction = "depends_on", foreign_key = forward$foreign_key,
          path = paste(current, forward$target_table_id, sep = " -> ")
        )
      }
      reverse <- model$dependencies[target_table_id == current]
      if (nrow(reverse)) {
        additions[[length(additions) + 1L]] <- data.table::data.table(
          level = level, parent_table = current, table = reverse$table_id,
          direction = "used_by", foreign_key = reverse$foreign_key,
          path = paste(current, reverse$table_id, sep = " <- ")
        )
      }
    }
    if (!length(additions)) break
    next_rows <- unique(data.table::rbindlist(additions, fill = TRUE), by = "table")
    next_rows <- next_rows[!table %in% visited]
    if (!nrow(next_rows)) break
    result <- data.table::rbindlist(list(result, next_rows), fill = TRUE)
    frontier <- next_rows$table
    visited <- c(visited, frontier)
  }
  result
}

#' Database schema model
#'
#' An S3 list of `data.table` objects returned by [build_schema_model()].
#'
#' The model contains schemas, tables, columns, primary and foreign keys,
#' forward dependencies and reverse usages. Table IDs have the form `schema.table`.
#'
#' Use [model_schemas()], [model_tables()], [model_columns()],
#' [model_dependencies()] and [model_usages()] to retrieve copies of model tables.
#' Pass the model to [dependency_tree()] or [render_schema_report()].
#' @name dbschema_model-class
NULL

# Internal helpers

#' Convert catalogue nullability values to mandatory flags
#'
#' @param x Flags from the metadata catalogue.
#' @return A logical vector. `TRUE`, `YES` and `1` map to `TRUE` regardless of
#'   case. Other values, including missing ones, map to `FALSE`.
#' @keywords internal
as_flag <- function(x) {
  result <- rep(FALSE, length(x))
  present <- !is.na(x)
  result[present] <- toupper(as.character(x[present])) %in% c("TRUE", "YES", "1")
  result
}

#' Build fully qualified table identifiers
#'
#' @param schema Schema names.
#' @param table Table names.
#' @return Character identifiers in `schema.table` form.
#' @keywords internal
table_id <- function(schema, table) paste(schema, table, sep = ".")

#' Require a schema model before accessing its tables
#'
#' @param model Object expected to inherit from `dbschema_model`.
#' @return The supplied model after its class has been checked.
#' @keywords internal
validate_model <- function(model) {
  if (!inherits(model, "dbschema_model")) {
    stop("`model` must be created by `build_schema_model()`.", call. = FALSE)
  }
  model
}

#' Resolve a dependency traversal entry point to its table
#'
#' @param entry_point One `schema.table` or `schema.table.column` identifier.
#' @return The `schema.table` identifier used as the traversal root.
#' @keywords internal
normalize_entry_point <- function(entry_point) {
  if (!is.character(entry_point) || length(entry_point) != 1L || is.na(entry_point) || !nzchar(entry_point)) {
    stop("`entry_point` must be one 'schema.table' or 'schema.table.column' value.", call. = FALSE)
  }
  parts <- strsplit(entry_point, ".", fixed = TRUE)[[1L]]
  if (!length(parts) %in% c(2L, 3L) || any(!nzchar(parts))) {
    stop("`entry_point` must be one 'schema.table' or 'schema.table.column' value.", call. = FALSE)
  }
  paste(parts[1:2], collapse = ".")
}
