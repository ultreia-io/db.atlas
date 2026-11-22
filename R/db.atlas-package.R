#' db.atlas: reusable database schema atlases
#'
#' Extract PostgreSQL metadata through application-supplied connection providers,
#' save and load snapshots, inspect dependencies, and render neutral reports.
#' The package uses [dbiutils::db_query()],
#' [dbiutils::with_db_connection()], and [dbiutils::read_sql_file()] for its
#' low-level DBI work and does not import a database driver.
#'
#' @import data.table
#' @keywords internal
"_PACKAGE"

# Internal helpers

#' Locate a bundled SQL query or report template
#'
#' The lookup uses the installed package. Local tests can fall back to paths
#' under the source package's `inst/` directory.
#'
#' @param type Resource directory, either `"sql"` or `"templates"`.
#' @param name Filename within that directory.
#' @param must_work When `TRUE`, require an existing file.
#' @return A resource path. If the lookup fails with `must_work = FALSE`,
#'   the result is an empty string or a candidate source path in local tests.
#' @keywords internal
atlas_resource <- function(type, name, must_work = TRUE) {
  if (!type %in% c("sql", "templates")) {
    stop("`type` must be either 'sql' or 'templates'.", call. = FALSE)
  }
  path <- system.file(type, name, package = "db.atlas", mustWork = FALSE)
  if (!nzchar(path) && identical(Sys.getenv("TESTTHAT"), "true")) {
    path <- file.path("inst", type, name)
  }
  if (must_work && (!nzchar(path) || !file.exists(path))) {
    stop("Package resource not found: ", type, "/", name, call. = FALSE)
  }
  path
}

# Register column names resolved inside data.table expressions for R CMD check.
utils::globalVariables(c(
  "color", "column", "columns", "description", "foreign_key", "group", "id",
  "mandatory", "primary_key", "primary_key_columns", "relation", "schema",
  "target_columns", "target_schema", "target_table", "target_table_id",
  "type", "usage_columns", "usage_foreign_key", "usage_schema",
  "usage_table", "usage_table_id"
))
