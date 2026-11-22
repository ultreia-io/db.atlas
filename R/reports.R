#' Configure an atlas report
#'
#' Use the settings to select schemas, choose sections, and customize the HTML
#' produced by [render_schema_report()] and [dependency_graph()].
#'
#' @param title Report title.
#' @param abstract Introductory text.
#' @param author Optional author.
#' @param logo Optional logo path.
#' @param analytics_html An optional HTML fragment or the path to a file containing it.
#' @param schemas Character vector of schema names. Use `NULL` to include all schemas.
#' @param schema_groups Named list mapping graph group labels to vectors of schema names.
#' @param schema_colors Named character vector of graph colors keyed by schema.
#' @param filename_prefix Filename prefix for generated reports.
#' @param output_directory Caller-selected output directory.
#' @param link_type Function accepting a fully qualified table ID of the form
#'   `schema.table` and returning a label for the relationship type.
#' @param link_url Function accepting `schema`, `table`, and `label` and returning an
#'   HTML link. The report and graph details pass a fully qualified table ID as
#'   the label. They display the result of `link_type` separately.
#' @param domain_labels Named character vector of labels.
#' @param timestamp_format Format passed to [base::format()] for the report
#'   timestamp. Use `""` to omit the timestamp and filename suffix.
#' @param sections Named logical vector controlling optional report sections.
#' @param template_overrides Named list containing custom `report`, `style`,
#'   `report_script`, `graph_style`, or `graph_script` paths.
#' @param schema_graph_links Named character vector mapping schema names to graph URLs.
#' @param graph_entry_point Optional `schema.table` or `schema.table.column` identifier
#'   selecting a root table for graph traversal.
#' @param graph_standalone_tables Fully qualified table IDs treated as leaves
#'   when reached during graph traversal.
#' @return An [atlas_report_config-class] object.
#' @export
atlas_report_config <- function(
    title = "Database schema atlas",
    abstract = "Database schema metadata, relationships, and descriptions.",
    author = NULL,
    logo = NULL,
    analytics_html = NULL,
    schemas = NULL,
    schema_groups = list(),
    schema_colors = character(),
    filename_prefix = "database_schema",
    output_directory = ".",
    link_type = function(table_id) "Database table",
    link_url = function(schema, table, label) {
      sprintf("<a href='#table_%s'>%s</a>", atlas_anchor(schema, table), escape_html(label))
    },
    domain_labels = character(),
    timestamp_format = "%Y-%m-%d_%H-%M-%S",
    sections = c(columns = TRUE, dependencies = TRUE, usages = TRUE, graphs = TRUE),
    template_overrides = list(),
    schema_graph_links = character(),
    graph_entry_point = NULL,
    graph_standalone_tables = character()) {
  scalar_text <- list(title = title, abstract = abstract, filename_prefix = filename_prefix,
                      output_directory = output_directory)
  invalid <- names(scalar_text)[!vapply(scalar_text, function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1L))]
  if (length(invalid)) stop("Invalid report setting(s): ", paste(invalid, collapse = ", "), call. = FALSE)
  if (!is.character(timestamp_format) || length(timestamp_format) != 1L || is.na(timestamp_format)) {
    stop("`timestamp_format` must be one character value.", call. = FALSE)
  }
  if (!is.null(schemas)) schemas <- validate_schemas(schemas)
  if (!is.function(link_type) || !is.function(link_url)) stop("Link generators must be functions.", call. = FALSE)
  section_names <- names(sections)
  sections <- stats::setNames(as.logical(sections), section_names)
  if (is.null(names(sections)) || !all(c("columns", "dependencies", "usages", "graphs") %in% names(sections))) {
    stop("`sections` must name columns, dependencies, usages, and graphs.", call. = FALSE)
  }
  result <- list(
    title = title, abstract = abstract, author = author, logo = logo,
    analytics_html = analytics_html, schemas = schemas, schema_groups = schema_groups,
    schema_colors = schema_colors, filename_prefix = filename_prefix,
    output_directory = output_directory, link_type = link_type, link_url = link_url,
    domain_labels = domain_labels, timestamp_format = timestamp_format,
    sections = sections, template_overrides = template_overrides,
    schema_graph_links = schema_graph_links,
    graph_entry_point = graph_entry_point, graph_standalone_tables = graph_standalone_tables
  )
  class(result) <- c("atlas_report_config", "list")
  result
}

#' Render a complete database schema report
#'
#' Writes an HTML report with columns, dependencies and reverse usages according
#' to `config$sections`. The report uses `config$filename_prefix` and an optional
#' timestamp suffix. With the graph section enabled, it also writes
#' `<filename_prefix>_dependencies.html` in the same directory.
#'
#' Use [generate_report_indexes()] to create directory navigation for the outputs.
#' Rendering requires rmarkdown and Pandoc.
#'
#' @param model A [dbschema_model-class] object.
#' @param config An [atlas_report_config-class] object.
#' @param timestamp Timestamp used in the report and filename.
#' @param quiet Passed to [rmarkdown::render()].
#' @return The normalized path to the main HTML report, invisibly.
#' @export
render_schema_report <- function(model, config = atlas_report_config(), timestamp = Sys.time(), quiet = TRUE) {
  model <- validate_model(model)
  config <- validate_config(config)
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Package `rmarkdown` is required to render reports.", call. = FALSE)
  dir.create(config$output_directory, recursive = TRUE, showWarnings = FALSE)
  formatted_timestamp <- if (identical(config$timestamp_format, "")) "" else format(timestamp, config$timestamp_format)
  timestamp_suffix <- if (nzchar(formatted_timestamp)) paste0("_", gsub("[^A-Za-z0-9_-]", "_", formatted_timestamp)) else ""
  output_file <- paste0(config$filename_prefix, timestamp_suffix, ".html")
  input <- tempfile(fileext = ".Rmd")
  on.exit(unlink(input), add = TRUE)
  logo <- copy_optional_asset(config$logo, config$output_directory)
  analytics <- if (is.null(config$analytics_html)) "" else {
    if (length(config$analytics_html) == 1L && file.exists(config$analytics_html)) {
      paste(readLines(config$analytics_html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else paste(config$analytics_html, collapse = "\n")
  }
  style <- paste(readLines(template_path(config, "style", "atlas.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  report_script <- paste(readLines(template_path(config, "report_script", "report.js"),
                                   warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  body <- build_report_body(model, config, timestamp)
  template <- paste(readLines(template_path(config, "report", "report.Rmd.tpl"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rendered <- whisker::whisker.render(template, list(
    title = config$title,
    abstract = config$abstract,
    author = if (is.null(config$author)) "" else config$author,
    timestamp = formatted_timestamp,
    logo = logo,
    has_logo = nzchar(logo),
    analytics = analytics,
    style = style,
    report_script = report_script,
    body = body
  ))
  writeLines(rendered, input, useBytes = TRUE)
  render_environment <- new.env(parent = baseenv())
  render_environment$report_tables <- attr(body, "widgets")
  result <- rmarkdown::render(input, output_file = output_file, output_dir = config$output_directory,
                              envir = render_environment, quiet = quiet)
  if (config$sections[["graphs"]]) {
    graph_file <- file.path(config$output_directory, paste0(config$filename_prefix, "_dependencies.html"))
    htmlwidgets::saveWidget(dependency_graph(model, config), graph_file, selfcontained = TRUE)
  }
  invisible(normalizePath(result, winslash = "/", mustWork = TRUE))
}

#' Generate directory indexes
#'
#' Creates a simple `index.html` in the root and each subdirectory. Existing
#' indexes are overwritten; generated links use relative paths. Asset directories
#' can be excluded from navigation and index generation.
#'
#' @param root_directory Root report directory.
#' @param title Index title.
#' @param exclude_directories Directory names to omit, including their descendants.
#' @return Paths of generated indexes, invisibly.
#' @export
generate_report_indexes <- function(root_directory, title = "Schema reports",
                                    exclude_directories = character()) {
  if (!dir.exists(root_directory)) stop("Root directory does not exist: ", root_directory, call. = FALSE)
  if (!is.character(exclude_directories) || anyNA(exclude_directories) ||
      any(!nzchar(exclude_directories)) || any(grepl("[/\\\\]", exclude_directories))) {
    stop("`exclude_directories` must contain directory names.", call. = FALSE)
  }
  directories <- unique(c(root_directory, list.dirs(root_directory, recursive = TRUE, full.names = TRUE)))
  root <- normalizePath(root_directory, winslash = "/")
  directories <- directories[vapply(directories, function(directory) {
    relative <- substring(normalizePath(directory, winslash = "/"), nchar(root) + 2L)
    !any(strsplit(relative, "/", fixed = TRUE)[[1L]] %in% exclude_directories)
  }, logical(1L))]
  indexes <- vapply(directories, function(directory) {
    children <- list.dirs(directory, recursive = FALSE, full.names = FALSE)
    children <- setdiff(children, exclude_directories)
    files <- list.files(directory, pattern = "\\.html$", full.names = FALSE)
    files <- setdiff(files, "index.html")
    items <- c(
      if (!identical(normalizePath(directory), normalizePath(root_directory))) "<li><a href='../index.html'>..</a></li>",
      sprintf("<li><a href='%s/index.html'>%s/</a></li>", utils::URLencode(children), escape_html(children)),
      sprintf("<li><a href='%s'>%s</a></li>", utils::URLencode(files), escape_html(files))
    )
    html <- sprintf("<!doctype html><html><head><meta charset='utf-8'><title>%s</title></head><body><h1>%s</h1><ul>%s</ul></body></html>",
                    escape_html(title), escape_html(title), paste(items, collapse = "\n"))
    path <- file.path(directory, "index.html")
    writeLines(html, path, useBytes = TRUE)
    path
  }, character(1L))
  invisible(indexes)
}

#' Create stable HTML anchors
#'
#' Joins the input with underscores, replaces unsupported characters with
#' underscores, and collapses repeated underscores. Report table links use
#' the resulting identifier with the `table_` prefix.
#'
#' @param ... Values to combine in the anchor, such as a schema and table name.
#' @return A lowercase HTML-safe identifier.
#' @export
atlas_anchor <- function(...) {
  value <- paste(..., sep = "_")
  value <- gsub("[^A-Za-z0-9_-]", "_", value)
  tolower(gsub("_+", "_", value))
}

#' Atlas report configuration
#'
#' An S3 list returned by [atlas_report_config()]. It contains the report title,
#' selected schemas and sections, output settings and optional branding or templates.
#'
#' Pass a configuration to [render_schema_report()] or [dependency_graph()].
#' @name atlas_report_config-class
NULL

# Internal helpers

#' Require a report configuration before rendering
#'
#' @param config Object expected to inherit from `atlas_report_config`.
#' @return The supplied configuration after its class has been checked.
#' @keywords internal
validate_config <- function(config) {
  if (!inherits(config, "atlas_report_config")) stop("`config` must be created by `atlas_report_config()`.", call. = FALSE)
  config
}

#' Resolve the color used for a schema's graph nodes and legend item
#'
#' @param schema One schema name.
#' @param config Report configuration containing optional colors keyed by schema.
#' @return The configured color or the default blue `#4C78A8`.
#' @keywords internal
schema_color <- function(schema, config) {
  if (schema %in% names(config$schema_colors)) unname(config$schema_colors[[schema]]) else "#4C78A8"
}

#' Resolve an application override or the installed template
#'
#' @param config Report configuration containing template overrides.
#' @param name Key to check in `config$template_overrides`.
#' @param default Bundled filename under the installed `templates/` directory.
#' @return The override path when supplied or the path to the bundled file.
#' @keywords internal
template_path <- function(config, name, default) {
  override <- config$template_overrides[[name]]
  if (is.null(override)) atlas_resource("templates", default) else override
}

#' Escape values for use as HTML text
#'
#' @param x Values to render, with missing values represented as empty text.
#' @return A character vector of escaped text.
#' @keywords internal
escape_html <- function(x) as.character(htmltools::htmlEscape(ifelse(is.na(x), "", x)))

#' Build the report Markdown and collect its table widgets
#'
#' @param model Validated schema model.
#' @param config Report configuration selecting schemas and sections.
#' @param timestamp Report timestamp supplied by the caller. The outer template
#'   formats it in the report header.
#' @return The report body as Markdown with the table widgets in its `widgets`
#'   attribute, indexed in the order used by the embedded R chunks.
#' @keywords internal
build_report_body <- function(model, config, timestamp) {
  schemas <- model$schemas
  if (!is.null(config$schemas)) schemas <- schemas[schema %in% config$schemas]
  # Match each key column to its flags while preserving the key's column order.
  column_names <- function(schema_name, table_name, names) {
    columns <- model$columns[schema == schema_name & table == table_name]
    columns <- columns[match(names, column)]
    paste(mapply(render_column_name, names, columns$mandatory,
                 columns$primary_key, columns$foreign_key, USE.NAMES = FALSE), collapse = "<br>")
  }
  sections <- list(legend = paste(
    "## Legend",
    '<span class="pk-icon" title="Primary key"></span> Primary key &nbsp; ',
    '<span class="fk-icon" title="Foreign key"></span> Foreign key &nbsp; ',
    '<span class="mandatory-icon" title="Mandatory"></span> Mandatory (NOT NULL)',
    sep = "\n\n"
  ))
  widgets <- list()
  # Store each widget and emit the chunk that displays it at this point in the report.
  render_table <- function(data, raw_columns = character()) {
    if (!nrow(data)) return("<p class='atlas-empty'>None.</p>")
    widgets[[length(widgets) + 1L]] <<- render_interactive_table(data, raw_columns)
    sprintf("```{r atlas-table-%s, echo=FALSE, message=FALSE}\nreport_tables[[%s]]\n```",
            length(widgets), length(widgets))
  }
  for (schema_name in schemas$schema) {
    schema <- schemas[schema == schema_name]
    schema_label <- if (schema_name %in% names(config$domain_labels)) {
      paste0(config$domain_labels[[schema_name]], " (", schema_name, ")")
    } else schema_name
    parts <- c(
      sprintf("## %s {#schema_%s}", escape_html(schema_label), atlas_anchor(schema_name)),
      render_markdown_description(schema$description[[1L]], quote = TRUE)
    )
    if (schema_name %in% names(config$schema_graph_links)) {
      parts <- c(parts, sprintf("[Dependency graph](%s)", config$schema_graph_links[[schema_name]]))
    }
    tables <- model$tables[schema == schema_name]
    for (row in seq_len(nrow(tables))) {
      table_name <- tables$table[[row]]
      id <- tables$table_id[[row]]
      table_parts <- c(
        sprintf("### %s {#table_%s .tabset}", escape_html(table_name), atlas_anchor(schema_name, table_name)),
        render_markdown_description(tables$description[[row]], quote = TRUE)
      )
      if (config$sections[["columns"]]) {
        value <- model$columns[schema == schema_name & table == table_name,
          list(column, type, mandatory, primary_key, foreign_key, description)]
        value[, column := mapply(render_column_name, column, mandatory, primary_key,
                                  foreign_key, USE.NAMES = FALSE)]
        value[, c("mandatory", "primary_key", "foreign_key") := NULL]
        table_parts <- c(table_parts, "#### Columns", render_table(value, "column"))
      }
      if (config$sections[["dependencies"]]) {
        value <- model$dependencies[table_id == id, list(
          columns = vapply(columns, function(names) column_names(schema_name, table_name, names), character(1L)),
          type = vapply(target_table_id, config$link_type, character(1L)),
          target = mapply(config$link_url, target_schema, target_table, target_table_id, USE.NAMES = FALSE),
          target_columns = mapply(column_names, target_schema, target_table, target_columns, USE.NAMES = FALSE)
        )]
        table_parts <- c(table_parts, "#### Dependencies", render_table(value, c("columns", "target", "target_columns")))
      }
      if (config$sections[["usages"]]) {
        value <- model$usages[table_id == id, list(
          columns = vapply(columns, function(names) column_names(schema_name, table_name, names), character(1L)),
          type = vapply(usage_table_id, config$link_type, character(1L)),
          usage = mapply(config$link_url, usage_schema, usage_table, usage_table_id, USE.NAMES = FALSE),
          usage_columns = mapply(column_names, usage_schema, usage_table, usage_columns, USE.NAMES = FALSE)
        )]
        table_parts <- c(table_parts, "#### Usages", render_table(value, c("columns", "usage", "usage_columns")))
      }
      parts <- c(parts, table_parts)
    }
    sections[[schema_name]] <- paste(parts, collapse = "\n\n")
  }
  body <- paste(unlist(sections, use.names = FALSE), collapse = "\n\n")
  attr(body, "widgets") <- widgets
  body
}

#' Render a column name and its constraint icons
#'
#' @param column Column name to escape for HTML output.
#' @param mandatory Whether the column requires a value.
#' @param primary_key Whether the column belongs to a primary key.
#' @param foreign_key Whether the column belongs to a foreign key.
#' @return An HTML fragment containing the escaped name and any applicable icons.
#' @keywords internal
render_column_name <- function(column, mandatory = FALSE, primary_key = FALSE, foreign_key = FALSE) {
  # Render one icon with an accessible label when its flag is enabled.
  icon <- function(enabled, class, label) {
    if (!isTRUE(enabled)) return("")
    sprintf('<span class="%s" title="%s" aria-label="%s"></span>', class, label, label)
  }
  label <- escape_html(column)
  if (isTRUE(primary_key)) label <- paste0('<span class="pk-column">', label, '</span>')
  paste0(icon(primary_key, "pk-icon", "Primary key"),
         icon(foreign_key, "fk-icon", "Foreign key"),
         icon(mandatory, "mandatory-icon", "Mandatory"), label)
}

#' Render a description with language badges
#'
#' @param description One schema, table or column description.
#' @param quote Whether to wrap the rendered description in a Markdown block quote.
#' @return An escaped HTML fragment, with a visible marker for empty descriptions.
#' @keywords internal
render_markdown_description <- function(description, quote = FALSE) {
  values <- parse_description(description)
  text <- if (!length(values) || !any(nzchar(trimws(unlist(values))))) {
    '<span class="error">Not filled</span>'
  } else {
    paste(vapply(names(values), function(language) {
      badge <- if (language == "EN") "bg-primary" else "bg-secondary"
      sprintf('<span class="badge %s">%s</span> %s',
              badge, escape_html(language), escape_html(values[[language]]))
    }, character(1L)), collapse = "<br>")
  }
  if (quote) paste0("> ", gsub("\n", "\n> ", text, fixed = TRUE)) else text
}

#' Render tabular data as a Markdown table
#'
#' @param data Tabular data to render. List columns are flattened for display.
#' @param raw_columns Names of columns containing markup to preserve.
#' @return A Markdown table as one string, or `*None.*` when there are no rows.
#' @keywords internal
render_markdown_table <- function(data, raw_columns = character()) {
  if (!nrow(data)) return("*None.*")
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  for (name in names(data)) {
    if (is.list(data[[name]])) data[[name]] <- flatten_list_column(data[[name]])
    if (!name %in% raw_columns) data[[name]] <- vapply(data[[name]], escape_html, character(1L))
  }
  header <- paste(names(data), collapse = " | ")
  separator <- paste(rep("---", ncol(data)), collapse = " | ")
  rows <- apply(data, 1L, paste, collapse = " | ")
  paste(c(paste0("| ", header, " |"), paste0("| ", separator, " |"), paste0("| ", rows, " |")), collapse = "\n")
}

#' Render tabular data as an interactive report table
#'
#' @param data Tabular data to render. Description cells receive language badges.
#' @param raw_columns Names of columns whose preformatted HTML should be preserved.
#' @return A DT widget with plain cells escaped as HTML text.
#' @keywords internal
render_interactive_table <- function(data, raw_columns = character()) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  for (name in names(data)) {
    if (is.list(data[[name]])) data[[name]] <- flatten_list_column(data[[name]])
    if (name == "description") {
      data[[name]] <- vapply(data[[name]], render_markdown_description, character(1L))
    } else if (!name %in% raw_columns) {
      data[[name]] <- vapply(data[[name]], escape_html, character(1L))
      data[[name]] <- gsub("\\r?\\n", "<br>", data[[name]])
    }
  }
  DT::datatable(
    data, rownames = FALSE, escape = FALSE, selection = "none",
    options = list(paging = FALSE, lengthChange = FALSE,
                   ordering = FALSE, autoWidth = FALSE)
  )
}

#' Copy an optional report asset into the output directory
#'
#' @param path Path to an existing asset or `NULL` when omitted.
#' @param output_directory Existing destination directory.
#' @return The normalized destination path, or an empty string for `NULL`.
#' @keywords internal
copy_optional_asset <- function(path, output_directory) {
  if (is.null(path)) return("")
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) stop("Optional asset not found: ", path, call. = FALSE)
  destination <- file.path(output_directory, basename(path))
  file.copy(path, destination, overwrite = TRUE)
  normalizePath(destination, winslash = "/", mustWork = TRUE)
}
