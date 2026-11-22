#' Create stable HTML anchors
#'
#' @param ... Values to combine.
#' @return A lowercase HTML-safe identifier.
#' @export
atlas_anchor <- function(...) {
  value <- paste(..., sep = "_")
  value <- gsub("[^A-Za-z0-9_-]", "_", value)
  tolower(gsub("_+", "_", value))
}

escape_html <- function(x) as.character(htmltools::htmlEscape(ifelse(is.na(x), "", x)))
