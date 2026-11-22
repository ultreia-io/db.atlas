read_atlas_sql <- function(name) {
  dbiutils::read_sql_file(atlas_resource("sql", name))
}
