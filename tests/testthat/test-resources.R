test_that("installed SQL and template resources resolve", {
  sql <- db.atlas:::atlas_resource("sql", "get_tables_foreign_keys.sql")
  template <- db.atlas:::atlas_resource("templates", "report.Rmd.tpl")
  expect_true(file.exists(sql))
  expect_true(file.exists(template))
  expect_match(db.atlas:::read_atlas_sql("get_tables_foreign_keys.sql"), "pg_constraint")
  expect_error(db.atlas:::atlas_resource("other", "x"), "either")
  expect_error(db.atlas:::atlas_resource("sql", "missing.sql"), "not found")
})
