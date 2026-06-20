# Shared DB connection for tests that need a live database.
#
# The lookup/search/decompose functions are thin wrappers over the prebuilt
# DuckDB file, so meaningful tests require it. When the DB is not present
# (e.g. CRAN, or a fresh checkout that hasn't downloaded it) these tests skip
# rather than fail.

local_hanzi_db <- function() {
  if (is.null(shinyhanzi::hanzi_db_path())) {
    testthat::skip("shinyhanzi database not available")
  }
  tryCatch(
    shinyhanzi::hanzi_db(),
    error = function(e) testthat::skip(paste("could not open database:", conditionMessage(e)))
  )
}
