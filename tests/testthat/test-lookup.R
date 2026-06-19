# Tests for hanzi_lookup and hanzi_pinyin require a live database.
# They are skipped on CRAN and when the DB is not available.

test_that("hanzi_db_path returns NULL when no DB present", {
  # In a clean test environment without a cached DB, path should be NULL
  # (This test relies on hanzi_db_path() gracefully returning NULL)
  skip_on_cran()
  path <- shinyhanzi::hanzi_db_path()
  # Either a valid path or NULL — just check the type
  expect_true(is.null(path) || (is.character(path) && length(path) == 1L))
})
