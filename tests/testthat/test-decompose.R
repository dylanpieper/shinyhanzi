# Decompose tests require a live database — skip on CRAN and CI without DB.
test_that("hanzi_components_of requires live DB", {
  skip_on_cran()
  # Just a placeholder; substantive tests run only with a built DB.
  expect_true(is.function(shinyhanzi::hanzi_components_of))
})
