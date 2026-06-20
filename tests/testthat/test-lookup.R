# hanzi_lookup() / hanzi_pinyin() — verified against standard Mandarin.

test_that("hanzi_db_path returns NULL or a single path string", {
  skip_on_cran()
  path <- shinyhanzi::hanzi_db_path()
  expect_true(is.null(path) || (is.character(path) && length(path) == 1L))
})

test_that("hanzi_lookup returns mmah metadata and cedict rows for 好", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_lookup("好", con)

  expect_identical(res$char, "好")
  expect_named(res$mmah, c("definition", "radical", "etymology_type",
                           "phonetic", "semantic", "etymology_hint"))

  # 好 is built from 女 (woman) + 子 (child); its Kangxi radical is 女.
  expect_identical(res$mmah$radical, "女")
  expect_match(res$mmah$definition, "good", ignore.case = TRUE)

  # CC-CEDICT carries both readings of 好.
  expect_s3_class(res$cedict, "data.frame")
  expect_true(all(c("traditional", "pinyin_toned", "pinyin_numbered",
                    "gloss", "is_word") %in% names(res$cedict)))
  expect_true("hǎo" %in% res$cedict$pinyin_toned)
})

test_that("hanzi_lookup returns the standard structure for an unknown character", {
  con <- local_hanzi_db()
  # U+20000, an unassigned-in-this-DB CJK Extension B code point.
  res <- shinyhanzi::hanzi_lookup("𠀀", con)

  expect_identical(res$char, "𠀀")
  expect_true(is.na(res$mmah$definition))
  expect_s3_class(res$cedict, "data.frame")
  expect_equal(nrow(res$cedict), 0L)
})

test_that("hanzi_lookup rejects non-scalar input", {
  con <- local_hanzi_db()
  expect_error(shinyhanzi::hanzi_lookup(c("好", "中"), con))
})

test_that("hanzi_pinyin returns the primary reading in both notations", {
  con <- local_hanzi_db()
  # 好 has multiple readings; the primary (most common) is hǎo / hao3.
  expect_identical(shinyhanzi::hanzi_pinyin("好", con), "hǎo")
  expect_identical(shinyhanzi::hanzi_pinyin("好", con, toned = FALSE), "hao3")

  # A few more high-frequency single characters.
  expect_identical(shinyhanzi::hanzi_pinyin("中", con), "zhōng")
  expect_identical(shinyhanzi::hanzi_pinyin("我", con), "wǒ")
})
