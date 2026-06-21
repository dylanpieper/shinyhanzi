# hanzi_word_stats() — word-level frequency, gloss, and constituent characters.

test_that("hanzi_word_stats returns frequency, gloss, and characters", {
  con <- local_hanzi_db()
  st <- shinyhanzi:::hanzi_word_stats("日光", con)

  # 日光 is in the word frequency list.
  expect_false(is.null(st$freq))
  expect_true(st$freq$rank[[1]] > 0)

  # CC-CEDICT entry with toned pinyin.
  expect_false(is.null(st$entry))
  expect_match(st$entry$pinyin_toned[[1]], "gu", fixed = TRUE)

  # Constituent characters, in order, enriched for tile display.
  expect_identical(st$chars$component, c("日", "光"))
  expect_true(all(c("pinyin_toned", "definition") %in% names(st$chars)))
})

test_that("hanzi_word_stats handles words absent from the frequency list", {
  con <- local_hanzi_db()
  # A valid two-character sequence that is not a ranked word.
  st <- shinyhanzi:::hanzi_word_stats("日水", con)
  expect_null(st$freq)
  expect_identical(st$chars$component, c("日", "水"))
})
