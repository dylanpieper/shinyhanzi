# hanzi_search() — English-meaning and pinyin search, verified against
# standard Mandarin vocabulary.

test_that("hanzi_search finds the common word for an English meaning", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_search("mother", n = 10, con = con)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("simplified", "traditional", "pinyin_toned",
                    "gloss", "score", "freq_rank") %in% names(res)))
  expect_lte(nrow(res), 10L)

  # 妈 (mā) is the everyday word for "mother"; frequency reweighting should
  # surface it among the top hits.
  expect_true("妈" %in% head(res$simplified, 5))
})

test_that("hanzi_search matches pinyin in toned, numbered and plain notation", {
  con <- local_hanzi_db()

  # 好 (hǎo, "good") should be found whether the query carries a tone mark,
  # a tone number, or no tone at all.
  for (q in c("hǎo", "hao3", "hao")) {
    res <- shinyhanzi::hanzi_search(q, n = 10, con = con)
    expect_true("好" %in% res$simplified, info = q)
  }
})

test_that("hanzi_search respects the result limit", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_search("water", n = 3, con = con)
  expect_lte(nrow(res), 3L)
})

test_that("hanzi_search returns an empty data frame for no matches", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_search("zzzxqqnomatch", n = 5, con = con)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
})

test_that("hanzi_search rejects empty queries", {
  con <- local_hanzi_db()
  expect_error(shinyhanzi::hanzi_search("", con = con))
})
