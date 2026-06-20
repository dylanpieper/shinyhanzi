# hanzi_decompose() / hanzi_components_of() — verified against standard
# character composition.

test_that("hanzi_decompose splits 好 into 女 + 子", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_decompose("好", "once", con = con)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("component", "is_intermediate", "definition",
                    "radical_name", "pinyin_toned") %in% names(res)))

  # 好 = 女 (woman) + 子 (child).
  expect_setequal(res$component, c("女", "子"))

  woman <- res[res$component == "女", ]
  expect_identical(woman$pinyin_toned, "nǚ")
  expect_match(woman$definition, "woman", ignore.case = TRUE)
})

test_that("hanzi_decompose returns no components for an atomic radical", {
  con <- local_hanzi_db()
  # 一 (one) is a single stroke with no sub-components.
  res <- shinyhanzi::hanzi_decompose("一", "once", con = con)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
})

test_that("hanzi_components_of finds characters containing 女", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_components_of("女", con = con)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("char", "rank") %in% names(res)))

  # 好 (good), 她 (she), 妈 (mum) all contain the 女 component.
  expect_true(all(c("好", "她") %in% res$char))

  # Results are ordered by ascending frequency rank.
  expect_false(is.unsorted(res$rank))
})
