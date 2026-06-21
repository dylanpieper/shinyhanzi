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

test_that("hanzi_decompose tags 形符/聲符 roles for a phono-semantic char", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_decompose("清", "once", con = con)

  expect_true("role" %in% names(res))
  # 清 = 氵 (water, semantic 形符) + 青 (qing, phonetic 聲符).
  expect_identical(res$role[res$component == "氵"], "semantic")
  expect_identical(res$role[res$component == "青"], "phonetic")

  etym <- attr(res, "etymology")
  expect_identical(etym$type, "pictophonetic")
  expect_identical(etym$semantic, "氵")
  expect_identical(etym$phonetic, "青")
})

test_that("hanzi_decompose leaves roles NA for an associative compound", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_decompose("好", "once", con = con)

  expect_true(all(is.na(res$role)))
  expect_identical(attr(res, "etymology")$type, "ideographic")
})

test_that("hanzi_decompose returns the etymological parts, not the stroke split", {
  con <- local_hanzi_db()

  # 光 is written ⺌ + 兀 but comes from 儿 (person) + 火 (fire). We return the
  # meaningful etymological components, never the misleading stroke chunks.
  guang <- shinyhanzi::hanzi_decompose("光", "once", con = con)
  expect_setequal(guang$component, c("儿", "火"))
  expect_false(any(c("⺌", "兀") %in% guang$component))
  expect_identical(attr(guang, "etymology")$type, "ideographic")
})

test_that("hanzi_decompose carries pictograph etymology with a hint", {
  con <- local_hanzi_db()
  res <- shinyhanzi::hanzi_decompose("日", "once", con = con)

  etym <- attr(res, "etymology")
  expect_identical(etym$type, "pictographic")
  expect_true(!is.na(etym$hint) && nzchar(etym$hint))
})

test_that("hanzi_decompose extracts no components from an abstract hint", {
  con <- local_hanzi_db()
  # 一 (one): the hint names representations in parentheses, not real parts.
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
