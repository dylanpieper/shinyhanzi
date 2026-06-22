test_that("parse_makemeahanzi parses entries correctly", {
  fixture <- paste(
    '{"character":"有","definition":"have / possess","pinyin":["yǒu"],"radical":"月","decomposition":"⿱𠂇月","etymology":{"type":"ideographic","hint":"To hold 𠂇 the moon 月 — meaning possession"}}',
    '{"character":"青","definition":"green / blue","pinyin":["qīng"],"radical":"青","decomposition":"⿱⺼月","etymology":{"type":"pictophonetic","phonetic":"月","semantic":"生"}}',
    '{"character":"好","definition":"good","pinyin":["hǎo","hào"],"radical":"女","decomposition":"⿰女子","etymology":{"type":"ideographic"}}',
    sep = "\n"
  )
  tmp <- tempfile(fileext = ".txt")
  writeLines(fixture, tmp)
  on.exit(unlink(tmp))

  result <- shinyhanzi:::parse_makemeahanzi(tmp)

  expect_named(result, c("characters", "readings"))
  expect_equal(nrow(result$characters), 3L)
  expect_true("char"           %in% names(result$characters))
  expect_true("etymology_type" %in% names(result$characters))
  expect_true("phonetic"       %in% names(result$characters))

  # Multiple readings
  hao_readings <- result$readings[result$readings$char == "好", ]
  expect_equal(nrow(hao_readings), 2L)
  expect_true(hao_readings$is_primary[[1]])
  expect_false(hao_readings$is_primary[[2]])
})

test_that("parse_cedict parses entries correctly", {
  fixture <- paste(
    "# CC-CEDICT",
    "有 有 [you3] /to have/there is/",
    "好 好 [hao3] /good/well/proper/",
    "好 好 [hao4] /to be fond of/to have a tendency to/",
    "電話 电话 [dian4 hua4] /telephone/phone call/",
    "u:hen 于人 [u:hen1] /test u-umlaut/",
    sep = "\n"
  )
  tmp <- tempfile(fileext = ".txt")
  writeLines(fixture, tmp, useBytes = TRUE)
  on.exit(unlink(tmp))

  result <- shinyhanzi:::parse_cedict(tmp)
  df     <- result$entries

  expect_true("id"          %in% names(df))
  expect_true("simplified"  %in% names(df))
  expect_true("is_word"     %in% names(df))
  expect_true("gloss"       %in% names(df))

  # 电话 is a word (nchar > 1)
  dianhua <- df[df$simplified == "电话", ]
  expect_true(nrow(dianhua) >= 1)
  expect_true(dianhua$is_word[[1]])

  # Single characters are not words
  you_row <- df[df$simplified == "有", ]
  expect_false(you_row$is_word[[1]])
})

test_that("parse_cjk_decomp handles atoms and components", {
  fixture <- paste(
    "一:a()",
    "好:c(女,子)",
    "有:c(𠂇,月)",
    sep = "\n"
  )
  tmp <- tempfile(fileext = ".txt")
  writeLines(fixture, tmp, useBytes = TRUE)
  on.exit(unlink(tmp))

  df <- shinyhanzi:::parse_cjk_decomp(tmp)

  expect_true("char"       %in% names(df))
  expect_true("comp_index" %in% names(df))
  expect_false("is_intermediate" %in% names(df))

  # Atom: 一 has comp_index == 0 and no component
  yi <- df[df$char == "一", ]
  expect_equal(yi$comp_index[[1]], 0L)
  expect_true(is.na(yi$component[[1]]))

  # 好 decomposes into 女 + 子
  hao <- df[df$char == "好", ]
  expect_setequal(hao$component, c("女", "子"))
})

test_that("numbered_to_toned converts correctly", {
  expect_equal(shinyhanzi:::numbered_to_toned("hao3"), "hǎo")
  expect_equal(shinyhanzi:::numbered_to_toned("ni3 hao3"), "nǐ hǎo")
  expect_equal(shinyhanzi:::numbered_to_toned("lü4"), "lǜ")
})

test_that("numbered_to_toned places the mark per Hanyu Pinyin rules", {
  # 'a'/'e' always win.
  expect_equal(shinyhanzi:::numbered_to_toned("xie4"), "xiè")
  expect_equal(shinyhanzi:::numbered_to_toned("yue4"), "yuè")
  # "ou" marks the o.
  expect_equal(shinyhanzi:::numbered_to_toned("ou1"), "ōu")
  # Otherwise the *last* vowel: uo/ui mark o/i, but iu marks u.
  expect_equal(shinyhanzi:::numbered_to_toned("guo2"), "guó")
  expect_equal(shinyhanzi:::numbered_to_toned("gui4"), "guì")
  expect_equal(shinyhanzi:::numbered_to_toned("shui3"), "shuǐ")
  expect_equal(shinyhanzi:::numbered_to_toned("liu2"), "liú")
  # Neutral tone (5) leaves the syllable unmarked; multi-syllable round-trips.
  expect_equal(shinyhanzi:::numbered_to_toned("zhong1 guo2"), "zhōng guó")
  expect_equal(shinyhanzi:::numbered_to_toned("dui4 bu5 qi3"), "duì bu qǐ")
})

test_that("toned_to_numbered converts correctly", {
  expect_equal(shinyhanzi:::toned_to_numbered("hǎo"), "hao3")
  expect_equal(shinyhanzi:::toned_to_numbered("nǐ hǎo"), "ni3 hao3")
})
