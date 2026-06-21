# decomp_frame() / hint_components() — type-aware copy + hint parsing.

test_that("decomp_frame enables role badges only for phono-semantic", {
  f <- shinyhanzi:::decomp_frame("pictophonetic")
  expect_identical(f$mode, "phonosemantic")
  expect_true(f$role_badges)
  expect_false(f$show_origin)
  expect_identical(f$badge_labels[["semantic"]], "meaning")
  expect_identical(f$badge_labels[["phonetic"]], "sound")
})

test_that("decomp_frame shows origin for ideographs and pictographs", {
  ideo <- shinyhanzi:::decomp_frame("ideographic")
  expect_identical(ideo$mode, "ideographic")
  expect_true(ideo$show_origin)
  expect_true(ideo$meaning_equation)
  expect_false(ideo$role_badges)

  pic <- shinyhanzi:::decomp_frame("pictographic")
  expect_identical(pic$mode, "pictograph")
  expect_true(pic$show_origin)
  expect_false(pic$role_badges)
})

test_that("decomp_frame falls back to plain for unknown / NA type", {
  f <- shinyhanzi:::decomp_frame(NA_character_)
  expect_identical(f$mode, "plain")
  expect_null(f$title)
  expect_true(nzchar(f$body_label))
})

test_that("decomp_frame supplies non-empty how-to-read copy for typed chars", {
  for (t in c("pictophonetic", "ideographic", "pictographic")) {
    expect_true(nzchar(shinyhanzi:::decomp_frame(t)$how_to_read), info = t)
  }
})

test_that("hint_components extracts inline parts but skips asides", {
  # Inline components are kept.
  expect_setequal(
    shinyhanzi:::hint_components("A woman 女 with a son 子"),
    c("女", "子")
  )
  # Parenthetical representations and cross-references are dropped.
  expect_length(
    shinyhanzi:::hint_components("Represents heaven (天), earth (旦)"),
    0L
  )
  expect_setequal(
    shinyhanzi:::hint_components("Meat on the ribs; compare 肉"),
    character(0)
  )
  # "simplified form of 漢" points to the traditional form, not a component;
  # the real parts after it are kept.
  expect_setequal(
    shinyhanzi:::hint_components("Simplified form of 漢; the Han 𦰩 river 氵"),
    c("𦰩", "氵")
  )
})
