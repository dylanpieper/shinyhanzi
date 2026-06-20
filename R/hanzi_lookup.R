#' Look up a Chinese character in the database
#'
#' Returns dictionary entries and character metadata for a single character,
#' organised by data source.
#'
#' @param char A single Chinese character (length-1 character string).
#' @param con A DuckDB connection from [hanzi_db()].
#' @return A named list with three elements:
#'   - `char`: the input character
#'   - `mmah`: list of makemeahanzi fields — `definition`, `radical`,
#'     `etymology_type`, `phonetic`, `semantic`, `etymology_hint`
#'   - `cedict`: data frame of CC-CEDICT rows (`traditional`, `pinyin_toned`,
#'     `pinyin_numbered`, `gloss`, `is_word`)
#'
#'   To get the primary pinyin reading use [hanzi_pinyin()].
#' @export
hanzi_lookup <- function(char, con = hanzi_db()) {
  stopifnot(is.character(char), length(char) == 1L)
  char <- nfc(char)

  char_row <- tbl(con, "characters") |>
    filter(.data$char == !!char) |>
    collect()

  entries <- tbl(con, "cedict") |>
    filter(.data$simplified == !!char) |>
    select("traditional", "pinyin_toned", "pinyin_numbered", "gloss", "is_word") |>
    collect()

  scalar <- function(field) {
    if (nrow(char_row) > 0) char_row[[field]][[1]] else NA_character_
  }

  list(
    char   = char,
    mmah   = list(
      definition     = scalar("definition"),
      radical        = scalar("radical"),
      etymology_type = scalar("etymology_type"),
      phonetic       = scalar("phonetic"),
      semantic       = scalar("semantic"),
      etymology_hint = scalar("etymology_hint")
    ),
    cedict = entries
  )
}

#' Get the primary pinyin for a Chinese character
#'
#' @param char A single Chinese character.
#' @param con A DuckDB connection from [hanzi_db()].
#' @param toned If `TRUE` (default) returns toned pinyin (e.g. "hǎo"); if
#'   `FALSE` returns numbered pinyin (e.g. "hao3").
#' @return A character string, or `NA` if not found.
#' @export
hanzi_pinyin <- function(char, con = hanzi_db(), toned = TRUE) {
  stopifnot(is.character(char), length(char) == 1L)
  char <- nfc(char)

  row <- tbl(con, "readings") |>
    filter(.data$char == !!char, .data$is_primary == TRUE) |>
    collect()

  if (nrow(row) == 0) {
    # Fallback to hanyupinyin
    tryCatch(
      return(hanyupinyin::to_pinyin(char)),
      error = function(e) return(NA_character_)
    )
  }

  if (toned) row$pinyin_toned[[1]] else row$pinyin_numbered[[1]]
}
