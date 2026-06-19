#' Look up a Chinese character in the database
#'
#' Returns dictionary entries, pinyin readings, radical, and etymology
#' information for a single character.
#'
#' @param char A single Chinese character (length-1 character string).
#' @param con A DuckDB connection from [hanzi_db()].
#' @return A named list with elements:
#'   - `char`: the input character
#'   - `definition`: makemeahanzi short definition
#'   - `radical`: the character's radical
#'   - `etymology_type`: "ideographic", "pictographic", or "pictophonetic"
#'   - `phonetic`, `semantic`, `etymology_hint`: etymology fields
#'   - `entries`: data frame of CC-CEDICT rows (pinyin_toned, pinyin_numbered, gloss, traditional, is_word)
#'   - `primary_pinyin`: toned pinyin of the primary reading
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

  primary <- tbl(con, "readings") |>
    filter(.data$char == !!char, .data$is_primary == TRUE) |>
    arrange(desc(.data$source == "makemeahanzi")) |>
    collect()

  primary_pinyin <- if (nrow(primary) > 0) primary$pinyin_toned[[1]] else NA_character_

  list(
    char           = char,
    definition     = if (nrow(char_row) > 0) char_row$definition[[1]]     else NA_character_,
    radical        = if (nrow(char_row) > 0) char_row$radical[[1]]        else NA_character_,
    etymology_type = if (nrow(char_row) > 0) char_row$etymology_type[[1]] else NA_character_,
    phonetic       = if (nrow(char_row) > 0) char_row$phonetic[[1]]       else NA_character_,
    semantic       = if (nrow(char_row) > 0) char_row$semantic[[1]]       else NA_character_,
    etymology_hint = if (nrow(char_row) > 0) char_row$etymology_hint[[1]] else NA_character_,
    entries        = entries,
    primary_pinyin = primary_pinyin
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
