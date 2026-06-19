#' Decompose a Chinese character into its components
#'
#' Returns the components of a character at one of three decomposition levels,
#' with English glosses and radical labels.
#'
#' @param char A single Chinese character.
#' @param level One of `"once"` (direct split), `"radical"` (down to radical
#'   components), or `"graphical"` (down to pictorial primitives).
#' @param con A DuckDB connection from [hanzi_db()].
#' @return A data frame with columns `component`, `is_intermediate`, `definition`
#'   (component gloss), `radical_name` (if the component is a Kangxi radical).
#' @export
hanzi_decompose <- function(char, level = c("once", "radical", "graphical"),
                             con = hanzi_db()) {
  level <- match.arg(level)
  stopifnot(is.character(char), length(char) == 1L)
  char <- nfc(char)

  if (level == "once" && DBI::dbExistsTable(con, "ids_components")) {
    comps <- tbl(con, "ids_components") |>
      filter(.data$char == !!char) |>
      arrange(.data$position) |>
      select("component") |>
      collect()
    # Filter out non-CJK placeholders (e.g. ？ U+FF1F used by makemeahanzi for unknown components)
    comps <- comps[vapply(comps$component, function(ch) {
      cp <- tryCatch(utf8ToInt(ch)[1L], error = function(e) NA_integer_)
      !is.na(cp) && ((cp >= 0x2E80L && cp <= 0x9FFFL) || (cp >= 0xF900L && cp <= 0xFAFFL))
    }, logical(1L)), ]
    comps$is_intermediate <- FALSE
  } else {
    comps <- tbl(con, "components") |>
      filter(.data$char == !!char, .data$level == !!level,
             !is.na(.data$component), .data$is_intermediate == FALSE) |>
      select("component", "is_intermediate") |>
      distinct() |>
      collect()
  }

  if (nrow(comps) == 0) return(comps)

  # Attach component definitions
  defs <- tbl(con, "characters") |>
    filter(.data$char %in% !!comps$component) |>
    select("char", "definition") |>
    collect()

  # Attach radical names — match primary radical AND variant forms (e.g. 氵 for 水)
  rad_all <- tbl(con, "radicals") |> select("radical", "variants", "meaning") |> collect()

  rad_primary <- rad_all[, c("radical", "meaning")]

  # Build variant → meaning lookup from comma-separated variants column
  variant_rows <- do.call(rbind, lapply(seq_len(nrow(rad_all)), function(i) {
    vs <- trimws(strsplit(rad_all$variants[[i]], ",")[[1]])
    vs <- vs[nzchar(vs)]
    if (!length(vs)) return(NULL)
    data.frame(radical = vs, meaning = rad_all$meaning[[i]], stringsAsFactors = FALSE)
  }))
  rad_lookup <- rbind(rad_primary, variant_rows)

  # Attach primary pinyin
  pinyins <- tbl(con, "readings") |>
    filter(.data$char %in% !!comps$component, .data$is_primary == TRUE) |>
    select("char", "pinyin_toned") |>
    collect()

  comps <- comps |>
    left_join(defs,       by = c("component" = "char")) |>
    left_join(rad_lookup, by = c("component" = "radical")) |>
    left_join(pinyins,    by = c("component" = "char")) |>
    dplyr::rename(definition = "definition", radical_name = "meaning")

  comps
}

#' Find characters that contain a given component
#'
#' Reverse decomposition: returns all characters in the database that contain
#' the specified component at any decomposition level.
#'
#' @param component A Chinese character or component.
#' @param level Decomposition level to search; `NULL` searches all levels.
#' @param con A DuckDB connection from [hanzi_db()].
#' @return A data frame with columns `char` and `rank` (frequency rank, if known).
#' @export
hanzi_components_of <- function(component, level = NULL, con = hanzi_db()) {
  stopifnot(is.character(component), length(component) == 1L)
  component <- nfc(component)

  q <- tbl(con, "components") |>
    filter(.data$component == !!component)

  if (!is.null(level)) q <- filter(q, .data$level == !!level)

  chars <- q |>
    filter(!is.na(.data$char), .data$is_intermediate == FALSE) |>
    select("char") |>
    distinct() |>
    collect()

  # Keep only characters in CJK Unicode ranges — filters out ASCII digits, punctuation,
  # and supplementary-plane extensions that don't render usefully for learners
  chars <- chars[vapply(chars$char, function(ch) {
    cp <- tryCatch(utf8ToInt(ch)[1L], error = function(e) NA_integer_)
    !is.na(cp) && ((cp >= 0x2E80L && cp <= 0x9FFFL) || (cp >= 0xF900L && cp <= 0xFAFFL))
  }, logical(1L)), ]

  # Join frequency ranks for sorting; keep only characters with a known rank
  # (frequency data covers characters that appear in real text corpora, which
  # is a better learner-relevance filter than requiring a cedict entry)
  ranks <- tbl(con, "char_frequency") |>
    select("char", "rank") |>
    collect()

  chars |>
    dplyr::inner_join(ranks, by = "char") |>
    dplyr::arrange(.data$rank)
}
