# Attach definition, radical_name and primary pinyin to a data frame of
# components (a column `component`). Shared by structural decomposition and the
# etymological origin components so both render identically.
enrich_components <- function(con, df) {
  if (nrow(df) == 0) return(df)

  defs <- tbl(con, "characters") |>
    filter(.data$char %in% !!df$component) |>
    select("char", "definition") |>
    collect()

  # Radical names — match primary radical AND variant forms (e.g. 氵 for 水)
  rad_all <- tbl(con, "radicals") |> select("radical", "variants", "meaning") |> collect()
  rad_primary <- rad_all[, c("radical", "meaning")]
  variant_rows <- do.call(rbind, lapply(seq_len(nrow(rad_all)), function(i) {
    vs <- trimws(strsplit(rad_all$variants[[i]], ",")[[1]])
    vs <- vs[nzchar(vs)]
    if (!length(vs)) return(NULL)
    data.frame(radical = vs, meaning = rad_all$meaning[[i]], stringsAsFactors = FALSE)
  }))
  rad_lookup <- rbind(rad_primary, variant_rows)
  # One meaning per radical (primary listed first, so it wins) — otherwise a
  # component matching both a primary and a variant row multiplies in the join
  rad_lookup <- rad_lookup[!duplicated(rad_lookup$radical), ]

  pinyins <- tbl(con, "readings") |>
    filter(.data$char %in% !!df$component, .data$is_primary == TRUE) |>
    select("char", "pinyin_toned") |>
    collect()

  df |>
    left_join(defs,       by = c("component" = "char")) |>
    left_join(rad_lookup, by = c("component" = "radical")) |>
    left_join(pinyins,    by = c("component" = "char")) |>
    dplyr::rename(definition = "definition", radical_name = "meaning")
}

#' Break a Chinese character into its meaningful components
#'
#' Returns the components that actually carry meaning or sound, with English
#' glosses and radical labels. For a phono-semantic character these are its
#' 形符 (semantic) and 聲符 (phonetic) parts; for other characters they are the
#' components named in its etymology. The raw graphical stroke split is never
#' returned — it carries no reliable meaning or sound and can mislead learners.
#'
#' @param char A single Chinese character.
#' @param level One of `"once"` (the character's meaningful components — the
#'   semantic/phonetic parts of a phono-semantic character, or the components
#'   named in its etymology), `"radical"`, or `"graphical"` (structural
#'   breakdown from the `components` table).
#' @param con A DuckDB connection from [hanzi_db()].
#' @return A data frame with columns `component`, `is_intermediate`, `definition`
#'   (component gloss), `radical_name` (if the component is a Kangxi radical), and
#'   `role` (`"semantic"`/`"phonetic"` for the 形符/聲符 of a phono-semantic
#'   character, otherwise `NA`). The character's etymology is attached as an
#'   attribute `attr(., "etymology")`: a list with `type`, `semantic`,
#'   `phonetic`, and `hint`.
#' @export
hanzi_decompose <- function(char, level = c("once", "radical", "graphical"),
                             con = hanzi_db()) {
  level <- match.arg(level)
  stopifnot(is.character(char), length(char) == 1L)
  char <- nfc(char)

  # Character-level etymology — the source of the meaningful components and the
  # type-aware framing (形符 / 聲符 roles, the pictograph/associative reading).
  etym_row <- tbl(con, "characters") |>
    filter(.data$char == !!char) |>
    select("etymology_type", "semantic", "phonetic", "etymology_hint") |>
    collect()
  etym <- if (nrow(etym_row) > 0) {
    list(
      type     = etym_row$etymology_type[[1L]],
      semantic = etym_row$semantic[[1L]],
      phonetic = etym_row$phonetic[[1L]],
      hint     = etym_row$etymology_hint[[1L]]
    )
  } else {
    list(type = NA_character_, semantic = NA_character_,
         phonetic = NA_character_, hint = NA_character_)
  }

  if (level == "once") {
    # Meaningful components only — never the raw stroke split. Phono-semantic
    # characters use their 形符/聲符 fields; everything else uses the components
    # named in the etymology hint (e.g. 儿 + 火 for 光).
    if (!is.na(etym$type) && etym$type == "pictophonetic" &&
        (!is.na(etym$semantic) || !is.na(etym$phonetic))) {
      parts <- c(semantic = etym$semantic, phonetic = etym$phonetic)
      parts <- parts[!is.na(parts)]
      comps <- data.frame(
        component        = unname(parts),
        is_intermediate  = rep(FALSE, length(parts)),
        role             = names(parts),
        stringsAsFactors = FALSE
      )
    } else {
      parts <- setdiff(hint_components(etym$hint), char)
      comps <- data.frame(
        component        = parts,
        is_intermediate  = rep(FALSE, length(parts)),
        role             = rep(NA_character_, length(parts)),
        stringsAsFactors = FALSE
      )
    }
  } else {
    comps <- tbl(con, "components") |>
      filter(.data$char == !!char, .data$level == !!level,
             !is.na(.data$component), .data$is_intermediate == FALSE) |>
      select("component", "is_intermediate") |>
      distinct() |>
      collect()
    comps$role <- NA_character_
  }

  if (nrow(comps) == 0) {
    comps <- tibble::as_tibble(comps)
    attr(comps, "etymology") <- etym
    return(comps)
  }

  comps <- tibble::as_tibble(enrich_components(con, comps))
  attr(comps, "etymology") <- etym
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
