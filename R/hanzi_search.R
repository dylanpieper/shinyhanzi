#' Search for Chinese characters and words by English meaning or pinyin
#'
#' Runs a BM25 full-text search over CC-CEDICT glosses (English) and a
#' tone-insensitive LIKE match over pinyin, merging and deduplicating results.
#' Pinyin queries may use tone marks (shǔi), tone numbers (shui3), or neither (shui).
#'
#' @param query An English word/phrase or pinyin syllable(s).
#' @param n Maximum results to return.
#' @param reweight_by_frequency If `TRUE`, BM25 scores are multiplied by the
#'   inverse log of character frequency rank so common characters surface first.
#' @param con A DuckDB connection from [hanzi_db()].
#' @return A tibble with columns `simplified`, `traditional`, `pinyin_toned`,
#'   `gloss`, `score`, `freq_rank`.
#' @export
hanzi_search <- function(query, n = 20L, reweight_by_frequency = TRUE,
                          con = hanzi_db()) {
  stopifnot(is.character(query), length(query) == 1L, nzchar(query))
  query <- stringr::str_squish(query)

  results <- list()

  # ---- Pinyin search (runs first so its results win deduplication) --------
  # Normalize: strip tone diacritics → ASCII, lowercase, keep only a-z/spaces/1-5
  pinyin_q <- tolower(stringi::stri_trans_general(query, "Latin-ASCII"))
  pinyin_q <- stringr::str_replace_all(pinyin_q, "[^a-z 1-5]", "")
  pinyin_q <- stringr::str_squish(pinyin_q)

  if (nzchar(pinyin_q)) {
    has_tone_num <- grepl("[1-5]", pinyin_q)
    db_expr <- if (has_tone_num)
      "lower(c.pinyin_numbered)"
    else
      "regexp_replace(lower(c.pinyin_numbered), '[1-5]', '', 'g')"

    freq_order <- if (reweight_by_frequency)
      "COALESCE(cf.rank, 99999)"
    else
      "c.id"

    sql_py <- sprintf(
      "SELECT c.simplified, c.traditional, c.pinyin_toned, c.gloss,
              NULL::DOUBLE AS score, cf.rank AS freq_rank
       FROM cedict c
       LEFT JOIN char_frequency cf ON cf.char = c.simplified
       WHERE %s LIKE '%%%s%%'
       ORDER BY %s
       LIMIT %d",
      db_expr,
      gsub("'", "''", pinyin_q),
      freq_order,
      as.integer(n)
    )
    results$pinyin <- tryCatch(
      DBI::dbGetQuery(con, sql_py),
      error = function(e) {
        cli::cli_warn("Pinyin search failed: {conditionMessage(e)}")
        data.frame()
      }
    )
  }

  # ---- English FTS --------------------------------------------------------
  # Normalize diacritics first so toned pinyin ("shǔi") becomes ASCII ("shui")
  # before stripping non-alphanumeric characters for the FTS tokenizer
  query_ascii <- stringi::stri_trans_general(query, "Latin-ASCII")
  query_clean <- stringr::str_squish(
    stringr::str_replace_all(query_ascii, "[^a-zA-Z0-9 ]", " ")
  )
  if (nzchar(query_clean)) {
    order_expr <- if (reweight_by_frequency)
      "score * (1.0 / LOG(COALESCE(CAST(freq_rank AS DOUBLE), 10000) + 1))"
    else
      "score"
    sql_fts <- sprintf(
      "SELECT simplified, traditional, pinyin_toned, gloss, score, freq_rank
       FROM (
         SELECT c.simplified, c.traditional, c.pinyin_toned, c.gloss,
                fts_main_cedict.match_bm25(c.id, '%s') AS score,
                cf.rank AS freq_rank
         FROM cedict c
         LEFT JOIN char_frequency cf ON cf.char = c.simplified
       ) sub
       WHERE score IS NOT NULL
       ORDER BY %s DESC
       LIMIT %d",
      gsub("'", "''", query_clean),
      order_expr,
      as.integer(n)
    )
    results$fts <- tryCatch(
      DBI::dbGetQuery(con, sql_fts),
      error = function(e) {
        cli::cli_warn("FTS search failed: {conditionMessage(e)}")
        data.frame()
      }
    )
  }

  # ---- Gloss LIKE search --------------------------------------------------
  # Catches words like "hello" that FTS misses due to ";" tokenization in CEDICT
  if (nzchar(query_clean)) {
    like_q <- gsub("'", "''", tolower(query_clean))
    sql_like <- sprintf(
      "SELECT c.simplified, c.traditional, c.pinyin_toned, c.gloss,
              NULL::DOUBLE AS score, cf.rank AS freq_rank
       FROM cedict c
       LEFT JOIN char_frequency cf ON cf.char = c.simplified
       WHERE lower(c.gloss) LIKE '%%%s%%'
       ORDER BY COALESCE(cf.rank, 99999)
       LIMIT %d",
      like_q,
      as.integer(n)
    )
    results$like <- tryCatch(
      DBI::dbGetQuery(con, sql_like),
      error = function(e) data.frame()
    )
  }

  # ---- Merge --------------------------------------------------------------
  # FTS hits first (English relevance), then pinyin-only hits by frequency
  combined <- do.call(rbind, Filter(function(d) is.data.frame(d) && nrow(d) > 0, results))
  if (is.null(combined) || nrow(combined) == 0) return(tibble::tibble())

  # Deduplicate: keep first occurrence per (simplified, pinyin_toned) pair
  key <- paste(combined$simplified, combined$pinyin_toned, sep = "")
  combined <- combined[!duplicated(key), ]
  tibble::as_tibble(combined[seq_len(min(nrow(combined), as.integer(n))), ])
}
