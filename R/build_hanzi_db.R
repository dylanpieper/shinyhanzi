#' Build the shinyhanzi DuckDB database from raw data sources
#'
#' Downloads and parses all open data sources (makemeahanzi, CC-CEDICT,
#' cjk-decomp, Jun Da frequency, Leiden word frequency, Kangxi radicals) into
#' a single DuckDB file with indexes and a full-text search index over English
#' glosses.
#'
#' Run **once by the package maintainer** to produce the release artifact, or
#' optionally by users who want to rebuild. Never called at app startup.
#'
#' @param out_path Path where the `.duckdb` file will be written.
#' @param sources_dir Optional directory containing pre-downloaded source files.
#'   If `NULL` each source is downloaded with `httr2`.
#' @param overwrite If `TRUE`, overwrite an existing database at `out_path`.
#' @return Invisible path to the written `.duckdb` file.
#' @examples
#' \dontrun{
#' build_hanzi_db()
#' }
#' @export
build_hanzi_db <- function(
    out_path    = file.path(cache_dir(), "shinyhanzi.duckdb"),
    sources_dir = NULL,
    overwrite   = FALSE) {
  if (file.exists(out_path) && !overwrite) {
    cli::cli_abort(
      "Database already exists at {out_path}. Use `overwrite = TRUE` to rebuild."
    )
  }
  if (file.exists(out_path)) unlink(out_path)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  resolve_source <- function(filename, url) {
    if (!is.null(sources_dir)) {
      p <- file.path(sources_dir, filename)
      if (file.exists(p)) return(p)
    }
    dest <- file.path(tempdir(), filename)
    if (!file.exists(dest)) {
      cli::cli_progress_step("Downloading {filename}...")
      httr2::req_perform(httr2::request(url) |> httr2::req_progress(), path = dest)
    }
    dest
  }

  MMAH_URL      <- "https://raw.githubusercontent.com/skishore/makemeahanzi/master/dictionary.txt"
  CEDICT_URL    <- "https://www.mdbg.net/chinese/export/cedict/cedict_1_0_ts_utf-8_mdbg.txt.gz"
  CJKDECOMP_URL <- "https://raw.githubusercontent.com/amake/cjk-decomp/master/cjk-decomp.txt"
  LEIDEN_URL    <- "https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/zh_cn/zh_cn_50k.txt"

  cli::cli_h1("Building shinyhanzi database")

  cli::cli_h2("Parsing makemeahanzi dictionary.txt")
  mmah <- parse_makemeahanzi(resolve_source("dictionary.txt", MMAH_URL))

  cli::cli_h2("Parsing CC-CEDICT")
  cedict_gz   <- resolve_source("cedict_1_0_ts_utf-8_mdbg.txt.gz", CEDICT_URL)
  cedict_path <- sub("\\.gz$", "", cedict_gz)
  if (!file.exists(cedict_path)) {
    con_gz <- gzcon(file(cedict_gz, "rb"))
    writeLines(readLines(con_gz, encoding = "UTF-8"), cedict_path)
    close(con_gz)
  }
  cedict <- parse_cedict(cedict_path)

  cli::cli_h2("Parsing cjk-decomp")
  decomp_df <- parse_cjk_decomp(resolve_source("cjk-decomp.txt", CJKDECOMP_URL))

  cli::cli_h2("Parsing Jun Da character frequency")
  junda_path <- system.file("extdata/junda_freq.tsv", package = "shinyhanzi")
  if (!nzchar(junda_path)) cli::cli_abort("Bundled Jun Da frequency file not found.")
  char_freq <- parse_jun_da(junda_path)

  cli::cli_h2("Parsing word frequency list")
  word_freq <- parse_word_frequency(resolve_source("zh_cn_50k.txt", LEIDEN_URL))

  radicals_path <- system.file("extdata/kangxi_radicals.csv", package = "shinyhanzi")
  radicals_df   <- readr::read_csv(radicals_path, show_col_types = FALSE)

  cli::cli_h2("Writing tables to DuckDB")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = out_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "characters",     mmah$characters, overwrite = TRUE)
  DBI::dbWriteTable(con, "readings",       mmah$readings,   overwrite = TRUE)
  DBI::dbWriteTable(con, "cedict",         cedict$entries,  overwrite = TRUE)
  DBI::dbWriteTable(con, "decomposition",  decomp_df,       overwrite = TRUE)
  DBI::dbWriteTable(con, "char_frequency", char_freq,       overwrite = TRUE)
  DBI::dbWriteTable(con, "word_frequency", word_freq,       overwrite = TRUE)
  DBI::dbWriteTable(con, "radicals",       radicals_df,     overwrite = TRUE)

  cli::cli_h2("Building derived tables")
  build_word_chars_table(con)
  build_components_table(con)
  build_ids_components_table(con)

  cli::cli_h2("Creating indexes")
  indexes <- c(
    "CREATE INDEX IF NOT EXISTS idx_characters_char     ON characters(char);",
    "CREATE INDEX IF NOT EXISTS idx_readings_char        ON readings(char);",
    "CREATE INDEX IF NOT EXISTS idx_cedict_simplified    ON cedict(simplified);",
    "CREATE INDEX IF NOT EXISTS idx_cedict_traditional   ON cedict(traditional);",
    "CREATE INDEX IF NOT EXISTS idx_decomp_char          ON decomposition(char);",
    "CREATE INDEX IF NOT EXISTS idx_components_char      ON components(char);",
    "CREATE INDEX IF NOT EXISTS idx_components_component ON components(component);",
    "CREATE INDEX IF NOT EXISTS idx_word_chars_char      ON word_chars(char);",
    "CREATE INDEX IF NOT EXISTS idx_word_chars_word      ON word_chars(word);",
    "CREATE INDEX IF NOT EXISTS idx_char_freq_char       ON char_frequency(char);"
  )
  walk(indexes, \(sql) DBI::dbExecute(con, sql))

  cli::cli_h2("Building full-text search index")
  build_fts_index(con)

  DBI::dbExecute(con, "ANALYZE;")
  cli::cli_alert_success("Database built: {out_path}")
  invisible(out_path)
}

# ---- Internal parsers -------------------------------------------------------

#' @keywords internal
parse_makemeahanzi <- function(path) {
  lines <- readr::read_lines(path, progress = FALSE)
  lines <- lines[nzchar(lines)]

  parsed <- compact(map(lines, function(line) {
    entry <- tryCatch(jsonlite::fromJSON(line), error = function(e) NULL)
    if (is.null(entry)) return(NULL)

    ch      <- nfc(entry$character)
    etym    <- entry$etymology %||% list()
    pinyins <- entry$pinyin

    char_row <- data.frame(
      char           = ch,
      definition     = entry$definition        %||% NA_character_,
      radical        = nfc(entry$radical       %||% NA_character_),
      decomposition  = entry$decomposition     %||% NA_character_,
      etymology_type = etym$type               %||% NA_character_,
      phonetic       = nfc(etym$phonetic       %||% NA_character_),
      semantic       = nfc(etym$semantic       %||% NA_character_),
      etymology_hint = etym$hint               %||% NA_character_,
      stringsAsFactors = FALSE
    )

    reading_row <- if (!is.null(pinyins) && length(pinyins) > 0) {
      data.frame(
        char            = ch,
        pinyin_toned    = pinyins,
        pinyin_numbered = map_chr(pinyins, toned_to_numbered),
        is_primary      = c(TRUE, rep(FALSE, length(pinyins) - 1L)),
        source          = "makemeahanzi",
        stringsAsFactors = FALSE
      )
    }

    list(char_row = char_row, reading_row = reading_row)
  }))

  list(
    characters = dplyr::bind_rows(map(parsed, "char_row")),
    readings   = dplyr::bind_rows(compact(map(parsed, "reading_row")))
  )
}

#' @keywords internal
parse_cedict <- function(path) {
  lines <- readr::read_lines(path, progress = FALSE,
                              locale = readr::locale(encoding = "UTF-8"))
  lines <- lines[!startsWith(lines, "#") & nzchar(lines)]

  pattern <- "^(\\S+)\\s+(\\S+)\\s+\\[([^\\]]+)\\]\\s+/(.+)/$"
  m <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  valid <- lengths(m) == 5L
  m     <- m[valid]

  traditional     <- vapply(m, `[[`, character(1), 2L)
  simplified      <- vapply(m, `[[`, character(1), 3L)
  pinyin_numbered <- vapply(m, `[[`, character(1), 4L)
  gloss_raw       <- vapply(m, `[[`, character(1), 5L)

  pinyin_numbered <- fix_cedict_umlaut(pinyin_numbered)
  pinyin_toned    <- vapply(pinyin_numbered, numbered_to_toned, character(1))
  gloss           <- gsub("/", " / ", gloss_raw, fixed = TRUE)

  data.frame(
    id              = seq_along(m),
    traditional     = nfc(traditional),
    simplified      = nfc(simplified),
    pinyin_numbered = pinyin_numbered,
    pinyin_toned    = pinyin_toned,
    gloss           = gloss,
    is_word         = nchar(simplified) > 1L,
    stringsAsFactors = FALSE
  ) |> (\(df) list(entries = df))()
}

#' @keywords internal
parse_cjk_decomp <- function(path) {
  lines <- readr::read_lines(path, progress = FALSE)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  pattern <- "^([^:]+):([a-zA-Z0-9]+)\\((.*)\\)$"
  m <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  valid <- lengths(m) == 4L
  m     <- m[valid]

  dplyr::bind_rows(map(m, function(match) {
    ch          <- nfc(match[[2]])
    decomp_type <- match[[3]]
    comp_str    <- match[[4]]
    is_inter    <- is_intermediate_token(match[[2]])

    if (!nzchar(comp_str)) {
      data.frame(char = ch, decomp_type = decomp_type, comp_index = 0L,
                 component = NA_character_, is_intermediate = is_inter,
                 stringsAsFactors = FALSE)
    } else {
      comps <- nfc(strsplit(comp_str, ",", fixed = TRUE)[[1]])
      data.frame(char = ch, decomp_type = decomp_type,
                 comp_index = seq_along(comps), component = comps,
                 is_intermediate = is_inter | is_intermediate_token(comps),
                 stringsAsFactors = FALSE)
    }
  }))
}

#' @keywords internal
parse_jun_da <- function(path) {
  lines <- readr::read_lines(path, progress = FALSE)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  parts <- strsplit(lines, "\t", fixed = TRUE)
  valid <- lengths(parts) >= 3L
  parts <- parts[valid]

  rank  <- as.integer(vapply(parts, `[[`, character(1), 1L))
  char  <- nfc(vapply(parts, `[[`, character(1), 2L))
  count <- as.numeric(vapply(parts, `[[`, character(1), 3L))

  total          <- sum(count, na.rm = TRUE)
  cumulative_pct <- cumsum(count) / total * 100

  data.frame(
    char           = char,
    rank           = rank,
    count          = count,
    cumulative_pct = cumulative_pct,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
parse_word_frequency <- function(path) {
  lines <- readr::read_lines(path, progress = FALSE)
  lines <- lines[nzchar(lines)]

  parts <- strsplit(lines, " ", fixed = TRUE)
  valid <- lengths(parts) >= 2L
  parts <- parts[valid]

  word  <- nfc(vapply(parts, `[[`, character(1), 1L))
  count <- as.numeric(vapply(parts, `[[`, character(1), 2L))
  rank  <- seq_along(word)

  band <- dplyr::case_when(
    rank <= 3000  ~ "Common",
    rank <= 15000 ~ "Uncommon",
    TRUE          ~ "Rare"
  )

  data.frame(word = word, rank = rank, count = count, band = band,
             stringsAsFactors = FALSE)
}

# IDS layout operators U+2FF0–U+2FFB; strip these to get bare components
IDS_OPERATORS <- intToUtf8(0x2FF0:0x2FFB, multiple = TRUE)

#' @keywords internal
parse_ids_components <- function(ids) {
  if (is.na(ids) || !nzchar(ids)) return(character(0))
  chars <- strsplit(ids, "", fixed = TRUE)[[1]]
  chars <- chars[!chars %in% IDS_OPERATORS]
  chars <- nfc(chars)
  chars[nzchar(chars)]
}

#' @keywords internal
build_ids_components_table <- function(con) {
  chars_df <- DBI::dbGetQuery(con, "SELECT char, decomposition FROM characters WHERE decomposition IS NOT NULL")
  rows <- compact(map(seq_len(nrow(chars_df)), function(i) {
    ch    <- chars_df$char[[i]]
    comps <- parse_ids_components(chars_df$decomposition[[i]])
    comps <- comps[comps != ch]           # exclude self-reference
    if (!length(comps)) return(NULL)
    data.frame(char = ch, component = comps, position = seq_along(comps),
               stringsAsFactors = FALSE)
  }))
  ids_tbl <- dplyr::bind_rows(rows)
  DBI::dbWriteTable(con, "ids_components", ids_tbl, overwrite = TRUE)
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ids_char ON ids_components(char);")
  cli::cli_alert_success("ids_components: {nrow(ids_tbl)} rows")
  invisible(NULL)
}

# ---- Derived table builders -------------------------------------------------

#' @keywords internal
build_word_chars_table <- function(con) {
  words <- DBI::dbGetQuery(
    con, "SELECT simplified FROM cedict WHERE is_word = TRUE"
  )$simplified
  words <- unique(words)

  wc <- dplyr::bind_rows(compact(map(words, function(w) {
    chars <- strsplit(w, "", fixed = TRUE)[[1]]
    if (!length(chars)) return(NULL)
    data.frame(word = w, char = chars, position = seq_along(chars),
               stringsAsFactors = FALSE)
  })))
  DBI::dbWriteTable(con, "word_chars", wc, overwrite = TRUE)
  cli::cli_alert_success("word_chars: {nrow(wc)} rows")
  invisible(NULL)
}

#' @keywords internal
build_components_table <- function(con) {
  decomp <- DBI::dbGetQuery(
    con,
    "SELECT char, comp_index, component, is_intermediate
     FROM decomposition WHERE component IS NOT NULL AND comp_index > 0"
  )

  adj <- split(decomp$component, decomp$char)

  expand_char <- function(ch, max_depth = 10L) {
    seen     <- character(0)
    frontier <- unique(adj[[ch]] %||% character(0))
    frontier <- frontier[!is.na(frontier)]
    all_comp <- frontier

    for (d in seq_len(max_depth)) {
      if (!length(frontier)) break
      nxt <- character(0)
      for (comp in frontier) {
        if (comp %in% seen) next
        seen <- c(seen, comp)
        children <- unique(adj[[comp]] %||% character(0))
        children <- children[!is.na(children) & !children %in% seen]
        nxt      <- c(nxt, children)
      }
      frontier <- unique(nxt)
      all_comp <- unique(c(all_comp, frontier))
    }
    all_comp
  }

  radicals_set <- DBI::dbGetQuery(con, "SELECT radical FROM radicals")$radical
  all_chars    <- unique(decomp$char)

  components <- unique(dplyr::bind_rows(compact(map(all_chars, function(ch) {
    direct    <- unique(adj[[ch]] %||% character(0))
    direct    <- direct[!is.na(direct)]
    graphical <- expand_char(ch)
    rad_lvl   <- direct[direct %in% radicals_set]

    parts <- compact(list(
      if (length(direct) > 0)
        data.frame(char = ch, component = direct, level = "once",
                   is_intermediate = is_intermediate_token(direct),
                   stringsAsFactors = FALSE),
      if (length(rad_lvl) > 0)
        data.frame(char = ch, component = rad_lvl, level = "radical",
                   is_intermediate = FALSE, stringsAsFactors = FALSE),
      if (length(graphical) > 0)
        data.frame(char = ch, component = graphical, level = "graphical",
                   is_intermediate = is_intermediate_token(graphical),
                   stringsAsFactors = FALSE)
    ))
    if (!length(parts)) return(NULL)
    dplyr::bind_rows(parts)
  }))))
  DBI::dbWriteTable(con, "components", components, overwrite = TRUE)
  cli::cli_alert_success("components: {nrow(components)} rows")
  invisible(NULL)
}

#' @keywords internal
build_fts_index <- function(con) {
  tryCatch(DBI::dbExecute(con, "LOAD fts;"), error = function(e) {
    DBI::dbExecute(con, "INSTALL fts;")
    DBI::dbExecute(con, "LOAD fts;")
  })
  DBI::dbExecute(con, "PRAGMA create_fts_index('cedict', 'id', 'gloss');")
  cli::cli_alert_success("FTS index created on cedict.gloss")
  invisible(NULL)
}

# ---- Pinyin conversion ------------------------------------------------------

TONED_VOWELS <- c(
  "a1" = "\u0101", "a2" = "\u00e1", "a3" = "\u01ce", "a4" = "\u00e0", "a5" = "a",
  "e1" = "\u0113", "e2" = "\u00e9", "e3" = "\u011b", "e4" = "\u00e8", "e5" = "e",
  "i1" = "\u012b", "i2" = "\u00ed", "i3" = "\u01d0", "i4" = "\u00ec", "i5" = "i",
  "o1" = "\u014d", "o2" = "\u00f3", "o3" = "\u01d2", "o4" = "\u00f2", "o5" = "o",
  "u1" = "\u016b", "u2" = "\u00fa", "u3" = "\u01d4", "u4" = "\u00f9", "u5" = "u",
  "\u00fc1" = "\u01d6", "\u00fc2" = "\u01d8", "\u00fc3" = "\u01da", "\u00fc4" = "\u01dc",
  "\u00fc5" = "\u00fc"
)

#' @keywords internal
numbered_to_toned <- function(x) {
  syllables <- strsplit(x, " ", fixed = TRUE)[[1]]
  toned <- vapply(syllables, function(syl) {
    if (!grepl("[1-5]$", syl)) return(syl)
    tone <- substr(syl, nchar(syl), nchar(syl))
    body <- substr(syl, 1, nchar(syl) - 1)
    vowels <- c("a", "e", "ou", "\u00fc", "u", "i", "o")
    marked <- body
    for (v in vowels) {
      if (grepl(v, body, fixed = TRUE)) {
        key <- paste0(v, tone)
        if (!is.na(TONED_VOWELS[key])) {
          marked <- sub(v, TONED_VOWELS[[key]], body, fixed = TRUE)
          break
        }
      }
    }
    marked
  }, character(1))
  paste(toned, collapse = " ")
}

#' @keywords internal
toned_to_numbered <- function(x) {
  # Process each space-separated syllable independently so the tone digit
  # goes at the end of the syllable, not immediately after the toned vowel.
  syllables <- strsplit(x, " ", fixed = TRUE)[[1]]
  result <- vapply(syllables, function(syl) {
    for (nm in names(TONED_VOWELS)) {
      tone    <- substr(nm, nchar(nm), nchar(nm))
      if (tone == "5") next
      toned_v <- TONED_VOWELS[[nm]]
      vowel   <- substr(nm, 1, nchar(nm) - 1)
      if (grepl(toned_v, syl, fixed = TRUE)) {
        plain <- gsub(toned_v, vowel, syl, fixed = TRUE)
        return(paste0(plain, tone))
      }
    }
    syl
  }, character(1))
  paste(result, collapse = " ")
}
