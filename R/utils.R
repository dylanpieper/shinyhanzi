pkg_env <- new.env(parent = emptyenv())

# 5-digit cjk-decomp internal token (non-Unicode intermediate component)
is_intermediate_token <- function(x) {
  grepl("^[0-9]{5}$", x, perl = TRUE)
}

# Split pinyin in numbered form (e.g. "hao3") into list(initial, final, tone)
split_pinyin <- function(x) {
  if (is.na(x) || !nzchar(x)) return(list(initial = NA_character_, final = NA_character_, tone = NA_character_))

  tone <- NA_character_
  body <- x
  if (grepl("[1-5]$", x)) {
    tone <- substr(x, nchar(x), nchar(x))
    body <- substr(x, 1, nchar(x) - 1)
  }

  # Multi-char initials first (longest-match)
  initials <- c("zh", "ch", "sh", "b", "p", "m", "f", "d", "t", "n",
                 "l", "g", "k", "h", "j", "q", "x", "r", "z", "c", "s",
                 "y", "w")
  initial <- NA_character_
  final   <- body
  for (ini in initials) {
    if (startsWith(body, ini)) {
      initial <- ini
      final   <- substr(body, nchar(ini) + 1, nchar(body))
      break
    }
  }

  list(initial = initial, final = final, tone = tone)
}

# NFC-normalize a character vector
nfc <- function(x) stringi::stri_trans_nfc(x)

# Replace CC-CEDICT u: with ü
fix_cedict_umlaut <- function(x) gsub("u:", "\u00fc", x, fixed = TRUE)

cache_dir <- function() tools::R_user_dir("shinyhanzi", "data")

# Convert numbered pinyin references embedded in CC-CEDICT glosses to toned form.
# e.g. "variant of 饷[xiang3]" -> "variant of 饷[xiāng]"
convert_gloss_pinyin <- function(gloss) {
  if (is.na(gloss)) return(gloss)
  stringr::str_replace_all(gloss, "\\[([^\\]]+)\\]", function(m) {
    vapply(m, function(match) {
      inner <- substr(match, 2L, nchar(match) - 1L)
      if (grepl("[a-zA-Z][1-5]", inner, perl = TRUE))
        paste0("[", numbered_to_toned(inner), "]")
      else
        match
    }, character(1L), USE.NAMES = FALSE)
  })
}
