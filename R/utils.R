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

# Han characters named in a free-text etymology hint (e.g. "A woman 女 with a
# son 子" -> c("女","子")).
han_chars <- function(s) {
  if (is.null(s) || length(s) == 0L || is.na(s)) return(character(0))
  m <- stringi::stri_extract_all_regex(s, "\\p{Han}")[[1]]
  if (length(m) == 1L && is.na(m)) return(character(0))
  unique(m)
}

# Component characters named in an etymology hint, dropping cross-reference
# clauses ("compare X", "see X", "variant of X", "cf. X") that name a related
# character rather than a constituent part.
hint_components <- function(s) {
  if (is.null(s) || length(s) == 0L || is.na(s)) return(character(0))
  # Parenthetical asides ("heaven (天)") name representations, not parts.
  s <- stringi::stri_replace_all_regex(s, "\\([^)]*\\)", "")
  # Cross-reference clauses ("compare 肉", "variant of 鬒", "simplified form of
  # 漢") name a related character, not a constituent part — drop the phrase and
  # the character it points to, but keep the rest of the hint.
  s <- stringi::stri_replace_all_regex(
    s,
    paste0(
      "(?i)\\b(?:compare|see|cf\\.?|variant of|",
      "(?:simplified |traditional |old |ancient |archaic )?form of)\\s*\\p{Han}+"
    ),
    ""
  )
  han_chars(s)
}

# Type-aware presentation spec + all learner-facing copy for a character's
# component breakdown. Single source of truth: edit strings here, never in the
# render logic. `type` is the makemeahanzi etymology_type (pictophonetic /
# ideographic / pictographic / NA).
decomp_frame <- function(type) {
  type <- type %||% NA_character_

  if (!is.na(type) && type == "pictophonetic") {
    list(
      mode        = "phonosemantic",
      title       = "Phono-semantic compound",
      subtitle    = "形聲 xíngshēng",
      how_to_read = "One part points to the meaning, the other to the sound.",
      role_badges = TRUE,
      show_origin = FALSE,
      meaning_equation = FALSE,
      badge_labels = c(semantic = "meaning", phonetic = "sound"),
      badge_tips   = c(
        semantic = "形符 — the semantic part, hinting at what it means",
        phonetic = "聲符 — the phonetic part, hinting at how it sounds"
      ),
      footnote = paste(
        "Sound hints reflect older pronunciations and may have drifted in",
        "modern Mandarin."
      ),
      origin_prefix = "Origin: "
    )
  } else if (!is.na(type) && type == "ideographic") {
    list(
      mode        = "ideographic",
      title       = "Ideographic character",
      subtitle    = "會意 huìyì · 指事 zhǐshì",
      how_to_read = paste(
        "The meaning is built from the parts themselves — there's no",
        "sound clue here."
      ),
      role_badges = FALSE,
      show_origin = TRUE,
      meaning_equation = TRUE,
      origin_prefix = "Origin: "
    )
  } else if (!is.na(type) && type == "pictographic") {
    list(
      mode        = "pictograph",
      title       = "Pictograph",
      subtitle    = "象形 xiàngxíng",
      how_to_read = "This character began as a picture — read it as a whole.",
      role_badges = FALSE,
      show_origin = TRUE,
      meaning_equation = FALSE,
      origin_prefix = "Origin: "
    )
  } else {
    list(
      mode        = "plain",
      title       = NULL,
      subtitle    = NULL,
      how_to_read = NULL,
      body_label  = "Components",
      role_badges = FALSE,
      show_origin = FALSE,
      meaning_equation = FALSE
    )
  }
}

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
