#' Run the shinyhanzi application
#'
#' Ensures the DuckDB database is present (downloading it if necessary), opens
#' a single read-only connection, then launches the Shiny app. The connection
#' is closed when the app exits.
#'
#' @param port Port for the Shiny server. `NULL` selects a random available port.
#' @param launch.browser If `TRUE` (default), opens a browser automatically.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @return Invisibly returns the `shiny.appobj` (called for its side effect).
#' @export
run_app <- function(port = NULL, launch.browser = TRUE, ...) {
  con <- hanzi_db()

  shiny::addResourcePath(
    "www",
    system.file("app/www", package = "shinyhanzi")
  )

  app <- shiny::shinyApp(
    ui = app_ui(),
    server = app_server(con),
    options = list(port = port, launch.browser = launch.browser),
    ...
  )
  shiny::runApp(app)
  invisible(app)
}

# ---- Theme ------------------------------------------------------------------

hanzi_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#1a1a1a",
    primary = "#8b1a1a",
    secondary = "#6b6b5a",
    base_font = bslib::font_google("Noto Sans"),
    heading_font = bslib::font_google("Noto Serif")
  )
}

# ---- UI ---------------------------------------------------------------------

app_ui <- function() {
  bslib::page_fluid(
    theme = hanzi_theme(),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "www/styles.css"),
      shiny::tags$script(src = "www/Speakit1.0.1.min.js"),
      shiny::tags$script(src = "www/tts.js"),
      shiny::tags$script(src = "www/app.js"),
      shiny::tags$script(src = "www/d3.v7.min.js"),
      shiny::tags$script(src = "www/freq_chart.js"),
    ),

    shiny::tags$nav(
      class = "navbar mb-4 px-3",
      shiny::tags$span(
        class = "navbar-brand mb-0",
        shiny::tags$strong("shinyhanzi"),
        shiny::tags$span(" (\u4eae\u6c49\u5b57)", style = "font-weight:300;")
      ),
      shiny::tags$span(
        class = "ms-auto text-muted small",
        "Developed by ",
        shiny::tags$a(
          href   = "https://github.com/dylanpieper",
          target = "_blank",
          rel    = "noopener noreferrer",
          shiny::icon("github"),
          " Dylan Pieper"
        )
      )
    ),

    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "1.5rem",
      fillable = FALSE,

      # Left: search nav (character | english) + writer
      shiny::tagList(
        bslib::card(
          fill = FALSE,
          bslib::card_body(
            padding = 0,
            bslib::navset_pill(
              id = "search_mode",

              bslib::nav_panel(
                title = "Hanzi",
                shiny::div(
                  class = "p-3",
                  shiny::textInput(
                    "char_input",
                    label = NULL,
                    value = "\u4eae",
                    placeholder = "Enter a character or word",
                    width = "100%"
                  ),
                  shiny::uiOutput("word_display")
                )
              ),

              bslib::nav_panel(
                title = "English \u2192 Hanzi",
                shiny::div(
                  class = "p-3",
                  shiny::textInput(
                    "search_query",
                    label = NULL,
                    placeholder = "Search by definition or pinyin",
                    width = "100%"
                  ),
                  shiny::uiOutput("search_results")
                )
              ),

              bslib::nav_panel(
                title = "Explore",
                browse_ui("browse")
              ),
              bslib::nav_item(
                shiny::actionButton("freq_plot_btn", NULL,
                                    icon  = shiny::icon("chart-area"),
                                    class = "btn-sm btn-outline-primary ms-1",
                                    title = "Frequency distribution")
              )
            )
          )
        ),
        writer_ui("writer")
      ),

      # Right: dict + decomp + appears-in
      shiny::tagList(
        dict_ui("dict"),
        appears_in_ui("appears_in")
      )
    )
  )
}

# ---- Server -----------------------------------------------------------------

app_server <- function(con) {
  function(input, output, session) {
    # Full NFC-normalized input (may be multiple chars)
    current_input <- shiny::reactive({
      shiny::req(input$char_input)
      s <- stringi::stri_trans_nfc(stringr::str_trim(input$char_input))
      if (!nzchar(s)) {
        return(NULL)
      }
      s
    })

    # Individual characters when input is a word
    input_chars <- shiny::reactive({
      s <- current_input()
      shiny::req(!is.null(s))
      strsplit(s, "", fixed = TRUE)[[1]]
    })

    # The single character driving writer / decomp / appears-in
    # — first char of input; updated when user clicks a tile in a multi-char word
    current_char <- shiny::reactive({
      chars <- input_chars()
      shiny::req(length(chars) > 0L)
      chars[[1L]]
    })

    char_data <- shiny::reactive({
      shiny::req(current_input())
      hanzi_lookup(current_input(), con)
    })

    # focused_char: the single character shown in writer/decomp/appears-in.
    # Defaults to first char of input; overridden when user clicks a word tile.
    focused_char_override <- shiny::reactiveVal(NULL)

    shiny::observe({
      input_chars() # invalidate override whenever the input changes
      focused_char_override(NULL)
    })

    focused_char <- shiny::reactive({
      override <- focused_char_override()
      if (!is.null(override)) {
        return(override)
      }
      chars <- input_chars()
      shiny::req(length(chars) > 0L)
      chars[[1L]]
    })

    focus_char <- function(ch) focused_char_override(ch)
    navigate <- function(ch) {
      shiny::updateTextInput(session, "char_input", value = ch)
    }

    shiny::observeEvent(
      input$word_tile_click,
      focus_char(input$word_tile_click)
    )

    focused_data <- shiny::reactive({
      shiny::req(focused_char())
      hanzi_lookup(focused_char(), con)
    })
    focused_appears <- shiny::reactive({
      shiny::req(focused_char())
      hanzi_components_of(focused_char(), con = con)
    })

    # English search
    search_data <- shiny::reactiveVal(NULL)
    result_idx <- shiny::reactiveVal(1L)

    # Word display in Hanzi nav — shown whenever a multi-char word is entered
    output$word_display <- shiny::renderUI({
      s <- current_input()
      if (is.null(s)) return(NULL)
      chars <- strsplit(s, "", fixed = TRUE)[[1]]
      if (length(chars) <= 1L) return(NULL)

      wd <- char_data()
      if (is.null(wd) || nrow(wd$cedict) == 0L) return(NULL)
      row <- wd$cedict[1L, ]

      shiny::div(
        class = "dict-entry-clickable d-flex align-items-start gap-3 mt-2 py-2 px-2",
        onclick = sprintf(
          "Shiny.setInputValue('word_stats_request','%s',{priority:'event'})",
          gsub("'", "\\\\'", s)
        ),
        shiny::tags$span(s, class = "hanzi-large"),
        shiny::div(
          class = "flex-grow-1",
          shiny::div(
            class = "d-flex align-items-center gap-2 mb-1",
            shiny::tags$strong(row$pinyin_toned[[1L]], class = "text-primary"),
            shiny::div(
              class = "d-flex align-items-center gap-2 ms-auto",
              shiny::tags$span(
                shiny::icon("circle-info"),
                class = "text-muted dict-info-icon",
                title = "Word info"
              ),
              shiny::tags$button(
                type    = "button",
                class   = "btn btn-link p-0 text-primary",
                title   = "Speak",
                onclick = sprintf("event.stopPropagation(); window.speakHanzi(%s, 0.9)",
                                  jsonlite::toJSON(s, auto_unbox = TRUE)),
                shiny::icon("volume-high")
              )
            )
          ),
          shiny::tags$span(
            convert_gloss_pinyin(row$gloss[[1L]]),
            class = "text-muted small"
          )
        )
      )
    })

    writer_server("writer", focused_char, session)
    dict_server(
      "dict",
      current_input,
      char_data,
      focused_char,
      focused_data,
      session
    )
    appears_in_page_size <- shiny::reactive({
      ps <- input$appears_in_page_size
      if (is.null(ps) || ps < 1L) 12L else as.integer(ps)
    })
    appears_in_server(
      "appears_in",
      focused_char,
      focused_appears,
      navigate,
      appears_in_page_size
    )
    browse_server("browse", navigate, con, session)

    shiny::observeEvent(input$freq_plot_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Character Frequency Distribution",
        shiny::tags$script(
          "document.querySelector('.modal-dialog').classList.add('modal-fullscreen');"
        ),
        shiny::htmlOutput("freq_chart_ui",
                          style = "width:100%; height:calc(100vh - 240px);"),
        footer    = shiny::modalButton("Close"),
        size      = "xl",
        easyClose = TRUE
      ))
    })

    output$freq_chart_ui <- shiny::renderUI({
      d    <- DBI::dbGetQuery(con,
        "SELECT cf.rank, cf.char, cf.cumulative_pct,
                (SELECT pinyin_toned FROM cedict WHERE simplified = cf.char ORDER BY id LIMIT 1) AS pinyin,
                (SELECT gloss       FROM cedict WHERE simplified = cf.char ORDER BY id LIMIT 1) AS gloss
         FROM char_frequency cf ORDER BY cf.rank"
      )
      json   <- jsonlite::toJSON(d, dataframe = "rows")
      div_id <- "freq-svg"
      shiny::tagList(
        shiny::tags$div(id = div_id,
                        style = "width:100%; height:calc(100vh - 240px);"),
        shiny::tags$script(shiny::HTML(sprintf(
          "(function(){
            var divId   = '%s';
            var data    = %s;
            var inputId = 'freq_chart_char_click';
            function draw() {
              var el = document.getElementById(divId);
              if (!el || !window.d3 || !window.drawFreqChart) return;
              drawFreqChart(divId, data, function(ch) {
                Shiny.setInputValue(inputId, ch, {priority: 'event'});
              });
            }
            function init() {
              var el = document.getElementById(divId);
              if (!el || !window.d3 || !window.drawFreqChart) {
                return setTimeout(init, 50);
              }
              var modal = el.closest('.modal');
              if (modal) {
                modal.addEventListener('shown.bs.modal', draw, { once: true });
                if (modal.classList.contains('show')) draw();
              } else {
                draw();
              }
            }
            init();
          })()",
          div_id, json
        )))
      )
    })

    shiny::observeEvent(input$freq_chart_char_click, {
      ch <- input$freq_chart_char_click
      shiny::req(!is.null(ch) && nzchar(ch))
      shiny::removeModal()
      navigate(ch)
      shiny::updateTabsetPanel(session, "search_mode", selected = "Hanzi")
    })

    # Live search with debounce
    search_attempted <- shiny::reactiveVal(FALSE)
    search_query_d <- shiny::debounce(shiny::reactive(input$search_query), 350)

    shiny::observe({
      q <- search_query_d()
      if (is.null(q) || !nzchar(stringr::str_trim(q))) {
        search_data(NULL)
        search_attempted(FALSE)
        return()
      }
      res <- hanzi_search(q, con = con)
      search_data(if (nrow(res) > 0) res else NULL)
      result_idx(1L)
      search_attempted(TRUE)
    })

    # Character stats modal — triggered by clicking a dict entry row
    shiny::observeEvent(input$char_stats_request, {
      ch <- input$char_stats_request
      if (is.null(ch) || !nzchar(ch)) {
        return()
      }
      stats  <- hanzi_char_stats(ch, con)
      decomp <- hanzi_decompose(ch, con)
      shiny::showModal(render_stats_modal(ch, stats, decomp))
    })

    # Word/char info modal — triggered by clicking a search result or word title.
    # Single characters route to the character modal; words to the word modal.
    shiny::observeEvent(input$word_stats_request, {
      tok <- input$word_stats_request
      shiny::req(!is.null(tok) && nzchar(tok))
      if (nchar(tok) == 1L) {
        shiny::showModal(render_stats_modal(
          tok, hanzi_char_stats(tok, con), hanzi_decompose(tok, con)
        ))
      } else {
        shiny::showModal(render_word_modal(tok, hanzi_word_stats(tok, con)))
      }
    })

    # Clicking a component tile inside the modal starts a new search on it
    shiny::observeEvent(input$modal_comp_click, {
      ch <- input$modal_comp_click
      shiny::req(!is.null(ch) && nzchar(ch))
      shiny::removeModal()
      navigate(ch)
      shiny::updateTabsetPanel(session, "search_mode", selected = "Hanzi")
    })

    shiny::observeEvent(input$result_prev, {
      i <- result_idx()
      if (i > 1L) result_idx(i - 1L)
    })

    shiny::observeEvent(input$result_next, {
      res <- search_data()
      i <- result_idx()
      if (!is.null(res) && i < nrow(res)) result_idx(i + 1L)
    })

    # Load the full word (or char) into the Hanzi panel whenever the result changes
    shiny::observe({
      res <- search_data()
      i <- result_idx()
      shiny::req(!is.null(res), i >= 1L, i <= nrow(res))
      navigate(res$simplified[[i]])
    })

    output$search_results <- shiny::renderUI({
      res <- search_data()
      if (is.null(res)) {
        if (isTRUE(search_attempted())) {
          return(shiny::p("No results found.", class = "text-muted mt-3"))
        }
        return(NULL)
      }

      i <- result_idx()
      n <- nrow(res)
      row <- res[i, ]
      word <- row$simplified

      shiny::div(
        shiny::div(
          class = "dict-entry-clickable d-flex align-items-start gap-3 mt-3 mb-3 py-2 px-2",
          onclick = sprintf(
            "Shiny.setInputValue('word_stats_request','%s',{priority:'event'})",
            gsub("'", "\\\\'", word)
          ),
          shiny::tags$span(word, class = "hanzi-large"),
          shiny::div(
            class = "flex-grow-1",
            shiny::div(
              class = "d-flex align-items-center gap-2 mb-1",
              shiny::tags$strong(row$pinyin_toned, class = "text-primary"),
              shiny::div(
                class = "d-flex align-items-center gap-2 ms-auto",
                shiny::tags$span(
                  shiny::icon("circle-info"),
                  class = "text-muted dict-info-icon",
                  title = "Word info"
                ),
                shiny::tags$button(
                  type    = "button",
                  class   = "btn btn-link p-0 text-primary",
                  title   = "Speak",
                  onclick = sprintf("event.stopPropagation(); window.speakHanzi(%s, 0.9)",
                                    jsonlite::toJSON(word, auto_unbox = TRUE)),
                  shiny::icon("volume-high")
                )
              )
            ),
            shiny::tags$span(
              convert_gloss_pinyin(row$gloss),
              class = "text-muted small"
            )
          )
        ),
        shiny::div(
          class = "d-flex align-items-center justify-content-between",
          shiny::actionButton(
            "result_prev",
            NULL,
            icon = shiny::icon("chevron-left"),
            class = if (i == 1L) {
              "btn-sm btn-outline-secondary disabled"
            } else {
              "btn-sm btn-outline-secondary"
            }
          ),
          shiny::tags$span(
            sprintf("%d of %d", i, n),
            class = "text-muted small"
          ),
          shiny::actionButton(
            "result_next",
            NULL,
            icon = shiny::icon("chevron-right"),
            class = if (i == n) {
              "btn-sm btn-outline-secondary disabled"
            } else {
              "btn-sm btn-outline-secondary"
            }
          )
        )
      )
    })
  }
}

# ---- Modules ----------------------------------------------------------------

# Writer
writer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::tags$span("Stroke Order"),
      shiny::tags$div(
        class = "d-flex align-items-center gap-2 ms-auto",
        shiny::actionButton(
          ns("step_back"),
          NULL,
          icon = shiny::icon("backward-step"),
          class = "btn-sm btn-outline-secondary",
          title = "Previous stroke"
        ),
        shiny::actionButton(
          ns("play_pause"),
          NULL,
          icon = shiny::icon("pause"),
          class = "btn-sm btn-outline-secondary",
          title = "Pause / Play"
        ),
        shiny::actionButton(
          ns("step_forward"),
          NULL,
          icon = shiny::icon("forward-step"),
          class = "btn-sm btn-outline-secondary",
          title = "Next stroke"
        ),
        shiny::actionButton(
          ns("practice"),
          "Practice",
          class = "btn-sm btn-outline-primary"
        ),
        shiny::tags$div(
          class = "vr mx-1"
        ),
        shiny::tags$input(
          type = "range",
          id = ns("speed"),
          min = "1",
          max = "5",
          value = "3",
          step = "1",
          class = "form-range",
          style = "width: 72px; cursor: pointer;",
          title = "Speed",
          oninput = "Shiny.setInputValue(this.id, this.value, {priority:'event'})"
        )
      ),
      class = "d-flex align-items-center"
    ),
    bslib::card_body(
      hanziWriterOutput(ns("canvas"), width = "100%", height = "260px")
    )
  )
}

writer_server <- function(id, current_char, session_parent) {
  shiny::moduleServer(id, function(input, output, session) {
    is_paused <- shiny::reactiveVal(FALSE)

    # Speed slider: 5 positions → actual multipliers
    speed_steps <- c(0.5, 0.75, 1, 1.5, 2)

    cur_speed <- function() {
      idx <- max(1L, min(5L, as.integer(input$speed %||% "3")))
      speed_steps[[idx]]
    }

    msg <- function(type, extra = list()) {
      session_parent$sendCustomMessage(
        type,
        c(list(target = session$ns("canvas")), extra)
      )
    }

    shiny::observeEvent(current_char(), {
      ch <- current_char()
      if (!is.null(ch)) {
        is_paused(FALSE)
        shiny::updateActionButton(
          session,
          "play_pause",
          icon = shiny::icon("pause")
        )
        msg("draw_hanzi", list(char = ch, speed = cur_speed()))
      }
    })

    shiny::observeEvent(input$play_pause, {
      if (is_paused()) {
        is_paused(FALSE)
        shiny::updateActionButton(
          session,
          "play_pause",
          icon = shiny::icon("pause")
        )
        msg("writer_resume")
      } else {
        is_paused(TRUE)
        shiny::updateActionButton(
          session,
          "play_pause",
          icon = shiny::icon("play")
        )
        msg("writer_pause")
      }
    })

    shiny::observeEvent(input$speed, ignoreNULL = TRUE, {
      msg("writer_set_speed", list(speed = cur_speed()))
    })

    shiny::observeEvent(input$step_back, msg("writer_step_back"))
    shiny::observeEvent(input$step_forward, msg("writer_step_forward"))
    shiny::observeEvent(input$practice, msg("writer_toggle_practice"))
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Dictionary
dict_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    fill = FALSE,
    bslib::card_header("Pinyin"),
    bslib::card_body(shiny::uiOutput(ns("content")))
  )
}

dict_server <- function(
  id,
  current_input,
  char_data,
  focused_char,
  focused_data,
  session_parent
) {
  shiny::moduleServer(id, function(input, output, session) {
    output$content <- shiny::renderUI({
      s <- current_input()
      shiny::req(!is.null(s))
      chars <- strsplit(s, "", fixed = TRUE)[[1]]
      is_word <- length(chars) > 1L

      # Character tile strip
      word_tiles <- if (is_word) {
        fc <- focused_char()
        shiny::div(
          class = "mb-3",
          shiny::tags$p(
            "Explore each character:",
            class = "text-muted small mb-2"
          ),
          shiny::div(
            class = "d-flex gap-2 flex-wrap",
            lapply(chars, function(ch) {
              is_focused <- !is.null(fc) && ch == fc
              shiny::tags$a(
                href = "javascript:void(0)",
                class = paste(
                  "hanzi-tile text-decoration-none text-body border",
                  if (is_focused) "border-primary fw-bold" else ""
                ),
                onclick = sprintf(
                  "Shiny.setInputValue('word_tile_click','%s',{priority:'event'});",
                  ch
                ),
                ch
              )
            })
          )
        )
      }

      # Dictionary entries for the focused character (or single-char input)
      data <- if (is_word) focused_data() else char_data()
      lookup <- if (is_word) focused_char() else s
      shiny::req(!is.null(data))
      entries <- data$cedict

      if (nrow(entries) == 0) {
        return(shiny::tagList(
          word_tiles,
          shiny::p(
            paste("No dictionary entries found for", lookup),
            class = "text-muted"
          )
        ))
      }

      shiny::tagList(
        word_tiles,
        lapply(seq_len(nrow(entries)), function(i) {
          e <- entries[i, ]
          py <- e[["pinyin_toned"]][[1L]]
          tr <- e[["traditional"]][[1L]]
          gl <- e[["gloss"]][[1L]]
          shiny::div(
            class = paste(
              "dict-entry-clickable py-2 px-2",
              if (nrow(entries) > 1L && i < nrow(entries)) {
                "border-bottom"
              } else {
                ""
              }
            ),
            onclick = sprintf(
              "Shiny.setInputValue('char_stats_request','%s',{priority:'event'})",
              gsub("'", "\\\\'", lookup)
            ),
            shiny::div(
              class = "d-flex align-items-center gap-2",
              shiny::tags$span(lookup, class = "hanzi-word"),
              shiny::tags$span(py, class = "fw-semibold text-primary"),
              if (!is.na(tr) && tr != lookup)
                shiny::tags$span(paste0("(", tr, ")"), class = "text-muted small"),
              shiny::div(
                class = "d-flex align-items-center gap-2 ms-auto",
                shiny::tags$span(
                  shiny::icon("circle-info"),
                  class = "text-muted dict-info-icon",
                  title = "Character info"
                ),
                shiny::tags$button(
                  type    = "button",
                  class   = "btn btn-link p-0 text-primary",
                  style   = "font-size:1.25rem;",
                  title   = "Speak",
                  onclick = sprintf(
                    "event.stopPropagation(); window.speakHanzi(%s, 0.9)",
                    jsonlite::toJSON(lookup, auto_unbox = TRUE)
                  ),
                  shiny::icon("volume-high")
                )
              )
            ),
            shiny::div(convert_gloss_pinyin(gl), class = "text-muted small mt-1")
          )
        })
      )
    })
  })
}

# Appears in
appears_in_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    fill = FALSE,
    bslib::card_header("Appears In"),
    bslib::card_body(
      shiny::div(id = ns("measure"), shiny::uiOutput(ns("content")))
    )
  )
}

appears_in_server <- function(
  id,
  current_char,
  appears_in,
  navigate,
  page_size
) {
  shiny::moduleServer(id, function(input, output, session) {
    page <- shiny::reactiveVal(1L)

    shiny::observe({
      current_char()
      page(1L)
    })

    shiny::observeEvent(input$prev_page, {
      p <- page()
      if (p > 1L) page(p - 1L)
    })

    shiny::observeEvent(input$next_page, {
      chars <- appears_in()
      if (is.null(chars) || nrow(chars) == 0L) {
        return()
      }
      n_pages <- ceiling(nrow(chars) / page_size())
      p <- page()
      if (p < n_pages) page(p + 1L)
    })

    shiny::observeEvent(
      page_size(),
      {
        page(1L)
      },
      ignoreInit = TRUE
    )

    output$content <- shiny::renderUI({
      chars <- appears_in()
      ch <- current_char()
      if (is.null(chars) || nrow(chars) == 0L) {
        return(shiny::p(
          paste0(ch, " is not found as a component of other characters."),
          class = "text-muted"
        ))
      }

      ps <- page_size()
      p <- page()
      n_total <- nrow(chars)
      n_pages <- ceiling(n_total / ps)
      start <- (p - 1L) * ps + 1L
      end <- min(p * ps, n_total)
      pg <- chars[start:end, ]

      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::actionButton(
          session$ns("prev_page"),
          NULL,
          icon = shiny::icon("chevron-left"),
          class = paste(
            "btn-sm btn-outline-secondary flex-shrink-0",
            if (p == 1L) "disabled" else ""
          )
        ),
        lapply(seq_len(nrow(pg)), function(i) {
          c2 <- pg$char[[i]]
          shiny::tags$a(
            href = "javascript:void(0)",
            class = "hanzi-tile text-decoration-none text-body border flex-shrink-0",
            onclick = sprintf(
              "Shiny.setInputValue('%s','%s',{priority:'event'})",
              session$ns("char_click"),
              c2
            ),
            c2
          )
        }),
        shiny::actionButton(
          session$ns("next_page"),
          NULL,
          icon = shiny::icon("chevron-right"),
          class = paste(
            "btn-sm btn-outline-secondary flex-shrink-0",
            if (p >= n_pages) "disabled" else ""
          )
        )
      )
    })

    shiny::observeEvent(input$char_click, navigate(input$char_click))
  })
}

# ---- Character stats --------------------------------------------------------

hanzi_char_stats <- function(char, con) {
  q <- function(sql) DBI::dbGetQuery(con, sql)
  esc <- function(s) gsub("'", "''", s)

  info <- q(sprintf(
    "SELECT definition, radical, etymology_type, phonetic, semantic, etymology_hint
                      FROM characters WHERE char = '%s' LIMIT 1",
    esc(char)
  ))
  freq <- q(sprintf(
    "SELECT rank, cumulative_pct FROM char_frequency WHERE char = '%s' LIMIT 1",
    esc(char)
  ))
  rad <- if (nrow(info) > 0 && !is.na(info$radical[[1]])) {
    q(sprintf(
      "SELECT number, meaning, pinyin FROM radicals WHERE radical = '%s' LIMIT 1",
      esc(info$radical[[1]])
    ))
  } else {
    data.frame()
  }

  list(
    info = if (nrow(info) > 0) info else NULL,
    freq = if (nrow(freq) > 0) freq else NULL,
    rad = if (nrow(rad) > 0) rad else NULL
  )
}

render_stats_modal <- function(char, stats, decomp = NULL) {
  # Definition (makemeahanzi) — the character's core meaning, shown at the top.
  def_ui <- if (!is.null(stats$info) &&
                !is.na(stats$info$definition[[1]]) &&
                nzchar(stats$info$definition[[1]])) {
    shiny::div(
      class = "mb-4",
      shiny::tags$p(class = "text-muted text-uppercase small fw-semibold mb-2",
                    "Definition"),
      shiny::tags$p(convert_gloss_pinyin(stats$info$definition[[1]]),
                    class = "mb-0")
    )
  }

  # Frequency section
  freq_ui <- if (!is.null(stats$freq)) {
    rank     <- stats$freq$rank[[1]]
    pct      <- round(stats$freq$cumulative_pct[[1]], 1)
    tier     <- char_tier(rank)
    shiny::div(
      class = "mb-4",
      shiny::tags$p(class = "text-muted text-uppercase small fw-semibold mb-2", "Frequency"),
      shiny::div(
        class = "d-flex align-items-center gap-2 mb-2",
        shiny::tags$span(tier$label,
                         class = paste0("badge text-bg-", tier$color)),
        shiny::tags$span(paste0("Rank #", formatC(rank, big.mark = ",")),
                         class = "text-muted small")
      ),
      shiny::tags$p(
        sprintf("The top %s characters cover %.1f%% of Chinese text",
                formatC(rank, big.mark = ","), pct),
        class = "text-muted small mb-0"
      )
    )
  }

  # Etymology now lives in the always-visible Decomposition card (type, 形符/聲符
  # roles, and origin hint), so the modal no longer repeats it here.

  # Radical section
  rad_ui <- if (!is.null(stats$rad) && !is.null(stats$info)) {
    rad_char <- stats$info$radical[[1]]
    ri <- stats$rad
    shiny::div(
      class = "mb-2",
      shiny::tags$p(
        class = "text-muted text-uppercase small fw-semibold mb-1",
        "Radical"
      ),
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$span(rad_char, class = "hanzi-tile border fs-5"),
        shiny::tags$span(
          sprintf("%s \u00b7 %s \u00b7 #%s",
                  ri$meaning[[1]],
                  numbered_to_toned(ri$pinyin[[1]]),
                  ri$number[[1]]),
          class = "text-muted small"
        )
      )
    )
  }

  # Composition section — type-aware meaningful components (clickable)
  comp_ui <- if (!is.null(decomp)) {
    etym  <- attr(decomp, "etymology")
    frame <- decomp_frame(if (is.null(etym)) NA_character_ else etym$type)
    hint  <- if (is.null(etym)) NA_character_ else etym$hint
    has_tiles <- nrow(decomp) > 0

    tile <- function(i) {
      cp    <- decomp$component[[i]]
      py    <- decomp$pinyin_toned[[i]]
      py    <- if (!is.null(py) && !is.na(py) && nzchar(py)) py else NULL
      rname <- decomp$radical_name[[i]]
      defn  <- decomp$definition[[i]]
      label <- if (!is.null(rname) && !is.na(rname) && nzchar(rname)) {
        rname
      } else if (!is.null(defn) && !is.na(defn) && nzchar(defn)) {
        defn
      } else {
        NULL
      }
      role  <- if ("role" %in% names(decomp)) decomp$role[[i]] else NA_character_
      badge <- if (isTRUE(frame$role_badges) && !is.na(role)) {
        shiny::tags$span(frame$badge_labels[[role]],
                         class = paste0("role-badge role-", role),
                         title = frame$badge_tips[[role]])
      }
      shiny::div(
        class = "decomp-tile text-center p-2",
        shiny::tags$a(
          href = "javascript:void(0)",
          class = "hanzi-tile text-decoration-none text-body border",
          cp,
          onclick = sprintf(
            "Shiny.setInputValue('modal_comp_click','%s',{priority:'event'})", cp
          )
        ),
        if (!is.null(py)) shiny::tags$div(py, class = "small text-primary mt-1"),
        if (!is.null(label)) shiny::tags$div(label, class = "small text-muted"),
        badge
      )
    }

    type_line <- if (!is.null(frame$title)) {
      shiny::div(
        class = "d-flex align-items-baseline gap-2 mb-1",
        shiny::tags$span(frame$title, class = "decomp-type-title"),
        shiny::tags$span(frame$subtitle, class = "decomp-subtitle")
      )
    }
    howto <- if (!is.null(frame$how_to_read)) {
      shiny::tags$p(frame$how_to_read, class = "decomp-how-to-read mb-2")
    }
    origin <- if (isTRUE(frame$show_origin) && !is.na(hint %||% NA) && nzchar(hint)) {
      shiny::tags$p(shiny::tags$span(frame$origin_prefix, class = "fw-semibold"),
                    hint, class = "decomp-origin small mb-2")
    }
    equation <- if (isTRUE(frame$meaning_equation) && nrow(decomp) > 1L) {
      shiny::div(paste(decomp$component, collapse = " + "),
                 class = "decomp-equation mb-2")
    }
    tiles <- if (has_tiles) {
      shiny::div(class = "d-flex flex-wrap gap-3 p-1",
                 lapply(seq_len(nrow(decomp)), tile))
    }
    footnote <- if (!is.null(frame$footnote) && has_tiles) {
      shiny::tags$p(frame$footnote, class = "decomp-footnote small fst-italic mt-2 mb-0")
    }

    if (!is.null(type_line) || !is.null(origin) || !is.null(tiles)) {
      shiny::div(
        class = "mb-4",
        shiny::tags$p(class = "text-muted text-uppercase small fw-semibold mb-2",
                      "Composition"),
        type_line, howto, origin, equation, tiles, footnote
      )
    }
  }

  shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-baseline gap-3",
      shiny::tags$span(char, class = "hanzi-large"),
      shiny::tags$span("Character Info", class = "text-muted small")
    ),
    def_ui,
    freq_ui,
    comp_ui,
    rad_ui,
    footer = shiny::modalButton("Close"),
    easyClose = TRUE,
    size = "m"
  )
}

char_tier <- function(rank) {
  rank <- as.integer(rank %||% NA)
  if (is.na(rank))    list(label = "Unknown",   color = "secondary")
  else if (rank <=  300) list(label = "Essential", color = "success")
  else if (rank <=  800) list(label = "Common",    color = "primary")
  else if (rank <= 1500) list(label = "Standard",  color = "info")
  else                    list(label = "Rare",      color = "secondary")
}

word_tier <- function(rank) {
  rank <- as.integer(rank %||% NA)
  if (is.na(rank))         list(label = "Uncommon",  color = "secondary")
  else if (rank <=  2000)  list(label = "Very common", color = "success")
  else if (rank <=  8000)  list(label = "Common",      color = "primary")
  else if (rank <= 20000)  list(label = "Occasional",  color = "info")
  else                     list(label = "Rare",        color = "secondary")
}

# ---- Word stats -------------------------------------------------------------

hanzi_word_stats <- function(word, con) {
  esc <- function(s) gsub("'", "''", s)
  freq <- DBI::dbGetQuery(con, sprintf(
    "SELECT rank, count FROM word_frequency WHERE word = '%s' LIMIT 1", esc(word)))
  entry <- DBI::dbGetQuery(con, sprintf(
    "SELECT pinyin_toned, gloss FROM cedict WHERE simplified = '%s' ORDER BY id LIMIT 1",
    esc(word)))
  chars <- strsplit(word, "", fixed = TRUE)[[1]]
  cdf <- enrich_components(
    con,
    data.frame(component = chars, is_intermediate = FALSE, stringsAsFactors = FALSE)
  )
  list(
    freq  = if (nrow(freq) > 0) freq else NULL,
    entry = if (nrow(entry) > 0) entry else NULL,
    chars = cdf
  )
}

render_word_modal <- function(word, stats) {
  # Pinyin + gloss
  head_ui <- if (!is.null(stats$entry)) {
    shiny::div(
      class = "mb-4",
      shiny::tags$p(stats$entry$pinyin_toned[[1]],
                    class = "fw-semibold text-primary mb-1"),
      shiny::tags$p(convert_gloss_pinyin(stats$entry$gloss[[1]]),
                    class = "text-muted small mb-0")
    )
  }

  # Word frequency
  freq_ui <- if (!is.null(stats$freq)) {
    rank <- stats$freq$rank[[1]]
    tier <- word_tier(rank)
    shiny::div(
      class = "mb-4",
      shiny::tags$p(class = "text-muted text-uppercase small fw-semibold mb-2",
                    "Word frequency"),
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$span(tier$label, class = paste0("badge text-bg-", tier$color)),
        shiny::tags$span(sprintf("Rank #%s of the 50,000 most common words",
                                 formatC(rank, big.mark = ",")),
                         class = "text-muted small")
      )
    )
  } else {
    shiny::div(
      class = "mb-4",
      shiny::tags$p(class = "text-muted text-uppercase small fw-semibold mb-2",
                    "Word frequency"),
      shiny::tags$p("Not in the 50,000-word frequency list.",
                    class = "text-muted small mb-0")
    )
  }

  # Constituent characters (clickable -> each character's own modal)
  cdf <- stats$chars
  char_tile <- function(i) {
    cp    <- cdf$component[[i]]
    py    <- cdf$pinyin_toned[[i]]
    py    <- if (!is.null(py) && !is.na(py) && nzchar(py)) py else NULL
    defn  <- cdf$definition[[i]]
    label <- if (!is.null(defn) && !is.na(defn) && nzchar(defn)) {
      trimws(strsplit(defn, ";")[[1]][[1]])
    } else {
      NULL
    }
    shiny::div(
      class = "decomp-tile text-center p-2",
      shiny::tags$a(
        href = "javascript:void(0)",
        class = "hanzi-tile text-decoration-none text-body border",
        cp,
        onclick = sprintf(
          "Shiny.setInputValue('char_stats_request','%s',{priority:'event'})",
          gsub("'", "\\\\'", cp)
        )
      ),
      if (!is.null(py)) shiny::tags$div(py, class = "small text-primary mt-1"),
      if (!is.null(label)) shiny::tags$div(label, class = "small text-muted")
    )
  }
  chars_ui <- shiny::div(
    class = "mb-2",
    shiny::tags$p(class = "text-muted text-uppercase small fw-semibold mb-2",
                  "Characters"),
    shiny::div(class = "d-flex flex-wrap gap-3 p-1",
               lapply(seq_len(nrow(cdf)), char_tile))
  )

  shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-baseline gap-3",
      shiny::tags$span(word, class = "hanzi-large"),
      shiny::tags$span("Word Info", class = "text-muted small")
    ),
    head_ui,
    freq_ui,
    chars_ui,
    footer = shiny::modalButton("Close"),
    easyClose = TRUE,
    size = "m"
  )
}

# ---- Explore (frequency browser) --------------------------------------------

browse_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-3",
    shiny::div(
      class = "d-flex align-items-center gap-2 mb-3",
      shiny::actionButton(ns("prev_page"), NULL, icon = shiny::icon("chevron-left"),
                          class = "btn-sm btn-outline-secondary"),
      shiny::actionButton(ns("next_page"), NULL, icon = shiny::icon("chevron-right"),
                          class = "btn-sm btn-outline-secondary"),
      shiny::textOutput(ns("page_info"), inline = TRUE),
      shiny::uiOutput(ns("mode_toggle"), class = "ms-auto", inline = TRUE),
      shiny::actionButton(ns("toggle_order"), NULL,
                          icon  = shiny::icon("arrow-down-1-9"),
                          class = "btn-sm btn-outline-secondary",
                          title = "Toggle order")
    ),
    shiny::uiOutput(ns("rows"))
  )
}

browse_server <- function(id, navigate, con, session_parent) {
  shiny::moduleServer(id, function(input, output, session) {
    PAGE    <- 5L
    page    <- shiny::reactiveVal(1L)
    desc    <- shiny::reactiveVal(FALSE)
    word_mode <- shiny::reactiveVal(FALSE)

    total <- shiny::reactive({
      if (word_mode()) {
        DBI::dbGetQuery(con,
          "SELECT COUNT(*) FROM word_frequency wf
           WHERE EXISTS (SELECT 1 FROM cedict WHERE simplified = wf.word AND gloss IS NOT NULL)"
        )[[1]]
      } else {
        DBI::dbGetQuery(con,
          "SELECT COUNT(*) FROM char_frequency cf
           WHERE EXISTS (SELECT 1 FROM cedict WHERE simplified = cf.char AND gloss IS NOT NULL)"
        )[[1]]
      }
    })
    n_pages <- shiny::reactive(ceiling(total() / PAGE))

    browse_data <- shiny::reactive({
      offset <- (page() - 1L) * PAGE
      dir    <- if (desc()) "DESC" else "ASC"
      if (word_mode()) {
        DBI::dbGetQuery(con, sprintf(
          "SELECT wf.rank, wf.word AS char,
                  (SELECT pinyin_toned FROM cedict
                   WHERE simplified = wf.word ORDER BY id LIMIT 1) AS pinyin_toned,
                  (SELECT gloss FROM cedict
                   WHERE simplified = wf.word ORDER BY id LIMIT 1) AS gloss
           FROM word_frequency wf
           WHERE EXISTS (SELECT 1 FROM cedict WHERE simplified = wf.word AND gloss IS NOT NULL)
           ORDER BY wf.rank %s
           LIMIT %d OFFSET %d",
          dir, PAGE, offset
        ))
      } else {
        DBI::dbGetQuery(con, sprintf(
          "SELECT cf.rank, cf.char,
                  (SELECT pinyin_toned FROM cedict
                   WHERE simplified = cf.char ORDER BY id LIMIT 1) AS pinyin_toned,
                  (SELECT gloss FROM cedict
                   WHERE simplified = cf.char ORDER BY id LIMIT 1) AS gloss
           FROM char_frequency cf
           WHERE EXISTS (SELECT 1 FROM cedict WHERE simplified = cf.char AND gloss IS NOT NULL)
           ORDER BY cf.rank %s
           LIMIT %d OFFSET %d",
          dir, PAGE, offset
        ))
      }
    })

    output$mode_toggle <- shiny::renderUI({
      w <- word_mode()
      btn <- function(label, val, active) {
        shiny::tags$button(
          type = "button",
          class = paste("btn btn-sm",
                        if (active) "btn-primary" else "btn-outline-secondary"),
          onclick = sprintf("Shiny.setInputValue('%s', %s, {priority:'event'})",
                            session$ns("set_word_mode"), tolower(as.character(val))),
          label
        )
      }
      shiny::div(
        class = "btn-group", role = "group",
        btn("Characters", FALSE, !w),
        btn("Words", TRUE, w)
      )
    })

    shiny::observeEvent(input$set_word_mode, {
      w <- isTRUE(input$set_word_mode)
      if (w != word_mode()) {
        word_mode(w)
        page(1L)
      }
    })

    output$page_info <- shiny::renderText({
      p   <- page()
      dir <- desc()
      tot <- total()
      r1  <- if (!dir) (p - 1L) * PAGE + 1L else tot - (p - 1L) * PAGE
      r2  <- if (!dir) min(p * PAGE, tot)  else max(tot - p * PAGE + 1L, 1L)
      sprintf("Rank %s\u2013%s", formatC(min(r1,r2), big.mark=","), formatC(max(r1,r2), big.mark=","))
    })

    shiny::observeEvent(input$toggle_order, {
      desc(!desc())
      page(1L)
      icon_name <- if (desc()) "arrow-up-9-1" else "arrow-down-1-9"
      shiny::updateActionButton(session, "toggle_order", icon = shiny::icon(icon_name))
    })

    shiny::observeEvent(input$prev_page, { if (page() > 1L) page(page() - 1L) })
    shiny::observeEvent(input$next_page, { if (page() < n_pages()) page(page() + 1L) })

    output$rows <- shiny::renderUI({
      d <- browse_data()
      w <- word_mode()
      lapply(seq_len(nrow(d)), function(i) {
        row  <- d[i, ]
        tier <- if (w) word_tier(row$rank) else char_tier(row$rank)
        # First semicolon-delimited sense only, truncated
        gloss <- if (!is.na(row$gloss %||% NA)) {
          g <- trimws(strsplit(row$gloss, ";")[[1L]][[1L]])
          if (nchar(g) > 38) paste0(substr(g, 1L, 38), "\u2026") else g
        } else ""

        shiny::div(
          class = "d-flex align-items-center gap-2 py-2 border-bottom browse-row",
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                            session$ns("char_click"), row$char),
          shiny::tags$span(paste0("#", row$rank),
                           class = "text-muted",
                           style = "font-size:0.7rem; width:2.8rem; flex-shrink:0;"),
          shiny::tags$span(row$char,
                           class = paste("border", if (w) "hanzi-tile-word" else "hanzi-tile")),
          shiny::div(
            class = "flex-grow-1 min-width-0",
            shiny::div(
              class = "d-flex align-items-center gap-2",
              shiny::tags$span(row$pinyin_toned %||% "",
                               class = "text-primary fw-semibold small"),
              shiny::tags$span(tier$label,
                               class = paste0("badge text-bg-", tier$color, " ms-auto"),
                               style = "font-size:0.6rem;")
            ),
            shiny::tags$div(gloss,
                            class = "text-muted text-truncate",
                            style = "font-size:0.72rem;")
          )
        )
      })
    })

    shiny::observeEvent(input$char_click, {
      ch <- input$char_click
      if (!is.null(ch) && nzchar(ch)) {
        navigate(ch)
        shiny::updateTabsetPanel(session_parent, "search_mode", selected = "Hanzi")
      }
    })

  })
}

