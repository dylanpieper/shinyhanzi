#' Create a Hanzi Writer output placeholder
#'
#' Generates the HTML structure for a Hanzi Writer animation canvas. Drive it
#' from the server with `session$sendCustomMessage("draw_hanzi", ...)`.
#'
#' @param outputId The Shiny output ID; also used as the DOM element ID.
#' @param width,height CSS dimensions of the canvas.
#' @return A `shiny.tag` suitable for use in `ui`.
#' @export
hanziWriterOutput <- function(outputId, width = "280px", height = "280px") {
  shiny::tagList(
    shiny::singleton(shiny::tags$head(
      shiny::tags$script(src = "www/hanzi-writer.min.js"),
      shiny::tags$script(src = "www/writer.js")
    )),
    shiny::tags$div(
      id    = outputId,
      style = paste0("width:", width, ";height:", height, ";"),
      class = "hanzi-writer-target"
    )
  )
}
