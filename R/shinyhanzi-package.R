#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr filter select mutate left_join collect tbl distinct arrange desc
## dbplyr powers the database backend that dplyr's `tbl()` dispatches to at
## runtime; import a symbol so the dependency is recognised as used.
#' @importFrom dbplyr build_sql
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbWriteTable dbExistsTable
#' @importFrom duckdb duckdb
#' @importFrom rlang `%||%` .data
#' @importFrom purrr map map_chr walk compact
NULL
