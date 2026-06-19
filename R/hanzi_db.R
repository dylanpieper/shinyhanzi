# GitHub Releases URL for the prebuilt database (set after first release)
HANZI_DB_URL <- "https://github.com/dylanpieper/shinyhanzi/releases/latest/download/shinyhanzi.duckdb"

#' Locate the shinyhanzi DuckDB database file
#'
#' Searches in order: bundled with the package → local user cache → not found (returns NULL).
#'
#' @return Absolute path to the `.duckdb` file, or `NULL` if not found.
#' @export
hanzi_db_path <- function() {
  bundled <- system.file("db/shinyhanzi.duckdb", package = "shinyhanzi")
  if (nzchar(bundled)) return(bundled)

  cached <- file.path(cache_dir(), "shinyhanzi.duckdb")
  if (file.exists(cached)) return(cached)

  NULL
}

#' Open (or return the cached) read-only DuckDB connection
#'
#' Returns the package-level cached connection if already open, otherwise
#' resolves the database path (downloading if necessary), opens a read-only
#' connection, loads the FTS extension, and caches it in `pkg_env`.
#'
#' @param path Optional explicit path to the `.duckdb` file. When supplied the
#'   package cache is bypassed (used internally by `build_hanzi_db()`).
#' @param read_only Logical; defaults to `TRUE`.
#' @return A `DBIConnection` object.
#' @export
hanzi_db <- function(path = NULL, read_only = TRUE) {
  if (is.null(path) && !is.null(pkg_env$con)) {
    if (DBI::dbIsValid(pkg_env$con)) return(pkg_env$con)
    pkg_env$con <- NULL
  }

  if (is.null(path)) {
    path <- hanzi_db_path()
    if (is.null(path)) {
      cli::cli_abort(c(
        "Database not found.",
        "i" = "Run {.run shinyhanzi::download_hanzi_db()} to download it."
      ))
    }
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)

  # Load FTS extension so match_bm25 is available at query time
  tryCatch(
    DBI::dbExecute(con, "LOAD fts;"),
    error = function(e) {
      tryCatch(
        {
          DBI::dbExecute(con, "INSTALL fts;")
          DBI::dbExecute(con, "LOAD fts;")
        },
        error = function(e2) NULL
      )
    }
  )

  if (read_only) pkg_env$con <- con
  con
}

#' Download the prebuilt shinyhanzi DuckDB database
#'
#' Fetches the `.duckdb` file from the GitHub Releases asset and saves it to
#' the user cache directory (`tools::R_user_dir("shinyhanzi", "data")`).
#'
#' @param url URL of the release asset. Defaults to the package constant.
#' @return Invisible path to the downloaded file.
#' @export
download_hanzi_db <- function(url = HANZI_DB_URL) {
  dest_dir  <- cache_dir()
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest_file <- file.path(dest_dir, "shinyhanzi.duckdb")

  cli::cli_progress_step("Downloading shinyhanzi database...")

  req  <- httr2::request(url) |> httr2::req_progress()
  tryCatch(
    httr2::req_perform(req, path = dest_file),
    error = function(e) {
      unlink(dest_file)
      cli::cli_abort("Download failed: {conditionMessage(e)}", call = NULL)
    }
  )

  cli::cli_alert_success("Database saved to {dest_file}")
  invisible(dest_file)
}
