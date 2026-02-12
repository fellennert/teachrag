#' Default intermediate directory path
#'
#' Returns the path set via `options(teachrag.intermediate_dir = ...)`.
#' If not set, uses the bundled intermediate data shipped with the package
#' (the pre-built course materials). Used as default for `ask_rag`, `ask_rag_chat`,
#' `run_app`, and `interactive_cli`.
#'
#' @return Character path, or NULL if not set and no bundled data available.
#' @export
teachrag_intermediate_dir <- function() {
  opt <- getOption("teachrag.intermediate_dir")
  if (!is.null(opt) && nzchar(opt)) return(opt)
  bundled <- system.file("intermediate", package = "teachrag")
  if (nzchar(bundled)) return(bundled)
  NULL
}

#' Default ragnar store path
#'
#' Returns the path set via `options(teachrag.store_path = ...)`.
#' If not set, uses the bundled store (teaching_db.ragnar.duckdb) in the
#' package's intermediate directory. Used as default for `ask_rag`, `ask_rag_chat`,
#' `run_app`, and `interactive_cli`.
#'
#' @return Character path, or NULL if not set and no bundled data available.
#' @export
teachrag_store_path <- function() {
  opt <- getOption("teachrag.store_path")
  if (!is.null(opt) && nzchar(opt)) return(opt)
  bundled_dir <- teachrag_intermediate_dir()
  if (!is.null(bundled_dir)) {
    return(file.path(bundled_dir, "teaching_db.ragnar.duckdb"))
  }
  NULL
}
