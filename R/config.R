#' Default intermediate directory path
#'
#' Returns the path set via `options(teachrag.intermediate_dir = ...)`.
#' Used as default for `ask_rag`, `ask_rag_chat`, `run_app`, and `interactive_cli`.
#'
#' @return Character path, or NULL if not set.
#' @export
teachrag_intermediate_dir <- function() {
  getOption("teachrag.intermediate_dir", default = NULL)
}

#' Default ragnar store path
#'
#' Returns the path set via `options(teachrag.store_path = ...)`.
#' Used as default for `ask_rag`, `ask_rag_chat`, `run_app`, and `interactive_cli`.
#'
#' @return Character path, or NULL if not set.
#' @export
teachrag_store_path <- function() {
  getOption("teachrag.store_path", default = NULL)
}
