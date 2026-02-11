#' Run the teaching RAG Shiny app
#'
#' Launches the Shiny app for interactive Q&A with Ollama or Claude.
#'
#' @param store_path Path to ragnar store.
#' @param intermediate_dir Path containing syllabus.rds.
#' @export
run_app <- function(
  store_path = teachrag_store_path(),
  intermediate_dir = teachrag_intermediate_dir()
) {
  if (is.null(store_path) || is.null(intermediate_dir)) {
    stop("store_path and intermediate_dir required. Set options(teachrag.store_path, teachrag.intermediate_dir) or pass explicitly.")
  }
  app_dir <- system.file("shinyapp", package = "teachrag")
  if (app_dir == "") {
    stop("Shiny app not found. Reinstall the package.")
  }
  options(
    teachrag.store_path = store_path,
    teachrag.intermediate_dir = intermediate_dir
  )
  shiny::runApp(app_dir, launch.browser = TRUE)
}
