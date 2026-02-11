#' Run the setup wizard
#'
#' Launches an interactive Shiny wizard to set up the teaching RAG system:
#' choose directories, parse materials, build the store, test a question,
#' and optionally launch the main app.
#'
#' @return Invisibly, the return value from the wizard. If the user clicks
#'   "Launch teaching RAG app", the main app is started.
#' @export
run_setup_wizard <- function() {
  app_dir <- system.file("setup_wizard", package = "teachrag")
  if (app_dir == "") {
    stop("Setup wizard not found. Reinstall the package.")
  }
  result <- shiny::runApp(app_dir, launch.browser = TRUE)
  if (is.list(result) && isTRUE(result$launch)) {
    run_app()
  }
  invisible(result)
}
