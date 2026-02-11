#' Check dependency status
#'
#' Returns which packages are missing or outdated. Does not install.
#'
#' @return List with `ok` (logical), `missing` (character), `outdated` (character).
#' @export
check_dependencies <- function() {
  deps <- teachrag_deps()
  missing <- character()
  outdated <- character()

  for (i in seq_len(nrow(deps))) {
    pkg <- deps$package[i]
    min_ver <- deps$version[i]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    } else if (min_ver != "0") {
      inst_ver <- as.character(utils::packageVersion(pkg))
      if (utils::compareVersion(inst_ver, min_ver) < 0) {
        outdated <- c(outdated, paste0(pkg, " (need >= ", min_ver, ", have ", inst_ver, ")"))
      }
    }
  }
  list(
    ok = length(missing) == 0 && length(outdated) == 0,
    missing = missing,
    outdated = outdated
  )
}


#' Required packages for teachrag
#'
#' @return A data frame with package names and minimum versions.
#' @keywords internal
teachrag_deps <- function() {
  data.frame(
    package = c(
      "digest", "dplyr", "ellmer", "fs", "ollamar", "purrr", "ragnar",
      "readr", "shiny", "stringr", "tibble", "tidyr"
    ),
    version = c(
      "0.6.0", "1.2.0", "0", "0", "0", "0", "0",
      "0", "0", "0", "0", "0"
    ),
    stringsAsFactors = FALSE
  )
}


#' Ensure required packages are installed
#'
#' Checks that all teachrag dependencies are installed with minimum versions.
#' Offers to install any missing or outdated packages.
#'
#' @param force If TRUE, attempt to install/update packages without prompting.
#' @return Invisibly, TRUE if all dependencies are satisfied, FALSE otherwise.
#' @export
ensure_dependencies <- function(force = FALSE) {
  deps <- teachrag_deps()
  missing <- character()
  outdated <- character()

  for (i in seq_len(nrow(deps))) {
    pkg <- deps$package[i]
    min_ver <- deps$version[i]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    } else if (min_ver != "0") {
      inst_ver <- as.character(utils::packageVersion(pkg))
      if (utils::compareVersion(inst_ver, min_ver) < 0) {
        outdated <- c(outdated, paste0(pkg, " (>= ", min_ver, ", installed: ", inst_ver, ")"))
      }
    }
  }

  to_install <- c(missing, sub(" \\(.*", "", outdated))
  if (length(to_install) == 0) {
    message("All required packages are installed.")
    return(invisible(TRUE))
  }

  msg <- paste(
    "The following package(s) are missing or need updating:",
    paste("  -", to_install, collapse = "\n"),
    "Install them now?",
    sep = "\n"
  )

  if (!force) {
    resp <- readline(paste0(msg, "\n[y/n]: "))
    if (tolower(substr(trimws(resp), 1, 1)) != "y") {
      message("Installation skipped. Run install.packages() for: ", paste(to_install, collapse = ", "))
      return(invisible(FALSE))
    }
  }

  for (pkg in unique(sub(" \\(.*", "", to_install))) {
    message("Installing ", pkg, "...")
    utils::install.packages(pkg, dependencies = TRUE)
  }
  message("Done.")
  invisible(TRUE)
}
