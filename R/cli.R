#' Interactive CLI for teaching RAG
#'
#' Prompts for model choice, then runs a question loop. Type 'quit' to exit,
#' 'new' to start a new conversation.
#'
#' @param store_path Path to ragnar store.
#' @param intermediate_dir Path containing syllabus.rds.
#' @export
interactive_cli <- function(
  store_path = teachrag_store_path(),
  intermediate_dir = teachrag_intermediate_dir()
) {
  if (is.null(store_path) || is.null(intermediate_dir)) {
    stop("store_path and intermediate_dir required. Set options(teachrag.store_path, teachrag.intermediate_dir) or pass explicitly.")
  }
  cat("Teaching materials RAG assistant\n")
  cat("Type a question (or 'quit' to exit). Type 'new' to start a new conversation.\n")
  cat("Optionally choose a model (default: qwen2.5:3b, other option: claude).\n\n")

  model_input <- readline(prompt = "Choose model [qwen2.5:3b/claude, Enter for default]: ")
  model <- stringr::str_squish(tolower(model_input))
  if (model == "") {
    model <- "qwen2.5:3b"
  } else if (!model %in% c("qwen2.5:3b", "claude")) {
    cat("Unknown model, using default (qwen2.5:3b).\n")
    model <- "qwen2.5:3b"
  }

  chat_state <- NULL
  repeat {
    question <- readline(prompt = "Question: ")
    if (identical(tolower(stringr::str_squish(question)), "quit")) {
      cat("Goodbye.\n")
      break
    }
    if (identical(tolower(stringr::str_squish(question)), "new")) {
      chat_state <- NULL
      cat("Started new conversation.\n\n")
      next
    }
    if (nchar(stringr::str_squish(question)) == 0) {
      next
    }

    res <- ask_rag_chat(
      chat_state,
      question,
      store_path = store_path,
      intermediate_dir = intermediate_dir,
      model = model,
      use_claude = (model == "claude")
    )
    chat_state <- res$chat_state
    cat("\nAnswer:\n")
    cat(res$answer, "\n\n")
  }
}
