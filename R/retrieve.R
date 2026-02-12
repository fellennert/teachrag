#' Single-turn RAG Q&A
#'
#' Retrieves chunks for the question, sends to LLM with content checker.
#' Supports Ollama (local) or Claude (API).
#'
#' @param question User question.
#' @param store_path Path to ragnar DuckDB store.
#' @param intermediate_dir Path containing syllabus.rds (for content checker).
#' @param model Model name (qwen2.5:3b or claude).
#' @param use_claude Use Claude API. Set ANTHROPIC_API_KEY for Claude.
#' @param top_k Number of chunks to retrieve.
#' @param verbose Print answer to console.
#' @param progress Optional function(value, detail) for progress updates, e.g. Shiny's setProgress.
#' @return List with question, answer, context_text, chunks.
#' @export
ask_rag <- function(
  question,
  store_path = teachrag_store_path(),
  intermediate_dir = teachrag_intermediate_dir(),
  model = "qwen2.5:3b",
  use_claude = FALSE,
  top_k = 5L,
  verbose = TRUE,
  progress = NULL
) {
  if (is.null(store_path) || is.null(intermediate_dir)) {
    stop("store_path and intermediate_dir required. Set options(teachrag.store_path, teachrag.intermediate_dir) or pass explicitly.")
  }
  if (!fs::file_exists(store_path)) {
    stop("RAG store not found at ", store_path, ". Run build_store() first.")
  }

  if (is.null(progress) && verbose) {
    progress <- function(value, detail) message(detail)
  }
  if (!is.null(progress)) progress(1/3, "Querying database...")
  store <- ragnar::ragnar_store_connect(store_path)
  chunks_q <- ragnar::ragnar_retrieve_bm25(store, question, top_k = top_k)

  if (nrow(chunks_q) == 0) {
    return(list(
      question = question,
      answer = "I could not find relevant information in the course materials.",
      chunks = chunks_q
    ))
  }

  context_text <- chunks_q |>
    dplyr::mutate(
      header = paste0("[chunk_id=", chunk_id, "]")
    ) |>
    dplyr::transmute(text = paste(header, text, sep = "\n")) |>
    purrr::pluck("text") |>
    paste(collapse = "\n\n---\n\n")

  answer <- ask_llm_with_context(
    question = question,
    context_text = context_text,
    model = model,
    use_claude = use_claude,
    intermediate_dir = intermediate_dir,
    progress = progress
  )

  result <- list(
    question = question,
    answer = answer,
    context_text = context_text,
    chunks = chunks_q
  )
  if (verbose) cat(result$answer, "\n")
  invisible(result)
}


#' Multi-turn RAG chat
#'
#' Pass chat_state to continue a conversation. Retrieves chunks dynamically
#' for each question to keep answers aligned with course content.
#'
#' @param chat_state From previous call, or NULL for first turn.
#' @param question User question.
#' @param store_path Path to ragnar store.
#' @param intermediate_dir Path containing syllabus.rds.
#' @param model Model name.
#' @param use_claude Use Claude API.
#' @param top_k Number of chunks to retrieve.
#' @param progress Optional function(value, detail) for progress updates, e.g. Shiny's setProgress.
#' @return List with question, answer, chunks, chat_state.
#' @export
ask_rag_chat <- function(
  chat_state,
  question,
  store_path = teachrag_store_path(),
  intermediate_dir = teachrag_intermediate_dir(),
  model = "qwen2.5:3b",
  use_claude = FALSE,
  top_k = 5L,
  progress = NULL
) {
  if (is.null(store_path) || is.null(intermediate_dir)) {
    stop("store_path and intermediate_dir required. Set options(teachrag.store_path, teachrag.intermediate_dir) or pass explicitly.")
  }
  if (!fs::file_exists(store_path)) {
    stop("RAG store not found at ", store_path, ". Run build_store() first.")
  }

  if (is.null(progress)) {
    progress <- function(value, detail) message(detail)
  }

  is_first_turn <- is.null(chat_state) || is.null(chat_state$chat_session)

  if (is_first_turn) {
    progress(1/3, "Querying database...")
    store <- ragnar::ragnar_store_connect(store_path)
    chunks_q <- ragnar::ragnar_retrieve_bm25(store, question, top_k = top_k)
    if (nrow(chunks_q) == 0) {
      return(list(
        question = question,
        answer = "I could not find relevant information in the course materials.",
        chunks = chunks_q,
        chat_state = NULL
      ))
    }
    context_text <- chunks_q |>
      dplyr::mutate(header = paste0("[chunk_id=", chunk_id, "]")) |>
      dplyr::transmute(text = paste(header, text, sep = "\n")) |>
      purrr::pluck("text") |>
      paste(collapse = "\n\n---\n\n")

    llm_res <- ask_llm_chat(
      question = question,
      context_text = context_text,
      model = model,
      use_claude = use_claude,
      chat_session = NULL,
      intermediate_dir = intermediate_dir,
      progress = progress
    )
    chat_state <- list(
      chat_session = llm_res$chat_session,
      context_text = context_text,
      chunks = chunks_q,
      model = model,
      use_claude = use_claude,
      store = store
    )
    return(list(
      question = question,
      answer = llm_res$answer,
      chunks = chunks_q,
      chat_state = chat_state
    ))
  }

  progress(1/3, "Querying database...")
  chunks_q <- ragnar::ragnar_retrieve_bm25(chat_state$store, question, top_k = top_k)
  if (nrow(chunks_q) == 0) {
    return(list(
      question = question,
      answer = "I could not find relevant information in the course materials.",
      chunks = chunks_q,
      chat_state = chat_state
    ))
  }
  context_text <- chunks_q |>
    dplyr::mutate(header = paste0("[chunk_id=", chunk_id, "]")) |>
    dplyr::transmute(text = paste(header, text, sep = "\n")) |>
    purrr::pluck("text") |>
    paste(collapse = "\n\n---\n\n")

  llm_res <- ask_llm_chat(
    question = question,
    context_text = context_text,
    model = chat_state$model,
    use_claude = chat_state$use_claude,
    chat_session = chat_state$chat_session,
    intermediate_dir = intermediate_dir,
    progress = progress
  )
  chat_state$chat_session <- llm_res$chat_session
  chat_state$context_text <- context_text
  chat_state$chunks <- chunks_q
  list(
    question = question,
    answer = llm_res$answer,
    chunks = chunks_q,
    chat_state = chat_state
  )
}
