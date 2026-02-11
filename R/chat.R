#' Ask LLM with context (single-turn)
#'
#' @param question User question.
#' @param context_text Retrieved chunk text.
#' @param model Model name (qwen2.5:3b or claude).
#' @param use_claude Use Claude API.
#' @param intermediate_dir Path containing syllabus.rds for content checker.
#' @return Character answer.
#' @keywords internal
ask_llm_with_context <- function(
  question,
  context_text,
  model = c("qwen2.5:3b", "claude"),
  use_claude = FALSE,
  intermediate_dir
) {
  model <- match.arg(model)
  use_claude <- use_claude || (model == "claude")

  system_prompt <- paste(
    "You are a teaching assistant for a computational social science course.",
    "Try to answer the question using the provided material.",
    collapse = " "
  )

  if (use_claude) {
    chat_session <- ellmer::chat_anthropic(
      system_prompt = system_prompt,
      model = "claude-3-haiku-20240307",
      params = list(temperature = 0.1),
      api_key = Sys.getenv("ANTHROPIC_API_KEY")
    )
  } else {
    ollamar::pull(model)
    chat_session <- ellmer::chat_ollama(
      system_prompt = system_prompt,
      model = model,
      params = list(temperature = 0.1)
    )
  }

  message <- stringr::str_c(
    "Context from course materials:\n\n",
    context_text,
    "\n\nQuestion:\n",
    question
  )
  resp <- chat_session$chat(message, echo = FALSE)

  syllabus_summary <- readr::read_rds(fs::path(intermediate_dir, "syllabus.rds")) |>
    dplyr::pull(text_clean)
  checker_prompt <- stringr::str_c(
    "You are an expert content checker for a computational social science course. ",
    "Accept responses that are somewhat related to one or multiple aspects covered in this syllabus:\n\n",
    syllabus_summary,
    "\n\nIf the answer is related to one or more parts of the summarized course content, output 'accepted'. Otherwise, output 'not accepted'."
  )
  if (use_claude) {
    chat_checker <- ellmer::chat_anthropic(
      system_prompt = checker_prompt,
      model = "claude-3-haiku-20240307",
      params = list(temperature = 0.1),
      api_key = Sys.getenv("ANTHROPIC_API_KEY")
    )
  } else {
    chat_checker <- ellmer::chat_ollama(
      system_prompt = checker_prompt,
      model = "qwen2.5:3b",
      params = list(temperature = 0.2)
    )
  }
  check <- chat_checker$chat(resp, echo = FALSE)
  if (stringr::str_detect(check, "^accepted")) resp else "This question is not covered in the course materials."
}


#' Ask LLM with context (multi-turn chat)
#'
#' @param question User question.
#' @param context_text Retrieved chunk text.
#' @param model Model name.
#' @param use_claude Use Claude API.
#' @param chat_session Existing session from previous turn, or NULL.
#' @param intermediate_dir Path containing syllabus.rds.
#' @return List with answer and chat_session.
#' @keywords internal
ask_llm_chat <- function(
  question,
  context_text,
  model = c("qwen2.5:3b", "claude"),
  use_claude = FALSE,
  chat_session = NULL,
  intermediate_dir
) {
  model <- match.arg(model)
  use_claude <- use_claude || (model == "claude")

  run_content_check <- function(resp) {
    syllabus_summary <- readr::read_rds(fs::path(intermediate_dir, "syllabus.rds")) |>
      dplyr::pull(text_clean)
    checker_prompt <- stringr::str_c(
      "You are an expert content checker for a computational social science course. ",
      "Accept responses that are somewhat related to one or multiple aspects covered in this syllabus:\n\n",
      syllabus_summary,
      "\n\nIf the answer is related to one or more parts of the summarized course content, output 'accepted'. Otherwise, output 'not accepted'."
    )
    if (use_claude) {
      chat_checker <- ellmer::chat_anthropic(
        system_prompt = checker_prompt,
        model = "claude-3-haiku-20240307",
        params = list(temperature = 0.1),
        api_key = Sys.getenv("ANTHROPIC_API_KEY")
      )
    } else {
      chat_checker <- ellmer::chat_ollama(
        system_prompt = checker_prompt,
        model = "qwen2.5:3b",
        params = list(temperature = 0.2)
      )
    }
    check <- chat_checker$chat(resp, echo = FALSE)
    if (stringr::str_detect(check, "^accepted")) resp else "This question is not covered in the course materials."
  }

  message <- stringr::str_c(
    "Context from course materials:\n\n",
    context_text,
    "\n\nQuestion:\n",
    question
  )

  if (is.null(chat_session)) {
    system_prompt <- paste(
      "You are a teaching assistant for a computational social science course.",
      "Try to answer the question using the provided material.",
      collapse = " "
    )
    if (use_claude) {
      chat_session <- ellmer::chat_anthropic(
        system_prompt = system_prompt,
        model = "claude-3-haiku-20240307",
        params = list(temperature = 0.1),
        api_key = Sys.getenv("ANTHROPIC_API_KEY")
      )
    } else {
      ollamar::pull(model)
      chat_session <- ellmer::chat_ollama(
        system_prompt = system_prompt,
        model = model,
        params = list(temperature = 0.1)
      )
    }
    resp <- chat_session$chat(message, echo = FALSE)
    answer <- run_content_check(resp)
  } else {
    resp <- chat_session$chat(message, echo = FALSE)
    answer <- run_content_check(resp)
  }

  list(answer = answer, chat_session = chat_session)
}
