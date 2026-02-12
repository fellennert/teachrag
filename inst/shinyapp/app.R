# Teaching RAG Shiny App - part of teachrag package
# Run via: teachrag::run_app(store_path, intermediate_dir)

ui <- fluidPage(
  titlePanel("Teaching RAG"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Backend"),
      radioButtons(
        "backend",
        label = NULL,
        choices = c("Ollama (local)" = "ollama", "Claude (API)" = "claude"),
        selected = "ollama"
      ),
      conditionalPanel(
        condition = "input.backend == 'ollama'",
        textInput("model_ollama", "Model (Ollama)", value = "qwen2.5:3b")
      ),
      conditionalPanel(
        condition = "input.backend == 'claude'",
        passwordInput("api_key", "API key (Claude)", placeholder = "Or set ANTHROPIC_API_KEY")
      ),
      hr(),
      p("Store: teaching_db.ragnar.duckdb in intermediate/", style = "font-size: 0.85em; color: #666;")
    ),
    mainPanel(
      width = 9,
      textAreaInput(
        "question",
        "Ask a question about the course",
        rows = 3,
        placeholder = "e.g. What is supervised machine learning?"
      ),
      actionButton("ask", "Ask", class = "btn-primary"),
      actionButton("new_chat", "New conversation", class = "btn-default"),
      br(), br(),
      hr(),
      h4("Conversation"),
      div(style = "max-height: 400px; overflow-y: auto;", uiOutput("conversation")),
      hr(),
      h4("Retrieved chunks"),
      dataTableOutput("chunks_table")
    )
  )
)

server <- function(input, output, session) {
  result <- shiny::reactiveVal(NULL)
  status <- shiny::reactiveVal("idle")
  chat_state <- shiny::reactiveVal(NULL)
  history <- shiny::reactiveVal(list())

  store_path <- getOption("teachrag.store_path")
  intermediate_dir <- getOption("teachrag.intermediate_dir")

  shiny::observeEvent(input$backend, {
    chat_state(NULL)
    history(list())
  })

  shiny::observeEvent(input$ask, {
    question <- trimws(input$question)
    if (question == "") {
      shiny::showNotification("Please enter a question.", type = "warning")
      return()
    }

    shiny::updateActionButton(session, "ask", label = "Asking...")

    model <- if (input$backend == "ollama") {
      input$model_ollama
    } else {
      if (nzchar(input$api_key)) {
        Sys.setenv(ANTHROPIC_API_KEY = input$api_key)
      }
      "claude"
    }
    use_claude <- (model == "claude")

    status("loading")
    result(NULL)

    res <- tryCatch(
      {
        shiny::withProgress(
          message = "Asking...",
          value = 0,
          detail = "Querying database...",
          {
            progress <- function(value, detail) shiny::setProgress(value = value, detail = detail)
            teachrag::ask_rag_chat(
              chat_state(),
              question,
              store_path = store_path,
              intermediate_dir = intermediate_dir,
              model = model,
              use_claude = use_claude,
              progress = progress
            )
          }
        )
      },
      error = function(e) {
        status("error")
        list(
          question = question,
          answer = paste0("Error: ", conditionMessage(e)),
          chunks = tibble::tibble()
        )
      }
    )

    shiny::updateActionButton(session, "ask", label = "Ask")
    if (status() != "error") {
      status("idle")
      chat_state(res$chat_state)
      history(c(history(), list(list(q = question, a = res$answer))))
      result(res)
    }
  })

  shiny::observeEvent(input$new_chat, {
    chat_state(NULL)
    history(list())
    result(NULL)
  })

  output$conversation <- shiny::renderUI({
    if (status() == "loading") {
      hist <- history()
      if (length(hist) == 0) {
        return(shiny::p(shiny::strong("Asking..."), style = "color: #666;"))
      }
    }
    hist <- history()
    if (length(hist) == 0) {
      return(shiny::p("Enter a question and click Ask to start.", style = "color: #666;"))
    }
    items <- lapply(seq_along(hist), function(i) {
      shiny::tagList(
        shiny::div(class = "well well-sm", style = "margin-bottom: 8px;",
          shiny::strong("Q: "), hist[[i]]$q
        ),
        shiny::div(class = "well", style = "margin-bottom: 12px; white-space: pre-wrap;",
          shiny::strong("A: "), hist[[i]]$a
        )
      )
    })
    if (status() == "loading") {
      items <- c(items, list(shiny::p(shiny::strong("Asking..."), style = "color: #666;")))
    }
    do.call(shiny::tagList, items)
  })

  output$chunks_table <- shiny::renderDataTable({
    res <- result()
    if (is.null(res) || nrow(res$chunks) == 0) {
      return(data.frame(message = "No chunks to display."))
    }
    txt_col <- if ("text" %in% names(res$chunks)) "text" else names(res$chunks)[ncol(res$chunks)]
    chunks_display <- res$chunks |>
      dplyr::mutate(
        text_snippet = substr(.data[[txt_col]], 1, 200),
        .keep = "unused"
      )
    cols <- intersect(c("chunk_id", "doc_type", "text_snippet"), names(chunks_display))
    chunks_display <- dplyr::select(chunks_display, dplyr::any_of(cols))
    chunks_display
  }, options = list(pageLength = 5, scrollX = TRUE))
}

shinyApp(ui, server)
