# teachrag Setup Wizard - multi-step guided setup

step_ui <- function(step, title, content) {
  shiny::div(
    class = "well",
    style = "margin-bottom: 20px;",
    shiny::h4(shiny::icon("caret-right"), " Step ", step, ": ", title),
    content
  )
}

ui <- shiny::fluidPage(
  title = "teachrag Setup Wizard",
  shiny::titlePanel(shiny::icon("graduation-cap"), "teachrag Setup Wizard"),
  shiny::p(
    "This wizard guides you through setting up the teaching RAG system. ",
    "You need course materials in a folder with syllabus/, slides/, and script/ subfolders."
  ),
  shiny::hr(),

  step_ui(0, "Dependencies", shiny::tagList(
    shiny::p("Ensure required R packages are installed (including dplyr >= 1.2.0):"),
    shiny::actionButton("check_deps", "Check / install dependencies", class = "btn-info"),
    shiny::br(), shiny::br(),
    shiny::uiOutput("deps_status")
  )),

  step_ui(1, "Course materials directory", shiny::tagList(
    shiny::p("Path to your course materials (containing syllabus/, slides/, script/):"),
    shiny::textInput("corpus_dir", NULL, placeholder = "/path/to/course_material", width = "100%"),
    shiny::p(shiny::em("Tip: Use the full path. On Mac, drag a folder into Terminal to paste its path."), style = "font-size: 0.9em; color: #666;")
  )),

  step_ui(2, "Output directory", shiny::tagList(
    shiny::p("Where to save parsed chunks and the RAG store (default: same folder as materials, in 'intermediate/'):"),
    shiny::textInput("output_dir", NULL, placeholder = "Leave empty for default", width = "100%"),
    shiny::p(shiny::em("Default: [materials_folder]/../intermediate"), style = "font-size: 0.9em; color: #666;")
  )),

  step_ui(3, "Parse materials", shiny::tagList(
    shiny::p("Convert course materials to chunks. This creates syllabus.rds and chunks.rds."),
    shiny::actionButton("do_parse", "Parse materials", class = "btn-primary"),
    shiny::br(), shiny::br(),
    shiny::uiOutput("parse_status")
  )),

  step_ui(4, "Build RAG store", shiny::tagList(
    shiny::p("Create the searchable store with embeddings. Requires Ollama with nomic-embed-text."),
    shiny::actionButton("do_build", "Build store", class = "btn-primary"),
    shiny::br(), shiny::br(),
    shiny::uiOutput("build_status")
  )),

  step_ui(5, "Test & launch", shiny::tagList(
    shiny::p("Try a question, then launch the full app."),
    shiny::fluidRow(
      shiny::column(8,
        shiny::textInput("test_question", "Test question", placeholder = "e.g. What is supervised machine learning?", width = "100%")
      ),
      shiny::column(4,
        shiny::actionButton("do_test", "Test", class = "btn-success")
      )
    ),
    shiny::uiOutput("test_result"),
    shiny::br(),
    shiny::actionButton("launch_app", "Launch teaching RAG app", class = "btn-primary btn-lg")
  )),

  shiny::hr(),
  shiny::p(shiny::em("teachrag: RAG for course materials with Ollama and Claude"), style = "font-size: 0.85em; color: #999;")
)

server <- function(input, output, session) {
  parse_done <- shiny::reactiveVal(FALSE)
  build_done <- shiny::reactiveVal(FALSE)

  output_dir_resolved <- shiny::reactive({
    corp <- trimws(input$corpus_dir)
    out <- trimws(input$output_dir)
    if (out == "") {
      if (corp != "") file.path(dirname(corp), "intermediate") else ""
    } else {
      out
    }
  })

  deps_status <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$check_deps, {
    shiny::withProgress(
      message = "Checking dependencies...",
      value = 0,
      {
        status <- teachrag::check_dependencies()
        shiny::setProgress(1)
        if (status$ok) {
          deps_status(list(ok = TRUE, msg = "All required packages are installed (including dplyr >= 1.2.0)."))
        } else {
          to_install <- c(status$missing, sub(" \\(.*", "", status$outdated))
          deps_status(list(
            ok = FALSE,
            msg = paste0(
              "Missing or outdated: ", paste(to_install, collapse = ", "), ". ",
              "Run: install.packages(c(\"", paste(unique(to_install), collapse = "\", \""), "\"))"
            )
          ))
        }
      }
    )
  })

  output$deps_status <- shiny::renderUI({
    s <- deps_status()
    if (is.null(s)) return(shiny::span())
    if (s$ok) {
      shiny::div(shiny::icon("check-circle", style = "color: green;"), " ", s$msg)
    } else {
      shiny::div(
        shiny::icon("exclamation-triangle", style = "color: orange;"),
        " ", s$msg,
        shiny::br(), shiny::br(),
        shiny::actionButton("install_deps", "Try to install now", class = "btn-warning")
      )
    }
  })

  shiny::observeEvent(input$install_deps, {
    status <- teachrag::check_dependencies()
    to_install <- unique(c(status$missing, sub(" \\(.*", "", status$outdated)))
    if (length(to_install) == 0) return()
    shiny::withProgress(
      message = "Installing packages...",
      value = 0,
      detail = paste(to_install, collapse = ", "),
      {
        tryCatch(
          {
            utils::install.packages(to_install, dependencies = TRUE)
            deps_status(list(ok = TRUE, msg = "Installation complete. Click 'Check' again to verify."))
            shiny::showNotification("Packages installed. Click 'Check' to verify.", type = "message")
          },
          error = function(e) {
            shiny::showNotification(paste("Install failed:", conditionMessage(e)), type = "error")
          }
        )
      }
    )
  })

  output$parse_status <- shiny::renderUI({
    if (!parse_done()) return(shiny::span())
    shiny::div(
      shiny::icon("check-circle", style = "color: green;"),
      " Parsing complete. syllabus.rds and chunks.rds created."
    )
  })

  output$build_status <- shiny::renderUI({
    if (!build_done()) return(shiny::span())
    shiny::div(
      shiny::icon("check-circle", style = "color: green;"),
      " Store built successfully."
    )
  })

  shiny::observeEvent(input$do_parse, {
    corp <- trimws(input$corpus_dir)
    if (corp == "" || !dir.exists(corp)) {
      shiny::showNotification("Please enter a valid corpus directory.", type = "error")
      return()
    }
    out <- output_dir_resolved()
    if (out == "") {
      shiny::showNotification("Could not resolve output directory.", type = "error")
      return()
    }
    shiny::withProgress(
      message = "Parsing materials...",
      value = 0,
      {
        shiny::setProgress(0.3)
        tryCatch(
          {
            teachrag::parse_materials(corpus_dir = corp, output_dir = out)
            parse_done(TRUE)
            shiny::setProgress(1)
            shiny::showNotification("Parsing complete.", type = "message")
          },
          error = function(e) {
            shiny::showNotification(paste("Error:", conditionMessage(e)), type = "error")
          }
        )
      }
    )
  })

  shiny::observeEvent(input$do_build, {
    if (!parse_done()) {
      shiny::showNotification("Run parsing first (Step 3).", type = "warning")
      return()
    }
    out <- output_dir_resolved()
    shiny::withProgress(
      message = "Building store (this may take a few minutes)...",
      value = 0,
      detail = "Creating embeddings...",
      {
        tryCatch(
          {
            shiny::setProgress(0.2)
            teachrag::build_store(output_dir = out)
            build_done(TRUE)
            shiny::setProgress(1)
            shiny::showNotification("Store built successfully.", type = "message")
          },
          error = function(e) {
            shiny::showNotification(paste("Error:", conditionMessage(e)), type = "error")
          }
        )
      }
    )
  })

  test_result <- shiny::eventReactive(input$do_test, {
    q <- trimws(input$test_question)
    if (q == "") return(shiny::p("Enter a question and click Test."))
    out <- output_dir_resolved()
    store_path <- file.path(out, "teaching_db.ragnar.duckdb")
    wal_path <- paste0(store_path, ".wal")
    if (!fs::file_exists(wal_path)) {
      return(shiny::div(shiny::icon("exclamation-triangle"), " Build the store first (Step 4)."))
    }
    res <- tryCatch(
      teachrag::ask_rag(q, store_path = store_path, intermediate_dir = out, verbose = FALSE),
      error = function(e) list(answer = paste0("Error: ", conditionMessage(e)))
    )
    shiny::div(
      class = "well",
      style = "white-space: pre-wrap; margin-top: 10px;",
      shiny::strong("Answer: "), res$answer
    )
  })

  output$test_result <- shiny::renderUI({
    test_result()
  })

  shiny::observeEvent(input$launch_app, {
    out <- output_dir_resolved()
    store_path <- file.path(out, "teaching_db.ragnar.duckdb")
    wal_path <- paste0(store_path, ".wal")
    if (out == "" || !fs::file_exists(wal_path)) {
      shiny::showNotification("Complete steps 3 and 4 first (parse + build store).", type = "warning")
      return()
    }
    out <- output_dir_resolved()
    options(
      teachrag.store_path = store_path,
      teachrag.intermediate_dir = out
    )
    shiny::stopApp(list(launch = TRUE))
  })
}

shiny::shinyApp(ui, server)
