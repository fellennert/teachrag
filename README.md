# teachrag

RAG (Retrieval-Augmented Generation) for course materials. Parses teaching materials (slides, scripts, syllabus), builds a DuckDB-backed ragnar store with embeddings, and provides Q&A via Ollama (local) or Claude (API).

## Requirements

Before installing or running teachrag, ensure you have:

- **R packages**: `dplyr` (>= 1.2.0) and others (see DESCRIPTION). Run `teachrag::check_dependencies()` to verify, or `teachrag::ensure_dependencies()` to install missing packages.
- **[Ollama](https://ollama.com/)** with models `qwen2.5:3b` and `nomic-embed-text` (for local Q&A and embeddings)
- **Claude API** (optional): if you prefer using Claude via API, set `ANTHROPIC_API_KEY` in your environment to use Claude instead of a local LLM. You will still need ollama and `nomic-embed-text` for the embeddings.

## Installation

```r
# From source
devtools::install("fellennert/teachrag")
```

## Out of the box (bundled data)

The package ships with pre-built course data. You can run Q&A immediately without any setup:

```r
library(teachrag)

# Single-turn Q&A (uses bundled data by default)
ask_rag("What is supervised machine learning?")

# Multi-turn chat
chat_state <- NULL
res1 <- ask_rag_chat(chat_state, "What is supervised machine learning?")
chat_state <- res1$chat_state
res2 <- ask_rag_chat(chat_state, "Can you give an example?")

# Shiny app (with progress bar)
run_app()

# CLI (prints status: Querying database → Producing answer → Fact-checking)
interactive_cli()
```

All of these show progress: **Querying database…** → **Producing initial answer…** → **Fact-checking answer…**

## Setup wizard (for your own materials)

To parse and index your own course materials:

```r
library(teachrag)
run_setup_wizard()
```

The wizard guides you through: choosing directories, parsing materials, building the store, testing a question, and launching the app.

## Custom paths

If you have your own intermediate directory and store:

```r
library(teachrag)

intermediate_dir <- "path/to/your/intermediate"  # contains chunks.rds, syllabus.rds, store
store_path <- file.path(intermediate_dir, "teaching_db.ragnar.duckdb")

# Pass explicitly, or set options
options(teachrag.intermediate_dir = intermediate_dir, teachrag.store_path = store_path)

ask_rag("What is supervised machine learning?")
run_app()
```

## Building the store

```r
# 1. Parse and chunk materials
parse_materials(corpus_dir = "path/to/course_material", output_dir = "path/to/intermediate")

# 2. Build ragnar store (requires nomic-embed-text via Ollama)
build_store(output_dir = "path/to/intermediate")
```
