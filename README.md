# teachrag

RAG (Retrieval-Augmented Generation) for course materials. Parses teaching materials (slides, scripts, syllabus), builds a DuckDB-backed ragnar store with embeddings, and provides Q&A via Ollama (local) or Claude (API).

## Installation

```r
# From source
devtools::install("fellennert/teachrag")
```

## Setup wizard (recommended for first-time setup)

```r
library(teachrag)
run_setup_wizard()
```

The wizard guides you through: choosing directories, parsing materials, building the store, testing a question, and launching the app.

## Quick start

```r
library(teachrag)

# Set paths (or use defaults via options)
intermediate_dir <- "path/to/your/intermediate"  # contains chunks.rds, syllabus.rds, store
store_path <- file.path(intermediate_dir, "teaching_db.ragnar.duckdb")

# Single-turn Q&A
ask_rag("What is supervised machine learning?", store_path = store_path, intermediate_dir = intermediate_dir)

# Multi-turn chat
chat_state <- NULL
res1 <- ask_rag_chat(chat_state, "What is supervised machine learning?", store_path = store_path, intermediate_dir = intermediate_dir)
chat_state <- res1$chat_state
res2 <- ask_rag_chat(chat_state, "Can you give an example?", store_path = store_path, intermediate_dir = intermediate_dir)

# Shiny app
run_app(store_path = store_path, intermediate_dir = intermediate_dir)

# CLI
interactive_cli(store_path = store_path, intermediate_dir = intermediate_dir)
```

## Building the store

```r
# 1. Parse and chunk materials
parse_materials(corpus_dir = "path/to/course_material", output_dir = "path/to/intermediate")

# 2. Build ragnar store (requires nomic-embed-text via Ollama)
build_store(output_dir = "path/to/intermediate")
```

## Requirements

- R packages: `dplyr (>= 1.2.0)` and others (see DESCRIPTION). Run `teachrag::check_dependencies()` to verify, or `teachrag::ensure_dependencies()` to install missing packages.
- [Ollama](https://ollama.com/) with `qwen2.5:3b` and `nomic-embed-text`
- For Claude: `ANTHROPIC_API_KEY` in environment
