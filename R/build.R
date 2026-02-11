#' Build ragnar store from chunks
#'
#' Reads chunks.rds from output_dir, reformats for ragnar, creates DuckDB store
#' with nomic-embed-text embeddings. Requires Ollama with nomic-embed-text model.
#'
#' @param output_dir Path containing chunks.rds. Default: `options(teachrag.intermediate_dir)`.
#' @return Invisibly, the path to the store.
#' @export
build_store <- function(output_dir = teachrag_intermediate_dir()) {
  if (is.null(output_dir)) {
    stop("output_dir required. Set options(teachrag.intermediate_dir = ...) or pass explicitly.")
  }
  chunks_path <- fs::path(output_dir, "chunks.rds")
  if (!fs::file_exists(chunks_path)) {
    stop("chunks.rds not found in ", output_dir, ". Run parse_materials() first.")
  }

  chunks <- readr::read_rds(chunks_path)
  store_location <- fs::path(output_dir, "teaching_db.ragnar.duckdb")

  chunks_reformatted <- chunks |>
    dplyr::rename(
      origin = doc_id,
      text = chunks
    ) |>
    dplyr::mutate(
      hash = digest::digest(text)
    ) |>
    dplyr::select(origin, hash, text)

  ollamar::pull("nomic-embed-text")
  embed_fun <- \(x) ragnar::embed_ollama(x, model = "nomic-embed-text")

  store <- ragnar::ragnar_store_create(
    store_location,
    embed = embed_fun,
    version = 1,
    overwrite = TRUE
  )

  for (i in seq_len(nrow(chunks_reformatted))) {
    ragnar::ragnar_store_insert(store, chunks_reformatted |> dplyr::slice(i))
  }
  ragnar::ragnar_store_build_index(store)

  invisible(store_location)
}
