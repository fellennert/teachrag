#' Parse and chunk course materials
#'
#' Discovers files in corpus_dir (qmd, Rmd, md, pdf), converts to markdown via
#' ragnar, chunks by doc type (slides by " --- ", scripts by "##" and "###"),
#' writes syllabus.rds and chunks.rds to output_dir.
#'
#' @param corpus_dir Path to course materials (syllabus/, slides/, script/).
#' @param output_dir Path for intermediate outputs. Default: `dirname(corpus_dir)/intermediate`.
#' @return Invisibly, the path to chunks.rds.
#' @export
parse_materials <- function(corpus_dir, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- file.path(dirname(corpus_dir), "intermediate")
  }
  fs::dir_create(output_dir)

  source_files <- fs::dir_ls(
    corpus_dir,
    recurse = TRUE,
    type = "file",
    regexp = "\\.(qmd|Rmd|md|pdf)$"
  )

  identify_doc_type <- function(path) {
    dplyr::case_when(
      stringr::str_detect(path, "syllabus") ~ "syllabus",
      stringr::str_detect(path, "slides") ~ "lecture",
      stringr::str_detect(path, "script") ~ "R script"
    )
  }

  documents <- tibble::tibble(
    doc_id = seq_along(source_files),
    source_path = source_files
  ) |>
    dplyr::mutate(doc_type = identify_doc_type(source_path))

  read_markdown_safely <- purrr::safely(ragnar::read_as_markdown)

  documents_md <- documents |>
    dplyr::mutate(
      md_obj = purrr::map(source_path, ~ read_markdown_safely(.x)),
      md = purrr::map(md_obj, "result"),
      error = purrr::map(md_obj, "error")
    ) |>
    dplyr::select(-md_obj) |>
    dplyr::mutate(
      text_clean = md |>
        as.character() |>
        stringr::str_replace_all("\\s+", " ") |>
        stringr::str_squish()
    )

  readr::write_rds(
    documents_md |>
      dplyr::filter(doc_type == "syllabus") |>
      dplyr::select(doc_type, text_clean),
    fs::path(output_dir, "syllabus.rds")
  )

  base_dir <- corpus_dir
  slides <- documents_md |>
    dplyr::filter(doc_type == "lecture") |>
    dplyr::mutate(
      title = text_clean |>
        stringr::str_extract("(?<= – ).*(?= (GWZ|NSG))"),
      text_clean = text_clean |>
        stringr::str_remove_all("Felix Lennert, M.Sc. [0-9]{1,2}"),
      chunks = text_clean |>
        stringr::str_split(pattern = " --- ")
    ) |>
    tidyr::unnest(chunks) |>
    dplyr::group_by(doc_id) |>
    dplyr::mutate(chunk_id = dplyr::row_number(), .before = 2)

  script <- documents_md |>
    dplyr::filter(doc_type == "R script") |>
    dplyr::mutate(
      title = text_clean |>
        stringr::str_extract('\"[^\"]*\"'),
      text_clean = text_clean |> stringr::str_remove("---.*---"),
      chunks = text_clean |>
        stringr::str_split("## ")
    ) |>
    tidyr::unnest(chunks) |>
    dplyr::mutate(
      chunks = chunks |>
        stringr::str_split("### ")
    ) |>
    tidyr::unnest(chunks) |>
    dplyr::group_by(doc_id) |>
    dplyr::mutate(chunk_id = dplyr::row_number(), .before = 2)

  chunked_materials <- dplyr::bind_rows(script, slides) |>
    dplyr::arrange(doc_id, chunk_id) |>
    dplyr::filter(stringr::str_length(chunks) >= 10) |>
    dplyr::mutate(
      path = source_path |> stringr::str_remove(base_dir),
      doc_id = stringr::str_c(doc_id, chunk_id, sep = "_")
    ) |>
    dplyr::select(doc_id, path, doc_type, chunks)

  readr::write_rds(
    chunked_materials,
    fs::path(output_dir, "chunks.rds")
  )

  invisible(fs::path(output_dir, "chunks.rds"))
}
