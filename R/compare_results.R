#' @title Compare Results
#' @description Function should be called in the end after all results are produced.
#' @return `list(2)` containing default and meta performances.
#' @export
compare_results = function() {
  # Reading default results
  result_files_default = list.files(
    "data/results/final_performance/default",
    pattern = "result_default",
    full.names = TRUE
  )
  results_default = lapply(result_files_default, readRDS)
  default_matrix = t(sapply(results_default, unlist))
  row_names_default = gsub(result_files_default, pattern = "[^0-9]", replacement = "")
  rownames(default_matrix) = row_names_default
  sorted_rows_default = as.character(sort(as.numeric(rownames(default_matrix))))
  default_matrix = default_matrix[sorted_rows, ]

  catf("\n[INFO] Default configuration has achieved following performances:\n")
  print(default_matrix)

  # Reading meta results
  result_files_meta = list.files("data/results/final_performance/meta", pattern = "result_meta", full.names = TRUE)
  results_meta = lapply(result_files_meta, readRDS)
  meta_matrix = t(sapply(results_meta, unlist))
  row_names_meta = gsub(result_files_meta, pattern = "[^0-9]", replacement = "")
  rownames(meta_matrix) = row_names_meta
  sorted_rows_meta = as.character(sort(as.numeric(rownames(meta_matrix))))
  meta_matrix = meta_matrix[sorted_rows_meta, ]

  catf("\n[INFO] Meta configuration has achieved following performances:\n")
  print(meta_matrix)

  invisible(list(default_perf = default_matrix, meta_perf = meta_matrix))
}
