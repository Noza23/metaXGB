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
  default_matrix = default_matrix[sorted_rows_default, ]

  # Reading meta results
  result_files_meta = list.files("data/results/final_performance/meta", pattern = "result_meta", full.names = TRUE)
  results_meta = lapply(result_files_meta, readRDS)
  meta_matrix = t(sapply(results_meta, unlist))
  row_names_meta = gsub(result_files_meta, pattern = "[^0-9]", replacement = "")
  rownames(meta_matrix) = row_names_meta
  sorted_rows_meta = as.character(sort(as.numeric(rownames(meta_matrix))))
  meta_matrix = meta_matrix[sorted_rows_meta, ]

  catf("\n[INFO] See performance table in Plots tab:\n")

  tbl = cbind(default_matrix, meta_matrix)
  kbl = knitr::kable(
    format = "html",
    tbl,
    caption = "Performance Overview",
    align = "c",
    col.names = rep(c("AUC", "timetrain"), 3)
  )
  kbl = kableExtra::add_header_above(
    kbl,
    header = c(" " = 1, "Default" = 2, "Highest" = 2, "Fastest" = 2),
    align = "center"
  )

  kbl_res = kableExtra::kable_styling(res, bootstrap_options = c("striped", "hover"))
  print(kbl_res)
  fn = "data/results/performance.html"
  kableExtra::save_kable(res_kbl, file = fn)
  catf("\n[INFO] Performance Table has been saved under %s\n", fn)

  invisible(list(default_perf = default_matrix, meta_perf = meta_matrix))
}
