#' @title Distribution of AUC
#' @description Return table of distribution of AUC for given data_ids.
#' @param data_ids `numeric()` numeric vector of data_ids
#' @param meta_data `data.table()` xgboost_meta_data
#' @return `table()` of frequencies of AUC.
#' @export
return_auc_distribution = function(data_ids, meta_data) {
  assertDataTable(meta_data)
  assertNumeric(data_ids)
  mask = meta_data$data_id %in% data_ids
  res = table(round(meta_data[mask, auc], 2))
  print(res)
  invisible(res)
}
