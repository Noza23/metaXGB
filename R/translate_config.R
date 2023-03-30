#' @title Translate HyperParameter Configuration
#' @description Transform Configuration into the format expected in mlr3.
#' @param config `data.table()` configurations to be translated
#' @return `data.table()` of configuration formated as expected in \strong{lrn("classif.xgboost)$param_set}
#' @export
translate_config = function(config) {
  config_names = c(
    "alpha_log", "colsample_bylevel", "colsample_bytree", "eta_log", "gamma_log", "lambda_log",
    "max_depth", "min_child_weight", "num_round", "subsample"
  )
  assertDataTable(config, any.missing = FALSE)
  assertNames(names(config), type = "unique", must.include = config_names)

  # Format Configurations as expected for mlr3 lrn("classif.xgboost")
  data.table(
    alpha = exp(config$alpha_log),
    colsample_bylevel = config$colsample_bylevel,
    colsample_bytree = config$colsample_bytree,
    eta = exp(config$eta_log),
    gamma = exp(config$gamma_log),
    lambda = exp(config$lambda_log),
    max_depth = config$max_depth,
    min_child_weight = config$min_child_weight,
    nrounds = config$num_round,
    subsample = config$subsample
  )
}
