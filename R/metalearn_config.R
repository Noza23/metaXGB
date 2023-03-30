#' Meta-Learn HP configuration
#' @description This function runs meta-learning pipeline using rest of the functions in the package.
#' It executes following steps:\cr
#' \enumerate{
#'   \item Run \code{\link{find_similar_tasks}} to find similar tasks for given task_id.
#'   \item Filter xgboost_meta_data by these similar tasks and run \code{\link{return_auc_distribution}} to get the first impression on target variable.
#'   \item Run \code{\link{classify_good_bad}} on filtered data to identify Hyperparameters which are "good" with high probability.
#'   \item Run \code{\link{run_perf_est_model}} on resulting "good" points to build performance prediction model.
#'   \item Select final configurations using \code{\link{select_configs}}.
#'   \item Translate them into the format expected in mlr3 for xgboost learner using \code{\link{translate_config}}.
#'   \item Resulting Hyperparameters are then used in \code{\link{run_config}} to run XGBoost on test task and evaluate them.
#' }
#' @param task_id `integer(1)` OpenML task_id
#' @param meta_feature_names `data.table()` names of task meta_features
#' @param meta_features `data.table()` task meta_features
#' @param xgboost_meta_data `data.table()` xgboost meta_data
#' @param seed `numeric(1)` for reproducibility
#' @return `data.table` of 2 different sets of HP configurations one with highest predicted accuracy and one with small training time.
#' @export
metalearn_config = function(
    task_id,
    meta_feature_names,
    meta_features,
    xgboost_meta_data,
    seed = NULL
  ) {
  start.time = Sys.time()
  if (!curl::has_internet()) {stop("Internet Connection not detected")}
  assertNumber(task_id, lower = 1)
  assertDataTable(meta_features)
  assertDataTable(xgboost_meta_data)
  assertNumber(seed)
  set.seed(seed)

  # Return similar Tasks for given task_id
  cluster = find_similar_tasks(
    task_id = task_id,
    meta_features = meta_features,
    meta_feature_name = meta_feature_names,
    meta_data = xgboost_meta_data
  )
  catf("\n[INFO] Test task with task_id %d has following meta_features after mutation:\n", task_id)
  print(cluster$new_task_meta_features)

  catf("\n[INFO] Cluster contains following datasets (data_ids): %s", paste(cluster$data_ids, collapse = ", "))
  # If learner for this cluster has already been trained and results exits, use it.
  configs_file = sprintf("data/results/meta_configs/configs_%s", cluster$class)
  if (file.exists(configs_file)) {
    configs_result = readRDS(configs_file)
    catf(
      "\n[INFO] Meta-Analysis for cluster %s has already been done, so configs file from data/results will be reused!",
      cluster$class
    )
    # Explicit return
    return(lapply(configs_result, translate_config))
  }

  cluster_data = meta_data[data_id %in% cluster$data_ids]
  # Check the distribution of AUC in cluster_data
  catf("\n[INFO] AUC distribution in the meta_dataset for given Cluster looks as follows:")
  auc_table = return_auc_distribution(cluster$data_ids, meta_data = xgboost_meta_data)

  grDevices::pdf(file = sprintf("plots/auc_dist_%s.pdf", cluster$class), width = 20)
  graphics::barplot(auc_table)
  grDevices::dev.off()
  catf("\n[INFO] AUC distribution plot has been saved under plots/auc_dist_%s.pdf\n", cluster$class)

  # Random Forest Classifier with default HP config to detect good/bad configurations in the datase
    perf_est_data = classify_good_bad(
    cluster_data,
    threshold = 0.5,
    nthread = future::availableCores()[[1]],
    class = cluster$class
  )

  # Random Forest Regression with default HP config for performance estimation
  perf_predictions = run_perf_est_model(
    perf_est_data,
    nthread = future::availableCores()[[1]],
    class = cluster$class
  )

  # Select best and fastest configuration for each dataset
  configs_result = select_configs(
    perf_predictions,
    perf_est_data = perf_est_data
  )

  # Save resulting configurations
  fn_configs = sprintf("configs_%s", cluster$class)
  saveRDS(configs_result, file = sprintf("data/results/meta_configs/%s", fn_configs))
  catf("[INFO] Selectd configurations have been saved under data/results/meta_configs/%s", fn_configs)

  # Timing
  catf(
    "\n [INFO] Running Meta-Analysis including creating Interpretability plots has lasted: %s",
    format(round(Sys.time() - start.time, 2))
  )
  # Translate configuration in a way that mlr3 expects: undo log transformation and adjust names
  lapply(configs_result, translate_config)
}
