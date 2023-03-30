#' @title Predict performance
#' @description Runs RandomForest Regression with default HP config and 5 fold CV resampling
#' on the "good" configurations resulted by the function call \code{\link{classify_good_bad}}
#' and tries to interpret it using \code{\link{interpret_model}} function.
#'
#' @param perf_est_data (`data.table(1)`) meta_data
#' @param nthread (`integer(1)`) number of cores to be used when training a predicting learner
#' @param class (`integer(1)`) identifier of the group of similar tasks
#' @return `data.table()` of performance predictions on the 5 test sets.
#' @export
run_perf_est_model = function(perf_est_data, nthread = future::availableCores()[[1]], class) {
  assertDataTable(perf_est_data)
  assertInt(nthread)
  assertString(class)

  # Regression Task for performance estimation
  perf_est_task = as_task_regr(
    x = perf_est_data,
    target = "auc",
    id = "performance_estimator"
  )
  perf_est_task$set_col_roles("data_id", roles = "stratum")

  # Regression RandomForest for performance estimation
  perf_est_learner = lrn(
    "regr.ranger",
    num.threads = nthread,
    importance = "impurity"
  )

  # 6 fold CV resampling
  rr = resample(
    perf_est_task,
    perf_est_learner,
    resampling = rsmp("cv", folds = 6),
    store_models = TRUE
  )
  cat("\n[INFO] Random Forest has achieved following MAE on test sets of 6 fold CV:\n\n")
  print(rr$score(msr("regr.mae")))
  Sys.sleep(5)

  preds = rr$prediction()

  # Interpret Random Forest predicting performance
  new_data_id = sample(preds$row_ids, size = 1)
  new_data = perf_est_data[new_data_id, ]
  cat("\n[INFO] Starting creating interpretation Plots in directory plots \n\n")
  interpret_learner(
    model = mlr3misc::map(as.data.table(rr)$learner, "model")[[1]],
    learner = rr$learners[[1]],
    task = perf_est_task$clone()$filter(rr$predictions()[[1]]$row_ids),
    new_data = new_data,
    class = class
  )

  # Saving performance results
  fn = sprintf("perf_estimator_perf_%s", class)
  res = list(
    regr.mae = preds$score(msr("regr.mae"))
  )
  saveRDS(res, file = sprintf("data/results/meta_learners/perf_estimator/%s", fn))
  catf("\n[INFO] Performance results have been saved under: data/results/meta_learners/perf_estimator/%s", fn)
  Sys.sleep(2)

  as.data.table(preds)
}
