#' @title Predict performance
#' @description Runs RandomForest Regression with default HP config and 5 fold CV resampling
#' on the "good" configurations resulted by the function call \code{\link{classify_good_bad}}.
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
    x = perf_est_data[, - c("data_id")],
    target = "auc",
    id = "performance_estimator"
  )

  # Regression RandomForest for performance estimation
  perf_est_learner = lrn(
    "regr.ranger",
    num.threads = 4,
    importance = "impurity"
  )

  #simple holdout resampling with stratification by target
  splits = partition(perf_est_task, stratify = TRUE)
  perf_est_learner$train(perf_est_task, row_ids = splits$train)
  preds = perf_est_learner$predict(perf_est_task, row_ids = splits$test)

  cat("\n[INFO] Random Forest for perf estimation has achieved following MAE on holdout 1/3 test split:\n\n")
  print(preds$score(msr("regr.mae")))

  #Interpret Random Forest predicting performance
  new_data_id = sample(splits$test, size = 1)
  new_data = perf_est_data[new_data_id, ]
  cat("\n[INFO] Starting creating interpretation Plots in directory plots \n\n")
  interpret_learner(
    learner = perf_est_learner,
    task = perf_est_task,
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
