#' @title Classify good / bad points
#' @description Function runs RandomForest Classifier with default HP config and 3 fold CV resampling
#' on synthetically created binary task where all configs with accuracy below 0.55 get "bad" label and
#' configs with accuracy over 0.55 get label "good".
#' @details For details refer to report in the repository.
#' @param cluster_data `data.table(1)` all data points from similar tasks
#' @param threshold `numeric(1)` threshold for bad/good labeling between 0 and 1
#' @param nthread `integer(1)` number of cores to be used when training a learner
#' @param class `integer(1)` cluster identifier
#' @return `data.table(1)` containing "good" configurations predicted with very high probability over 0.9.
#' All this "good" configuration will be then passed on to \code{\link{run_perf_est_model}} for building
#' performance estimator.
#' @export
classify_good_bad = function(cluster_data, threshold = 0.55, nthread = future::availableCores()[[1]], class) {
  assertDataTable(cluster_data)
  assertNumber(threshold, lower = 0 , upper = 1)
  assertInt(nthread)
  assertString(class)
  # replace AUC column with "good" "bad" at some threshold level
  classifier_data = cluster_data[, - c("task_id", "dataset", "repl")
  ][, auc := ifelse(auc > threshold, "good", "bad")]

  # Create binary classification task
  classifier_task = as_task_classif(
    x = classifier_data,
    target = "auc",
    id = "Good_Bad_Classifier",
    positive = "good"
  )
  # Set stratum for stratification in resampling
  classifier_task$set_col_roles("data_id", roles = "stratum")

  # Classification Random Forest as learner
  classifier_rf = lrn(
    "classif.ranger",
    num.threads = nthread,
    importance = "impurity"
  )
  classifier_rf$predict_type = "prob"
  classifier_rf$encapsulate = c(train = "evaluate", predict = "evaluate")

  # 3 fold CV resampling
  rr = resample(
    classifier_task,
    classifier_rf,
    resampling = rsmp("cv", folds = 3),
    store_models = TRUE
  )

  cat("\n[INFO] Random Forest has achieved following performances on test sets of 3 fold CV:\n\n")
  print(rr$score())
  Sys.sleep(5)

  cat("\n[INFO] Setting prediction threshold to 0.90 to keep only good points with very high probability\n\n")
  predicts = rr$prediction()

  cat("[INFO] Classifier at prediction threshold 0.90 yields following confusion matrix and TPR, FPR:\n")
  predicts$set_threshold(0.90)
  print(predicts$confusion)
  print(predicts$score(msrs(c("classif.tpr", "classif.fpr"))))

  # Saving performance results
  fn = sprintf("GoodBad_perf_%s", class)
  res = list(
    classif.ce = rr$score(),
    confusion_90 = predicts$confusion
  )
  saveRDS(res, file = sprintf("data/results/meta_learners/classifier/%s", fn))
  catf("\n[INFO] Performance results have been saved under: data/results/meta_learners/classifier/%s", fn)
  Sys.sleep(2)

  # Take resulting "good" points from the cluster data for performance estimation model.
  perf_est_ids = as.data.table(predicts)[response == "good", row_ids]
  cluster_data[perf_est_ids, - c("task_id", "dataset", "repl")]
}
