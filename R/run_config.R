#' @title Run XGBoost Classifier
#' @description Function has 2 modes:\cr
#' \enumerate{
#'   \item If default is set to TRUE, XGBoost is trained with default configuration.
#'   \item If meta_config is st to TRUE \code{\link{metalearn_config}} gets called to start
#'    meta-learning pipeline and after finding it XGBoost is trained with meta configuration.
#' }
#' @details requires internet connection.
#' @param task_id `integer(1)` task id
#' @param meta_features `data.table()` task meta_features
#' @param config `list()` named list of configurations in case neither default nor meta_config, but custom config
#' @param meta_config `logical(1)` if TRUE meta-analysis is done to find configuration
#' @param default `logical(1)` if TRUE learner is trained with default configuration
#' @param seed `numeric(1)` seed for reproducibility
#' @param nthread `integer(1)` number of cores to be used when training and predicting with learner
#' @return invisible `list()` containing resulting AUC and train_time measures.
#' @export
run_config = function(
    task_id,
    config = NULL,
    meta_config = TRUE,
    default = FALSE,
    seed = NULL,
    nthread = future::availableCores()[[1]],
    meta_features
) {
  # Check internet connection
  if (!curl::has_internet()) {stop("Internet Connection not detected")}
  assertInt(task_id, lower = 1)
  assertNumeric(seed)
  assertFlag(meta_config)
  assertFlag(default)
  assertInt(nthread)

  # Case custom configuration
  if(!is.null(config)) {
    meta_config = FALSE
    default = FALSE
  }

  # Case default configuration
  if (default) {
    # Case if Task has already been trained with default config.
    fn = sprintf("data/results/final_performance/default/result_default_%d", task_id)
    if (file.exists(fn)) {
      cat(
        "\n[INFO] Task has already been trained with the default config and has achieved following performance:\n"
      )
      default_results = readRDS(fn)
      catf(
        "[INFO] Default config scored an AUC of %f in %fs on task ID %i.\n",
        default_results$auc,
        default_results$traintime,
        task_id
      )
      return(invisible(default_results))
    }
    meta_config = FALSE
    default_config = list(
      nrounds = 464,
      eta = 0.0082,
      subsample = 0.982,
      max_depth = 11,
      min_child_weight = 3.30,
      colsample_bytree = 0.975,
      colsample_bylevel = 0.9,
      lambda = 0.06068,
      alpha = 0.00235,
      gamma = 0
    )
    if (!is.null(config)) warning("Provided config will be overwritten with default configuration")
    config = default_config
    cat("\n[INFO] Default Configuration is set\n")
  }

  # Case meta configuration: run two different HP configs through function recursion: one for best one for fastest
  if (meta_config) {
    # Case if Task has already been trained with meta config.
    fn = sprintf("data/results/final_performance/meta/result_meta_%d", task_id)
    if (file.exists(fn)) {
      cat(
        "\n[INFO] Task has already been trained with meta config and has achieved following performance:\n"
      )
      meta_results = readRDS(fn)
      print(t(sapply(meta_results, unlist)))
      return(invisible(meta_results))
    }
    # Case not yet trained:
    meta_config = metalearn_config(
      task_id,
      meta_feature_names,
      meta_features = meta_features,
      meta_data
    )
    # If best and fastest are the same run only once
    if (identical(meta_config$best, meta_config$fastest)) {
      cat("\n[INFO] Both best and fastest configurations are identical, hereby it will be run only once.\n")
      meta_config$fastest = NULL
    }
    meta_final_result = lapply(
      meta_config,
      FUN = function(x) {
        apply(
          x[1, ],
          MARGIN = 1,
          FUN = function(x) run_config(task_id, config = as.list(x), seed = seed, default = FALSE, meta_config = FALSE)
        )
      }
    )
    # Save meta results
    fn = sprintf("data/results/final_performance/meta/result_meta_%d", task_id)
    saveRDS(meta_final_result, file = fn)
    catf("\n[INFO] Meta results have been saved under %s\n", fn)
    # Explicit return
    return(invisible(meta_final_result))
  }

  assertList(config, names = "named", null.ok = FALSE)
  set.seed(seed)

  # Get train test tasks
  catf("Task ID: %i\n", task_id)
  train_test_tasks = get_train_test_tasks(task_id)

  # Prepare learner
  learner = lrn("classif.xgboost", nthread = nthread)
  learner$predict_type = "prob"
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  # Set Hyperparameters
  learner$param_set$values = insert_named(learner$param_set$values, config)
  # Calculate Accuracy
  learner$train(train_test_tasks$train_task)
  measure = if (length(train_test_tasks$train_task$class_names) > 2L) msr("classif.mauc_aunu") else msr("classif.auc")
  auc = learner$predict(train_test_tasks$test_task)$score(measure)
  # Result
  catf(
    "[INFO] The %s config scored an AUC of %f in %fs on task ID %i.\n",
    ifelse(default, "default", "meta"),
    auc,
    learner$state$train_time,
    task_id
  )

  final_result = list(auc = auc, traintime = learner$state$train_time)
  # Save default results
  if (default) {
    fn = sprintf("data/results/final_performance/default/result_default_%d", task_id)
    saveRDS(final_result, file = fn)
    catf("\n[INFO] Default results have been saved under %s\n", fn)
  }
  invisible(final_result)
}
