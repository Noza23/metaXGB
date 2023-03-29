#' @title Get Train and Test tasks
#' @description Downloads task and its resampling structure from OpenML.
#' Use first train and test fold of the 10-fold CV as train, test task respectively.
#' @details requires internet connection.
#' @param task_id `integer(1)` task id
#' @return `list()`containing train and test tasks.
#' @export
get_train_test_tasks = function(task_id) {
  if (!curl::has_internet()) {stop("Internet Connection not detected")}
  assertInt(task_id, lower = 0)

  catf("Note: This Function uses only the first of the 10 CV train & test splits\n")
  #get task
  task = tsk("oml", task_id = task_id)
  #get resampling splits
  resampling = rsmp("oml", task_id = task_id)

  # we only use the first of the 10 CV folds
  train_idx = resampling$train_set(1L)
  test_idx = resampling$test_set(1L)
  train_task = task$clone()$filter(rows = train_idx)
  test_task = task$clone()$filter(rows = test_idx)

  # Impute missing values
  preprocessor = po("imputemode", affect_columns = selector_type(c("logical", "character", "factor", "ordered"))) %>>%
    po("imputemedian", affect_columns = selector_type(c("numeric", "integer"))) %>>%
    po("encode")
  train_task = preprocessor$train(train_task)[[1L]]
  test_task = preprocessor$predict(test_task)[[1L]]

  list(train_task = train_task, test_task = test_task)
}
