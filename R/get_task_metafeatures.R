#' @title Get meta features from OpenML
#' @description Downloads meta_features from OpenML.org for given task_id.
#' @details requires internet connection.
#' @param task_id `integer(1)` task id
#' @param meta_feature_names `character()` names of task meta_features
#' @return `data.table()` of task meta_features.
#' @export
get_task_metafeatures = function(task_id, meta_feature_names = meta_feature_names) {
  assertInt(task_id, lower = 1)
  assertCharacter(meta_feature_names, any.missing = FALSE)
  if (!curl::has_internet()) {stop("Internet Connection not detected")}

  meta_features = mlr3oml::list_oml_tasks(task_id)
  avaliable = meta_feature_names[meta_feature_names %in% names(meta_features)]
  res = meta_features[, ..avaliable]
  #workaround for the case that specific meta-feature is not available for a given task.
  not_avaliable = meta_feature_names[!meta_feature_names %in% names(meta_features)]
  if (length(not_avaliable)) {
    not_avaliable_dt = data.table(rep(NA_real_, length(not_avaliable)))
    names(not_avaliable_dt) = not_avaliable
    res = cbind(res, not_avaliable_dt)
  }
  res
}
