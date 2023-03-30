#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3learners
#' @import mlr3pipelines
#' @import mlr3misc
#' @import mlr3oml
#' @import ggplot2

#' @title  Test IDs
#' @description test_ids refers to task_ids from OpenML.org used in Analysis.
#' @export
test_ids = c(
  16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970,
  146212, 146825, 167119, 167125, 168332, 168336
)

#' @title  Meta_Feature Names
#' @description Names of the meta_features used in Analysis.
#' @export
meta_feature_names = c(
  "data_id", "name", "status", "MajorityClassSize", "MaxNominalAttDistinctValues","MinorityClassSize",
  "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances","NumberOfInstancesWithMissingValues",
  "NumberOfMissingValues", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"
)


#' @title  Default Configuration
#' @description Default HyperParameter Configuration for xgboost classifier.
#' @export
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
