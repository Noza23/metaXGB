#' @import checkmate
#' @import data.table
#' @import mlr3
#' @import mlr3learners
#' @import mlr3pipelines

# @import curl
# @import e1071
# @import future
# @import mlr3oml
# @import mlr3misc

# Task-ids used in the Analysis
#' @export
test_ids = c(
  16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970,
  146212, 146825, 167119, 167125, 168332, 168336
)
# Names of the meta-features used in Analysis
#' @export
meta_feature_names = c(
  "data_id", "name", "status", "MajorityClassSize", "MaxNominalAttDistinctValues","MinorityClassSize",
  "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances","NumberOfInstancesWithMissingValues",
  "NumberOfMissingValues", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"
)
# Default HP Configuration
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
