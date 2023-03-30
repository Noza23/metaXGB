#' @title Mutate meta_features
#'
#' @description Mutates meta_features to make them comparable across different tasks:\cr
#' \tabular{cc}{
#'   \strong{Name} \tab \strong{Fromula} \cr
#'   MinorityMajorityClassRatio \tab \eqn{MinorityClassSize / MajorityClassSize}\cr
#'   MaxNominalAttInstanceRatio \tab \eqn{MaxNominalAttDistinctValues / NumberOfInstances}\cr
#'   NumberOfClasses \tab \eqn{NumberOfClasses}\cr
#'   NumberOfFeatures\tab \eqn{NumberOfFeatures}\cr
#'   MissingValueSizeOfFeatureMatrixRatio \tab \deqn{NumberOfMissingValues / (NumberOfFeatures * NumberOfInstances)}\cr
#'   NumberOfInstancesWithMissingVlauesRatio \tab \deqn{NumberOfInstancesWithMissingValues / NumberOfInstances}
#' }
#' @param features `data_table()` data.table of meta-features
#' @return `data.table()` containing mutated meta_features.
#' @export
mutate_metafeatures = function(features) {
  meta_feature_names = c(
    "data_id", "MajorityClassSize", "MaxNominalAttDistinctValues","MinorityClassSize", "NumberOfClasses",
    "NumberOfFeatures", "NumberOfInstances","NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures","NumberOfSymbolicFeatures"
  )
  assertDataTable(features)
  assertTRUE(all(meta_feature_names %in% names(features)))

  features[, .(
    data_id,
    "MinorityMajorityClassRatio" = round(MinorityClassSize / MajorityClassSize, 4),
    "MaxNominalAttInstanceRatio" = round(MaxNominalAttDistinctValues / NumberOfInstances, 4),
    NumberOfClasses =  NumberOfClasses,
    NumberOfFeatures = NumberOfFeatures,
    "SymbolicFeaturesRatio" = round(NumberOfSymbolicFeatures / NumberOfFeatures, 4),
    "NumberOfInstancesTsd" = NumberOfInstances / 1000,
    "MissingValueSizeOfFeatureMatrixRatio" = round(NumberOfMissingValues / (NumberOfFeatures * NumberOfInstances), 4),
    "NumberOfInstancesWithMissingVlauesRatio" =  round(NumberOfInstancesWithMissingValues / NumberOfInstances, 4)
  )]
}
