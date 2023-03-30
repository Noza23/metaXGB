# Setting working directory to directory where the script is located.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Reading meta_features.
features = data.table::fread("data/external/features.csv")
# Reading xgboost_meta_data.
meta_data = metaBoost::read_meta_data(file = "data/external/xgboost_meta_data.csv")
# Get test_ids used in the paper.
test_ids = metaBoost::test_ids

# Run Baseline on Default Configuration {test_ids}
# Note: tasks with task_ids c(146825, 168332) can take up to 12 hours each, so in case of fast analysis exclude them from test_ids.
result_default = lapply(
  test_ids,
  FUN = metaBoost::run_config,
  seed = 111,
  defult = TRUE
)

# Meta-Learn configuration and run in on {test_ids}: Meta-Learning takes about 15 min for each task_id.
# Note: tasks with task_ids c(146825, 168332) can take up to 12 hours each, so in case of fast analysis exclude them from test_ids.
result_meta = lapply(
  metaBoost::test_ids,
  FUN = metaBoost::run_config,
  seed = 111,
  meta_config = TRUE,
  meta_features = features
)

# Compare results
metaBoost::compare_results()
