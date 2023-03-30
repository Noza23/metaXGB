#' @title Cosine Similarity between tasks
#' @description Using meta-features of different tasks, first mutate meta_features as described in
#' \code{\link{mutate_metafeatures}}
#' and then compute cosine similarities on them to select 3 most similar tasks for given task_id.
#' @param task_id `integer(1)` task identifier on OpenML
#' @param meta_features `data.table()` task_meta_features
#' @param meta_feature_names `character()` meta_feature names
#' @param meta_data `data.table()` xgboost_meta_data
#' @return `list()` containing data_ids of 3 most similar tasks, class identifier and their meta_features.
#' @export
find_similar_tasks = function(task_id, meta_features, meta_feature_names, meta_data) {

  assertInt(task_id, lower = 0)
  assertDataTable(meta_fetures)
  assertCharacter(meta_feature_names, any.missing = FALSE)
  assertDataTable(meta_data)

  # Get new task meta-features.
  new_meta_features = get_task_metafeatures(task_id, meta_feature_names)
  # Mutate meta-features
  new_meta_features_mutated = mutate_metafeatures(new_meta_features)
  meta_features_mutated = mutate_metafeatures(meta_features)

  # Keep only data_ids that achieve at least for once over 90% accuracy
  selected_data_ids = meta_data[, .(max.auc = max(auc)), by = "data_id"][max.auc > 0.8, data_id]
  meta_features_mutated = meta_features_mutated[data_id %in% selected_data_ids]
  # Impute missing values
  imp_num = po(
    "imputelearner",
    learner = lrn("regr.rpart"),
    affect_columns = selector_type(c("numeric", "integer")),
    id = "impute_missing"
  )
  meta_features_mutated = imp_num$train(list(mlr3cluster::as_task_clust(meta_features_mutated)))[[1]]$data()
  new_meta_features_mutated = imp_num$predict(list(mlr3cluster::as_task_clust(new_meta_features_mutated)))[[1]]$data()
  setcolorder(new_meta_features_mutated, names(meta_features_mutated))

  # Similarity function (cosine similarity)
  cos_sim = function(x, y) sum(x %*% y) / sqrt(sum(x^2) %*% sum(y^2))

  similarities = apply(
    meta_features_mutated[, - c("data_id")],
    MARGIN = 1,
    cos_sim,
    as.numeric(new_meta_features_mutated[, - c("data_id")])
  )
  # Choose at most 3 similar data sets in max similarity diviance 0.05
  data_ids = meta_features_mutated$data_id[order(rank(-similarities))]
  similarities_chosen = round(
    stats::na.omit(sort(similarities[similarities > (max(similarities) - 0.05)], decreasing = TRUE)[1:3]),
    digits = 3
  )

  # Similarity plot
  sim_plot = ggplot2::ggplot(
    data = data.frame(data_id = factor(data_ids, levels = data_ids), cos_similarity = sort(similarities, T))
  )  +
    ggplot2::geom_col(aes(x = data_id, y = cos_similarity)) +
    ggplot2::theme_classic() + ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  fn = sprintf("plots/similarity/similarity_%s.pdf", task_id)
  catf("[INFO] Saving similarity plot in %s\n", fn)
  # Saving plot
  ggplot2::ggsave(sim_plot, filename = fn)

  n = length(similarities_chosen)
  data_ids = data_ids[seq_len(n)]

  catf(
    "\n[INFO] Tasks with following data_ids: %s\n have been identified as most similar with following cos similarity scores:\n%s\n ",
    paste0(data_ids, collapse = ", "),
    paste0(similarities_chosen, collapse = ", ")
  )
  # Return corresponding data_ids
  list(
    data_ids = data_ids,
    class = paste0(data_ids, collapse = "_"),
    task_meta_features = meta_features_mutated[data_id %in% data_ids],
    new_task_meta_features = new_meta_features_mutated
  )
}
