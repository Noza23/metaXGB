#' @title Interpret Performance Estimator
#' @description Predictions are interpreted using simple Feature Impurity bar plots and
#' Ceteris Paribus Plots from DALEX package.
#' @param model (`Learner(1)`) model extracted from mlr3 learner
#' @param learner (`Learner(1)`) mlr3 learner
#' @param task (`Task(1)`) mlr3 task
#' @param new_data (`data.table(1)`) Single test data point for Ceteris Paribus Plots
#' @param class (`character(1)`) identifier for group of similar tasks
#' @return invisble `list()` containing plots.
#' @export
interpret_model = function(model, learner, task, new_data, class) {
  assert_learner(learner)
  assert_task(task)
  assertDataTable(new_data, nrows = 1)
  assertString(class)

  # Feature Impurity bar plot
  grDevices::pdf(file = sprintf("plots/impurity_PerfEst_%s.pdf", class), width = 30)
  graphics::barplot(learner$importance())
  grDevices::dev.off()
  catf(
    "\n[INFO] Feature Impurity plot for Perf Estimator has been saved under plots/impurity_PerfEst_%s.pdf\n",
    class
  )
  Sys.sleep(2)

  # Ceteris Paribus Plots
  learner_explain = DALEX::explain(
    model = model,
    data = task$data()[, - c("auc")],
    y = task$truth()
  )
  cp = suppressWarnings(predict_profile(learner_explain, new_data))
  cp_DALEX = plot(cp) + ggplot2::ggtitle("Ceteris paribus for new prediction", " ")
  ggplot2::ggsave(
    cp_DALEX,
    filename = sprintf("plots/CP_%s.pdf", class),
    height = 15,
    width = 20
  )
  catf(
    "\n[INFO] Ceteris Paribus plot for Perf Estimator has been saved under plots/CP_%s.pdf\n",
    class
  )
  Sys.sleep(2)

  catf("\n[INFO] See interpretation plots in directory plots")
  invisible(list(CP = cp_DALEX))
}
