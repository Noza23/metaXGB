#' @title Set up Project directory and Download data
#' @details file to be downloaded is 148 MB large, so in case your internet connection is very slow, set timeout to a large value since
#' after "timeout" seconds function exits with a timeout error.
#' @param dir `character(1)` path where, project directory should be set up
#' @param timeout `numeric(1)` function timeout in seconds
#' @export
setup_project = function(dir, timeout = 300) {
  if (dir == "Directory where the Project should be set up") return(invisible(NULL))
  assertString(dir)
  assertNumber(timeout)
  catf("[INFO] Creating Project directory metaXGB in %s\n", dir)
  Sys.sleep(1)
  dir.create(sprintf("%s/metaXGB", dir))

  catf("[INFO] Changing working directory to project directory metaXGB\n")
  Sys.sleep(1)
  setwd(sprintf("%s/metaXGB", dir))

  catf("[INFO] Creating subdirectories 'data' and 'plots'\n")
  Sys.sleep(1)
  dir.create("data")
  dir.create("data/external")
  dir.create("data/results")
  dir.create("data/results/final_performance")
  dir.create("data/results/final_performance/meta")
  dir.create("data/results/final_performance/default")
  dir.create("data/results/meta_configs")
  dir.create("data/results/meta_learners")
  dir.create("data/results/meta_learners/classifier")
  dir.create("data/results/meta_learners/perf_estimator")
  dir.create("plots")
  dir.create("plots/similarity")
  dir.create("plots/CP")
  dir.create("plots/auc_dist")
  dir.create("plots/impurity")

  catf("[INFO] Downloading task meta_features and xgboost_meta_data in directory data/results'\n")
  options(timeout = timeout)

  # Case data has already been downloaded
  if (
    all(
      sapply(c("features.csv", "xgboost_meta_data.csv"), function(x) file.exists(sprintf("data/external/%s", x)))
    )
  ) {
    catf("[INFO] Data has already been downloaded in the directory data/external")
    return(invisible(NULL))
  }

  link = "https://syncandshare.lrz.de/dl/fiV9MfvupyNzWpT99M5RhFh2/.dir"
  utils::download.file(link, destfile = "data/external/external_data.zip", mode = "wb")

  catf("[INFO] Unziping data ...'\n")
  utils::unzip("data/external/external_data.zip", exdir = "data/external/")

  catf("\n[INFO] Download main.R script from Github https://github.com/Noza23/metaBoost/paper \n")
  utils::download.file("https://github.com/Noza23/metaBoost/raw/main/paper/main.R", destfile = "main.R")

  cat("\n[INFO] Setting up the Project has been completed!\n")
  cat("\n[INFO] Follow the R script 'main.R' located in project directory metaXGB!\n")
  # Setting back timeout option!
  options(timeout = 60)

  invisible(NULL)
}




