#'
#'
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, mlbench, dplyr)

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/nb.R")
source("models/svm.R")
source("models/nn.R")

.train <- function(scale_method, pca = FALSE) {
  # Prepare the dataset
  dataset <- read.csv("./dataset/winequality-train.csv")
  dataset <- lapply(dataset, function(x) treat_outliers(x, method = "IQR"))

  if (pca) {
    dataset <- feature_selection_pca(dataset)
  } else {
    dataset <- normalize_dataset(dataset, method = scale_method)
  }

  # Register parallel processing
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)

  # Train the models
  m1 <- nb_classification(dataset)
  #m2 <- dt_classification(dataset)
  m3 <- svm_classification(dataset)
  #m4 <- nn_classification(dataset)

  # Stop using parallel computing
  stopCluster(cluster)

  # Write Logs
  # write_log("nb", res1$cm, m1$train_time, res1$pred_time)
  # write_log("dt", res2$cm, m2$train_time, res2$pred_time)
  # write_log("svm", res3$cm, m3$train_time, res3$pred_time)
  # write_log("nn", res4$cm, m4$train_time, res4$pred_time)
}

scale_method <- "z_score" #"min_max"
pca <- TRUE

.train(scale_method = scale_method, pca = pca)
