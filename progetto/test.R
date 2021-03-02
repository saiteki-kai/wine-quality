 .get_model <- function(name) {
  filename <- file.path("./results/models", paste0(name, "_model.RDS"))
  if (file.exists(filename)) {
    readRDS(filename)
  }
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, mlbench, dplyr)

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/svm.R")

# Prepare the dataset
testset <- read.csv('./dataset/winequality-test.csv')
testset$quality <- factor(testset$quality)

# load models
obj1 <- .get_model("dt")
obj2 <- .get_model("svm")

# Evaluate the model
res1 <- evaluate_model(obj1$model, testset, obj1$pre_proc)
res2 <- evaluate_model(obj2$model, testset, obj2$pre_proc)

# Write Logs
write_log("dt", res1$cm, res1$pred_time)
write_log("svm", res2$cm, res2$pred_time)

# Plot AUCs ROC & PRC
predictions <- list(dt_pred = res1$pred, svm_pred = res2$pred)
plot_roc_and_prc_all(testset, predictions)


