#' Test
#'

.get_model <- function(model_name, pre_proc_method) {
  filename <- file.path("../output", paste0(model_name, "_", pre_proc_method, ".RDS"))
  if (file.exists(filename)) {
    readRDS(filename)
  }
}

 #' Write a log file
#' @param model_name model's name
#' @param cm a confusion matrix
#' @param pred_time prediction time
#'
.write_log <- function(model_name, pre_proc_name, cm, pred_time) {
  file <- file.path("../output", paste0(model_name, "_", pre_proc_name, ".log"))
  write.table(paste("model_name: ", model_name), file, row.names = FALSE, col.names = FALSE)
  write.table(paste("pred_time: ", pred_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(capture.output(cm), file, row.names = FALSE, col.names = FALSE, append = TRUE)
}

#' Plot ROC and precision_recall_curve for all the models specified
#' @param dataset a dataset
#' @param models a list of models
#'
#' @return AUCs for ROC and PRC for all the models
.plot_roc_and_prc_all <- function(dataset, predictions) {
  #nb_pred <- as.numeric(predictions$nb_pred)
  dt_pred <- as.numeric(predictions$dt_pred)
  svm_pred <- as.numeric(predictions$svm_pred)
  #nn_pred <- as.numeric(predictions$nn_pred)

  scores <- join_scores(dt_pred, svm_pred)
  y <- as.numeric(dataset$quality)
  labels <- join_labels(y, y)
  mmmdat <- mmdata(scores, labels, modnames = c("nb", "svm"), dsids = c(1, 2))
  res <- evalmod(mmmdat) # mode = 'aucroc'

  # Calculate CI of AUCs with normal distibution
  auc_ci <- auc_ci(res)

  # Use knitr::kable to display the result in a table format
  knitr::kable(auc_ci)

  # Plot ROC and PRC
  autoplot(res)

  return(res)
}

#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
#'
#' @return list containing the confusion matrix, the predictions and the prediction time
.evaluate_model <- function(model, dataset, pre_proc) {

  # Apply transformations
  if (length(pre_proc) != 0) {
    cols <- ncol(dataset)
    transformed <- predict(pre_proc, dataset[, -cols])
    transformed$quality <- dataset$quality
  } else {
    transformed <- dataset
  }

  # Predict
  start_time <- Sys.time()
  pred <- predict(model, newdata = transformed)
  end_time <- Sys.time()
  time <- end_time - start_time

  # ROC and PRC
  #sscurves <- evalmod(scores = as.numeric(pred), labels = combined$test$quality)
  #autoplot(sscurves)
  #auc_ci <- auc_ci(sscurves)
  #knitr::kable(auc_ci)

  # Print confusion matrix
  cm <- confusionMatrix(data = pred, reference = dataset$quality, mode = 'prec_recall', positive = "good")

  out <- list(cm = cm, pred = pred, pred_time = time)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, mlbench, dplyr)

# Local functions
source("utils.R")

# Prepare the dataset
testset <- read.csv('../data/winequality-test.csv')
testset$quality <- factor(testset$quality)

for (method in c("pca", "z-score")) {
  # Load models
  obj1 <- .get_model("rpart2", method)
  obj2 <- .get_model("svmRadial", method)

  # Evaluate the model
  res1 <- .evaluate_model(obj1$model, testset, obj1$pre_proc)
  res2 <- .evaluate_model(obj2$model, testset, obj2$pre_proc)

  # Write Logs
  .write_log("rpart2", method, res1$cm, res1$pred_time)
  .write_log("svmRadial", method, res2$cm, res2$pred_time)

  # Plot AUCs ROC & PRC
  predictions <- list(dt_pred = res1$pred, svm_pred = res2$pred)
  .plot_roc_and_prc_all(testset, predictions)
}
