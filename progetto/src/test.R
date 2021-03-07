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
.plot_roc_and_prc_all <- function(reference, prediction, names, method) {

  scores <- join_scores(prediction)

  labels <- rep(list(reference), length(names))
  labels <- join_labels(labels)

  mmmdat <- mmdata(scores, labels, modnames = names, dsids = as.numeric(as.factor(names)))
  res <- evalmod(mmmdat, mode = "rocprc")

  # Plot ROC and PRC
  autoplot(res) + title(method)

  # Calculate CI of AUCs with normal distibution
  auc_ci <- auc_ci(res)

  # Use knitr::kable to display the result in a table format
  print(knitr::kable(auc_ci))

  print(res)

  return(res)
}

#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
#'
#' @return list containing the confusion matrix, the predictions and the prediction time
.evaluate_model <- function(model, dataset) {

  # Apply transformations
  if (length(model$preProcess) != 0) {
    cols <- ncol(dataset)
    transformed <- predict(model$preProcess, dataset[, -cols])
    transformed$quality <- dataset$quality
    model$preProcess <- NULL
  } else {
    transformed <- dataset
  }

  # Predict
  start_time <- Sys.time()
  probs <- predict(model, newdata = transformed, type = "prob")
  pred <- factor(ifelse(probs$good >= 0.5, "good", "bad")) # TODO: CHECK
  end_time <- Sys.time()
  time <- end_time - start_time

  # Print confusion matrix
  cm <- confusionMatrix(data = pred, reference = dataset$quality, mode = 'prec_recall', positive = "good")

  out <- list(cm = cm, pred = probs, pred_time = time)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, mlbench, dplyr)

# Local functions
source("utils.R")

# Prepare the dataset
testset <- read.csv('../data/winequality-test.csv')
testset$quality <- factor(testset$quality)

for (method in c("pca", "z-score", "min-max")) {
  # Load models
  rpart.model <- .get_model("rpart2", method)
  svmRadial.model <- .get_model("svmRadial", method)
  svmLinear.model <- .get_model("svmLinear", method)

  # Evaluate the model
  res1 <- .evaluate_model(rpart.model, testset)
  res2 <- .evaluate_model(svmRadial.model, testset)
  res3 <- .evaluate_model(svmLinear.model, testset)

  # Write Logs
  .write_log("rpart2", method, res1$cm, res1$pred_time)
  .write_log("svmRadial", method, res2$cm, res2$pred_time)
  .write_log("svmLinear", method, res3$cm, res3$pred_time)

  predictions <- list(pred1 = res1$pred$good, pred2 = res2$pred$good, pred3 = res3$pred$good)

  print(paste0("Method: ", method))

  # Plot AUCs ROC & PRC
  .plot_roc_and_prc_all(testset$quality, predictions, c("rpart2", "svmRadial", "svmLinear"), method)

  # Model Comparison
  cv.values <- resamples(list(rpart2 = rpart.model, svmRadial = svmRadial.model, svmLinear = svmLinear.model))
  summary(cv.values)
  print(dotplot(cv.values, metric = "ROC"))
  print(bwplot(cv.values, layout = c(3, 1)))
  print(splom(cv.values, metric = "ROC"))
}

# TODO: save plots and divide subsampled results from non subsampled
# TODO: save tuning plot(model)
# TODO: test statistici

