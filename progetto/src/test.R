#' Test
#'

.get_model <- function(model_name, pre_proc_method) {
  filename <- file.path("../output", paste0(model_name, "_", pre_proc_method, ".RDS"))

  if (file.exists(filename)) {
    return(readRDS(filename))
  }
}

#' Write a log file
#' @param model_name model's name
#' @param cm a confusion matrix
#' @param pred_time prediction time
#'
.write_log <- function(model_name, pre_proc_name, cm, pred_time) {
  filename <- file.path(
    "../output",
    paste0(model_name, "_", pre_proc_name, ".log")
  )

  write.table(
    paste0("model_name: ", model_name),
    filename,
    row.names = FALSE,
    col.names = FALSE
  )
  write.table(
    paste0("pred_time: ", pred_time),
    filename,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
  write.table(
    capture.output(cm),
    filename,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
}

#' Plot ROC and precision_recall_curve for all the models specified
#' @param dataset a dataset
#' @param models a list of models
#'
#' @return AUCs for ROC and PRC for all the models
.plot_roc_and_prc_all <- function(labels, probs, method) {
  library(pROC)

  # Create dataframe for roc
  df <- as.data.frame(probs)
  df$target <- labels

  roc.list <- roc(target ~ ., data = df, levels = levels(labels), ci = TRUE)
  auc_res <- lapply(roc.list, function(roc) list(ci = roc$ci, auc = roc$auc))

  p <- ggroc(roc.list, legacy.axes = T) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#aaaaaa") +
    ggtitle(method) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = "bottom"
    )

  list(plot = p, auc = auc_res)
}

#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
#'
#' @return list containing the confusion matrix, the predictions
#'      and the prediction time
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
  pred <- predict(model, newdata = transformed)
  end_time <- Sys.time()
  time <- end_time - start_time

  # Print confusion matrix
  cm <- confusionMatrix(
    data = pred,
    reference = dataset$quality,
    mode = "prec_recall",
    positive = "good"
  )

  list(cm = cm, probs = probs, pred_time = time)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, ggplot2, grid, pROC, dplyr)

# Local functions
source("utils.R")

# Prepare the dataset
testset <- read.csv("../data/winequality-test.csv")
testset$quality <- factor(testset$quality)

for (method in c("pca", "z-score", "min-max")) {
  # Load models
  rpart.model <- .get_model("rpart2", method)
  svmRadial.model <- .get_model("svmRadial", method)
  svmLinear.model <- .get_model("svmLinear", method)
  svmPoly.model <- .get_model("svmPoly", method)

  # Evaluate the model
  res1 <- .evaluate_model(rpart.model, testset)
  res2 <- .evaluate_model(svmRadial.model, testset)
  res3 <- .evaluate_model(svmLinear.model, testset)
  res4 <- .evaluate_model(svmPoly.model, testset)

  # Write Logs
  .write_log("rpart2", method, res1$cm, res1$pred_time)
  .write_log("svmRadial", method, res2$cm, res2$pred_time)
  .write_log("svmLinear", method, res3$cm, res3$pred_time)
  .write_log("svmPoly", method, res4$cm, res4$pred_time)

  predictions <- list(
    rpart2 = res1$probs$good,
    svmRadial = res2$probs$good,
    svmLinear = res3$probs$good,
    svmPoly = res4$probs$good
  )

  # Plot AUCs ROC & PRC
  roc_prc <- .plot_roc_and_prc_all(testset$quality, predictions, method)

  print_or_save(roc_prc$plot,
    file.path("../plots/roc", paste0(method, ".png")),
    save = TRUE,
    wide = FALSE
  )

  #print(roc_prc$auc)

  # Model Comparison
  cv.values <- resamples(
    list(
      rpart2 = rpart.model,
      svmRadial = svmRadial.model,
      svmLinear = svmLinear.model,
      svmPoly = svmPoly.model
    )
  )

  #summary(cv.values)
  print_or_save(dotplot(cv.values, metric = "ROC"),
    "../plots/comparison/dotplot.png",
    save = TRUE,
    wide = TRUE
  )
  print_or_save(bwplot(cv.values, layout = c(3, 1)),
    "../plots/comparison/bwplot.png",
    save = TRUE,
    wide = TRUE
  )
  print_or_save(splom(cv.values, metric = "ROC"),
    "../plots/comparison/splom.png",
    save = TRUE,
    wide = TRUE
  )
}

# TODO: save plots and divide subsampled results from non subsampled
# TODO: test statistici
