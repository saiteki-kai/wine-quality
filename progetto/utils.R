#' Convert the quality attribute based on the configuration
#' @param dataset a dataset
#' @param config the followings:
#' (config = 1) -> (0 -> bad-quality, 1 -> good-quality)
#' (config = 2) -> (0 -> low-quality, 1 -> medium-quality, 2 -> high-quality)
#' (config = 3) -> (0 -> low-quality, ..., 10 -> high-quality levels of quality)
#' @return the processed dataset
preprocess_dataset <- function(dataset, config) {
  dataset$type <- NULL

  if (config == 1) {
    dataset$quality <- ifelse(dataset$quality > 6, 0, 1)
    dataset$quality <- factor(dataset$quality, levels = c(0, 1)) # labels = c("bad", "good")
  } else if (config == 2) {
    dataset$quality <- ifelse(dataset$quality <= 5, 0, ifelse(dataset$quality <= 7, 1, 2))
    dataset$quality <- factor(dataset$quality, levels = c(0, 1, 2)) # labels = c("low", "medium", "high")
  }
  dataset
}

#' Patition the dataset based on the class attribute
#'
#' @param dataset a dataset
#' @return the partition composed of train and test
partition_dataset <- function(dataset) {
  index <- createDataPartition(dataset$quality, p = 0.75, list = FALSE)
  train <- dataset[index,]
  test <- dataset[-index,]

  list(train = train, test = test)
}

#' Normalize the dataset using z-score normalization
#'
#' @param dataset a dataset
#' @return the normalized dataset
normalize_dataset <- function(dataset) {
  scaled <- scale(dataset[names(dataset) != "quality"])
  scaled <- as.data.frame(scaled)

  scaled$quality <- dataset$quality
  scaled
}

#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
evaluate_model <- function(model, dataset) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(rjson)

  # Predict
  start_time <- Sys.time()
  pred <- predict(model, dataset[names(dataset) != "quality"])
  end_time <- Sys.time()
  time <- end_time - start_time

  # ROC and PRC
  #sscurves <- evalmod(scores = as.numeric(pred), labels = combined$test$quality)
  #autoplot(sscurves)
  #auc_ci <- auc_ci(sscurves)
  #knitr::kable(auc_ci)

  # Print confusion matrix
  cm <- confusionMatrix(data = pred, reference = dataset$quality, mode = 'prec_recall')

  measures <- data.frame(
    accuracy = cm$overall["Accuracy"],
    f1 = cm$byClass["F1"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"]
  )

 out <- list(cm = cm, measures = measures, pred_time = time)
}

#' Combine the red and the white datasets and add type attribute.
#' Save the result as a csv file
#'
#' @return the combined dataset
combine_redwhite <- function() {
  redwine <- read.csv("./dataset/winequality-red.csv")
  whitewine <- read.csv("./dataset/winequality-white.csv")

  redwine$type <- "red"
  whitewine$type <- "white"

  wines <- rbind(redwine, whitewine)

  # Save the combined dataset
  write.csv(wines, "./dataset/winequality-combined.csv", row.names = FALSE)

  wines
}

#' Plot ROC and precision_recall_curve for all the models specified
#' @param dataset a dataset
#' @param models a list of models
#'
#' @return AUCs for ROC and PRC for all the models
plot_roc_and_prc_all <- function(dataset, models) {
  nb_pred <- as.numeric(predict(models$nb_model, dataset[names(dataset) != "quality"]))
  dt_pred <- as.numeric(predict(models$dt_model, dataset[names(dataset) != "quality"]))
  svm_pred <- as.numeric(predict(models$svm_model, dataset[names(dataset) != "quality"]))
  nn_pred <- as.numeric(predict(models$nn_model, dataset[names(dataset) != "quality"]))

  scores <- join_scores(nb_pred, dt_pred, svm_pred, nn_pred)
  labels <- join_labels(dataset$quality, dataset$quality, dataset$quality, dataset$quality)
  mmmdat <- mmdata(scores, labels, modnames = c("nb", "dt", "svm", "nn"), dsids = c(1, 2, 3, 4))
  sscurves <- evalmod(mmmdat) # mode = 'aucroc'

  # Plot ROC and PRC
  autoplot(sscurves)

  # Calculate CI of AUCs with normal distibution
  auc_ci <- auc_ci(sscurves)

  # Use knitr::kable to display the result in a table format
  knitr::kable(auc_ci)
}

#' Write a log file
#' @param model_name model's name
#' @param measures model's measures after training
#' @param train_time training time
#' @param pred_time prediction time
#'
write_log <- function (model_name, measures, train_time, pred_time) {
  file <- file.path("./results", paste0(model_name, "_.log"))
  write.table(paste("model_name: ", model_name), file, row.names = FALSE, col.names = FALSE)
  write.table(paste("pred_time: ", pred_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("train_time: ", train_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table("metrics: ", file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("accuracy: ", measures$accuracy), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("f1: ", measures$f1), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("precision: ", measures$precision), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("recall: ", measures$recall), file, row.names = FALSE, col.names = FALSE, append = TRUE)
}
