# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, precrec, factoextra, multiROC, ggplot2, dplyr)

#' Convert the quality attribute based on the configuration
#' @param dataset a dataset
#' @param config indicates the class configuration to be used
#'
#' @details
#' config = 1  -> 2 classes (0: bad-quality, 1: good-quality)
#' config = 2  -> 3 classes (0: low-quality, 1: medium-quality, 2: high-quality)
#' config = 3 (default) -> 10 classes (0: low-quality, ..., 10: high-quality)
#' @return the processed dataset
preprocess_dataset <- function(dataset, config = 1) {
  #dataset$type <- factor(dataset$type)

  if (config == 1) {
    dataset$quality <- ifelse(dataset$quality > 6, 1, 0)
    dataset$quality <- factor(dataset$quality, levels = c(0, 1)) # labels = c("bad", "good")
  } else if (config == 2) {
    dataset$quality <- ifelse(dataset$quality <= 5, 0, ifelse(dataset$quality < 7, 1, 2))
    dataset$quality <- factor(dataset$quality, levels = c(0, 1, 2)) # labels = c("low", "medium", "high")
  }

  dataset
}

#' Partion the dataset based on the class attribute
#'
#' @param dataset a dataset
#' @return the partition composed of train and test
partition_dataset <- function(dataset) {
  index <- createDataPartition(dataset$quality, p = 0.75, list = FALSE)
  train <- dataset[index,]
  test <- dataset[-index,]

  list(train = train, test = test)
}


#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
evaluate_model <- function(model, dataset, pre_proc) {

  # Apply transformations
  if(length(pre_proc) != 0) {
    cols <- ncol(dataset)
    transformed <- predict(pre_proc, dataset[, -cols])
    transformed$quality <- dataset$quality
  } else {
    transformed <- dataset
  }

  # Predict
  start_time <- Sys.time()
  pred <- predict(model, transformed)
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
plot_roc_and_prc_all <- function(dataset, predictions) {

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

#' Write a log file
#' @param model_name model's name
#' @param cm a confusion matrix
#' @param pred_time prediction time
#'
write_log <- function(model_name, cm, pred_time) {
  file <- file.path("./results/models", paste0(model_name, "_.log"))
  write.table(paste("model_name: ", model_name), file, row.names = FALSE, col.names = FALSE)
  write.table(paste("pred_time: ", pred_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(capture.output(cm), file, row.names = FALSE, col.names = FALSE, append = TRUE)
}

#' Detect outliers using the Interquartile Range (IQR) approach or Winsorinzing (Percentile Capping).
#' All points that lie outside the upper limit or below the lower limit can be considered outliers.
#'
#' @param data numeric array
#' @param method method of outliers detection
#' @param win.quantiles quantile limits for the winsorinzing method
#'
#' @details
#' Method IQR:
#'
#' IQR = 75th quantile - 25th quantile
#' Lower Limit = 25th quantile - 1.5 * IQR
#' Upper Limit = 75th quantile + 1.5 * IQR
#'
#' Method Winsorizing:
#'
#' Lower Limit =  5th quantile
#' Upper Limit = 95th quantile
#'
#' @return a list containing the lower and upper limits
detect_outliers <- function(data, method = "winsorizing", win.quantiles = c(0.05, 0.95)) {
  if (method == "winsorizing") {
    quantiles <- quantile(data, win.quantiles, names = FALSE)
    lower <- quantiles[1]
    upper <- quantiles[2]
  } else if (method == "IQR") {
    quantiles <- quantile(data, c(0.25, 0.75), names = FALSE)
    lower <- quantiles[1] - 1.5 * IQR(data)
    upper <- quantiles[2] + 1.5 * IQR(data)
  } else {
    stop("`mode` should be either `IQR` or `winsorizing`")
  }

  list(lower = lower, upper = upper)
}

#' Treat the outliers using different methods
#'
#' @param dataset a dataset
#' @param method method of outliers detection
#' @param win.quantiles quantile limits for the winsorinzing method
#'
#' @return the dataset without outliers
treat_outliers <- function(data, method = "winsorizing", win.quantiles = c(0.05, 0.95), outlier.rm = FALSE) {
  if (method == "winsorizing") {
    bounds <- detect_outliers(data, win.quantiles = win.quantiles)
    data[data < bounds[1]] <- bounds$lower
    data[data > bounds[2]] <- bounds$upper
  } else if (method == "IQR") {
    bounds <- detect_outliers(data, method = "IQR")
    data[data < bounds$lower | data > bounds$upper] <- ifelse(outlier.rm, NA, median(data))
  } else {
    stop("`mode` should be either `IQR` or `winsorizing`")
  }
  data
}

save_plot_png <- function(filename, plot, wide = FALSE) {
  ggsave(filename, plot = plot, device = "png", height = 6.67, width = ifelse(wide, 13.34, 6.67))
}

create_dataset <- function(dataset) {
  partition <- dataset %>%
    mutate(type = NULL) %>%
    preprocess_dataset(1) %>%
    partition_dataset()

  write.csv(partition$train, "./dataset/winequality-train.csv", row.names = FALSE)
  write.csv(partition$test, "./dataset/winequality-test.csv", row.names = FALSE)
}

subsempling <- function(trainset, method) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
    pacman::p_load(DMwR, ROSE)

  set.seed(9560)
  res <- NULL

  if (method == "down") {
    res <- downSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
    names(res)[names(res) == "Class"] <- "quality"
  }
  else if (method == "up") {
    res <- upSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
    names(res)[names(res) == "Class"] <- "quality"
  }
  else if (method == "SMOTE") {
   res <- SMOTE(quality ~ ., trainset, perc.over = 600,perc.under=100)
  } else {
    res <- ROSE(quality ~ ., data  = trainset)$data
  }
}

parallelTrain <- function (trainset, train_control, pre_proc, train_func) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
    pacman::p_load(doParallel)

  # Register parallel processing
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)

  # Train the model
  model <- train_func(trainset, train_control, pre_proc)

  # Stop using parallel computing
  stopCluster(cluster)

  model
}