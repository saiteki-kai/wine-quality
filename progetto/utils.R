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
preprocess_dataset <- function(dataset, config = 3) {
  #dataset$type <- factor(dataset$type)

  if (config == 1) {
    dataset$quality <- ifelse(dataset$quality > 6, 0, 1)
    dataset$quality <- factor(dataset$quality, levels = c(0, 1)) # labels = c("bad", "good")
  } else if (config == 2) {
    dataset$quality <- ifelse(dataset$quality <= 5, 0, ifelse(dataset$quality <= 7, 1, 2))
    dataset$quality <- factor(dataset$quality, levels = c(0, 1, 2)) # labels = c("low", "medium", "high")
  }

  dataset
}

#' Partion the dataset based on the class attribute
#'
#' @param dataset a dataset
#' @return the partition composed of train and test
partition_dataset <- function(dataset) {
  index <- createDataPartition(dataset$quality, p = 0.80, list = FALSE)
  train <- dataset[index,]
  test <- dataset[-index,]

  list(train = train, test = test)
}


.norm_minmax <- function(x) { (x - min(x)) / (max(x) - min(x)) }

#' Normalize the dataset using z-score normalization
#'
#' @param dataset a dataset
#' @return the normalized dataset
normalize_dataset <- function(dataset, method) {
  to_scale <- dataset %>% select(where(is.numeric))

  if (method == "min_max") {
    scaled <- scale(to_scale)
  } else if (method == "z_score") {
    scaled <- lapply(to_scale, .norm_minmax)
  }

  scaled <- as.data.frame(scaled)
  scaled$quality <- dataset$quality
  scaled
}

#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
evaluate_model <- function(model, dataset) {
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

  out <- list(cm = cm, pred_time = time)
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
plot_roc_and_prc_all <- function(dataset, models, n_classes) {
  nb_pred <- predict(models$nb_model, dataset[names(dataset) != "quality"])
  dt_pred <- predict(models$dt_model, dataset[names(dataset) != "quality"])
  svm_pred <- predict(models$svm_model, dataset[names(dataset) != "quality"])
  nn_pred <- predict(models$nn_model, dataset[names(dataset) != "quality"])

  if (n_classes == 2) {
    nb_pred <- as.numeric(nb_pred)
    dt_pred <- as.numeric(dt_pred)
    svm_pred <- as.numeric(svm_pred)
    nn_pred <- as.numeric(nn_pred)

    scores <- join_scores(nb_pred, dt_pred, svm_pred, nn_pred)
    y <- as.numeric(dataset$quality)
    labels <- join_labels(y, y, y, y)
    mmmdat <- mmdata(scores, labels, modnames = c("nb", "dt", "svm", "nn"), dsids = c(1, 2, 3, 4))
    res <- evalmod(mmmdat) # mode = 'aucroc'

    # Calculate CI of AUCs with normal distibution
    auc_ci <- auc_ci(res)

    # Use knitr::kable to display the result in a table format
    knitr::kable(auc_ci)

    # Plot ROC and PRC
    autoplot(res)

    return(res)
  } else {
    res1 <- .multiclasses_roc(dataset, nb_pred)
    res2 <- .multiclasses_roc(dataset, dt_pred)
    res3 <- .multiclasses_roc(dataset, svm_pred)
    res4 <- .multiclasses_roc(dataset, nn_pred)
    return(list(res1, res2, res3, res4))
  }
}

#' Write a log file
#' @param model_name model's name
#' @param cm a confusion matrix
#' @param train_time training time
#' @param pred_time prediction time
#'
write_log <- function(model_name, cm, train_time, pred_time) {
  file <- file.path("./results/models", paste0(model_name, "_.log"))
  write.table(paste("model_name: ", model_name), file, row.names = FALSE, col.names = FALSE)
  write.table(paste("pred_time: ", pred_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("train_time: ", train_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
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
treat_outliers <- function(data, method = "winsorizing", win.quantiles = c(0.05, 0.95)) {
  if (method == "winsorizing") {
    bounds <- detect_outliers(data, win.quantiles = win.quantiles)
    data[data < bounds[1]] <- bounds$lower
    data[data > bounds[2]] <- bounds$upper
  } else if (method == "IQR") {
    bounds <- detect_outliers(data)
    data[data < bounds$lower | data > bounds$upper] <- median(data)
  } else {
    stop("`mode` should be either `IQR` or `winsorizing`")
  }
  data
}

save_plot_png <- function(filename, plot, wide = FALSE) {
  ggsave(filename, plot = plot, device = "png", height = 6.67, width = ifelse(wide, 13.34, 6.67))
}

.multiclasses_roc <- function(dataset, predictions) {
  n0_true <- as.numeric(dataset$quality == 0)
  n1_true <- as.numeric(dataset$quality == 1)
  n2_true <- as.numeric(dataset$quality == 2)

  S4_pred_m1 <- data.frame(predictions)
  n0_pred_m1 <- as.numeric(S4_pred_m1 == 0)
  n1_pred_m1 <- as.numeric(S4_pred_m1 == 1)
  n2_pred_m1 <- as.numeric(S4_pred_m1 == 2)

  final_df <- data.frame(n0_true, n1_true, n2_true,
                         n0_pred_m1, n1_pred_m1, n2_pred_m1)

  roc_res <- multi_roc(final_df, force_diag = T)
  pr_res <- multi_pr(final_df, force_diag = T)

  plot_roc_df <- plot_roc_data(roc_res)
  plot_pr_df <- plot_pr_data(pr_res)

  prc <- ggplot(plot_pr_df, aes(x = Recall, y = Precision)) +
    geom_path(aes(color = Group, linetype = Method), size = 1.5) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.justification = c(1, 0), legend.position = c(.95, .05), legend.title = element_blank(), legend.background = element_rect(fill = NULL, size = 0.5, linetype = "solid", colour = "black"))
  roc <- ggplot(plot_roc_df, aes(x = 1 - Specificity, y = Sensitivity)) +
    geom_path(aes(color = Group, linetype = Method)) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = 'grey', linetype = 'dotdash') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.justification = c(1, 0), legend.position = c(.95, .05), legend.title = element_blank(), legend.background = element_rect(fill = NULL, size = 0.5, linetype = "solid", colour = "black"))

  list(roc, prc)
}

feature_selection_pca <- function(dataset) {
  # new_dataset <- (((pca$x + pca$center) * pca$scale)  %*% pca$rotation[, keep])
  x <- dataset %>% select(where(is.numeric))
  pca <- prcomp(x, scale = TRUE, center = TRUE)
  eig <- get_eig(pca)
  keep <- eig$cumulative.variance.percent < 90
  features <- pca$x[, keep]
  out <- data.frame(features, quality = dataset$quality)
}

create_dataset <- function(dataset) {
  partition <- dataset %>%
    mutate(type = NULL) %>%
    preprocess_dataset(1) %>%
    partition_dataset()

  write.csv(partition$train, "./dataset/winequality-train.csv", row.names = FALSE)
  write.csv(partition$test, "./dataset/winequality-test.csv", row.names = FALSE)
}
