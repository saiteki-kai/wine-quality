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
  index <- createDataPartition(dataset$quality, p = 0.75, list = FALSE)
  train <- dataset[index,]
  test <- dataset[-index,]

  list(train = train, test = test)
}


.norm_minmax <- function(x){ (x- min(x)) /(max(x)-min(x)) }

#' Normalize the dataset using z-score normalization
#'
#' @param dataset a dataset
#' @return the normalized dataset
normalize_dataset <- function(dataset, method) {
  to_scale <- dataset %>% select(where(is.numeric))

  if (method == "min_max") {
    scaled <- scale(to_scale)
  } else if(method == "z_score") {
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
  file <- file.path("./results", paste0(model_name, "_.log"))
  write.table(paste("model_name: ", model_name), file, row.names = FALSE, col.names = FALSE)
  write.table(paste("pred_time: ", pred_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("train_time: ", train_time), file, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(capture.output(cm), file, row.names = FALSE, col.names = FALSE, append = TRUE)
}

#' Detect outliers using the Interquartile Range (IQR) approach.
#' All points that lie outside the upper limit or below the lower limit can be considered outliers.
#'
#' @param data numeric array
#'
#' @details
#' IQR = 75th quantile - 25th quantile
#' Lower Limit = 25th quantile - 1.5 * IQR
#' Upper Limit = 75th quantile + 1.5 * IQR
#'
#' @return a list containing the lower and upper limits
detect_outliers <- function(data) {
  quantiles <- quantile(data, c(0.25, 0.75), names = FALSE)
  lower <- quantiles[1] - 1.5 * IQR(data)
  upper <- quantiles[2] + 1.5 * IQR(data)

  list(lower = lower, upper = upper)
}

#' Remove the outliers setting values to NA
#'
#' @param dataset a dataset
#' @return the dataset without outliers
remove_outliers <- function(dataset) {
  out <- dataset
  for (i in names(out)) {
    values <- out[[i]]
    if (is.numeric(values)) {
      bounds <- detect_outliers(values)
      is_outlier <- values < bounds$lower | values > bounds$upper
      out[[i]][is_outlier] <- NA
    }
  }
  out
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
  out <- data.frame(features, quality=dataset$quality)
}