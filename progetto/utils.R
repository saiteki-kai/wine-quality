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

  # Print confusion matrix
  cm <- confusionMatrix(data = pred, reference = dataset$quality, mode = 'prec_recall')

  measures <- data.frame(
    accuracy = cm$overall["Accuracy"],
    f1 = cm$byClass["F1"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    time = end_time - start_time
  )
  saveRDS(measures, file.path("./results/", paste0(model$method, "_test.rds")))

  cm
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
