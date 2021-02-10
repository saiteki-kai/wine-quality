#' Convert the quality attribute to a binary attribute
#' The new quality has the value "good" if quality was greater than 6, "bad" otherwise
#'
#' @param dataset a dataset
#' @return the processed dataset
preprocess_dataset <- function(dataset) {
  dataset$quality <- ifelse(dataset$quality > 6, "good", "bad")
  dataset$quality <- factor(dataset$quality, levels = c("good", "bad"))
  dataset
}

#' Patition the dataset based on the class attribute
#'
#' @param dataset a dataset
#' @return the partition composed of train and test
partition_dataset <- function(dataset) {
  index <- createDataPartition(dataset$quality, p = 0.9, list = FALSE)
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
  # Predict
  pred <- predict(model, dataset)

  # Print confusion matrix
  cm <- confusionMatrix(data = pred, reference = dataset$quality)

  cm$byClass["F1"]
}

#' Combine the red and the white datasets and add type attribute
combine_redwhite <- function() {
  redwine <- read.csv("./dataset/winequality-red.csv")
  whitewine <- read.csv("./dataset/winequality-white.csv")

  redwine$type <- "red"
  whitewine$type <- "white"

  wines <- rbind(redwine, whitewine)
}
