preProcessDataset <- function(dataset) {
  dataset$quality <- ifelse(dataset$quality > 6, "good", "bad")
  dataset$quality <- factor(dataset$quality, levels = c("good", "bad"))
  dataset
}

partitionDataset <- function(dataset) {
  index <- createDataPartition(dataset$quality, p = 0.9, list = FALSE)
  train <- dataset[index,]
  test <- dataset[-index,]

  list(train = train, test = test)
}

normalizeDataset <- function(dataset) {
  scaled <- scale(dataset[names(dataset) != "quality"])
  scaled <- as.data.frame(scaled)

  scaled$quality <- dataset$quality
  scaled
}

evaluateModel <- function(model, dataset) {
  # Predict
  pred <- predict(model, dataset)

  # Print confusion matrix
  confusionMatrix(data = pred, reference = dataset$quality)
}