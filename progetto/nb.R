#' Naive Bayes Classification
#'
#' add description.
#'

bayes_classification <- function(trainset) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret)

  # 10-Fold cross validation
  tr_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5
  )

  # Set seed for repeatability
  set.seed(314)

  # Train the model
  start_train_time <- Sys.time()
  nb_model <- train(
    quality ~ .,
    data = trainset,
    method = "nb",
    trControl = tr_control
  )
  end_train_time <- Sys.time()
  time_train <- end_train_time - start_train_time

  file <- paste(nb_model$method, "log.txt", sep='_')
  write.table(paste(time_train, "ms", sep="."), file, row.names = FALSE)

  # Save the model
  save(nb_model, file = "./models/nb_model.RData")

  # Return
  nb_model
}
