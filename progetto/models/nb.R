#' Naive Bayes Classification
#'
#' add description.
#'

nb_classification <- function(trainset) {
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
  start_time <- Sys.time()
  nb_model <- train(
    quality ~ .,
    data = trainset,
    method = "nb",
    trControl = tr_control
  )
  end_time <- Sys.time()
  time <- end_time - start_time

  # Save the model
  saveRDS(nb_model, file = "./results/models/nb_model.RDS")

  # Return
  list(model = nb_model, train_time = time)
}
