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

  # Train the model
  nb_model <- train(
    quality ~ .,
    data = trainset,
    method = "nb",
    trControl = tr_control
  )

  # Save the model
  save(nb_model, file = "./models/nb_model.RData")

  # Return
  nb_model
}
