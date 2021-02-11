#' Neural Network Classification
#'
#' add description.
#'

neural_network_classification <- function(trainset) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret)

  # 10-Fold cross validation
  tr_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5
  )

  #x <- expand.grid(size = c(1, 2, 3, 4, 5, 6, 7), decay = c(0, 10^(-2), 10^(-3), 10^(-4)))
  params <- expand.grid(
    layer1 = 9,
    layer2 = 7,
    layer3 = 5
  )

  # Train the model
  nn_model <- train(
    quality ~ .,
    data = trainset,
    method = "mlpML", #neuralnet
    trControl = tr_control,
    tuneGrid = params
  )

  # Save the model
  save(nn_model, file = "./models/nn_model.RData")

  # Return
  nn_model
}
