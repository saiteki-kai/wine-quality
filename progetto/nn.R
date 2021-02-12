#' Neural Network Classification
#'
#' add description.
#'

neural_network_classification <- function(trainset) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, doParallel)

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
  nn_model <- train(
    quality ~ .,
    data = trainset,
    method = "mlpML",
    trControl = tr_control,
    tuneGrid = expand.grid(layer1 = 9, layer2 = 7, layer3 = 5)
  )
  end_train_time <- Sys.time()
  time_train <- end_train_time - start_train_time

  file <- file.path("./results", paste0(nn_model$method, "_train.log"))
  write.table(paste(time_train, "ms"), file, row.names = FALSE, col.names = FALSE)

  # Save the model
  save(nn_model, file = "./models/nn_model.RData")

  # Return
  nn_model
}
