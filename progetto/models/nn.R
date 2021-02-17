#' Neural Network Classification
#'
#' add description.
#'

nn_classification <- function(trainset, method, train_control) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, doParallel)

  # Set seed for repeatability
  set.seed(314)

  # Train the model
  start_time <- Sys.time()
  nn_model <- train(
    quality ~ .,
    data = trainset,
    method = "mlpML",
    trControl = train_control,
    tuneGrid = expand.grid(layer1 = 9, layer2 = 7, layer3 = 5), #tuning numero nodi
    preProc = method
  )
  end_time <- Sys.time()
  time <- end_time - start_time
  print(paste0("train_time: ", time))

  # Save the model
  saveRDS(nn_model, file = "./results/models/nn_model.RDS")

  # Return
  list(model = nn_model, train_time = time)
}
