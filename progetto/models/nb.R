#' Naive Bayes Classification
#'
#' add description.
#'

nb_classification <- function(trainset, method, train_control) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret)

  # Set seed for repeatability
  set.seed(314)

  # Train the model
  start_time <- Sys.time()
  nb_model <- train(
    quality ~ .,
    data = trainset,
    method = "nb",
    trControl = train_control,
    preProc = method
  )
  end_time <- Sys.time()
  time <- end_time - start_time
  print(paste0("train_time: ", time))

  # Save the model
  saveRDS(nb_model, file = "./results/models/nb_model.RDS")

  # Return
  list(model = nb_model, train_time = time)
}
