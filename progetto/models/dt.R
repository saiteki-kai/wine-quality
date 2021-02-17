#' Decision Tree Classification
#'
#' add description.
#'

dt_classification <- function(trainset, method, train_control) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, klaR, rattle, pROC, dplyr)

  # Set seed for repeatability
  set.seed(314)

  # Train the model
  start_time <- Sys.time()
  dt_model <- train(
    quality ~ .,
    data = trainset,
    method = "rpart2", # rpart
    tuneGrid = expand.grid(maxdepth = 2:10),
    #tuneLength = 10,
    trControl = train_control,
    preProc = method
  )
  end_time <- Sys.time()
  time <- end_time - start_time
  print(paste0("train_time: ", time))

  # Print Tuning Process
  plot(dt_model)

  # Print Tree
  fancyRpartPlot(dt_model$finalModel)

  # Save the model
  saveRDS(dt_model, file = "./results/models/dt_model.RDS")

  # Return
  list(model = dt_model, train_time = time)
}


