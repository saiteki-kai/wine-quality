#' Decision Tree Classification
#'
#' add description.
#'

dt_classification <- function(trainset) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, klaR, rattle, pROC, dplyr)

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
  dt_model <- train(
    quality ~ .,
    data = trainset,
    method = "rpart2", # rpart
    tuneGrid = expand.grid(maxdepth = 2:10),
    #tuneLength = 10,
    trControl = tr_control
  )
  end_time <- Sys.time()
  time <- end_time - start_time

  # Print Tuning Process
  plot(dt_model)

  # Print Tree
  fancyRpartPlot(dt_model$finalModel)

  # Save the model
  save(dt_model, file = "./models/dt_model.RData")

  # Return
  list(model = dt_model, train_time = time)
}


