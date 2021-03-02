#' Decision Tree Classification
#'
#' add description.
#'

dt_classification <- function(trainset, train_control, pre_proc) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, klaR, rattle, pROC, dplyr)

  # Set seed for repeatability
  set.seed(6314)

  # Apply transformations
  cols <- ncol(trainset)
  trasformed <- predict(pre_proc, trainset[, -cols])
  trasformed$quality <- trainset$quality

  # Train the model
  start_time <- Sys.time()
  dt_model <- train(
    quality ~ .,
    data = trasformed,
    method = "rpart2", # rpart, rpart2
    tuneGrid = expand.grid(maxdepth = 2:10),
    #tuneLength = 10,
    trControl = train_control
  )
  end_time <- Sys.time()
  time <- end_time - start_time
  print(paste0("train_time: ", time))

  # Print Tuning Process
  plot(dt_model)

  # Print Tree
  fancyRpartPlot(dt_model$finalModel)

  # Save the model
  obj <- list(model = dt_model, train_time = time, pre_proc = pre_proc)
  saveRDS(obj, file = "./results/models/dt_model.RDS")

  # Return
  obj
}


