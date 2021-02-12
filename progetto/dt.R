#' Decision Tree Classification
#'
#' add description.
#'

decision_tree_classification <- function(trainset) {
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
  start_train_time <- Sys.time()
  dt_model <- train(
    quality ~ .,
    data = trainset,
    method = "rpart2",
    tuneGrid = expand.grid(maxdepth = 2:10),
    #tuneLength=20,
    #metric = "ROC",
    trControl = tr_control,
  )
  end_train_time <- Sys.time()
  time_train <- end_train_time - start_train_time

  file <- file.path("./results", paste0(dt_model$method, "_train.log"))
  write.table(paste(time_train, "ms"), file, row.names = FALSE, col.names = FALSE)

  # Print Tuning Process
  plot(dt_model)

  # Print Tree
  fancyRpartPlot(dt_model$finalModel)

  # Save the model
  save(dt_model, file = "./models/dt_model.RData")

  # Return
  dt_model
}


