#' Decision Tree Classification
#'
#' add description.
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, klaR, rattle, pROC, dplyr)


decision_tree_classification <- function(trainset) {

  # 10-Fold cross validation
  tr_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5
  )

  # Set seed for repeatability
  set.seed(314)

  # Train the model
  dt_model <- train(
    quality ~ .,
    data = trainset,
    method = "rpart2",
    tuneGrid = expand.grid(maxdepth = 2:10),
    #tuneLength=20,
    #metric = "ROC",
    trControl = tr_control,
  )

  # Print Tuning Process
  plot(dt_model)

  # Print Tree
  fancyRpartPlot(dt_model$finalModel)

  # Save the model
  save(dt_model, file = "./models/dt_model.RData")

  # Return
  dt_model
}


