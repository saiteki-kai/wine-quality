#' Support Vector Machine Classification
#'
#' add description.
#'

svm_classification <- function(trainset) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret)

  # 10-Fold cross validation
  tr_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5
  )

  # Set seed for repeatability
  set.seed(314)

  #tuneLength=10 svmRadial
  #tuneLength=4 svmPoly
  grid_radial <- expand.grid(
    sigma = c(0.01, 0.5, 1), #c(0,0.01,0.1,0.5,0.75,0.9,1),
    C = c(0.01, 1, 2) #0,0.01,0.1,0.5,0.75,0.9,1,1.5,2,5)
  )

  # Train the model
  start_train_time <- Sys.time()
  svm_model <- train(
    quality ~ .,
    data = trainset,
    method = "svmRadial", #svmLinear, svmPoly, svmRadial, svmRadialWeights
    tuneGrid = grid_radial,
    #tuneLength=10,
    trControl = tr_control
  )
  end_train_time <- Sys.time()
  time_train <- end_train_time - start_train_time

  file <- file.path("./results", paste0(svm_model$method, "_train.log"))
  write.table(paste(time_train, "ms"), file, row.names = FALSE, col.names = FALSE)

  # Save the model
  save(svm_model, file = "./models/svm_model.RData")

  # Return
  svm_model
}