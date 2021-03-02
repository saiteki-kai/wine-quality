#' Support Vector Machine Classification
#'
#' add description.
#'

svm_classification <- function(trainset, train_control, pre_proc) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret)

  # Set seed for repeatability
  set.seed(6314)

  # Apply transformations
  cols <- ncol(trainset)
  trasformed <- predict(pre_proc, trainset[, -cols])
  trasformed$quality <- trainset$quality

  #tuneLength=10 svmRadial
  #tuneLength=4 svmPoly
  "grid_radial <- expand.grid(
    sigma = c(0.01, 0.5, 1), #c(0,0.01,0.1,0.5,0.75,0.9,1),
    C = c(0.01, 1, 2) #0,0.01,0.1,0.5,0.75,0.9,1,1.5,2,5)
  )"
  grid_radial <- expand.grid(
    sigma = 0.9,#c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4),
    C = 1.2#c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4)
  )

  # Train the model
  start_time <- Sys.time()
  svm_model <- train(
    quality ~ .,
    data = trasformed,
    method = "svmRadial", #svmLinear, svmPoly, svmRadial, svmRadialWeights
    tuneGrid = grid_radial,
    #tuneLength=10,
    trControl = train_control
  )
  end_time <- Sys.time()
  time <- end_time - start_time
  print(paste0("train_time: ", time))

  # Save the model
  obj <- list(model = svm_model, train_time = time, pre_proc = pre_proc)
  saveRDS(obj, file = "./results/models/svm_model.RDS")

  # Return
  obj
}