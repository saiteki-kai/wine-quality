#' Support Vector Machine Classification
#'
#' add description.
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, klaR)

svm_classification <- function(trainset) {
  
  # 10-Fold cross validation
  tr_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5
  )
  
  #tuneLength=10 svmRadial
  #tuneLength=4 svmPoly
  grid_radial <- expand.grid(
    sigma = c(0.01,0.5, 1), #c(0,0.01,0.1,0.5,0.75,0.9,1),
    C = c(0.01,1,2) #0,0.01,0.1,0.5,0.75,0.9,1,1.5,2,5)
  )
  
  # Train the model
  svm_model <- train(
    quality ~ .,
    data = trainset,
    method = "svmRadial", #svmLinear, svmPoly, svmRadial, svmRadialWeights
    tuneGrid=grid_radial,
    #tuneLength=10,
    trControl = tr_control
  )
  
  # Save the model
  save(svm_model, file = "./models/svm_model.RData")
  
  # Return
  svm_model
}