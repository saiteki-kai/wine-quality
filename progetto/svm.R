#' Support Vector Machine Classification
#'
#' add description.
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, klaR)

# Read the dataset
redwine <- read.csv("progetto/dataset/winequality-red.csv")

# Add class attribute
redwine$good <- ifelse(redwine$quality > 6, "Yes", "No")
redwine$good <- factor(redwine$good)
redwine$quality <- NULL

# Partition the dataset
index <- createDataPartition(redwine$good, p = 0.7, list = FALSE)
redwine_train <- redwine[index,]
redwine_test <- redwine[-index,]

# Preprocess method
pre_process <- "range" # c("center", "scale")

# 10-Fold cross validation
tr_control <- trainControl(
  method = "repeatedcv",
  number = 10
)


#tuneLength=10 svmRadial
#tuneLength=4 svmPoly
grid_radial <- expand.grid(
  sigma = c(0,0.01,0.1,0.5,0.75,0.9,1),
  C = c(0,0.01,0.1,0.5,0.75,0.9,1,1.5,2,5)
)

# Train the model
svm_model <- train(
  good ~ .,
  data = redwine_train,
  method = "svmRadial", #svmLinear, svmPoly, svmRadial, svmRadialWeights
  preProcess = pre_process,
  tuneGrid=grid_radial,
  #tuneLength=10,
  trControl = tr_control
)

# Predict
nb_pred <- predict(svm_model, redwine_test, preProcess = pre_process)

# Print confusion matrix
confusionMatrix(data = nb_pred, reference = redwine_test$good)

# Save the model
save(svm_model, file = "progetto/models/svm_model.RData")
