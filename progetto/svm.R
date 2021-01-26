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

# Train the model
nb_model <- train(
  good ~ .,
  data = redwine_train,
  method = "svmLinear", #svmPoly, svmRadial
  preProcess = pre_process,
  tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
  #tuneLength=4,\1
  #tuneGrid = expand.grid(C = seq(1e-3, 1e-4, length = 20)),
  trControl = tr_control
)

# Predict
nb_pred <- predict(nb_model, redwine_test, preProcess = pre_process)

# Print confusion matrix
confusionMatrix(data = nb_pred, reference = redwine_test$good)

# Save the model
save(nb_model, file = "progetto/models/nb_model.RData")
