#' Naive Bayes Classification
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
  method = "nb",
  preProcess = pre_process,
  trControl = tr_control
)

# Predict
nb_pred <- predict(nb_model, redwine_test, preProcess = pre_process)

# Print confusion matrix
confusionMatrix(data = nb_pred, reference = redwine_test$good)
