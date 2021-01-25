## Naive Bayes Classification

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
redwineTrain <- redwine[index,]
redwineTest <- redwine[-index,]

# 10-Fold cross validation
control <- trainControl(
  method = "repeatedcv",
  number = 10
)

# Train the model
nb.model <- train(
  good ~ .,
  data = redwineTrain,
  method = "nb",
  preProcess = c("center", "scale"),
  trControl = control
)

# Predict
nb.pred <- predict(nb.model, redwineTest, preProcess = c("center", "scale"))

# Print confusion matrix
confusionMatrix(data = nb.pred, reference = redwineTest$good)
