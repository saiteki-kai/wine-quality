library(caret)

dataset <- read.csv("../data/winequality-white.csv")

# Setup quality
dataset$quality <- ifelse(dataset$quality > 6, "good", "bad")
dataset$quality <- factor(dataset$quality, levels = c("bad", "good"))

# dataset$quality <- ifelse(dataset$quality <= 5, "bad", ifelse(dataset$quality > 7, "good", "medium"))
# dataset$quality <- factor(dataset$quality, levels = c("bad", "medium", "good"))

# Create Partition
index <- createDataPartition(dataset$quality, p = 0.80, list = FALSE)
trainset <- dataset[index, ]
testset <- dataset[-index, ]

# Downsample
# trainset <- downSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
# names(trainset)[names(trainset) == "Class"] <- "quality"

train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1
)

cols <- ncol(trainset)

# Apply transformations
pre_proc <- preProcess(trainset[, -cols], method = c("scale", "center", "pca"), verbose = TRUE)
trainset_pca <- predict(pre_proc, trainset[, -cols])
trainset_pca$quality <- trainset$quality

model <- train(
  quality ~ .,
  data = trainset_pca,
  method = "mlp",
  trControl = train_control
)

# Apply transformations
testset_pca <- predict(pre_proc, testset[, -cols])
testset_pca$quality <- testset$quality

# Predict testset
pred <- predict(model, newdata = testset_pca)
cm <- confusionMatrix(pred, testset_pca$quality, mode = "prec_recall", positive = "good")
