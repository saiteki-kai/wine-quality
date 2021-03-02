#'
#'
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, DMwR, caret, dplyr)

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/svm.R")


# Prepare the dataset
dataset <- read.csv('./dataset/winequality-combined.csv')

# Setup quality
dataset$quality <- ifelse(dataset$quality > 6, 'good', 'bad')
dataset$quality <- factor(dataset$quality)
dataset$type <- NULL

# Create Partition
index <- createDataPartition(dataset$quality, p = 0.75, list = FALSE)
trainset <- dataset[index,]
testset <- dataset[-index,]

#Subsempling
trainset <- subsempling(trainset, "SMOTE")

# Preprocess ("scale", "center", "pca", "YeoJohnson", "BoxCox")
cols <- ncol(trainset)
pre_proc <- preProcess(trainset[, -cols], method = "YeoJohnson", thresh = 0.9, verbose = TRUE)

# 10-Fold cross validation
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  allowParallel = TRUE
)

# Train the models
m1 <- parallelTrain(trainset, train_control, pre_proc, dt_classification)
m2 <- parallelTrain(trainset, train_control, pre_proc, svm_classification)