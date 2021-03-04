#'
#'
#'
#'

.prepare_dataset <- function() {
  # Prepare the dataset
  dataset <- read.csv('./dataset/winequality-combined.csv')

  # Setup quality
  dataset$quality <- ifelse(dataset$quality > 6, 'good', 'bad')
  dataset$quality <- factor(dataset$quality)
  dataset$type <- NULL

  # Create Partition
  index <- createDataPartition(dataset$quality, p = 0.7, list = FALSE)
  trainset <- dataset[index,]
  testset <- dataset[-index,]

  write.csv(trainset, "./dataset/winequality-train.csv", row.names = FALSE)
  write.csv(testset, "./dataset/winequality-test.csv", row.names = FALSE)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, DMwR, caret, dplyr)

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/svm.R")

.prepare_dataset()

# Read trainset
trainset <- read.csv('./dataset/winequality-train.csv')
trainset$quality <- factor(trainset$quality)

#Subsempling
trainset <- subsempling(trainset, "SMOTE")

# Tuning parameters
grid_radial <- expand.grid(
  sigma = 0.9, # c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4),
  C = 1.2 # c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4)
)
grid_tree <- expand.grid(maxdepth = 2:10)

# Train the models
train_model(trainset, "rpart2", "pca", grid_tree)
train_model(trainset, "svmRadial", "pca", grid_radial)
