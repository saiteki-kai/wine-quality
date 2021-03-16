#' This script transform the quality label in a binary label and create the
#' partition between training and test sets.

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("./utils.R")

# Load the dataset
dataset <- read.csv("../data/winequality-white.csv")

# Setup quality
dataset <- relabeling(dataset)

# Create Partition
set.seed(444)
index <- createDataPartition(dataset$quality, p = 0.70, list = FALSE)
trainset <- dataset[index, ]
testset <- dataset[-index, ]

write.csv(trainset, "../data/winequality-train.csv", row.names = FALSE)
write.csv(testset, "../data/winequality-test.csv", row.names = FALSE)
