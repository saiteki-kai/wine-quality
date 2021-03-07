# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("utils.R")

# Load the dataset
dataset <- read.csv("../data/winequality-combined.csv")

# Setup quality
dataset <- relabelling(dataset, 1)
dataset$type <- NULL

# Create Partition
set.seed(444)
index <- createDataPartition(dataset$quality, p = 0.75, list = FALSE)
trainset <- dataset[index, ]
testset <- dataset[-index, ]

write.csv(trainset, "../data/winequality-train.csv", row.names = FALSE)
write.csv(testset, "../data/winequality-test.csv", row.names = FALSE)
