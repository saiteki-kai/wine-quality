# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Load the dataset
dataset <- read.csv('../data/winequality-combined.csv')

# Setup quality
dataset$quality <- ifelse(dataset$quality > 6, 'good', 'bad')
dataset$quality <- factor(dataset$quality)
dataset$type <- NULL

# Create Partition
set.seed(444)
index <- createDataPartition(dataset$quality, p = 0.7, list = FALSE)
trainset <- dataset[index,]
testset <- dataset[-index,]

write.csv(trainset, "../data/winequality-train.csv", row.names = FALSE)
write.csv(testset, "../data/winequality-test.csv", row.names = FALSE)
