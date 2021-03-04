#' Train
#'
#'
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("./utils.R")
source("./train_model.R")

# Read trainset
trainset <- read.csv('./dataset/winequality-train.csv')
trainset$quality <- factor(trainset$quality)

#Subsampling
trainset <- subsampling(trainset, "SMOTE")

# Tuning parameters
grid_radial <- expand.grid(
  sigma = 0.9, # c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4),
  C = 1.2 # c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4)
)
# grid_radial <- expand.grid(
#   sigma = c(0.01, 0.5, 1), #c(0,0.01,0.1,0.5,0.75,0.9,1),
#   C = c(0.01, 1, 2) #0,0.01,0.1,0.5,0.75,0.9,1,1.5,2,5)
# )
grid_tree <- expand.grid(maxdepth = 2:10)

# Train the models
model.tree <- train_model(trainset, "rpart2", "pca", grid_tree)
model.svm <- train_model(trainset, "svmRadial", "pca", grid_radial)
