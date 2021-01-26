# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, dplyr)

# Local functions
source("./utils.R")
source("./bayes.R")

# Prepare the dataset
redwine <- read.csv("./dataset/winequality-red.csv") %>%
  preProcessDataset() %>%
  partitionDataset()

# Normalize the data
redwine$train <- normalizeDataset(redwine$train)
redwine$test <- normalizeDataset(redwine$test)

# Train the model
nb_model <- bayesClassification(redwine)

# Evaluate the model
evaluateModel(nb_model, redwine$train)
evaluateModel(nb_model, redwine$test)
