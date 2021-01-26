# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, dplyr)

# Local functions
source("./utils.R")
source("./bayes.R")

# Prepare the dataset
redwine <- read.csv("./dataset/winequality-red.csv") %>%
  preprocess_dataset() %>%
  partition_dataset()

# Normalize the data
redwine$train <- normalize_dataset(redwine$train)
redwine$test <- normalize_dataset(redwine$test)

# Train the model
nb_model <- bayes_classification(redwine)

# Evaluate the model
evaluate_model(nb_model, redwine$train)
evaluate_model(nb_model, redwine$test)
