# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, dplyr)

# Local functions
source("./utils.R")
source("dt.R")
source("nb.R")
source("./svm.R")
source("./nn.R")

# Prepare the dataset
combined <- read.csv("./dataset/winequality-combined.csv") %>%
  preprocess_dataset(1) %>%
  partition_dataset()

# Normalize the data
combined$train <- normalize_dataset(combined$train)
combined$test <- normalize_dataset(combined$test)

# Train the model
nb_model <- bayes_classification(combined$train)
dt_model <- decision_tree_classification(combined$train)
svm_model <- svm_classification(combined$train)
nn_model <- neural_network_classification(combined$train)

# Evaluate the model
evaluate_model(nb_model, combined$test)
evaluate_model(dt_model, combined$test)
evaluate_model(svm_model, combined$test)
evaluate_model(nn_model, combined$test)
