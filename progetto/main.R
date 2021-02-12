# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, dplyr, doParallel)

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

# Register parallel processing
cores <- detectCores()
registerDoParallel(cores = cores)
cluster <- makeCluster(cores)

# Train the model
nb_model <- nb_classification(combined$train)
dt_model <- dt_classification(combined$train)
svm_model <- svm_classification(combined$train)
nn_model <- nn_classification(combined$train)

# Stop using parallel computing
stopCluster(cluster)

# Evaluate the model
evaluate_model(nb_model, combined$test)
evaluate_model(dt_model, combined$test)
evaluate_model(svm_model, combined$test)
evaluate_model(nn_model, combined$test)
