.get_model <- function(name) {
  filename <- file.path("./models", paste0(name, "_model.RDS"))
  if (file.exists(filename)) {
    readRDS(filename)
  }
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, mlbench, dplyr)

# Local functions
source("./utils.R")
source("./dt.R")
source("./nb.R")
source("./svm.R")
source("./nn.R")

# Prepare the dataset
combined <- read.csv("./dataset/winequality-combined.csv") %>%
  mutate(type = NULL) %>%
  preprocess_dataset(2) %>%
  remove_outliers() %>%
  na.omit() %>%
  partition_dataset()

# Normalize the data
combined$test <- normalize_dataset(combined$test)

# Feature Selection 1
combined$test$alcohol <- NULL
combined$test$density <- NULL
#combined$test$volatile.acidity <- NULL
#combined$test$chlorides <- NULL
combined$test$residual.sugar <- NULL
combined$test$sulphates <- NULL
combined$test$citric.acid <- NULL
combined$test$pH <- NULL
combined$test$free.sulfur.dioxide <- NULL
#combined$test$total.sulfur.dioxide <- NULL

m1 <- .get_model("nb")
m2 <- .get_model("dt")
m3 <- .get_model("svm")
m4 <- .get_model("nn")

# Evaluate the model
res_eval_1 <- evaluate_model(m1, combined$test)
res_eval_2 <- evaluate_model(m2, combined$test)
res_eval_3 <- evaluate_model(m3, combined$test)
res_eval_4 <- evaluate_model(m4, combined$test)

# Plot AUCs ROC & PRC
n_classes <- length(unique(as.numeric(combined$test$quality) - 1))
models <- list(nb_model = m1, dt_model = m2, svm_model = m3, nn_model = m4)
plot_roc_and_prc_all(combined$test, models, n_classes)
