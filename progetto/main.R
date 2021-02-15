.train_model <- function (name) {
   filename <- file.path("./models", paste0(name, "_model.RDS"))
   file.exists(filename)
}

.get_model <- function(name) {
  if (.train_model(name)) {
    filename <- file.path("./models", paste0(name, "_model.RDS"))
    readRDS(filename)
  }
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, dplyr)

# Local functions
source("./utils.R")
source("./dt.R")
source("./nb.R")
source("./svm.R")
source("./nn.R")

# Prepare the dataset
combined <- read.csv("./dataset/winequality-combined.csv") %>%
  preprocess_dataset(2) %>%
  partition_dataset()

# Normalize the data
combined$train <- normalize_dataset(combined$train)
combined$test <- normalize_dataset(combined$test)

# Feature Selection 1
#combined$train$residual.sugar <- NULL
#combined$train$sulphates <- NULL
#combined$train$citric.acid <- NULL
#combined$train$pH <- NULL
#combined$train$free.sulfur.dioxide <- NULL
#combined$train$total.sulfur.dioxide <- NULL

#combined$test$residual.sugar <- NULL
#combined$test$sulphates <- NULL
#combined$test$citric.acid <- NULL
#combined$test$pH <- NULL
#combined$test$free.sulfur.dioxide <- NULL
#combined$test$total.sulfur.dioxide <- NULL

# Feature Selection 2
#combined$train <- calculate_pca(combined$train)
#combined$test <- calculate_pca(combined$test)

# Register parallel processing
cores <- detectCores()
registerDoParallel(cores = cores)
cluster <- makeCluster(cores)

# Train the models
res_train1 <- .train_model("nb") %??% nb_classification(combined$train)
res_train2 <- .train_model("dt") %??% dt_classification(combined$train)
res_train3 <- .train_model("svm") %??% svm_classification(combined$train)
res_train4 <- .train_model("nn") %??% nn_classification(combined$train)

m1 <- .get_model("nb") %??% res_train1$model
m2 <- .get_model("dt") %??% res_train2$model
m3 <- .get_model("svm") %??% res_train3$model
m4 <- .get_model("nn") %??% res_train4$model

# Stop using parallel computing
stopCluster(cluster)

# Write Logs
.train_model("nb") %??% write_log("nb", res1$cm, m1$train_time, res1$pred_time)
.train_model("dt") %??% write_log("dt", res2$cm, m2$train_time, res2$pred_time)
.train_model("svm") %??% write_log("svm", res3$cm, m3$train_time, res3$pred_time)
.train_model("nn") %??% write_log("nn", res4$cm, m4$train_time, res4$pred_time)

# Evaluate the model
res_eval_1 <- evaluate_model(m1, combined$test)
res_eval_2 <- evaluate_model(m2, combined$test)
res_eval_3 <- evaluate_model(m3, combined$test)
res_eval_4 <- evaluate_model(m4, combined$test)

# Plot AUCs ROC & PRC
n_classes <- length(unique(as.numeric(combined$test$quality)-1))
models <- list(nb_model=m1, dt_model=m2, svm_model=m3, nn_model=m4)
plot_roc_and_prc_all(combined$test, models, n_classes)
