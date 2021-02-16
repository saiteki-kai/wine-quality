.train <- function(outliers_method = NULL, scale_method, pca = FALSE) {
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
    preprocess_dataset(1) %>%
    remove_outliers() %>%
    na.omit() %>%
    partition_dataset()

  if (pca) {
    # Feature Selection
    combined$train <- feature_selection_pca(combined$train)
    combined$test <- feature_selection_pca(combined$test)
  } else {
    # Normalize the data
    method <- scale_method
    combined$train <- normalize_dataset(combined$train, method)
    combined$test <- normalize_dataset(combined$test, method)
  }


  # Register parallel processing
  cores <- detectCores()
  registerDoParallel(cores = cores)
  cluster <- makeCluster(cores)

  # Train the models
  m1 <- nb_classification(combined$train)
  #m2 <- dt_classification(combined$train)
  m3 <- svm_classification(combined$train)
  #m4 <- nn_classification(combined$train)

  # Stop using parallel computing
  stopCluster(cluster)

  # Evaluate the model
  res1 <- evaluate_model(m1$model, combined$test)
  #res2 <- evaluate_model(m2$model, combined$test)
  res3 <- evaluate_model(m3$model, combined$test)
  #res4 <- evaluate_model(m4$model, combined$test)

  # Write Logs
  write_log("nb", res1$cm, m1$train_time, res1$pred_time)
  #write_log("dt", res2$cm, m2$train_time, res2$pred_time)
  write_log("svm", res3$cm, m3$train_time, res3$pred_time)
  #write_log("nn", res4$cm, m4$train_time, res4$pred_time)
}

outliers_method <- NULL
scale_method <- "z_score" #"min_max"
pca <- FALSE
.train(outliers_method, scale_method = scale_method, pca = TRUE)

# Feature Selection 1
#combined$train$alcohol <- NULL
#combined$train$density <- NULL
#combined$train$volatile.acidity <- NULL
#combined$train$chlorides <- NULL
#combined$train$residual.sugar <- NULL
#combined$train$sulphates <- NULL
#combined$train$citric.acid <- NULL
#combined$train$pH <- NULL
#combined$train$free.sulfur.dioxide <- NULL
#combined$train$total.sulfur.dioxide <- NULL

#combined$test$alcohol <- NULL
#combined$test$density <- NULL
#combined$test$volatile.acidity <- NULL
#combined$test$chlorides <- NULL
#combined$test$residual.sugar <- NULL
#combined$test$sulphates <- NULL
#combined$test$citric.acid <- NULL
#combined$test$pH <- NULL
#combined$test$free.sulfur.dioxide <- NULL
#combined$test$total.sulfur.dioxide <- NULL

# Feature Selection 2
#combined$train <- calculate_pca(combined$train)
#combined$test <- calculate_pca(combined$test)

"highlyCorrelated <- findCorrelation(cor(combined$train[names(combined$train) != 'quality']), cutoff=0.5)
print(highlyCorrelated)

importance <- varImp(m3, scale=FALSE)
plot(importance)"