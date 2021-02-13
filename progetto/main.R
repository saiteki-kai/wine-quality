# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, dplyr, doParallel, ggplot2, grid, precrec, factoextra)

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

# Calculate PCA
#combined$train <- calculate_pca(combined$train)
#combined$test <- calculate_pca(combined$test)

# Register parallel processing
cores <- detectCores()
registerDoParallel(cores = cores)
cluster <- makeCluster(cores)

# Train the model
m1 <- nb_classification(combined$train)
m2 <- dt_classification(combined$train)
m3 <- svm_classification(combined$train)
m4 <- nn_classification(combined$train)

# Stop using parallel computing
stopCluster(cluster)

# Evaluate the model
res1 <- evaluate_model(m1$model, combined$test)
res2 <- evaluate_model(m2$model, combined$test)
res3 <- evaluate_model(m3$model, combined$test)
res4 <- evaluate_model(m4$model, combined$test)

# Plot AUCs ROC & PRC
plot_roc_and_prc_all(combined$test, list(nb_model=m1$model,
                                         dt_model=m2$model,
                                         svm_model=m3$model,
                                         nn_model=m4$model))

# Write Logs
write_log(m1$model$method, res1$measures, m1$train_time, res1$pred_time)
write_log(m2$model$method, res2$measures, m2$train_time, res2$pred_time)
write_log(m3$model$method, res3$measures, m3$train_time, res3$pred_time)
write_log(m4$model$method, res4$measures, m4$train_time, res4$pred_time)