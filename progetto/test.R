 .get_model <- function(name) {
  filename <- file.path("./results/models", paste0(name, "_model.RDS"))
  if (file.exists(filename)) {
    readRDS(filename)
  }
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, doParallel, ggplot2, grid, precrec, factoextra, checkmate, multiROC, dummies, mlbench, dplyr)

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/nb.R")
source("models/svm.R")
source("models/nn.R")

# Prepare the dataset
dataset <- read.csv("./dataset/winequality-test.csv") %>%
  mutate(quality = factor(quality))

# Feature Selection 1
#dataset$alcohol <- NULL
#dataset$density <- NULL
#dataset$volatile.acidity <- NULL
#dataset$chlorides <- NULL
#dataset$residual.sugar <- NULL
#dataset$sulphates <- NULL
#dataset$fixed.acidity <- NULL
#dataset$citric.acid <- NULL
#dataset$pH <- NULL
#dataset$free.sulfur.dioxide <- NULL
#dataset$total.sulfur.dioxide <- NULL

m1 <- .get_model("nb")
m2 <- .get_model("dt")
m3 <- .get_model("svm")
m4 <- .get_model("nn")

# Evaluate the model
res1 <- evaluate_model(m1, dataset)
res2 <- evaluate_model(m2, dataset)
res3 <- evaluate_model(m3, dataset)
res4 <- evaluate_model(m4, dataset)

# Write Logs
write_log("nb", res1$cm, res1$pred_time)
write_log("dt", res2$cm, res2$pred_time)
write_log("svm", res3$cm, res3$pred_time)
write_log("nn", res4$cm, res4$pred_time)

# Plot AUCs ROC & PRC
models <- list(nb_model = m1, dt_model = m2, svm_model = m3, nn_model = m4)
#models <- list(nb_model = m1, dt_model = m2, svm_model = m3)
plot_roc_and_prc_all(dataset, models)
