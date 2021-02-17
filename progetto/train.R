#'
#'
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, dplyr)

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/nb.R")
source("models/svm.R")
source("models/nn.R")

.train <- function(preprocess) {
  # Prepare the dataset
  dataset <- read.csv("./dataset/winequality-train.csv") %>%
    mutate(quality = factor(quality)) %>%
    lapply(function(x) {
      if (is.numeric(x)) {
        treat_outliers(x, method = 'IQR') #
      } else { x }
    }) %>%
    as.data.frame() %>%
    downsampling()

  # 10-Fold cross validation
  tr_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    preProcOptions = list(thresh = 0.9),
    allowParallel = TRUE
  )

  # Register parallel processing
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)

  # Train the models
  m1 <- nb_classification(dataset, preprocess, tr_control)
  m2 <- dt_classification(dataset, preprocess, tr_control)
  m3 <- svm_classification(dataset, preprocess, tr_control)
  m4 <- nn_classification(dataset, preprocess, tr_control)

  # Stop using parallel computing
  stopCluster(cluster)
}

#method <- c("center", "scale", "pca")
#method <- "range"
method <- c("center", "scale")

.train(preprocess = method)
