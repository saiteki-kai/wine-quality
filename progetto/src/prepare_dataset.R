# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("./utils.R")

#' Compute parameters for the preprocessing
#'
#' @param trainset the training set
#' @param name the preprocessing method's name
#'
#' @return the preProcess object
.pre_proc <- function(trainset, name) {
  data <- trainset[, -ncol(trainset)]

  if (name == "pca") {
    pre_proc <- preProcess(data,
      method = c("center", "scale", "pca"), thresh = 0.9
    )
  } else if (name == "z-score") {
    pre_proc <- preProcess(data, method = c("center", "scale"))
  } else if (name == "min-max") {
    pre_proc <- preProcess(data, method = "range")
  } else {
    stop("`method` should be either `pca`, `z-score` or `min-max`")
  }

  pre_proc
}

#' Compute subsampling
#'
#' @param trainset the training set
#' @param method the subsampling method's name
#'
#' @return the subsampled trainset
.subsampling <- function(trainset, method) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(DMwR, ROSE)

  set.seed(444)

  if (method == "down") {
    res <- downSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
    names(res)[names(res) == "Class"] <- "quality"
  }
  else if (method == "up") {
    res <- upSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
    names(res)[names(res) == "Class"] <- "quality"
  }
  else if (method == "SMOTE") {
    res <- SMOTE(quality ~ ., trainset, perc.over = 100, perc.under = 200)
  } else {
    res <- ROSE(quality ~ ., data = trainset)$data
  }

  res
}


.apply_scaling <- function(data, scale_type='z-score', pre_proc=NULL) {
  if (pre_proc != NULL) {
    trasformed <- predict(pre_proc, data[, -ncol(data)])
    trasformed$quality <- data$quality
  } else {
    pre_proc <- .pre_proc(data, scale_type)
    trasformed <- predict(pre_proc, data[, -ncol(data)])
    trasformed$quality <- data$quality
  }
  list(data=trasformed, pre_proc=pre_proc)
}


.apply_pre_proc <- function(trainset, testset, pre_proc, scale_func, keep_outliers, balanced) {

  # remove outliers if needed
  trainset <- ifelse(keep_outliers, trainset, remove_outliers(trainset, 'IQR'))

  # apply SMOTE if needed
  trainset <- ifelse(balanced, .subsampling(trainset, 'SMOTE'), trainset)

  # apply scaling if needed
  res1 <- ifelse(is_empty(scale_func), trainset, .apply_scaling(trainset, scale_func, NULL))
  res2 <- ifelse(is_empty(scale_func), testset, .apply_scaling(testset, scale_func, res1$pre_proc))

  out <- list(
    trainset = res1$data,
    testset = res2$data,
    pre_proc = res1$pre_proc
  )

  # Save dataset
  saveRDS(out, file = "../data/preprocessed.RDS")
}

#' Dataset PreProcess Config:
#' dataset_path: the path where the dataset is stored
#' label_cfg: a number of relabaling configuration (1,2 or 3)
#' keep_outliers: a boolean value wheter or not to keep in trainset outliers.
#' balanced: a boolean value iff TRUE SMOTE is applied to balance the trainset
#' scale_func: a string rapresenting the name of the scaling function ('z-score', 'min-max', 'pca')
#' split_perc: the percentage of split for trainset and testset
#' seed: the number to set for the dataset split

dataset_path <- "../data/winequality-combined.csv"
label_cfg <- 1
keep_outliers <- FALSE
balanced <- FALSE
scale_func <- 'z-score'
split_perc <- 0.7
seed <- 444

# Load the dataset
dataset <- read.csv(dataset_path)

# Setup quality
dataset <- relabeling(dataset, label_cfg)
dataset$type <- NULL

# Create Partition
set.seed(seed)
index <- createDataPartition(dataset$quality, p = split_perc, list = FALSE)
trainset <- dataset[index, ]
testset <- dataset[-index, ]

#write.csv(trainset, "../data/winequality-train.csv", row.names = FALSE)
#write.csv(testset, "../data/winequality-test.csv", row.names = FALSE)

# Apply PreProcess
out <- .apply_pre_proc(trainset, testset, scale_func, keep_outliers, balanced)
# out <- train_transformed, test_transformed, pre_proc
