#' Train
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, MLmetrics, kernlab, dplyr)

# Source scripts
source("./utils.R")
source("./config.R")

# Local functions

#' Train the model with the tuning specified on the trainset.
#' 5-fold cross validation and 5 repetitions are used.
#' The tuning optimizes the AUC of the precision-recall curve.
#'
#' @param trainset a dataset
#' @param model_name the model name of caret
#' @param tune_grid a tune grid
#' @param tune_length number of parameter for the tuning (default 3)
#'
#' @return the trained model
.train_model <- function(trainset, model_name, tune_grid = NULL, tune_length = 3) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, doParallel)

  folds <- 5
  repeats <- 5
  grid_size <- ifelse(is.null(tune_grid), tune_length, prod(dim(tune_grid)))

  # Set seed for repeatability
  set.seed(444)
  seeds <- vector(mode = "list", length = (folds * repeats) + 1)
  for (i in 1:(length(seeds) - 1)) seeds[[i]] <- sample.int(n = 1000, grid_size)
  seeds[[length(seeds)]] <- sample.int(1000, 1)

  # 5-Fold cross validation
  train_control <- trainControl(
    method = "repeatedcv",
    repeats = repeats,
    classProbs = TRUE,
    summaryFunction = prSummary,
    seeds = seeds,
    index = createMultiFolds(trainset$quality, folds, repeats),
    allowParallel = TRUE
  )

  # Register parallel processing
  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)

  # Train the model
  model <- train(
    quality ~ .,
    data = trainset,
    method = model_name,
    tuneGrid = tune_grid,
    tuneLength = tune_length,
    metric = "AUC",
    trControl = train_control
  )

  # Stop using parallel computing
  stopCluster(cluster)

  # Print train time
  print(paste0(round(model$times$everything["elapsed"], 4), "s"))

  # Return
  model
}

#' Compute parameters for the preprocessing
#'
#' @param trainset the training set
#' @param type the preprocessing type
#'
#' @return the preProcess object
.pre_proc <- function(trainset, type) {
  data <- trainset[, -ncol(trainset)]

  if (type == "pca") {
    pre_proc <- preProcess(data,
      method = c("center", "scale", "pca"), thresh = 0.95
    )
  } else if (type == "z-score") {
    pre_proc <- preProcess(data, method = c("center", "scale"))
  } else if (type == "min-max") {
    pre_proc <- preProcess(data, method = "range")
  } else {
    stop("`type` should be either `pca`, `z-score` or `min-max`")
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

# Load Dataset
trainset <- read.csv("../data/winequality-train.csv") %>%
  mutate(quality = factor(quality))

# Remove Outliers
if (!keep_outliers) {
  trainset <- remove_outliers_iqr(trainset)
}

# Subsampling
if (subsample) {
  trainset <- .subsampling(trainset, "SMOTE")
}

for (preproc_type in preproc_types) {
  print(paste("Pre-Processing: ", preproc_type))

  # Apply pre-processing
  pre_proc <- .pre_proc(trainset, preproc_type)
  transformed <- predict(pre_proc, trainset[, -ncol(trainset)])
  transformed$quality <- trainset$quality

  for (model in models) {
    print(paste0("training ", model$name, "..."))

    if (!is.null(model$tune_grid$scale)) {
      scale <- 1 / (ncol(transformed) - 1)
      model$tune_grid$scale < scale
    }

    # Train model
    m <- .train_model(transformed, model$name,
      tune_grid = model$tune_grid,
      tune_length = model$tune_length
    )
    # Add the (caret) preProcess object in the model
    m$preProcess <- pre_proc

    # Save model
    create_dir_if_not_exists(outputs_path)
    saveRDS(m,
      file = file.path(
        outputs_path,
        paste0(model$name, "_", preproc_type, ".RDS")
      )
    )

    # m <- .get_model(model$name, preproc_type)

    if (!is.null(model$tune_grid) || !is.null(model$tune_length)) {
      # Get the best tune
      best <- sprintf("%s: %s", names(m$bestTune), m$bestTune)
      best <- paste(best, collapse = ", ")

      # Plot tuning parameters
      plot <- ggplot(m) +
        ggtitle(paste(model$name, preproc_type, best, sep = " - ")) +
        theme(plot.title = element_text(hjust = 0.5))

      create_dir_if_not_exists(tuning_path)
      print_or_save(plot,
        filename = file.path(tuning_path, paste0(model$name, "_", preproc_type, ".png")),
        save = TRUE,
        wide = TRUE
      )
    }
  }
}
