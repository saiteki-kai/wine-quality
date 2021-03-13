#' Train
#'
#'
#'

.train_model <- function(trainset, model_name, tune_grid = NULL) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, doParallel)

  folds <- 10
  repeats <- 3
  grid_size <- prod(dim(tune_grid))

  # Set seed for repeatability
  set.seed(444)
  seeds <- vector(mode = "list", length = (folds * repeats) + 1)
  for (i in 1:(length(seeds) - 1)) seeds[[i]] <- sample.int(n = 1000, grid_size)
  seeds[[length(seeds)]] <- sample.int(1000, 1)

  # 10-Fold cross validation
  train_control <- trainControl(
    method = "repeatedcv",
    repeats = repeats,
    classProbs = TRUE,
    summaryFunction = prSummary, #twoClassSummary,
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
    metric = "Precision", #ROC
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

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("./utils.R")
source("./config.R")

# Load Dataset
trainset <- read.csv("../data/winequality-train.csv") %>%
  mutate(quality = factor(quality))

# Remove Outliers
if (keep_outliers) {
  trainset <- trainset %>%
    lapply(function(x) {
      if (is.numeric(x)) {
        treat_outliers(x, method = "IQR")
      } else {
        x
      }
    }) %>%
    as.data.frame()
}

# Subsampling
if (subsample) {
  trainset <- .subsampling(trainset, "SMOTE")
}

for (method in pre_proc_methods) {
  print(paste("Method: ", method))

  # Apply pre-processing
  pre_proc <- .pre_proc(trainset, method)
  trasformed <- predict(pre_proc, trainset[, -ncol(trainset)])
  trasformed$quality <- trainset$quality

  for (model in models) {
    print(paste0("training ", model$name, "..."))

    # Train model
    m <- .train_model(trasformed, model$name, model$tune_grid)
    # Add the (caret) preProcess object in the model
    m$preProcess <- pre_proc

    # Save model
    saveRDS(m,
      file = file.path(outputs_path, paste0(model$name, "_", method, ".RDS"))
    )

    # m <- .get_model(model$name, method)

    # Get the best tune
    best <- sprintf("%s: %s", names(m$bestTune), m$bestTune)
    best <- paste(best, collapse = ", ")

    # Plot tuning parameters
    plot <- ggplot(m) +
      ggtitle(paste(model$name, method, best, sep = " - ")) +
      theme(plot.title = element_text(hjust = 0.5))

    print_or_save(plot,
      filename = file.path(tuning_path, paste0(model$name, "_", method, ".png")),
      save = TRUE,
      wide = TRUE
    )
  }
}
