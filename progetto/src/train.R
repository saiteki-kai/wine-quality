#' Train
#'
#'
#'

.train_model <- function(trainset, model_type, tune_grid = NULL) {
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
    summaryFunction = twoClassSummary,
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
    method = model_type,
    tuneGrid = tune_grid,
    metric = "ROC",
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

# Read trainset
trainset <- read.csv("../data/winequality-train.csv")
trainset$quality <- factor(trainset$quality)

# Subsampling
# trainset <- subsampling(trainset, "SMOTE")

# Tuning parameters
# degree: The degree of the polynomial kernel function. This has to be an positive integer.
# scale: The scaling parameter of the polynomial kernel is a convenient way of normalizing patterns without the need to modify the data itself
# C: The offset used in a polynomial kernel
# sigma: The inverse kernel width used by the Gaussian kernal
# maxdepth: The max depth of the tree
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/dots

C <- c(0.1, 0.5, 1, 1.5, 2) # seq(0.25, 2, 0.25)

grid_linear <- expand.grid(C = C)
grid_radial <- expand.grid(sigma = c(0.1, 0.5, 1), C = C)
grid_poly <- expand.grid(degree = 1:6, scale = 1, C = C)
grid_tree <- expand.grid(maxdepth = 2:10)

tuning_path <- "../plots/tuning"

models <- list(
  list(name = "rpart2", tune_grid = grid_tree),
  list(name = "svmLinear", tune_grid = grid_linear),
  list(name = "svmRadial", tune_grid = grid_radial),
  list(name = "svmPoly", tune_grid = grid_poly)
)

for (method in c("pca", "z-score", "min-max")) {
  print(paste("Method: ", method))

  # Apply pre-processing
  pre_proc <- .pre_proc(trainset, method)
  trasformed <- predict(pre_proc, trainset[, -ncol(trainset)])
  trasformed$quality <- trainset$quality

  for (model in models) {
    print(paste0("training ", model$name, "..."))

    # Train model
    m <- .train_model(trasformed, model$name, model$tune_grid)
    m$preProcess <- pre_proc

    # Save model
    saveRDS(m,
      file = paste0("../output/", model$name, "_", method, ".RDS")
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
