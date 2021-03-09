#' Train
#'
#'
#'

.train_model <- function(trainset, method, pre_proc_method, tune_grid = NULL) {
  # Install packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(caret, doParallel)

  # Set seed for repeatability
  set.seed(6314)

  # Apply transformations
  cols <- ncol(trainset)
  pre_proc <- .pre_proc(trainset, pre_proc_method)
  trasformed <- predict(pre_proc, trainset[, -cols])
  trasformed$quality <- trainset$quality

  # 10-Fold cross validation
  train_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 1,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    allowParallel = TRUE
  )

  # Register parallel processing
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)

  # Train the model
  model <- train(
    quality ~ .,
    data = trasformed,
    method = method,
    tuneGrid = tune_grid,
    metric = "ROC",
    trControl = train_control
  )

  # Stop using parallel computing
  stopCluster(cluster)

  # Print train time
  print(model$times)

  # Save the model
  model$preProcess <- pre_proc
  saveRDS(model,
    file = paste0("../output/", method, "_", pre_proc_method, ".RDS")
  )

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

  set.seed(9560)

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

grid_radial <- expand.grid(
  sigma = c(0.1, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4),
  C = c(0.1, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)
)
grid_linear <- expand.grid(
  C = c(0.1, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)
)
grid_poly <- expand.grid(
  degree = 1:4,
  scale = 1,
  C = c(0.1, 1, 5) # 100, 1000)
)
grid_tree <- expand.grid(maxdepth = 2:10)

tuning_path <- "../plots/tuning"

models <- list(
  # list(name = "rpart2", tune_grid = grid_tree),
  # list(name = "svmLinear", tune_grid = grid_linear),
  # list(name = "svmRadial", tune_grid = grid_radial),
  list(name = "svmPoly", tune_grid = grid_poly)
)

# Train the models
for (method in c("pca", "z-score", "min-max")) {
  print(paste("Method:", method))

  for (model in models) {
    print(paste0("training ", model$name), "...")

    m <- .train_model(trainset, model$name, method, model$tune_grid)
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
