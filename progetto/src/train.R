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

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("./utils.R")


#' Train Model Config:
#' @param rds an object rds that contains the data preprocessed
#' @param model_name caret model name
#' @param tune_grid a set of parameters to use for grid search
train_model <- function(rds, model_name, tune_grid) {

  print(paste0("training ", model_name, "..."))

  # Train model
  m <- .train_model(rds$trainset, model_name, tune_grid)
  m$preProcess <- rds$pre_proc

  # Save model
  saveRDS(m, file = paste0("../output/", model_name, "_", rds$scale_method, ".RDS"))

  # Get the best tune
  best <- sprintf("%s: %s", names(m$bestTune), m$bestTune)
  best <- paste(best, collapse = ", ")

  # Plot tuning parameters
  plot <- ggplot(m) +
    ggtitle(paste(model_name, rds$scale_method, best, sep = " - ")) +
    theme(plot.title = element_text(hjust = 0.5))

  tuning_path <- "../plots/tuning"
  print_or_save(plot,
    filename = file.path(tuning_path, paste0(model_name, "_", rds$scale_method, ".png")),
    save = TRUE,
    wide = TRUE
  )

  m
}
