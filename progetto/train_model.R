#' Train Model
#'
#' add description.
#'

train_model <- function(trainset, method, pre_proc_method, tune_grid = NULL) {
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
    repeats = 3,
    allowParallel = TRUE
  )

  # Register parallel processing
  cluster <- makeCluster(detectCores())
  registerDoParallel(cluster)

  # Train the model
  start_time <- Sys.time()
  model <- train(
    quality ~ .,
    data = trasformed,
    method = method,
    tuneGrid = tune_grid,
    trControl = train_control
  )
  end_time <- Sys.time()

  # Stop using parallel computing
  stopCluster(cluster)

  # Print train time
  time <- end_time - start_time
  print(paste0("train time: ", time))

  # Save the model
  obj <- list(model = model, train_time = time, pre_proc = pre_proc)
  saveRDS(obj, file = paste0("./results/models/", method, "_", pre_proc_method, ".RDS"))

  # Return
  obj
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
    pre_proc <- preProcess(data, method = c("center", "scale", "pca"), thresh = 0.9)
  } else if (name == "z-score") {
    pre_proc <- preProcess(data, method = c("center", "scale"))
  } else if (name == "min-max") {
    pre_proc <- preProcess(data, method = "range")
  } else {
    stop("`method` should be either `pca`, `z-score` or `min-max`")
  }

  pre_proc
}