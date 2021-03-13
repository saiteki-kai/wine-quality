#' Test
#'

.get_model <- function(model_name, pre_proc_method) {
  filename <- file.path(outputs_path, paste0(model_name, "_", pre_proc_method, ".RDS"))

  if (file.exists(filename)) {
    return(readRDS(filename))
  }

  NULL
}

#' Write a log file
#' @param model_name model's name
#' @param cm a confusion matrix
#' @param pred_time prediction time
#'
.write_log <- function(model_name, pre_proc_name, cm, pred_time) {
  filename <- file.path(
    outputs_path,
    paste0(model_name, "_", pre_proc_name, ".log")
  )

  write.table(
    paste0("model_name: ", model_name),
    filename,
    row.names = FALSE,
    col.names = FALSE
  )
  write.table(
    paste0("pred_time: ", pred_time),
    filename,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
  write.table(
    capture.output(cm),
    filename,
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
}

#' Plot ROC and precision_recall_curve for all the models specified
#' @param
#' @param
#'
#' @return AUCs for ROC and PRC
.plot_roc_prc <- function(labels, probs, method) {
  models <- names(probs)

  labels <- rep(list(labels), length(probs))
  labels <- join_labels(labels)

  scores <- join_scores(probs)

  mdat <- mmdata(scores, labels,
    modnames = models,
    dsids = as.numeric(as.factor(models))
  )

  sscurves <- evalmod(mdat, mode = "rocprc")

  # Plot ROC and PRC
  png(file.path(roc_path, paste0(method, ".png")),
    units = "in",
    res = 300,
    height = 6.67,
    width = ifelse(TRUE, 13.34, 6.67)
  )
  autoplot(sscurves)
  dev.off()

  # Format AUC
  aucs <- auc(sscurves) %>% dplyr::select("modnames", "curvetypes", "aucs")
  aucs$curvetypes <- factor(aucs$curvetypes, levels = unique(aucs$curvetypes))
  aucs <- aucs %>%
    split(aucs$curvetypes) %>%
    lapply(function(x) {
      x <- dplyr::select(x, "modnames", "aucs")
      names(x) <- c("model", "AUC")
      x
    })

  aucs
}

#' Print all the measures for the model evaluation
#'
#' @param model classification model
#' @param dataset a dataset to predict
#'
#' @return list containing the confusion matrix, the predictions
#'      and the prediction time
.evaluate_model <- function(model, dataset) {
  # Apply pre-processing
  if (!is.null(model$preProcess)) {
    transformed <- predict(model$preProcess, dataset[, -ncol(dataset)])
    transformed$quality <- dataset$quality
    # Remove (caret) preProcess object to avoid applying transformations
    model$preProcess <- NULL
  } else {
    transformed <- dataset
  }

  print(model)

  # Predict
  start_time <- Sys.time()
  probs <- predict(model, newdata = transformed, type = "prob")
  pred <- predict(model, newdata = transformed)
  end_time <- Sys.time()
  time <- end_time - start_time

  # Print confusion matrix
  cm <- confusionMatrix(
    data = pred,
    reference = dataset$quality,
    mode = "prec_recall",
    positive = "good"
  )

  list(cm = cm, probs = probs, pred_time = time)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, ggplot2, grid, precrec, dplyr)

# Local functions
source("./utils.R")
source("./config.R")

# Prepare the dataset
testset <- read.csv("../data/winequality-test.csv")
testset$quality <- factor(testset$quality)

for (method in pre_proc_methods) {
  print(paste("Method: ", method))

  for (model in models) {
    print(paste0("evaluate ", model$name, "..."))

    # Load the model
    mod <- .get_model(model$name, method)
    models[[model$name]]$model <- mod

    # Evaluate the model
    res <- .evaluate_model(mod, testset)
    models[[model$name]]$results <- res

    # Save results
    .write_log(model$name, method, res$cm, res$pred_time)
  }

  # Get predictions
  predictions <- lapply(models, function(x) x$results$probs$good)

  # Plot AUCs ROC & PRC
  aucs <- .plot_roc_prc(testset$quality, predictions, method)
  print(aucs)

  # Resample results
  cv.values <- resamples(lapply(models, function(x) x$model))

  # Model comparison

  # summary(cv.values)
  # print_or_save(dotplot(cv.values, metric = "ROC"),
  #   "../plots/comparison/dotplot.png",
  #   save = TRUE,
  #   wide = TRUE
  # )
  # print_or_save(bwplot(cv.values, layout = c(3, 1)),
  #   "../plots/comparison/bwplot.png",
  #   save = TRUE,
  #   wide = TRUE
  # )
  # print_or_save(splom(cv.values, metric = "ROC"),
  #   "../plots/comparison/splom.png",
  #   save = TRUE,
  #   wide = TRUE
  # )
}

# print(t_test(pred1-pred2, paired = TRUE))

# TODO: save plots and divide subsampled results from non subsampled
# TODO: test statistici
# TODO: rename variables (designation clashs)
