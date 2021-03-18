#' Test
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, ggplot2, precrec, dplyr)

# Source scripts
source("./utils.R")
source("./config.R")

# Local functions
.get_model <- function(model_name, preproc_type) {
  filename <- file.path(
    outputs_path,
    paste0(model_name, "_", preproc_type, ".RDS")
  )

  if (file.exists(filename)) {
    return(readRDS(filename))
  }

  NULL
}

#' Write a log file
#' @param model_name model's name
#' @param preproc_type type of preprocessing
#' @param cm a confusion matrix
#' @param pred_time prediction time
#'
.write_log <- function(model_name, preproc_type, cm, pred_time) {
  create_dir_if_not_exists(outputs_path)

  filename <- file.path(
    outputs_path,
    paste0(model_name, "_", preproc_type, ".log")
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

#' Plot ROC and PRC for all the models specified
#'
#' @param labels target labels
#' @param probs list with probabilities of positive class of each model
#' @param preproc_type type of pre-processing
#'
#' @examples
#'
#' ## example of three models and a binary class
#' labels <- c(1, 1, 0, 0)
#' probs <- list(
#'   model1 = c(0.5, 0.6, 0.2, 0.4),
#'   model2 = c(0.1, 0.4, 0.3, 0.2),
#'   model3 = c(0.4, 0.7, 0.8, 0.5)
#' )
#' .plot_roc_prc(labels, probs, "z-score")
#' @return AUCs of ROC and PRC for each model
.plot_roc_prc <- function(labels, probs, preproc_type) {
  models <- names(probs)

  labels <- rep(list(labels), length(probs))
  labels <- join_labels(labels)

  scores <- join_scores(probs)

  mdat <- mmdata(scores, labels,
    modnames = models,
    dsids = as.numeric(as.factor(models))
  )

  sscurves <- evalmod(mdat, mode = "rocprc")

  # Save ROC and PRC
  create_dir_if_not_exists(roc_path)
  png(file.path(roc_path, paste0(preproc_type, ".png")),
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
#' @param dataset a dataset to predict
#' @param model classification model
#'
#' @return list containing the confusion matrix, the predictions and the
#' prediction time
.evaluate_model <- function(dataset, model) {
  # Apply pre-processing
  if (!is.null(model$preProcess)) {
    transformed <- predict(model$preProcess, dataset[, -ncol(dataset)])
    transformed$quality <- dataset$quality
    # Remove (caret) preProcess object to avoid applying transformations
    model$preProcess <- NULL
  } else {
    transformed <- dataset
  }

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

# Prepare the dataset
testset <- read.csv("../data/winequality-test.csv")
testset$quality <- factor(testset$quality)

for (preproc_type in preproc_types) {
  print(paste("Pre-Processing: ", preproc_type))

  for (model in models) {
    print(paste0("evaluate ", model$name, "..."))

    # Load the model
    mod <- .get_model(model$name, preproc_type)
    models[[model$name]]$model <- mod

    # Evaluate the model
    res <- .evaluate_model(testset, mod)
    models[[model$name]]$results <- res

    # Save results
    .write_log(model$name, preproc_type, res$cm, res$pred_time)
  }

  # Get predictions
  predictions <- lapply(models, function(x) x$results$probs$good)

  # Plot AUCs ROC & PRC
  aucs <- .plot_roc_prc(testset$quality, predictions, preproc_type)
  print(aucs)

  # Resample results
  if (length(models) > 1) {
    cv.values <- resamples(lapply(models, function(x) x$model))

    # Model comparison
    summary(cv.values)
    create_dir_if_not_exists(comparison_path)
    print_or_save(dotplot(cv.values, metric = c("Precision", "Recall"), layout = c(1, 2)),
      file.path(comparison_path, paste0(preproc_type, "_dotplot.png")),
      save = TRUE,
      wide = TRUE
    )
    print_or_save(bwplot(cv.values, layout = c(1, 4)),
      file.path(comparison_path, paste0(preproc_type, "_bwplot.png")),
      save = TRUE,
      wide = TRUE
    )
    print_or_save(splom(cv.values, metric = "Precision"),
      file.path(comparison_path, paste0(preproc_type, "_splom.png")),
      save = TRUE,
      wide = TRUE
    )
  }
}

# print(t_test(pred1-pred2, paired = TRUE))
