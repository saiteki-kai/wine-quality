# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, dplyr)

# Local functions
source("./utils.R")
source("./config.R")

# Load Dataset
trainset <- read.csv("../data/winequality-train.csv") %>%
  mutate(quality = factor(quality))

# Remove Outliers
if (!keep_outliers) {
  trainset <- trainset %>%
    lapply(function(x) {
      if (is.numeric(x)) {
        treat_outliers(x, method = "IQR", outlier.rm = TRUE)
      } else {
        x
      }
    }) %>%
    as.data.frame() %>%
    na.omit()
}

# Subsampling
if (subsample) {
  trainset <- .subsampling(trainset, "SMOTE")
}

for (preproc_type in preproc_types) {
  print(paste("Pre-Processing: ", preproc_type))

  # Apply pre-processing
  pre_proc <- .pre_proc(trainset, preproc_type)
  trasformed <- predict(pre_proc, trainset[, -ncol(trainset)])
  trasformed$quality <- trainset$quality

  for (model in models) {
    print(paste0("training ", model$name, "..."))

    # Train model
    m <- .train_model(trasformed, model$name,
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
