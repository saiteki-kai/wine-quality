#' This script contains the configuration used for train and test scripts.
#' You can specify whether keep outliers or subsample the training set,
#' the models to be trained and their tuning parameters, the pre-processing
#' method to apply on both training and test sets, and the folders for saving
#' results and plots.

# Global Parameters
keep_outliers <- TRUE
subsample <- FALSE

# Folders
subfolder <- ifelse(keep_outliers, "outliers", "no-outliers")

outputs_path <- file.path("..", "output", subfolder)
tuning_path <- file.path("..", "plots", "tuning", subfolder)
roc_path <- file.path("..", "plots", "roc", subfolder)
comparison_path <- file.path("..", "plots", "comparison", subfolder)

# Tuning parameters

# degree:   The degree of the polynomial kernel function.
#           This has to be an positive integer.
#
# scale:    The scaling parameter of the polynomial kernel is a convenient way
#           of normalizing patterns without the need to modify the data itself
#
# C:        The offset used in a polynomial kernel
#
# sigma:    The inverse kernel width used by the Gaussian kernal
#
# maxdepth: The max depth of the tree
#
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/dots

C <- 2^c(0, 1, 2, 3) # c(0.1, 0.5, 1, 1.5, 2) # seq(0.25, 2, 0.25)

grid_linear <- expand.grid(C = C)
grid_radial <- expand.grid(sigma = c(0.01, 0.1, 0.5, 1), C = C)
grid_poly <- expand.grid(degree = 1:3, scale = 1, C = C)
grid_tree <- expand.grid(cp = c(0.001, 0.005, 0.01, 0.05, 0.1))

# Models
models <- list(
  rpart2 = list(name = "rpart2", tune_grid = NULL, tune_length = 10),
  svmLinear = list(name = "svmLinear", tune_grid = grid_linear),
  svmRadial = list(name = "svmRadial", tune_grid = grid_radial)
  # nb = list(name = "nb", tune_grid = NULL, tune_length = 3)
  # svmPoly = list(name = "svmPoly", tune_grid = grid_poly)
  # knn = list(name = "knn", tune_grid = NULL, tune_length = 10)
)

# Pre-processing Types
preproc_types <- list("z-score", "pca")
