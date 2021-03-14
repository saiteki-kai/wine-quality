#' Config
#'
#' This file contains the configuration for train and test.
#'

# Global Parameters ------------------------------------------------------------

keep_outliers <- TRUE
subsample <- FALSE

#  Paths -----------------------------------------------------------------------

tuning_path <- file.path(
  "../plots/tuning",
  ifelse(keep_outliers, "outliers", "no-outliers")
)

outputs_path <- file.path(
  "../output",
  ifelse(keep_outliers, "outliers", "no-outliers")
)

roc_path <- file.path(
  "../plots/roc",
  ifelse(keep_outliers, "outliers", "no-outliers")
)

# Tuning parameters ------------------------------------------------------------

# degree:   The degree of the polynomial kernel function.
#           This has to be an positive integer.
#
# scale:    The scaling parameter of the polynomial kernel is a convenient way of
#           normalizing patterns without the need to modify the data itself
#
# C:        The offset used in a polynomial kernel
#
# sigma:    The inverse kernel width used by the Gaussian kernal
#
# maxdepth: The max depth of the tree
#
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/dots

C <- c(0.1, 0.5, 1, 1.5, 2) # seq(0.25, 2, 0.25)

grid_linear <- expand.grid(C = C)
grid_radial <- expand.grid(sigma = c(0.1, 0.5, 1), C = C)
grid_poly <- expand.grid(degree = 1:6, scale = 1, C = C)
grid_tree <- expand.grid(maxdepth = 2:10)

# ------------------------------------------------------------------------------

models <- list(
  rpart2 = list(name = "rpart2", tune_grid = grid_tree),
  svmLinear = list(name = "svmLinear", tune_grid = grid_linear),
  svmRadial = list(name = "svmRadial", tune_grid = grid_radial)
  # list(name = "svmPoly", tune_grid = grid_poly)
)

preproc_types <- list("pca", "z-score")

# ------------------------------------------------------------------------------
