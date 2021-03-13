
# Train Paths ------------------------------------------------------------------

tuning_path <- file.path(
  "../plots/tuning",
  ifelse(remove_outliers, "outliers", "no-outliers")
)

results_path <- file.path(
  "../output",
  ifelse(remove_outliers, "outliers", "no-outliers")
)

# Test Paths -------------------------------------------------------------------

log_path <- "../output"
roc_path <- "../plots/roc"
model_path <- "../output"

# Tuning parameters ------------------------------------------------------------

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

# Global parameters ------------------------------------------------------------

remove_outliers <- FALSE
subsample <- FALSE

models <- list(
  rpart2 = list(name = "rpart2", tune_grid = grid_tree),
  svmLinear = list(name = "svmLinear", tune_grid = grid_linear),
  svmRadial = list(name = "svmRadial", tune_grid = grid_radial)
  # list(name = "svmPoly", tune_grid = grid_poly)
)

pre_proc_methods <- list("pca", "z-score")
