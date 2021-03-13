#' Train
#'
#'
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret)

# Local functions
source("./utils.R")
source("./prepare_dataset.R")


# Tuning parameters
# degree: The degree of the polynomial kernel function. This has to be an positive integer.
# scale: The scaling parameter of the polynomial kernel is a convenient way of normalizing patterns without the need to modify the data itself
# C: The offset used in a polynomial kernel
# sigma: The inverse kernel width used by the Gaussian kernal
# maxdepth: The max depth of the tree
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-27/topics/dots

C <- c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9, 1, 1.2, 1.5) # seq(0.25, 2, 0.25)
grid_linear <- expand.grid(C = C)
grid_radial <- expand.grid(sigma = c(0.2, 0.5, 0.8), C = C)
grid_poly <- expand.grid(degree = 1:6, scale = 1, C = C)
grid_tree <- expand.grid(maxdepth = 2:10)

models <- list(
  list(name = "rpart2", tune_grid = grid_tree),
  list(name = "svmLinear", tune_grid = grid_linear),
  list(name = "svmRadial", tune_grid = grid_radial),
  list(name = "svmPoly", tune_grid = grid_poly)
)

scale_methods <- c("pca", "z-score", "min-max")


# Read trainset & testset
trainset <- read.csv("../data/winequality-train.csv")
testset <- read.csv("../data/winequality-test.csv")

for (method in scale_methods) {
  config <- apply_pre_proc(trainset, testset,
                      scale_method=method,
                      keep_outliers=TRUE,
                      balanced=FALSE, save=FALSE)

  for (model in models) {
    train_model(config, model$name, model$tune_grid)
  }
}
