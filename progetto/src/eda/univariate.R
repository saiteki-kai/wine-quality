#' Univariate
#'
#' add description.
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, corrplot, naniar, patchwork, psych, dplyr)

# Source scripts
source("../utils.R")

# Local functions

.plot_distribution <- function(data, attribute, target = NULL) {
  if (is.null(target)) {
    p <- data %>%
      ggplot(aes_string(x = attribute, group = target)) +
      geom_density(alpha = 0.2, fill = "#ff6666", color = "red")
  } else {
    p <- data %>%
      ggplot(aes_string(x = attribute, group = target, fill = target, color = target)) +
      geom_density(alpha = 0.2)
  }
  p + theme(axis.title.y = element_blank())
}

.plot_distributions <- function(data, target = NULL) {
  p <- .plot_distribution(data, "alcohol", target) +
    .plot_distribution(data, "density", target) +
    .plot_distribution(data, "chlorides", target) +
    .plot_distribution(data, "volatile.acidity", target) +
    .plot_distribution(data, "residual.sugar", target) +
    .plot_distribution(data, "sulphates", target) +
    .plot_distribution(data, "citric.acid", target) +
    .plot_distribution(data, "fixed.acidity", target) +
    .plot_distribution(data, "pH", target) +
    .plot_distribution(data, "free.sulfur.dioxide", target) +
    .plot_distribution(data, "total.sulfur.dioxide", target) +
    plot_layout(guides = "collect")
}

univariate_path <- file.path("..", "..", "plots", "eda", "univariate")
create_dir_if_not_exists(univariate_path)
save <- TRUE

# Load dataset
trainset <- read.csv("../../data/winequality-train.csv")
trainset$quality <- factor(trainset$quality)

# Summary report
s1 <- print_stats(trainset, latex = TRUE)
write(s1, file.path(univariate_path, "train_summary.tex"))

# Check Missing Values
miss_var_summary(trainset)

# Plot Attributes Distribution
p1 <- .plot_distributions(trainset)
p2 <- .plot_distributions(trainset, target = "quality")

print_or_save(p1,
  file.path(univariate_path, "distribution1.png"),
  save = save,
  wide = TRUE
)
print_or_save(p2,
  file.path(univariate_path, "distribution2.png"),
  save = save,
  wide = TRUE
)
