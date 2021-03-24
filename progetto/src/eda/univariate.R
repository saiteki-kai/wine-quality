#' Exploratory Data Analysis
#'
#' add description.
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, corrplot, naniar, patchwork, psych, dplyr)

# Source scripts
source("../utils.R")

# Local functions
.global_distribution <- function(data, attribute) {
  p <- data %>% ggplot(aes_string(x = attribute, title = attribute)) +
    geom_density(alpha = 0.2, color = "red", fill = "#FF6666", bins = 40)
  # geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
  # geom_freqpoly(color = "blue", bins = 40)
  #print(p)
}

.class_distribution <- function(data, attribute, target) {
  p <- data %>%
    ggplot(aes_string(x = attribute, group = target, fill = target, color = target)) +
    geom_density(alpha = 0.2, bins = 40) +
    ggtitle(attribute)
  #print(p)
}

# Import dataset
trainset <- read.csv("../../data/winequality-train.csv")
trainset$quality <- factor(trainset$quality)

# Summary report
describe(trainset)

# Check Missing Values
miss_var_summary(trainset)

# Check Attributes Distribution
.global_distribution(trainset, "alcohol") +
.global_distribution(trainset, "density") +
.global_distribution(trainset, "chlorides") +
.global_distribution(trainset, "volatile.acidity") +
.global_distribution(trainset, "residual.sugar") +
.global_distribution(trainset, "sulphates") +
.global_distribution(trainset, "citric.acid") +
.global_distribution(trainset, "fixed.acidity") +
.global_distribution(trainset, "pH") +
.global_distribution(trainset, "free.sulfur.dioxide") +
.global_distribution(trainset, "total.sulfur.dioxide")

.class_distribution(trainset, "alcohol", "quality") +
.class_distribution(trainset, "density", "quality") +
.class_distribution(trainset, "chlorides", "quality") +
.class_distribution(trainset, "volatile.acidity", "quality") +
.class_distribution(trainset, "residual.sugar", "quality") +
.class_distribution(trainset, "sulphates", "quality") +
.class_distribution(trainset, "citric.acid", "quality") +
.class_distribution(trainset, "fixed.acidity", "quality") +
.class_distribution(trainset, "pH", "quality") +
.class_distribution(trainset, "free.sulfur.dioxide", "quality") +
.class_distribution(trainset, "total.sulfur.dioxide", "quality")


"
for (i in dplyr::select(trainset, -'quality') %>% names()) {
  if (is.numeric(trainset[[i]])) {
    p <- .global_distribution(trainset, i) +
      .class_distribution(trainset, i, 'quality')
    print(p)
  }
}
"
