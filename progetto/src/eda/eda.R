#' Exploratory Data Analysis
#'
#' add description.
#'

.global_distribution <- function(data, attribute) {
  p <- data %>% ggplot(aes_string(x = attribute, title = attribute)) +
    geom_density(alpha = 0.2, color = "red", fill = "#FF6666", bins = 40)
    #geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
    #geom_freqpoly(color = "blue", bins = 40)
  #print(p)
}

.class_distribution <- function(data, attribute, target) {
  p <- data %>%
    ggplot(aes_string(x = attribute, group = target, fill = target, color = target)) +
    geom_density(alpha = 0.2, bins = 40) +
    ggtitle(attribute)
  #print(p)
}

.type_barplot <- function(data, color, title) {
  data %>% ggplot(aes(x = quality, fill = quality)) +
    geom_bar(alpha = 0.7, color = "black") +
    ylim(0, 4000) +
    ggtitle(title)
}

.combined_barplot <- function(data, title) {
  data %>% ggplot(aes(x = quality)) +
    scale_fill_manual(values = c("red" = "#FF6666", "white" = "white")) +
    geom_bar(aes(fill = type), position = "dodge2", alpha = 0.7, color = "black") +
    # stat_count(
    #   aes(y = ..count..),
    #   label = y ,
    #   geom = "text",
    #   position = position_dodge(width = 1),
    #   vjust = -0.5, size = 2,
    #   color = "black") +
    ggtitle(title)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, corrplot, naniar, patchwork, dplyr)

# Local functions
source("../utils.R")

# Import datasets
dataset <- read.csv("../../data/winequality-combined.csv")

# create partition
set.seed(444)
index <- createDataPartition(dataset$quality, p = 0.70, list = FALSE)
trainset <- dataset[index, ]

# only white wine
whitewine <- filter(trainset, type == "white")

library(psych)
# Summary report
describe(whitewine)

# Check Missing Values
miss_var_summary(whitewine)

# white wine quality by different class
config0_whitewine <- relabeling(whitewine, 3)

# white wine separate by two class
config1_whitewine <- relabeling(whitewine, 1)

# white wine separate by three class
config2_whitewine <- relabeling(whitewine, 2)


p4 <- .type_barplot(config0_whitewine, "#FFFFFF", "by 10 class")
p5 <- .type_barplot(config1_whitewine, "#FFFFFF", "by two class(x>6)")
p6 <- .type_barplot(config2_whitewine, "#FFFFFF", "by three class(x<5, 5<=x<7, x>=7)")

print(p4 + p6 + p5)

for (i in dplyr::select(whitewine, -"quality") %>% names()) {
  if (is.numeric(whitewine[[i]])) {
    p <- .global_distribution(whitewine, i) +
    .class_distribution(config1_whitewine, i, "quality")
    #.class_distribution(config2_whitewine, i, "quality")
    print(p)
  }
}

# source("./outliers.R")
# source("./multivariate.R")
