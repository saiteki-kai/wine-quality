#' Exploratory Data Analysis
#'
#' add description.
#'

.global_distribution <- function(data, attribute) {
  p <- data %>% ggplot(aes_string(x = attribute, title = attribute)) +
    geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
    geom_freqpoly(color = "blue", bins = 40)
  print(p)
}

.class_distribution <- function(data, attribute, ymax) {
  p1 <- data %>%
    filter(quality == 0) %>%
    ggplot(aes_string(x = attribute)) +
    geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
    ylim(0, ymax) +
    ggtitle(paste(attribute, "quality 0"))

  p2 <- data %>%
    filter(quality == 1) %>%
    ggplot(aes_string(x = attribute)) +
    geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
    ylim(0, ymax) +
    ggtitle(paste(attribute, "quality 1"))

  print(p1 + p2)
}

.type_barplot <- function(data, color, title) {
  data %>% ggplot(aes(x = quality)) +
    geom_bar(alpha = 0.7, color = "black", fill = color) +
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
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork, factoextra)

# Local functions
source("./utils.R")

# Import datasets
combined <- read.csv("./dataset/winequality-combined.csv")
redwine <- read.csv("./dataset/winequality-red.csv")
whitewine <- read.csv("./dataset/winequality-white.csv")

# Summary report
summary(combined)

# Check Missing Values
miss_var_summary(combined)

#red AND white wine quality by different class
#combined separate by two class of quality
config1 <- preprocess_dataset(combined, 1)

#combined separate by three class of quality
config2 <- preprocess_dataset(combined, 2)


#red and white wine quality
p1 <- .combined_barplot(combined, "red and white quality")
p2 <- .combined_barplot(config1, "quality by two class")
p3 <- .combined_barplot(config2, "quality by three class")

print((p1 + p3 + p2) + plot_layout(guides = "collect"))

#red OR white wine quality by different class
#red and white wine separate by two class
config1_redwine <- preprocess_dataset(redwine, 1)
config1_whitewine <- preprocess_dataset(whitewine, 1)

#red and white wine separate by three class
config2_redwine <- preprocess_dataset(redwine, 2)
config2_whitewine <- preprocess_dataset(whitewine, 2)

p1 <- .type_barplot(redwine, "#FF6666", "red wine")
p2 <- .type_barplot(config1_redwine, "#FF6666", "red wine by two class")
p3 <- .type_barplot(config2_redwine, "#FF6666", "red wine by three class")

p4 <- .type_barplot(whitewine, "#FFFFFF", "white wine")
p5 <- .type_barplot(config1_whitewine, "#FFFFFF", "white wine by two class")
p6 <- .type_barplot(config2_whitewine, "#FFFFFF", "white wine by three class")

print((p1 + p3 + p2) / (p4 + p6 + p5))

ymax <- data.frame(
  fixed.acidity = 800,
  volatile.acidity = 800,
  citric.acid = 1100,
  residual.sugar = 1300,
  chlorides = 1700,
  free.sulfur.dioxide = 900,
  total.sulfur.dioxide = 600,
  density = 900,
  pH = 500,
  sulphates = 500,
  alcohol = 400
)

for (i in select(combined, -c("type", "quality")) %>% names()) {
  if (is.numeric(combined[[i]])) {
    .global_distribution(combined, i)
    .class_distribution(config1, i, ymax[[i]])
  }
}
#
# source("./outliers.R")
# source("./multivariate.R")
