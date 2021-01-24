## Exploratory Data Analysis

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(corrplot, ggplot2)

# Read the dataset
redwine <- read.csv("dataset/winequality-red.csv")

# Plot correlation matrix
corrplot.mixed(cor(redwine), tl.pos = 'lt', tl.cex = .8, number.cex = .8)

# Add class attribute
redwine$good <- ifelse(redwine$quality > 6, 1, 0)
redwine$good <- factor(redwine$good)

# Summary report
summary(redwine)

plot_variable_by_class <- function(data, input, class) {
  p <- ggplot(data, aes_string(x = input, fill = class, color = class)) +
    geom_histogram(position = "identity", alpha = 0.5)

  # p <- p + geom_vline(data, aes_string(xintercept = mean(input), color = class), linetype = "dashed")

  print(p)
}

plot_variable_boxplot <- function(data, input, class) {
  p <- ggplot(data, aes_string(y = input, x = class, fill = class, color = class)) +
    geom_boxplot(outlier.shape = "cross")

  print(p)
}

plot_class_barplot <- function(data, class) {
  p <- ggplot(data, aes_string(x = class, fill = class, color = class)) +
    geom_bar()

  print(p)
}


plot_class_barplot(redwine, "good")

plot_variable_by_class(redwine, "pH", "good")
plot_variable_boxplot(redwine, "pH", "good")
