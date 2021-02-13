#' Exploratory Data Analysis
#'
#' add description.
#'

.plot_variable_by_class <- function(data, input, class) {
  data %>%
    ggplot(aes_string(x = input, fill = class, color = class)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.2)
}

.plot_variable_boxplot <- function(data, input, class) {
  outlier_bounds <- .detect_outliers(data[[input]])

  data %>%
    ggplot(aes_string(y = input, x = class, fill = class)) +
    geom_boxplot(outlier.shape = "cross") +
    # geom_jitter(aes_string(color = class), size = 0.4, alpha = 0.5) +
    geom_hline(yintercept = outlier_bounds$lower, linetype = 'dotted') +
    geom_hline(yintercept = outlier_bounds$upper, linetype = 'dotted') +
    labs(y = input)
}

.plot_class_barplot <- function(data, class) {
  data %>%
    ggplot(aes_string(x = class, fill = class, color = class)) +
    geom_bar()
}

.plot_pca <- function(dataset) {
  normalized <- normalize_dataset(dataset)
  pca <- prcomp(cov(normalized[1:11]))

  # Calculate eigenvalue to check how many PCs we can pick up
  eig <- get_eigenvalue(pca)
  print(eig)
  pca_plot <- fviz_eig(pca, addlabels = TRUE, ncp = 11)
  plot(pca_plot)
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

# Distribuzione Dati Red & White
.plot_class_barplot(combined, "quality")

# Distribuzione Dati Red Only
.plot_class_barplot(redwine, "quality")

# Distribuzione Dati White Only
.plot_class_barplot(whitewine, "quality")

# Distribuzione Dati white only Configurazione 1: Multiclasse
config3 <- preprocess_dataset(whitewine, 3)
.plot_class_barplot(config3, "quality")

# Distribuzione Dati white only Configurazione 2: 3 Classi
config2 <- preprocess_dataset(whitewine, 2)
.plot_class_barplot(config2, "quality")

# Distribuzione Dati white only Configurazione 3: 2 Classi
config1 <- preprocess_dataset(whitewine, 1)
.plot_class_barplot(config1, "quality")

# Plot boxplot
.plot_variable_boxplot(config1, "pH", "quality")

# Distribuzione delle singole covariate
.plot_variable_by_class(config1, "pH", "quality")

# Plot correlation matrix
corrplot.mixed(cor(config1[names(config1) != "quality"]), tl.pos = "lt", tl.cex = .8, number.cex = .8)

# PCA Analysis
.plot_pca(config1)
