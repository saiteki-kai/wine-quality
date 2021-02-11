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
  data %>%
    ggplot(aes_string(y = input, x = class, fill = class, color = class)) +
    geom_boxplot(outlier.shape = "cross")
}

.plot_class_barplot <- function(data, class) {
  data %>%
    ggplot(aes_string(x = class, fill = class, color = class)) +
    geom_bar()
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork)

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

# Distribuzione Dati Red & White Configurazione 1: Multiclasse

# Distribuzione Dati Red & White Configurazione 2: 3 Classi
# ...

# Distribuzione Dati Red & White Configurazione 3: 2 Classi
# ...


# Boxplot delle singole covariate
.plot_variable_boxplot(redwine, "fixed.acidity", "quality")
.plot_variable_boxplot(redwine, "volatile.acidity", "quality")
.plot_variable_boxplot(redwine, "citric.acid", "quality")
.plot_variable_boxplot(redwine, "residual.sugar", "quality")
.plot_variable_boxplot(redwine, "chlorides", "quality")
.plot_variable_boxplot(redwine, "free.sulfur.dioxide", "quality")
.plot_variable_boxplot(redwine, "total.sulfur.dioxide", "quality")
.plot_variable_boxplot(redwine, "density", "quality")
.plot_variable_boxplot(redwine, "pH", "quality")
.plot_variable_boxplot(redwine, "sulphates", "quality")
.plot_variable_boxplot(redwine, "alcohol", "quality")

# Distribuzione delle singole covariate
.plot_variable_by_class(combined, "pH", "quality")

# Plot correlation matrix
corrplot.mixed(cor(combined[names(combined) != "quality"]), tl.pos = "lt", tl.cex = .8, number.cex = .8)

# PCA Analysis
# Ripetere i plot dopo aver fatto pca
# ...
