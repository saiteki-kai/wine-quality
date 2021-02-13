#' Exploratory Data Analysis
#'
#' add description.
#'

.plot_outliers <- function(data, input) {
  values <- data[[input]]
  outlier_bounds <- .detect_outliers(values)

  min_input <- min(values)
  max_input <- max(values)

  boxplot <- data %>%
    ggplot(aes_string(x = input)) +
    geom_boxplot(outlier.shape = "cross") +
    geom_vline(xintercept = outlier_bounds$lower, linetype = 'dotted') +
    geom_vline(xintercept = outlier_bounds$upper, linetype = 'dotted') +
    theme(axis.text.y = element_blank()) +
    scale_x_continuous(limits = c(min_input, max_input), breaks = pretty(values, n = 5))

  # MyBreaks <- c(-Inf, outlier_bounds$lower, outlier_bounds$upper, Inf)
  # MyColours <- c("black", "blue", "yellow")
  # data$breaks <- cut(values, MyBreaks)
  # TODO: aggiustare la scala dei breaks

  without_outliers <- data.frame(x = values[values >= outlier_bounds$lower & values <= outlier_bounds$upper])

  histogram <- data %>%
    ggplot(aes_string(x = input), show.legend = TRUE) + # fill = "breaks"
    geom_histogram(aes(y = ..density..), binwidth = 0.025, position = "identity", alpha = 0.5) +
    geom_vline(xintercept = outlier_bounds$lower, linetype = 'dotted') +
    geom_vline(xintercept = outlier_bounds$upper, linetype = 'dotted') +
    geom_density(colour = "blue") +
    geom_density(data = without_outliers, mapping = aes(x = x), colour = "red") +
    # scale_fill_manual(breaks = levels(data$breaks), values = MyColours) +
    labs(y = "count", x = NULL) +
    scale_x_continuous(limits = c(min_input, max_input), breaks = pretty(values, n = 5)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )

  (histogram / boxplot) + plot_layout(heights = c(5, 1), guides = "collect")
}

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

.detect_outliers <- function(data) {
  quantiles <- quantile(data, c(0.25, 0.75), names = FALSE)
  lower <- quantiles[1] - 1.5 * IQR(data)
  upper <- quantiles[2] + 1.5 * IQR(data)

  list(lower = lower, upper = upper)
}

.plot_pca <- function (dataset) {
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


# Check Missing Values
miss_var_summary(combined)

# Plot ouliers
.plot_outliers(config1, "pH")

# Plot boxplot
.plot_variable_boxplot(config1, "pH", "quality")

# Boxplot delle singole covariate
#lapply(combined, function(variable) { # variable is not the column name??
#  .plot_variable_boxplot(combined, variable, "quality")
#})

# Distribuzione delle singole covariate
.plot_variable_by_class(config1, "pH", "quality")


# Plot correlation matrix
corrplot.mixed(cor(config1[names(config1) != "quality"]), tl.pos = "lt", tl.cex = .8, number.cex = .8)


# PCA Analysis
.plot_pca(config1)
