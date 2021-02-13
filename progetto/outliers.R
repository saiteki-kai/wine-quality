#' Outliers Analysis
#'
#' add description
#'

#' Plots all points distinguishing outliers from non-outliers
#' and allows you to hide them through the \code{hide.outliers} parameter.
#' Outliers are identified through the \code{detect_outliers} function.
#'
#' @param dataset a dataset
#' @param attribute an attribute name
#' @param label an attribute name indicating the label
#' @param hide.outliers a logical indicating whether outliers should be hidden
#' @return the plot
.plot_scatter <- function(dataset, attribute, label, hide.outliers = FALSE) {
  values <- dataset[[attribute]]

  bounds <- detect_outliers(values)
  is_outlier <- values < bounds$lower | values > bounds$upper

  dataset$outlier <- FALSE
  dataset[is_outlier,]$outlier <- TRUE

  dataset %>%
    filter(!is_outlier | !hide.outliers) %>%
    ggplot(aes_string(x = label, y = attribute)) +
    geom_jitter(aes(colour = outlier), alpha = 0.3) +
    ggtitle(paste(ifelse(hide.outliers, "Without", "With"), "Outliers")) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(.99, .99),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
}

#'
.plot_hist_and_boxplot <- function(dataset, attribute) {
  values <- dataset[[attribute]]
  outlier_bounds <- detect_outliers(values)

  min_attribute <- min(values)
  max_attribute <- max(values)

  x_scale <- scale_x_continuous(limits = c(min_attribute, max_attribute), breaks = pretty(values, n = 5))

  boxplot <- dataset %>%
    ggplot(aes_string(x = attribute)) +
    geom_boxplot(aes(y = ""), outlier.shape = "cross") +
    geom_vline(xintercept = outlier_bounds$lower, linetype = 'dotted') +
    geom_vline(xintercept = outlier_bounds$upper, linetype = 'dotted') +
    labs(y = NULL) +
    x_scale +
    theme(axis.text.y = element_blank())

  histogram <- dataset %>%
    ggplot(aes_string(x = attribute), show.legend = TRUE) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
    geom_vline(xintercept = outlier_bounds$lower, linetype = 'dotted') +
    geom_vline(xintercept = outlier_bounds$upper, linetype = 'dotted') +
    annotate(
      x = outlier_bounds$lower,
      y = +Inf,
      label = "\nQ1 - 1.5 * IQR",
      hjust = 2,
      angle = 90,
      geom = "text"
    ) +
    annotate(
      x = outlier_bounds$upper,
      y = +Inf,
      label = "\nQ3 + 1.5 * IQR",
      hjust = 2,
      angle = 90,
      geom = "text"
    ) +
    labs(y = "count", x = NULL) +
    x_scale +
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

# ---------------------------------------------------------------------------------------------------------------------

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, naniar, patchwork)

# Import local functions
source("./utils.R")

dataset <- read.csv("./dataset/winequality-combined.csv")
dataset$quality <- factor(dataset$quality) # IMPORTANT

# Print histogram and boxplot for each attribute
for (attribute in names(dataset)) {
  if (is.numeric(dataset[[attribute]])) {
    p <- .plot_hist_and_boxplot(dataset, attribute)
    print(p)
  }
}

# Print scatter plot for each attribute with and without outliers
for (label in c("quality", "type")) {
  for (attribute in names(dataset)) {
    if (is.numeric(dataset[[attribute]])) {
      p <- .plot_scatter(dataset, attribute, label) +
        .plot_scatter(dataset, attribute, label, hide.outliers = TRUE)
      print(p)
    }
  }
}

# Visualize the number of outliers per attribute
dataset <- read.csv("./dataset/winequality-combined.csv")
dataset <- preprocess_dataset(dataset, 1)
dataset <- remove_outliers(dataset)
gg_miss_var(dataset, quality)
gg_miss_fct(dataset, quality)
