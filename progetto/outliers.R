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

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, naniar, patchwork)

# Import local functions
source("./utils.R")

dataset <- read.csv("./dataset/winequality-combined.csv")
dataset$quality <- factor(dataset$quality) # IMPORTANT

# Print a scatter plot for each attribute with and without outliers
for (label in c("quality", "type")) {
  for (attribute in names(dataset)) {
    if (is.numeric(dataset[[attribute]])) {
      p <- .plot_scatter(dataset, attribute, label) +
        .plot_scatter(dataset, attribute, label, hide.outliers = TRUE)
      print(p)
    }
  }
}


# Analyse Outliers
# dataset <- preprocess_dataset(dataset, 1)
dataset <- remove_outliers(dataset)
#gg_miss_var(dataset, quality)
#gg_miss_fct(dataset, quality)
