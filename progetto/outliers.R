# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork, factoextra)

.detect_outliers <- function(data) {
  quantiles <- quantile(data, c(0.25, 0.75), names = FALSE)
  lower <- quantiles[1] - 1.5 * IQR(data)
  upper <- quantiles[2] + 1.5 * IQR(data)

  list(lower = lower, upper = upper)
}

.plot_scatter_outliers <- function(dataset, attribute, hide.outliers = FALSE) {
  values <- dataset[[attribute]]

  bounds <- .detect_outliers(values)
  is_outlier <- values < bounds$lower | values > bounds$upper

  dataset$outlier <- FALSE
  dataset[is_outlier,]$outlier <- TRUE

  dataset %>%
    filter(!is_outlier | !hide.outliers) %>%
    ggplot(aes_string(x = "quality", y = attribute)) +
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

combined <- read.csv("./dataset/winequality-combined.csv")

.plot_scatter(combined, "residual.sugar", hide.outliers = FALSE) +
  .plot_scatter(combined, "residual.sugar", hide.outliers = TRUE)
