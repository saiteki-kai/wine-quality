#' Outliers Analysis
#'
#' add description
#'

# #' Plots all points distinguishing outliers from non-outliers
# #' and allows you to hide them through the \code{hide.outliers} parameter.
# #' Outliers are identified through the \code{detect_outliers} function.
# #'
# #' @param dataset a dataset
# #' @param attribute an attribute name
# #' @param label an attribute name indicating the label
# #' @param hide.outliers a logical indicating whether outliers should be hidden
# #' @return the plot
# .plot_scatter <- function(dataset, attribute, label, hide.outliers = FALSE) {
#   values <- dataset[[attribute]]
#
#   bounds <- detect_outliers(values)
#   is_outlier <- values < bounds$lower | values > bounds$upper
#
#   dataset$outlier <- FALSE
#   dataset[is_outlier,]$outlier <- TRUE
#
#   dataset %>%
#     filter(!is_outlier | !hide.outliers) %>%
#     ggplot(aes_string(x = label, y = attribute)) +
#     geom_jitter(aes(colour = outlier), alpha = 0.3) +
#     ggtitle(paste(ifelse(hide.outliers, "Without", "With"), "Outliers")) +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       legend.position = c(.99, .99),
#       legend.justification = c("right", "top"),
#       legend.box.just = "right",
#       legend.margin = margin(6, 6, 6, 6)
#     )
# }
#
# #'
# .plot_hist_and_boxplot <- function(dataset, attribute) {
#   values <- dataset[[attribute]]
#   win_bounds <- detect_outliers(values)
#   iqr_bounds <- detect_outliers(values, method = "IQR")
#
#   min_attribute <- min(values)
#   max_attribute <- max(values)
#
#   x_scale <- scale_x_continuous(limits = c(min_attribute, max_attribute), breaks = pretty(values, n = 5))
#
#   boxplot <- dataset %>%
#     ggplot(aes_string(x = attribute)) +
#     geom_boxplot(aes(y = ""), outlier.shape = "cross") +
#     geom_vline(xintercept = iqr_bounds$lower, linetype = 'dotted', color = "red") +
#     geom_vline(xintercept = iqr_bounds$upper, linetype = 'dotted', color = "red") +
#     geom_vline(xintercept = win_bounds$lower, linetype = 'dotted', color = "blue") +
#     geom_vline(xintercept = win_bounds$upper, linetype = 'dotted', color = "blue") +
#     labs(y = NULL) +
#     x_scale +
#     theme(axis.text.y = element_blank())
#
#   histogram <- dataset %>%
#     ggplot(aes_string(x = attribute), show.legend = TRUE) +
#     geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
#     geom_vline(xintercept = iqr_bounds$lower, linetype = 'dotted', color = "red") +
#     geom_vline(xintercept = iqr_bounds$upper, linetype = 'dotted', color = "red") +
#     geom_vline(xintercept = win_bounds$lower, linetype = 'dotted', color = "blue") +
#     geom_vline(xintercept = win_bounds$upper, linetype = 'dotted', color = "blue") +
#     annotate(
#       x = iqr_bounds$lower,
#       y = +Inf,
#       label = "\nQ1 - 1.5 * IQR",
#       hjust = 2,
#       vjust = 0,
#       color = "red",
#       size = 3,
#       fontface = 'bold',
#       angle = 90,
#       geom = "text"
#     ) +
#     annotate(
#       x = iqr_bounds$upper,
#       y = +Inf,
#       label = "\nQ3 + 1.5 * IQR",
#       hjust = 2,
#       vjust = 0,
#       color = "red",
#       size = 3,
#       fontface = 'bold',
#       angle = 90,
#       geom = "text"
#     ) +
#     annotate(
#       x = win_bounds$lower,
#       y = +Inf,
#       label = "\n5th quantile",
#       hjust = 4,
#       color = "blue",
#       size = 3,
#       fontface = 'bold',
#       angle = 90,
#       geom = "text"
#     ) +
#     annotate(
#       x = win_bounds$upper,
#       y = +Inf,
#       label = "\n95th quantile",
#       hjust = 4,
#       color = "blue",
#       size = 3,
#       fontface = 'bold',
#       angle = 90,
#       geom = "text"
#     ) +
#     labs(y = "count", x = NULL) +
#     x_scale +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks.x = element_blank(),
#       legend.position = c(.95, .95),
#       legend.justification = c("right", "top"),
#       legend.box.just = "right",
#       legend.margin = margin(6, 6, 6, 6)
#     )
#
#   (histogram / boxplot) + plot_layout(heights = c(5, 1), guides = "collect")
# }

# ---------------------------------------------------------------------------------------------------------------------

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, naniar, patchwork)

# Import local functions
source("../utils.R")

# dataset <- read.csv("../dataset/winequality-combined.csv")
# dataset <- preprocess_dataset(dataset, 1)
# dataset$quality <- factor(dataset$quality) # IMPORTANT

# Print histogram and boxplot for each attribute
# for (attribute in names(dataset)) {
#   if (is.numeric(dataset[[attribute]])) {
#     p <- .plot_hist_and_boxplot(dataset, attribute)
#
#     #filename <- file.path("../results/plots/outliers", paste0(attribute, "_histbox.png"))
#     #save_plot_png(filename, plot = p)
#
#     print(p)
#   }
# }

# Print scatter plot for each attribute with and without outliers
#
# for (label in c("quality", "type")) {
#   for (attribute in names(dataset)) {
#     if (is.numeric(dataset[[attribute]])) {
#       p <- .plot_scatter(dataset, attribute, label) +
#         .plot_scatter(dataset, attribute, label, hide.outliers = TRUE)
#
#       #filename <- file.path("../results/plots/outliers", paste(attribute, label, "scatter.png", sep = "_"))
#       #save_plot_png(filename, plot = p, wide = TRUE)
#
#       print(p)
#     }
#   }
# }

dataset <- read.csv("../dataset/winequality-combined.csv")
dataset <- preprocess_dataset(dataset, 1)

.plot_boxplot <- function(data, attribute, title, limits) {
  data %>% ggplot(aes_string(x = attribute)) +
    stat_boxplot(aes(y = ""), geom = "errorbar", width = 0.5) +
    geom_boxplot(aes(y = ""), outlier.shape = "cross") +
    geom_jitter(aes(y = quality, colour = quality), alpha = 0.4, size = 0.5) +
    xlim(limits) +
    ggtitle(title) +
    labs(y = NULL, x = NULL) +
    theme(plot.title = element_text(hjust = 0.5))
}

.plot_qqplot <- function(data, attribute, title) {
  data %>% ggplot(aes_string(sample = attribute)) +
    stat_qq(alpha = 0.5) +
    stat_qq_line() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

d_iqr <- dataset
d_win1 <- dataset
d_win2 <- dataset
for (i in names(dataset)) {
  if (is.numeric(dataset[[i]])) {
    d_iqr[[i]] <- treat_outliers(dataset[[i]], "IQR")
    d_win1[[i]] <- treat_outliers(dataset[[i]])
    d_win2[[i]] <- treat_outliers(dataset[[i]], win.quantiles = c(0.01, 0.99))

    x_lims <- c(min(dataset[[i]]), max(dataset[[i]]))

    p0 <- .plot_boxplot(dataset, i, "Original", x_lims)
    p1 <- .plot_boxplot(d_iqr, i, "IQR Method", x_lims)
    p2 <- .plot_boxplot(d_win1, i, "Winsorizing 90%", x_lims)
    p3 <- .plot_boxplot(d_win2, i, "Winsorizing 100%", x_lims)

    plot <- (p0 / p1 / p2 / p3) +
      plot_layout(guides = "collect") +
      labs(x = i)

    filename <- file.path("./plots/outliers", paste0(i, "_boxplot.png"))
    save_plot_png(filename, plot = plot, wide = TRUE)

    q0 <- .plot_qqplot(dataset, i, "Original")
    q1 <- .plot_qqplot(d_iqr, i, "IQR Method")
    q2 <- .plot_qqplot(d_win1, i, "Winsorizing 90%")
    q3 <- .plot_qqplot(d_win2, i, "Winsorizing 100")

    plot <- (q0 + q1 + q2 + q3) + plot_layout(guides = "collect")

    filename <- file.path("./plots/outliers", paste0(i, "_qqplot.png"))
    save_plot_png(filename, plot = plot)
  }
}

# Z-score scartato perché non tutte le distribfuzioni sono normali
# Winsorizing 100% scartato poiché se è troppo skewed, tiene gli outliers
# Scelta tra IQR (con imputation median) e Winsorizing 90% tramite i Q-Q plot
# Metodo scelto: IQR

# TODO: quantificare il numero di outliers con la tecnica scelta

# gg_miss_var(dataset, quality)
# gg_miss_fct(dataset, quality)
