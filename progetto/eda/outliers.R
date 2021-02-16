#' Outliers Analysis
#'
#' add description
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, naniar, patchwork)

# Import local functions
source("../utils.R")

dataset <- read.csv("../dataset/winequality-train.csv") %>%
  mutate(quality = factor(quality))

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

    filename <- file.path("../plots/outliers", paste0(i, "_boxplot.png"))
    save_plot_png(filename, plot = plot, wide = TRUE)

    q0 <- .plot_qqplot(dataset, i, "Original")
    q1 <- .plot_qqplot(d_iqr, i, "IQR Method")
    q2 <- .plot_qqplot(d_win1, i, "Winsorizing 90%")
    q3 <- .plot_qqplot(d_win2, i, "Winsorizing 100")

    plot <- (q0 + q1 + q2 + q3) + plot_layout(guides = "collect")

    filename <- file.path("../plots/outliers", paste0(i, "_qqplot.png"))
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
