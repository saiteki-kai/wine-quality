#' This script performs outlier analysis on the training set.
#' Contains local functions to show the various plots (boxplot, qqplot, density
#' plot).
#' The methods used are IQR and Winsoring (90%, 98%).

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, naniar, patchwork, kableExtra, psych, dplyr)

# Source scripts
source("../utils.R")

# Local functions
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

.plot_distribution <- function(data, attribute, title, limits) {
  data %>% ggplot(aes_string(x = attribute)) +
    geom_density() +
    xlim(limits) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

.print_stats <- function(df) {
  df$quality <- NULL

  stats <- psych::describe(df) %>% round(2)
  stats %>%
    dplyr::select(-c("mad", "trimmed", "range", "se", "n")) %>%
    kbl("latex", booktabs = T) %>%
    kable_styling(latex_options = c("striped", "scale_down"))
}

outliers_path <- file.path("..", "..", "plots", "eda", "outliers")
create_dir_if_not_exists(outliers_path)

dataset <- read.csv("../../data/winequality-train.csv") %>%
  mutate(quality = factor(quality))

d_iqr <- dataset
d_win1 <- dataset
d_win2 <- dataset

for (i in names(dataset)) {
  if (is.numeric(dataset[[i]])) {
    d_iqr[[i]] <- treat_outliers(dataset[[i]], "IQR", outlier.rm = TRUE)
    d_win1[[i]] <- treat_outliers(dataset[[i]])
    d_win2[[i]] <- treat_outliers(dataset[[i]], win.quantiles = c(0.01, 0.99))

    x_lims <- c(min(dataset[[i]]), max(dataset[[i]]))

    p0 <- .plot_boxplot(dataset, i, "Original", x_lims)
    p1 <- .plot_boxplot(d_iqr, i, "IQR Method", x_lims)
    p2 <- .plot_boxplot(d_win1, i, "Winsorizing 90%", x_lims)
    p3 <- .plot_boxplot(d_win2, i, "Winsorizing 98%", x_lims)

    plot <- (p0 / p1 / p2 / p3) +
      plot_layout(guides = "collect") +
      labs(x = i)

    print_or_save(plot,
      filename = file.path(outliers_path, paste0(i, "_boxplot.png")),
      save = FALSE,
      wide = TRUE
    )

    d0 <- .plot_distribution(dataset, i, "Original", x_lims)
    d1 <- .plot_distribution(na.omit(d_iqr), i, "IQR Method", x_lims)
    d2 <- .plot_distribution(d_win1, i, "Winsorizing 90%", x_lims)
    d3 <- .plot_distribution(d_win2, i, "Winsorizing 98%", x_lims)

    plot <- (d0 + d1 + d2 + d3) +
      plot_layout(guides = "collect") +
      labs(x = i)

    print_or_save(plot,
      filename = file.path(outliers_path, paste0(i, "_distribution.png")),
      save = FALSE,
      wide = TRUE
    )

    q0 <- .plot_qqplot(dataset, i, "Original")
    q1 <- .plot_qqplot(na.omit(d_iqr), i, "IQR Method")
    q2 <- .plot_qqplot(d_win1, i, "Winsorizing 90%")
    q3 <- .plot_qqplot(d_win2, i, "Winsorizing 98%")

    plot <- (q0 + q1 + q2 + q3) + plot_layout(guides = "collect")

    print_or_save(plot,
      filename = file.path(outliers_path, paste0(i, "_qqplot.png")),
      save = FALSE
    )
  }
}

s1 <- .print_stats(dataset)
s2 <- .print_stats(d_iqr)

write(s1, file.path(outliers_path, "with_outliers.tex"))
write(s2, file.path(outliers_path, "without_outliers.tex"))

# gg_miss_var(dataset, quality)
# gg_miss_fct(dataset, quality)
