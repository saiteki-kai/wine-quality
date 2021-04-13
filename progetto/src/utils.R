#' This file contains the utility functions used in the whole project.

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, ggplot2, kableExtra, psych, dplyr)

#' Convert the quality attribute based on the configuration
#'
#' @param dataset a dataset
#' @param config the class configuration to be used
#' @param type when TRUE keep the type attribute, if present, otherwise
#' remove it (the default)
#'
#' @details
#' config = 1  -> 2 levels  (0: bad-quality, 1: good-quality)
#' config = 2  -> 3 levels  (0: low-quality, 1: medium-quality, 2: high-quality)
#' otherwise   -> 10 levels (0: low-quality, ..., 10: high-quality)
#'
#' @return the processed dataset
relabeling <- function(dataset, config = 1, type = FALSE) {
  if (type) {
    dataset$type <- factor(dataset$type)
  } else {
    dataset$type <- NULL
  }

  if (config == 1) {
    dataset$quality <- ifelse(dataset$quality > 6, "good", "bad")
    dataset$quality <- factor(dataset$quality)
  } else if (config == 2) {
    dataset$quality <- ifelse(
      dataset$quality < 5,
      "low",
      ifelse(dataset$quality < 7, "medium", "high")
    )
    dataset$quality <- factor(dataset$quality,
      levels = c("low", "medium", "high")
    )
  } else {
    dataset$quality <- factor(dataset$quality)
  }

  dataset
}

#' Combine the red and the white datasets and add the type attribute.
#' Save the result as a csv file
#'
#' @return the combined dataset
combine_redwhite <- function() {
  redwine <- read.csv("../data/winequality-red.csv")
  whitewine <- read.csv("../data/winequality-white.csv")

  redwine$type <- "red"
  whitewine$type <- "white"

  wines <- rbind(redwine, whitewine)

  # Save the combined dataset
  write.csv(wines, "../data/winequality-combined.csv", row.names = FALSE)

  wines
}

#' Detect outliers using the Interquartile Range (IQR) approach or
#' Winsorinzing (Percentile Capping).
#' All points that lie outside the upper limit or below the lower
#' limit can be considered outliers.
#'
#' @param data numeric array
#' @param method method of outliers detection
#' @param win.quantiles quantile limits for the winsorinzing method
#'
#' @details
#' Method IQR:
#'
#' IQR = 75th quantile - 25th quantile
#' Lower Limit = 25th quantile - 1.5 * IQR
#' Upper Limit = 75th quantile + 1.5 * IQR
#'
#' Method Winsorizing:
#'
#' Lower Limit =  5th quantile
#' Upper Limit = 95th quantile
#'
#' @return a list containing the lower and upper limits
detect_outliers <- function(data,
                            method = "winsorizing",
                            win.quantiles = c(0.05, 0.95)) {
  if (method == "winsorizing") {
    quantiles <- quantile(data, win.quantiles, names = FALSE)
    lower <- quantiles[1]
    upper <- quantiles[2]
  } else if (method == "IQR") {
    quantiles <- quantile(data, c(0.25, 0.75), names = FALSE)
    lower <- quantiles[1] - 1.5 * IQR(data)
    upper <- quantiles[2] + 1.5 * IQR(data)
  } else {
    stop("`mode` should be either `IQR` or `winsorizing`")
  }

  list(lower = lower, upper = upper)
}

#' Treat the outliers using different methods
#'
#' @param dataset a dataseta
#' @param method method of outliers detection
#' @param win.quantiles quantile limits for the winsorinzing method
#'
#' @return the dataset without outliers
treat_outliers <- function(data, method = "winsorizing",
                           win.quantiles = c(0.05, 0.95),
                           outlier.rm = FALSE) {
  if (method == "winsorizing") {
    bounds <- detect_outliers(data, win.quantiles = win.quantiles)
    data[data < bounds[1]] <- bounds$lower
    data[data > bounds[2]] <- bounds$upper
  } else if (method == "IQR") {
    bounds <- detect_outliers(data, method = "IQR")
    data[data < bounds$lower | data > bounds$upper] <-
      ifelse(outlier.rm, NA, median(data))
  } else {
    stop("`mode` should be either `IQR` or `winsorizing`")
  }
  data
}

#' Remove the outliers given a dataset, using the IQR method
#'
#' @param trainset a dataset
#'
#' @return the dataset without outliers
remove_outliers_iqr <- function(trainset) {
  trainset %>%
    lapply(function(x) {
      if (is.numeric(x)) {
        treat_outliers(x, method = "IQR", outlier.rm = TRUE)
      } else {
        x
      }
    }) %>%
    as.data.frame() %>%
    na.omit()
}

#' Save plot (ggplot or normal plot) as png
#'
#' @param filename file name to save
#' @param plot plot to save
#' @param wide when TRUE, saves the plot using a wide horizontal
#' proportion, otherwise a square proportion (the default).
save_plot_png <- function(filename, plot, wide = FALSE) {
  if ("gg" %in% class(plot)) {
    ggsave(filename,
      plot = plot,
      device = "png",
      height = 6.67,
      width = ifelse(wide, 13.34, 6.67)
    )
  } else {
    png(filename,
      units = "in",
      res = 300,
      height = 6.67,
      width = ifelse(wide, 13.34, 6.67)
    )
    print(plot)
    dev.off()
  }
}

#' Save a plot as png or just print it
#'
#' @param filename file name to save
#' @param plot plot to save
#' @param save when TRUE saves the plot, otherwise just print it
#' @param wide when TRUE set a wide horizontal proportion for the plot,
#' otherwise a square proportion (the default).
print_or_save <- function(plot, filename, save = FALSE, wide = FALSE) {
  if (save) {
    save_plot_png(filename, plot = plot, wide = wide)
  } else {
    print(plot)
  }
}

#' Create the directory of the specified path
#'
#' @param path folder path name
#'
#' @return returns TRUE if the directory was created successfully or
#' already exists, FALSE otherwise.
create_dir_if_not_exists <- function(path) {
  if (!dir.exists(path)) {
    out <- dir.create(path, recursive = TRUE)
    return(out)
  }
  TRUE
}

#' Print a summary of descriptive statistics (mean, median, min, max, skewness
#' and kurtosis)
#'
#' @param df a dataframe of numerical variables
#' @param latex when TRUE, generate the summary in latex, otherwise print it
#'
#' @return a dataframe containing the statistics
print_stats <- function(df, latex = FALSE) {
  df$quality <- NULL

  stats <- psych::describe(df) %>% round(2)
  stats <- stats %>%
    dplyr::select(-c("mad", "trimmed", "range", "se", "n"))

  if (latex) {
    stats <- stats %>%
      kbl("latex", booktabs = T) %>%
      kable_styling(latex_options = c("striped", "scale_down"))
    return(stats)
  }

  stats
}
