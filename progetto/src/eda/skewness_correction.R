.plot_qqplot <- function(data, attribute, title) {
  data %>% ggplot(aes_string(sample = attribute)) +
    stat_qq(alpha = 0.5) +
    stat_qq_line() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

.global_distribution <- function(data1, data2, attribute) {
  p <- data1 %>% ggplot(aes_string(x = attribute, title = attribute)) +
    geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
    geom_freqpoly(color = "blue", bins = 40)
  q <- data2 %>% ggplot(aes_string(x = attribute, title = attribute)) +
    geom_histogram(alpha = 0.7, color = "red", fill = "#FF6666", bins = 40) +
    geom_freqpoly(color = "blue", bins = 40)
  (p + q) / ( .plot_qqplot(data1, attribute, "") + .plot_qqplot(data2, attribute, ""))
}

.pre_proc <- function(trainset, name) {
  data <- trainset[, -ncol(trainset)]

  if (name == "pca") {
    pre_proc <- preProcess(data, method = c("center", "scale", "pca"), thresh = 0.9)
  } else if (name == "z-score") {
    pre_proc <- preProcess(data, method = c("center", "scale"))
  } else if (name == "min-max") {
    pre_proc <- preProcess(data, method = "range")
  } else if (name == "BoxCox") {
    pre_proc <- preProcess(data, method = c("center", "scale", "BoxCox"))
  } else if (name == "YeoJohnson") {
    pre_proc <- preProcess(data, method = c("center", "scale", "YeoJohnson"))
  } else {
    stop("`method` should be either `pca`, `z-score` or `min-max`")
  }

  pre_proc
}

.skew_and_kurtosi <- function(trainset, target) {
  trainset <- trainset[names(trainset) != target]
  skewness <- psych::skew(as.data.frame(trainset))
  kurtosis <- psych::kurtosi(as.data.frame(trainset))
  cbind(skew=skewness, kurtosi=kurtosis)
}

# Install packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(corrplot, ggplot2, dplyr, naniar,
               patchwork, factoextra, reshape2, ggcorrplot, scales, GGally, MASS, bestNormalize)

# boxcox funziona solo su features strettamente positive!!!

# Local functions
source('../utils.R')

# Read trainset
trainset <- read.csv('../../data/winequality-train.csv')
trainset$quality <- factor(trainset$quality)

# Apply transformedations
data <- trainset

data$quality <- NULL
#data$alcohol <- NULL
#data$density <- NULL
#data$pH <- NULL
#data$citric.acid <- NULL
#data$total.sulfur.dioxide <- NULL

pre_proc <- preProcess(data, method = c("scale", "center", "BoxCox"), verbose = TRUE)
transformed <- predict(pre_proc, data)

transformed$quality <- trainset$quality
#transformed$alcohol <- trainset$alcohol
#transformed$density <- trainset$density
#transformed$pH <- trainset$pH
#transformed$citric.acid <- trainset$citric.acid
#transformed$total.sulfur.dioxide <- trainset$total.sulfur.dioxide

.global_distribution(trainset, transformed, "alcohol")
.global_distribution(trainset, transformed, "density")
.global_distribution(trainset, transformed, "pH")
.global_distribution(trainset, transformed, "citric.acid")
.global_distribution(trainset, transformed, "total.sulfur.dioxide")

.global_distribution(trainset, transformed, "volatile.acidity")
.global_distribution(trainset, transformed, "residual.sugar")
.global_distribution(trainset, transformed, "sulphates")
.global_distribution(trainset, transformed, "fixed.acidity")
.global_distribution(trainset, transformed, "chlorides")
.global_distribution(trainset, transformed, "free.sulfur.dioxide")

.skew_and_kurtosi(trainset, "quality")

.skew_and_kurtosi(transformed, "quality")
