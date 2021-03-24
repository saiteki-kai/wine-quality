#' Multivariate Analysis

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  corrplot, ggplot2, dplyr, naniar,
  patchwork, factoextra, reshape2,
  ggcorrplot, scales, GGally, MASS, bestNormalize, psych
)

# Source scripts
source("../utils.R")

# Local functions
.plot_pca <- function(trainset) {
  trainset$quality <- NULL

  pca <- prcomp(trainset, center = TRUE, scale = TRUE)
  eig <- get_eigenvalue(pca)
  var <- get_pca_var(pca)

  print(eig)

  fviz_eig(pca, addlabels = TRUE, ncp = 11)

  fviz_pca_var(pca, col.var = "black")

  corrplot(var$cos2, is.corr = FALSE)

  fviz_pca_var(pca,
    col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
  )

  corrplot(var$contrib, is.corr = FALSE)

  fviz_pca_var(pca,
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
  )
}

.plot_scatter <- function(trainset, attr1, attr2, color, title) {
  ggplot(trainset, aes_string(x = attr1, y = attr2)) +
    geom_jitter(aes_string(color = color), size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ x, color = "black") +
    ggtitle(title)
}

.plot_boxplot <- function(trainset, attr1, class, title) {
  p <- ggplot(trainset, aes_string(y = attr1, x = class, fill = class)) +
    geom_boxplot() +
    ggtitle(title)
  p
}

.plot_corrmatrix <- function(trainset, title) {
  corr <- round(cor(trainset), 2)
  corr <- corr
  p.mat <- cor_pmat(corr)
  ggcorrplot(corr, hc.order = TRUE, p.mat = p.mat, lab = TRUE, insig = "blank") +
    ggtitle(title)
}

.plot_pairplot <- function(data, class, title) {
  pm <- ggpairs(data,
    mapping = ggplot2::aes(color = factor(class)),
    upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"),
    lower = list(continuous = wrap("smooth_loess", alpha = 0.3, size = 1)), # color = "#488bf7"
    title = title
  )
  pm
}



# Import dataset
trainset <- read.csv("../../data/winequality-train.csv")
trainset$quality <- factor(trainset$quality)

# trainset numeric quality
trainset_num <- trainset
trainset_num$quality <- as.numeric(trainset$quality) - 1

# Plot correlation matrix
.plot_corrmatrix(trainset_num, "White data Correlations")

# Log 10 Transformation (just for visualization purpose)
trainset$alcohol <- log10(trainset$alcohol)
trainset$density <- log10(trainset$density)
trainset$residual.sugar <- log10(trainset$residual.sugar)
trainset$volatile.acidity <- log10(trainset$volatile.acidity)
trainset$citric.acid <- log10(trainset$citric.acid)
trainset$pH <- log10(trainset$pH)
trainset$fixed.acidity <- log10(trainset$fixed.acidity)
trainset$free.sulfur.dioxide <- log10(trainset$free.sulfur.dioxide)
trainset$total.sulfur.dioxide <- log10(trainset$total.sulfur.dioxide)
trainset$sulphates <- log10(trainset$sulphates)
trainset$chlorides <- log10(trainset$chlorides)

# Correlation with quality
.plot_boxplot(trainset, "alcohol", "quality", "") +
.plot_boxplot(trainset, "density", "quality", "") +
.plot_boxplot(trainset, "chlorides", "quality", "") +
.plot_boxplot(trainset, "volatile.acidity", "quality", "") +
.plot_boxplot(trainset, "residual.sugar", "quality", "") +
.plot_boxplot(trainset, "sulphates", "quality", "") +
.plot_boxplot(trainset, "citric.acid", "quality", "") +
.plot_boxplot(trainset, "fixed.acidity", "quality", "") +
.plot_boxplot(trainset, "pH", "quality", "") +
.plot_boxplot(trainset, "free.sulfur.dioxide", "quality", "") +
.plot_boxplot(trainset, "total.sulfur.dioxide", "quality", "")

# Other Relevants cases:
# - residual.sugar and density +0.84
# - alcohol and density -0.77
# - free.sulfur.dioxide and total.sulfur.dioxide +0.62
# - total.sulfur.dioxide and density +0.52
# - residual.sugar e alcohol -0.45
# - total.sulfur.dioxide e alcohol -0.45

# (penso di toglierle queste due)
# - fixed.acidity e pH -0.43
# - free.sulfur.dioxide e alcohol -0.25


.plot_scatter(
  trainset, " residual.sugar", "density", "quality",
  "caso di correlazione tra residual.sugar e density +0.84"
)
.plot_scatter(
  trainset, "alcohol", "density", "quality",
  "caso di correlazione tra density e alcohol -0.77"
)
.plot_scatter(
  trainset, "free.sulfur.dioxide", "total.sulfur.dioxide", "quality",
  "caso di correlazione tra total.sulfur.dioxide e free.sulfur.dioxide +0.62"
)
.plot_scatter(
  trainset, "total.sulfur.dioxide", "density", "quality",
  "caso di correlazione tra density e alcohol +0.52"
)
.plot_scatter(
  trainset, "alcohol", "residual.sugar", "quality",
  "caso di correlazione tra residual.sugar e alcohol -0.45"
)
.plot_scatter(
  trainset, "alcohol", "total.sulfur.dioxide", "quality",
  "caso di correlazione tra total.sulfur.dioxide e alcohol -0.45"
)

.plot_scatter(
  trainset, "fixed.acidity", "pH", "quality",
  "caso di correlazione tra fixed.acidity e pH -0.43"
)
.plot_scatter(
  trainset, "free.sulfur.dioxide", "alcohol", "quality",
  "caso di correlazione tra free.sulfur.dioxide e alcohol -0.25"
)
