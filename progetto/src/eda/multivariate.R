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
.plot_pca <- function(trainset, thresh = 95, save = TRUE, no.outliers = FALSE) {
  quality <- trainset$quality
  trainset$quality <- NULL

  # Perform PCA
  res.pca <- prcomp(trainset, center = TRUE, scale = TRUE)
  eig <- get_eigenvalue(res.pca)

  # Number of components to keep with the selected threshold
  pc_keep <- nrow(eig[eig$cumulative.variance.percent <= thresh, ])
  print(paste("Principal Components to keep:", pc_keep))

  transformed <- as.data.frame(res.pca$x[, 1:pc_keep])
  transformed$quality <- quality

  # Explained Variance
  p1 <- fviz_eig(res.pca, addlabels = TRUE, ncp = NULL, title = "Explained Variance")

  # Variables Contribution
  p2 <- fviz_pca_var(res.pca,
    col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
  )

  # First 2 Components
  p3 <- fviz_pca_ind(res.pca,
    geom.ind = "point",
    alpha.ind = 0.75,
    repel = TRUE,
    col.ind = quality,
    addEllipses = TRUE,
    legend.title = "Quality"
  )

  p4 <- .plot_pairplot(
    transformed[, c("PC1", "PC2", "PC3", "quality")],
    "quality",
    "Pair Plot"
  )

  # Print or save plots

  pca_path <- file.path("..", "..", "plots", "eda", "pca")
  create_dir_if_not_exists(pca_path)

  suffix <- paste0(ifelse(no.outliers, "O", "NoO"), ".png")

  print_or_save(p1,
    filename = file.path(pca_path, paste0("variance_", suffix)),
    save = save,
    wide = TRUE
  )
  print_or_save(p2,
    filename = file.path(pca_path, paste0("variables_", suffix)),
    save = save,
    wide = TRUE
  )
  print_or_save(p3,
    filename = file.path(pca_path, paste0("individuals_", suffix)),
    save = save,
    wide = TRUE
  )
  print_or_save(p4,
    filename = file.path(pca_path, paste0("pairs_", suffix)),
    save = save,
    wide = TRUE
  )

  transformed
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
  corr <- cor(trainset)
  # p.mat <- cor_pmat(corr)
  #
  # p <- ggcorrplot(corr,
  #                 p.mat = p.mat,
  #                 sig.level = 0.01,
  #                 show.diag = TRUE,
  #                 lab = TRUE,
  #                 lab_col = "red",
  #                 lab_size = 2,
  #                 pch = 2,
  #                 pch.col = "green",
  #                 pch.cex = 10,
  #                 tl.cex = 12,
  #                 tl.col = "black",
  #                 tl.srt = 33,
  #                 insig = "pch",
  #                 type = "upper"
  # )
  #
  # p + ggtitle(title)

  p.mat <- cor.mtest(trainset)$p

  corrplot.mixed(corr,
    tl.pos = "lt",
    tl.cex = .8,
    number.cex = .8,
    upper = "color",
    p.mat = p.mat,
    sig.level = 0.01
  )

  recordPlot()
}

.plot_pairplot <- function(data, target, title) {
  pm <- ggpairs(data,
    aes_string(color = target),
    upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"),
    lower = list(continuous = wrap("smooth_loess", alpha = 0.3, size = 1)), # color = "#488bf7"
    title = title
  )
  pm
}

# Import dataset
trainset <- read.csv("../../data/winequality-train.csv")
trainset$quality <- factor(trainset$quality)

# Remove Outliers
trainset_noo <- remove_outliers(trainset)

# Plot PCA
pca_o <- .plot_pca(trainset)
pca_noo <- .plot_pca(trainset_noo, no.outliers = TRUE)

# trainset numeric quality
trainset_num <- trainset
trainset_num$quality <- as.numeric(trainset$quality) - 1
trainset_noo$quality <- as.numeric(trainset_noo$quality) - 1

# Plot Correlation Matrix
corr_o <- .plot_corrmatrix(trainset_num, "White data Correlations")
corr_noo <- .plot_corrmatrix(trainset_noo, "White data Correlations")

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
#
# # Other Relevants cases:
# # - residual.sugar and density +0.84
# # - alcohol and density -0.77
# # - free.sulfur.dioxide and total.sulfur.dioxide +0.62
# # - total.sulfur.dioxide and density +0.52
# # - residual.sugar e alcohol -0.45
# # - total.sulfur.dioxide e alcohol -0.45
#
# # (penso di toglierle queste due)
# # - fixed.acidity e pH -0.43
# # - free.sulfur.dioxide e alcohol -0.25
#
#
# .plot_scatter(
#   trainset, " residual.sugar", "density", "quality",
#   "caso di correlazione tra residual.sugar e density +0.84"
# )
# .plot_scatter(
#   trainset, "alcohol", "density", "quality",
#   "caso di correlazione tra density e alcohol -0.77"
# )
# .plot_scatter(
#   trainset, "free.sulfur.dioxide", "total.sulfur.dioxide", "quality",
#   "caso di correlazione tra total.sulfur.dioxide e free.sulfur.dioxide +0.62"
# )
# .plot_scatter(
#   trainset, "total.sulfur.dioxide", "density", "quality",
#   "caso di correlazione tra density e alcohol +0.52"
# )
# .plot_scatter(
#   trainset, "alcohol", "residual.sugar", "quality",
#   "caso di correlazione tra residual.sugar e alcohol -0.45"
# )
# .plot_scatter(
#   trainset, "alcohol", "total.sulfur.dioxide", "quality",
#   "caso di correlazione tra total.sulfur.dioxide e alcohol -0.45"
# )
#
# .plot_scatter(
#   trainset, "fixed.acidity", "pH", "quality",
#   "caso di correlazione tra fixed.acidity e pH -0.43"
# )
# .plot_scatter(
#   trainset, "free.sulfur.dioxide", "alcohol", "quality",
#   "caso di correlazione tra free.sulfur.dioxide e alcohol -0.25"
# )
