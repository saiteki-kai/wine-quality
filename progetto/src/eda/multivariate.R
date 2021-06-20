#' Multivariate Analysis

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggcorrplot, corrplot, GGally, factoextra)

# Source scripts
source("../utils.R")

# Local functions

.plot_pca <- function(trainset, thresh = 95, save = TRUE, no.outliers = FALSE) {
  quality <- trainset$quality
  trainset$quality <- NULL

  # Perform PCA
  res.pca <- prcomp(trainset, center = TRUE, scale = TRUE)
  eig <- get_eigenvalue(res.pca)
  print(eig)

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

.plot_corrmatrix <- function(trainset, title, outlier, save = TRUE) {
  corr <- round(cor(trainset), 3)
  corr <- corr
  p.mat <- cor_pmat(corr)
  p <- ggcorrplot(corr, hc.order = TRUE, lab = TRUE, p.mat = p.mat, insig = "blank")

  p <- p + ggtitle(title)

  corr_path <- file.path("..", "..", "plots", "eda", "Correlation_matrix")
  create_dir_if_not_exists(corr_path)
  suffix <- paste0(ifelse(outlier, "O", "NoO"), ".png")

  print_or_save(p,
    filename = file.path(corr_path, paste0("Correlation_matrix_.png", suffix)),
    save = save,
    wide = TRUE
  )
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
trainset_noo <- remove_outliers_iqr(trainset)

# Plot PCA
pca_o <- .plot_pca(trainset)
pca_noo <- .plot_pca(trainset_noo, no.outliers = TRUE)

# trainset numeric quality
trainset_num <- trainset
trainset_num$quality <- as.numeric(trainset$quality) - 1
trainset_noo$quality <- as.numeric(trainset_noo$quality) - 1

# Plot Correlation Matrix
corr_o <- .plot_corrmatrix(trainset_num, "Red data Correlations", outlier = TRUE)
corr_noo <- .plot_corrmatrix(trainset_noo, "Red data Correlations without outliers", outlier = FALSE)
