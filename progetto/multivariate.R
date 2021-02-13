.plot_pca <- function(dataset) {
  normalized <- normalize_dataset(dataset)

  pca <- prcomp(cov(normalized[1:11]))
  eig <- get_eigenvalue(pca)
  var <- get_pca_var(pca)

  print(eig)

  fviz_eig(pca, addlabels = TRUE, ncp = 11)

  print("a")

  fviz_pca_var(pca, col.var = "black")

  print("b")
  corrplot(var$cos2, is.corr=FALSE)

  print("c")
  fviz_pca_var(pca, col.var = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)

  print("c")
  corrplot(var$contrib, is.corr=FALSE)

  print("d")
  fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

  print("e")
}

.plot_class_barplot <- function(data, class) {
  data %>%
    ggplot(aes_string(x = class, fill = class, color = class)) +
    geom_bar()
}

.plot_scatter <- function(dataset, attr1, attr2, color, title) {
  ggplot(dataset, aes(x=dataset[[attr1]], y=dataset[[attr2]], color=dataset$quality)) +
      geom_jitter(size=2, alpha=0.7) +
      ggtitle(title)
}

.plot_correlation_heatmap <- function(dataset) {
  dataset$quality <- as.numeric(dataset$quality) - 1
  cormat <- round(cor(dataset),3)
  ggplot(data = melt(cormat), aes(Var1, Var2, fill=value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
     midpoint = 0, limit = c(-1,1), space = "Lab",
     name="Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1)) +
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
}


# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork, factoextra, reshape2)

# Local functions
source("./utils.R")


# Import datasets
combined <- read.csv("./dataset/winequality-combined.csv")

# Distribuzione Dati Combined Configurazione 1: 2 Classi
config1 <- preprocess_dataset(combined, 1)
.plot_class_barplot(config1, "quality")


# Plot correlation matrix
.plot_correlation_heatmap(config1)

.plot_scatter(config1,
              "total.sulfur.dioxide",
              "free.sulfur.dioxide",
              "quality",
              "caso di alta correlazione tra total.sulfur.dioxide e free.sulfur.dioxide")

.plot_scatter(config1,
              "density",
              "alcohol",
              "quality",
              "caso di alta correlazione tra density e alcohol")

.plot_scatter(config1,
              "sulphates",
              "alcohol",
              "quality",
              "caso di bassa correlazione tra sulphates e alcohol")

.plot_scatter(config1,
              "quality",
              "free.sulfur.dioxide",
              "quality",
              "caso di bassa correlazione tra free.sulfur.dioxide e quality")

# PCA Analysis
.plot_pca(config1)