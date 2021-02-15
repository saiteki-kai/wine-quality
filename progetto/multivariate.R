.plot_pca <- function(dataset) {
  dataset$quality <- NULL

  pca <- prcomp(dataset, center = TRUE, scale = TRUE)
  eig <- get_eigenvalue(pca)
  var <- get_pca_var(pca)

  print(eig)

  fviz_eig(pca, addlabels = TRUE, ncp = 11)

  print('a')

  fviz_pca_var(pca, col.var = 'black')

  print('b')
  corrplot(var$cos2, is.corr = FALSE)

  print('c')
  fviz_pca_var(pca, col.var = 'cos2',
               gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
               repel = TRUE)

  print('c')
  corrplot(var$contrib, is.corr = FALSE)

  print('d')
  fviz_pca_var(pca, col.var = 'contrib',
               gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
               repel = TRUE)

  print('e')
}

.plot_variable_boxplot <- function(data, input, class) {
  data %>%
    ggplot(aes_string(y = input, x = class, fill = class)) +
    geom_jitter(aes_string(color = class), size = 0.4, alpha = 0.5) +
    labs(y = input)
}

.plot_scatter <- function(dataset, attr1, attr2, color, title) {
  ggplot(dataset, aes_string(x = attr1, y = attr2)) +
    geom_jitter(aes_string(color = color), size = 2, alpha = 0.7) +
    geom_smooth(method = 'lm', formula = y ~ x) +
    ggtitle(title)
}

.plot_median_barplot <- function(dataset, attr1) {
  x <- aggregate(dataset[[attr1]], by = list(dataset$quality), FUN = median)
  ggplot(data = x, aes_string(x = "Group.1", y = "x")) +
    geom_bar(stat = "identity", aes_string(fill = "Group.1")) +
    ggtitle(paste0("variable", " = ", attr1))
}

.plot_correlation_heatmap <- function(dataset) {
  dataset$quality <- as.numeric(dataset$quality) - 1
  cormat <- round(cor(dataset), 3)
  ggplot(data = melt(cormat), aes(Var1, Var2, fill = value)) +
    geom_tile(color = 'white') +
    scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                         midpoint = 0, limit = c(-1, 1), space = 'Lab',
                         name = 'Pearson\nCorrelation') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1)) +
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = 'black', size = 4)
}


# Install packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork, factoextra, reshape2)

# Local functions
source('./utils.R')


# Import datasets
combined <- read.csv('./dataset/winequality-combined.csv')
#redwine <- read.csv('./dataset/winequality-red.csv')
#whitewine <- read.csv('./dataset/winequality-white.csv')

# Distribuzione Dati Combined Configurazione 2: 3 Classi
config2 <- preprocess_dataset(combined, 2)
# .plot_class_barplot(config2, 'quality')

# Plot paris
# pairs(config2, col=config2$quality)

# Plot correlation matrix
.plot_correlation_heatmap(config2)

config2$alcohol <- log10(config2$alcohol)
config2$density <- log10(config2$density)
config2$chlorides <- log10(config2$chlorides)
config2$volatile.acidity <- log10(config2$volatile.acidity)
config2$residual.sugar <- log10(config2$residual.sugar)
config2$sulphates <- log10(config2$sulphates)
config2$citric.acid <- log10(config2$citric.acid)
config2$pH <- log10(config2$pH)
config2$free.sulfur.dioxide <- log10(config2$free.sulfur.dioxide)
config2$total.sulfur.dioxide <- log10(config2$total.sulfur.dioxide)

.plot_scatter(config2, 'alcohol', 'density', 'quality', 'caso di alta correlazione tra total.sulfur.dioxide e free.sulfur.dioxide')
.plot_scatter(config2, 'free.sulfur.dioxide', 'total.sulfur.dioxide', 'quality', 'caso di alta correlazione tra density e alcohol')
.plot_scatter(config2, 'total.sulfur.dioxide', 'residual.sugar', 'quality', '')
.plot_scatter(config2, 'density', 'residual.sugar', 'quality', '')

# Plot bar for the median values.
.plot_median_barplot(config2, 'alcohol')
.plot_median_barplot(config2, 'density')
.plot_median_barplot(config2, 'chlorides')
.plot_median_barplot(config2, 'residual.sugar')
.plot_median_barplot(config2, 'volatile.acidity')
.plot_median_barplot(config2, 'sulphates')
.plot_median_barplot(config2, 'citric.acid')
.plot_median_barplot(config2, 'pH')
.plot_median_barplot(config2, 'free.sulfur.dioxide')
.plot_median_barplot(config2, 'total.sulfur.dioxide')

"
1 - As alcohol level increase ==> Quality increases
2 - As chlorides level decreases ==> Quality increases
3 - As citric acid level increases ==> Quality increases
4 - As density decreases ==> Quality increases
5 - fixed acidity ==> can’t say impact on Quality
6 - As free sulfur dioxide increases ==> Quality increases
7 - pH ==> can’t say impact on Quality
8 - As residual sugar increases ==> Quality increases
9 - sulphates ==> can’t say impact on Quality
10 - total sulfur dioxide ==> can’t say impact on Quality
11 - As the volatile acidity decreases ==> Quality increases

But since, only below four contributes towards wine quality : (alcohol, density, volatile acidity, chlorides)
- Increase in the alcohol qty, increases the quality of the wine.
- Decrease in the density of the wine, increases the quality of the wine.
- Decrease in the volatile acidity of the wine, increases the quality of the wine.
- Decrease in chlorides, increases the quality of the wine.
"

# ---------------------------------------------------------------------------------------------------------------------

pca <- prcomp(dataset, scale = TRUE, center = TRUE)
dataset$quality <- NULL
dataset$type <- NULL
eig <- get_eig(pca)
keep <- eig$cumulative.variance.percent < 95
new_dataset <- (pca$x %*% pca$rotation[, keep]) *
  pca$scale[keep] - pca$center[keep]  # NON sono sicuro di questo filtro (ordine delle cose??)
