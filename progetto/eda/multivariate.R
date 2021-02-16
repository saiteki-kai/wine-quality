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
    ggtitle(attr1)
}

.plot_mean_barplot <- function(dataset, attr1) {
  x <- aggregate(dataset[[attr1]], by = list(dataset$quality), FUN = mean)
  ggplot(data = x, aes_string(x = "Group.1", y = "x")) +
    geom_bar(stat = "identity", aes_string(fill = "Group.1")) +
    ggtitle(attr1)
}

# Install packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork, factoextra, reshape2, ggcorrplot)

# Local functions
source('../utils.R')


# Import datasets
combined <- read.csv('../dataset/winequality-combined.csv')

config2 <- preprocess_dataset(combined, 1)
config2$type <- NULL
#config2$type <- ifelse(config2$type == "red", 0, 1)
config2$quality <- as.numeric(config2$quality) - 1

# Plot paris
#pairs(config2, col=config2$quality)

# Plot correlation matrix
corr <- round(cor(config2), 3)
corr <- corr
p.mat <- cor_pmat(corr)
ggcorrplot(corr,  hc.order = TRUE, lab = TRUE, p.mat = p.mat, insig = "blank")

#.plot_correlation_heatmap(config2)
#corrplot.mixed(cor(config2[names(config2) != "quality"]), tl.pos = "lt", tl.cex = .8, number.cex = .8)


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


.plot_scatter(config2, 'alcohol', 'density', 'quality',
              'caso di alta correlazione tra density e alcohol')
.plot_scatter(config2, 'free.sulfur.dioxide', 'total.sulfur.dioxide', 'quality',
              'caso di alta correlazione tra total.sulfur.dioxide e free.sulfur.dioxide')
.plot_scatter(config2, 'total.sulfur.dioxide', 'residual.sugar', 'quality',
              'caso di media correlazione tra total.sulfur.dioxide e residual.sugar')
.plot_scatter(config2, 'density', 'residual.sugar', 'quality', '') #non posso dirlo!

# Plot bar for the median values.
.plot_median_barplot(config2, 'alcohol') +
.plot_median_barplot(config2, 'density') +
.plot_median_barplot(config2, 'volatile.acidity') +
.plot_median_barplot(config2, 'chlorides') +
.plot_median_barplot(config2, 'citric.acid') +
.plot_median_barplot(config2, 'fixed.acidity') +
.plot_median_barplot(config2, 'free.sulfur.dioxide') +
.plot_median_barplot(config2, 'total.sulfur.dioxide') +
.plot_median_barplot(config2, 'sulphates') +
.plot_median_barplot(config2, 'residual.sugar') +
.plot_median_barplot(config2, 'pH')

# Plot bar for the mean values
.plot_mean_barplot(config2, 'alcohol') +
.plot_mean_barplot(config2, 'density') +
.plot_mean_barplot(config2, 'volatile.acidity') +
.plot_mean_barplot(config2, 'chlorides') +
.plot_mean_barplot(config2, 'citric.acid') +
.plot_mean_barplot(config2, 'fixed.acidity') +
.plot_mean_barplot(config2, 'free.sulfur.dioxide') +
.plot_mean_barplot(config2, 'total.sulfur.dioxide') +
.plot_mean_barplot(config2, 'sulphates') +
.plot_mean_barplot(config2, 'residual.sugar') +
.plot_mean_barplot(config2, 'pH')

"
As alcohol level increase ==> Quality increases
As density decreases ==> Quality increases
As the volatile acidity decreases ==> Quality increases
As chlorides level decreases ==> Quality increases
As citric acid level increases ==> Quality increases

fixed acidity ==> can’t say impact on Quality
As free sulfur dioxide increases ==> Quality increases
total sulfur dioxide ==> can’t say impact on Quality
sulphates ==> can’t say impact on Quality
As residual sugar increases ==> Quality increases
pH ==> can’t say impact on Quality

But since, only below four contributes towards wine quality : (alcohol, density, volatile acidity, chlorides)
- Increase in the alcohol qty, increases the quality of the wine.
- Decrease in the density of the wine, increases the quality of the wine.
- Decrease in the volatile acidity of the wine, increases the quality of the wine.
- Decrease in chlorides, increases the quality of the wine.
"



# Feature Selection 1
#combined$train$alcohol <- NULL
#combined$train$density <- NULL
#combined$train$volatile.acidity <- NULL
#combined$train$chlorides <- NULL
#combined$train$residual.sugar <- NULL
#combined$train$sulphates <- NULL
#combined$train$citric.acid <- NULL
#combined$train$pH <- NULL
#combined$train$free.sulfur.dioxide <- NULL
#combined$train$total.sulfur.dioxide <- NULL

#combined$test$alcohol <- NULL
#combined$test$density <- NULL
#combined$test$volatile.acidity <- NULL
#combined$test$chlorides <- NULL
#combined$test$residual.sugar <- NULL
#combined$test$sulphates <- NULL
#combined$test$citric.acid <- NULL
#combined$test$pH <- NULL
#combined$test$free.sulfur.dioxide <- NULL
#combined$test$total.sulfur.dioxide <- NULL

# Feature Selection 2
#combined$train <- calculate_pca(combined$train)
#combined$test <- calculate_pca(combined$test)

"highlyCorrelated <- findCorrelation(cor(combined$train[names(combined$train) != 'quality']), cutoff=0.5)
print(highlyCorrelated)

importance <- varImp(m3, scale=FALSE)
plot(importance)"