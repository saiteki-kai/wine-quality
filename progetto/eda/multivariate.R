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

.plot_boxplot <- function(dataset, attr1, class, title) {
  p <- ggplot(dataset, aes_string(x=attr1, fill=class)) +
    geom_boxplot() +
    ggtitle(title)
  p
}


.combined_barplot <- function(data, title) {
  p <- ggplot(data, aes(x = quality)) +
    scale_fill_manual(values = c("red" = "#FF6666", "white" = "white")) +
    geom_bar(aes(fill = type), position = "dodge2", alpha = 0.7, color = "black") +
    # stat_count(
    #   aes(y = ..count..),
    #   label = y ,
    #   geom = "text",
    #   position = position_dodge(width = 1),
    #   vjust = -0.5, size = 2,
    #   color = "black") +
    ggtitle(title)
  p
}

.plot_pieplot <- function(dataset) {
  mytable <- table(dataset$type)
  lbls <- paste(names(mytable), "\n", mytable, sep="")
  pie(mytable, labels = lbls,
      main="Wine Type Distribution",
      col=c('#FF6666', '#FFFFFF'))
}

.plot_corrmatrix <- function(dataset, title) {
  corr <- round(cor(dataset), 2)
  corr <- corr
  p.mat <- cor_pmat(corr)
  ggcorrplot(corr,  hc.order = TRUE, lab = TRUE, p.mat = p.mat, insig = "blank") +
    ggtitle(title)
}

.plot_pairplot <- function(data, class, title) {
 pm <- ggpairs(data,
  mapping = ggplot2::aes(color = factor(class)),
  upper = list(continuous = "cor", combo = "box_no_facet", discrete = "facetbar", na = "na"),
  lower = list(continuous = wrap("smooth_loess", alpha = 0.3, size = 1)), #color = "#488bf7"
  title = title
)
pm
}

.print_stats <- function(df) {
  df$quality <- NULL
  stats <- describe(df)
  res <- stats %>% dplyr::select(-c("mad", "trimmed", "range", "se", "n"))
  res
}


# Install packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(corrplot, ggplot2, dplyr, naniar,
               patchwork, factoextra, reshape2,
               ggcorrplot, scales, GGally, MASS, bestNormalize, psych)

# Local functions
source('../utils.R')



########################################################################################################################

# Import dataset Red & White
dataset <- read.csv("../dataset/winequality-combined.csv") %>%
  preprocess_dataset(2) %>%
  mutate(quality = factor(quality)) %>%
  mutate(type = factor(type)) %>%
  as.data.frame()

# Type Distribution
.plot_pieplot(dataset)

# Quality Distribution
.combined_barplot(dataset, "3 class config")

#dataset$type <- ifelse(dataset$type == "red", 1, 0)
#dataset$type <- as.numeric(dataset$type) - 1
#dataset$quality <- as.numeric(dataset$quality) - 1

# Split Data Red & White Only
red <- dataset[dataset$type == "red", ]
red$type <- NULL
red$quality <- as.numeric(red$quality) - 1

.print_stats(red)

white <- dataset[dataset$type == "white", ]
white$type <- NULL
white$quality <- as.numeric(white$quality) - 1

.print_stats(white)

# Plot correlation matrix Red and White Only
.plot_corrmatrix(red, "Red Only Correlations") +
.plot_corrmatrix(white, "White Only Correlations")

########################################################################################################################



# Plot correlation matrix Red & White
dataset$quality <- as.numeric(dataset$quality) - 1
dataset$type <- NULL
.plot_corrmatrix(dataset, "Red & White Correlations")


# Log 10 Transformation (just for visualization purpose)
dataset$alcohol <- log10(dataset$alcohol)
dataset$density <- log10(dataset$density)
dataset$residual.sugar <- log10(dataset$residual.sugar)

dataset$volatile.acidity <- log10(dataset$volatile.acidity)
dataset$citric.acid <- log10(dataset$citric.acid)
dataset$pH <- log10(dataset$pH)
dataset$fixed.acidity <- log10(dataset$fixed.acidity)

dataset$free.sulfur.dioxide <- log10(dataset$free.sulfur.dioxide)
dataset$total.sulfur.dioxide <- log10(dataset$total.sulfur.dioxide)
dataset$sulphates <- log10(dataset$sulphates)

dataset$chlorides <- log10(dataset$chlorides)


# Correlation with quality
.plot_scatter(dataset, 'quality', 'alcohol', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'density', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'chlorides', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'volatile.acidity', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'residual.sugar', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'sulphates', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'citric.acid', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'fixed.acidity', 'quality', '') #+
.plot_scatter(dataset, 'quality','pH', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'free.sulfur.dioxide', 'quality', '') #+
.plot_scatter(dataset, 'quality', 'total.sulfur.dioxide', 'quality', '') #+
  plot_layout(guides = "collect")

# Other Relevants cases
.plot_scatter(dataset, 'free.sulfur.dioxide', 'total.sulfur.dioxide', 'quality',
              'caso di alta correlazione tra total.sulfur.dioxide e free.sulfur.dioxide')
.plot_scatter(dataset, 'total.sulfur.dioxide', 'residual.sugar', 'quality',
              'caso di media correlazione tra total.sulfur.dioxide e residual.sugar')


.plot_scatter(dataset, 'alcohol', 'density', 'quality',
              'caso di alta correlazione tra density e alcohol')
.plot_scatter(dataset, 'alcohol', 'residual.sugar', 'quality',
              'caso di correlazione tra alcohol e residual.sugar')


.plot_scatter(dataset, 'density', 'chlorides', 'quality',
              'caso di media correlazione tra density e chlorides')

.plot_scatter(dataset, 'chlorides', 'volatile.acidity', 'quality',
              'caso di media correlazione tra chlorides e volatile.acidity')
.plot_scatter(dataset, 'chlorides', 'sulphates', 'quality',
              'caso di media correlazione tra chlorides e sulphates')


.plot_scatter(dataset, 'volatile.acidity', 'free.sulfur.dioxide', 'quality',
              'caso di media correlazione tra volatile.acidity e free.sulfur.dioxide')
.plot_scatter(dataset, 'volatile.acidity', 'total.sulfur.dioxide', 'quality',
              'caso di media correlazione tra volatile.acidity e total.sulfur.dioxide')
.plot_scatter(dataset, 'volatile.acidity', 'citric.acid', 'quality',
              'caso di media correlazione tra volatile.acidity e citric.acid')


# pair plot for (alcohol, density, volatile acidity, chlorides)
df <- data.frame(alcohol=dataset$alcohol,
                   density=dataset$density,
                   volatile.acidity=dataset$volatile.acidity,
                   chlorides=dataset$chlorides)
.plot_pairplot(df, dataset$quality, "4 most important features (alcohol, density, volatile acidity, chlorides)")


# pair plot for (alcohol, density, residual.sugar)
df <- data.frame(alcohol=dataset$alcohol,
                   density=dataset$density,
                   residual.sugar=dataset$residual.sugar)
.plot_pairplot(df, dataset$quality, "(alcohol, density, residual.sugar)")

# pair plot for (volatile.acidity, citric.acid, pH, fixed.acidity)
df <- data.frame(volatile.acidity=dataset$volatile.acidity,
                   citric.acid=dataset$citric.acid,
                   pH=dataset$pH,
                   fixed.acidity=dataset$fixed.acidity)
.plot_pairplot(df, dataset$quality, "(volatile.acidity, citric.acid, pH, fixed.acidity)")

# pair plot for (free.sulfur.dioxide, total.sulfur.dioxide, sulphates)
df <- data.frame(free.sulfur.dioxide=dataset$free.sulfur.dioxide,
                   total.sulfur.dioxide=dataset$total.sulfur.dioxide,
                   sulphates=dataset$sulphates)
.plot_pairplot(df, dataset$quality, "(free.sulfur.dioxide, total.sulfur.dioxide, sulphates)")



# T-test for feature selection
#t.test(dataset$quality, dataset$sulphates)

#x <- dataset[dataset$quality == "bad", ]
#y <- dataset[dataset$quality == "good", ]
#t.test(x$alcohol,y$alcohol)
#bartlett.test(alcohol ~ quality, trainset_pca)
#t.test(alcohol ~ quality, trainset_pca, var.equal=FALSE, conf.level=0.95)
#t.test(alcohol ~ quality, trainset_pca, var.equal=TRUE, conf.level=0.95)

#library(lattice)
#histogram(~ alcohol | quality, data=trainset_pca, layout=c(1,2))
#boxplot(alcohol ~ quality, data = trainset_pca, names=c("bad","good"), ylab="Value")

#res.aov <- aov(weight ~ group, data = my_data)
#summary(res.aov)