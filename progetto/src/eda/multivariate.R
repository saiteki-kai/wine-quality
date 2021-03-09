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
    geom_smooth(method = "lm", formula = y ~ x) +
    ggtitle(title)
}

.plot_median_barplot <- function(trainset, attr1) {
  x <- aggregate(trainset[[attr1]], by = list(trainset$quality), FUN = median)
  ggplot(data = x, aes_string(x = "Group.1", y = "x")) +
    geom_bar(stat = "identity", aes_string(fill = "Group.1")) +
    ggtitle(attr1)
}

.plot_boxplot <- function(trainset, attr1, class, title) {
  p <- ggplot(trainset, aes_string(x = attr1, fill = class)) +
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

.plot_pieplot <- function(trainset) {
  mytable <- table(trainset$type)
  lbls <- paste0(names(mytable), "\n", mytable)
  pie(mytable,
    labels = lbls,
    main = "Wine Type Distribution",
    col = c("#FF6666", "#FFFFFF")
  )
}

.plot_corrmatrix <- function(trainset, title) {
  corr <- round(cor(trainset), 2)
  corr <- corr
  p.mat <- cor_pmat(corr)
  ggcorrplot(corr, hc.order = TRUE, lab = TRUE, p.mat = p.mat, insig = "blank") +
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

.print_stats <- function(df) {
  df$quality <- NULL
  stats <- describe(df)
  res <- stats %>% dplyr::select(-c("mad", "trimmed", "range", "se", "n"))
  res
}


# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  corrplot, ggplot2, dplyr, naniar,
  patchwork, factoextra, reshape2,
  ggcorrplot, scales, GGally, MASS, bestNormalize, psych
)

# Local functions
source("../utils.R")



########################################################################################################################

# Load the dataset
dataset <- read.csv("../../data/winequality-combined.csv")

# Setup quality
dataset <- relabeling(dataset)

# Create Partition
set.seed(444)
index <- createDataPartition(dataset$quality, p = 0.70, list = FALSE)
trainset <- dataset[index, ]
testset <- dataset[-index, ]


# Type Distribution
.plot_pieplot(trainset)

# Quality Distribution
.combined_barplot(trainset, "3 class config")

# trainset$type <- ifelse(trainset$type == "red", 1, 0)
# trainset$type <- as.numeric(trainset$type) - 1
# trainset$quality <- as.numeric(trainset$quality) - 1

# Split Data Red & White Only
red <- trainset[trainset$type == "red", ]
red$type <- NULL
red$quality <- as.numeric(red$quality) - 1

.print_stats(red)

white <- trainset[trainset$type == "white", ]
white$type <- NULL
white$quality <- as.numeric(white$quality) - 1

.print_stats(white)

# Plot correlation matrix Red and White Only
.plot_corrmatrix(red, "Red Only Correlations") +
  .plot_corrmatrix(white, "White Only Correlations")

########################################################################################################################



# Plot correlation matrix Red & White
trainset$quality <- as.numeric(trainset$quality) - 1
trainset$type <- NULL
.plot_corrmatrix(trainset, "Red & White Correlations")


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
.plot_scatter(trainset, "quality", "alcohol", "quality", "") #+
.plot_scatter(trainset, "quality", "density", "quality", "") #+
.plot_scatter(trainset, "quality", "chlorides", "quality", "") #+
.plot_scatter(trainset, "quality", "volatile.acidity", "quality", "") #+
.plot_scatter(trainset, "quality", "residual.sugar", "quality", "") #+
.plot_scatter(trainset, "quality", "sulphates", "quality", "") #+
.plot_scatter(trainset, "quality", "citric.acid", "quality", "") #+
.plot_scatter(trainset, "quality", "fixed.acidity", "quality", "") #+
.plot_scatter(trainset, "quality", "pH", "quality", "") #+
.plot_scatter(trainset, "quality", "free.sulfur.dioxide", "quality", "") #+
.plot_scatter(trainset, "quality", "total.sulfur.dioxide", "quality", "") #+
plot_layout(guides = "collect")

# Other Relevants cases
.plot_scatter(
  trainset, "free.sulfur.dioxide", "total.sulfur.dioxide", "quality",
  "caso di alta correlazione tra total.sulfur.dioxide e free.sulfur.dioxide"
)
.plot_scatter(
  trainset, "total.sulfur.dioxide", "residual.sugar", "quality",
  "caso di media correlazione tra total.sulfur.dioxide e residual.sugar"
)


.plot_scatter(
  trainset, "alcohol", "density", "quality",
  "caso di alta correlazione tra density e alcohol"
)
.plot_scatter(
  trainset, "alcohol", "residual.sugar", "quality",
  "caso di correlazione tra alcohol e residual.sugar"
)


.plot_scatter(
  trainset, "density", "chlorides", "quality",
  "caso di media correlazione tra density e chlorides"
)

.plot_scatter(
  trainset, "chlorides", "volatile.acidity", "quality",
  "caso di media correlazione tra chlorides e volatile.acidity"
)
.plot_scatter(
  trainset, "chlorides", "sulphates", "quality",
  "caso di media correlazione tra chlorides e sulphates"
)


.plot_scatter(
  trainset, "volatile.acidity", "free.sulfur.dioxide", "quality",
  "caso di media correlazione tra volatile.acidity e free.sulfur.dioxide"
)
.plot_scatter(
  trainset, "volatile.acidity", "total.sulfur.dioxide", "quality",
  "caso di media correlazione tra volatile.acidity e total.sulfur.dioxide"
)
.plot_scatter(
  trainset, "volatile.acidity", "citric.acid", "quality",
  "caso di media correlazione tra volatile.acidity e citric.acid"
)


# pair plot for (alcohol, density, volatile acidity, chlorides)
df <- data.frame(
  alcohol = trainset$alcohol,
  density = trainset$density,
  volatile.acidity = trainset$volatile.acidity,
  chlorides = trainset$chlorides
)
.plot_pairplot(df, trainset$quality, "4 most important features (alcohol, density, volatile acidity, chlorides)")


# pair plot for (alcohol, density, residual.sugar)
df <- data.frame(
  alcohol = trainset$alcohol,
  density = trainset$density,
  residual.sugar = trainset$residual.sugar
)
.plot_pairplot(df, trainset$quality, "(alcohol, density, residual.sugar)")

# pair plot for (volatile.acidity, citric.acid, pH, fixed.acidity)
df <- data.frame(
  volatile.acidity = trainset$volatile.acidity,
  citric.acid = trainset$citric.acid,
  pH = trainset$pH,
  fixed.acidity = trainset$fixed.acidity
)
.plot_pairplot(df, trainset$quality, "(volatile.acidity, citric.acid, pH, fixed.acidity)")

# pair plot for (free.sulfur.dioxide, total.sulfur.dioxide, sulphates)
df <- data.frame(
  free.sulfur.dioxide = trainset$free.sulfur.dioxide,
  total.sulfur.dioxide = trainset$total.sulfur.dioxide,
  sulphates = trainset$sulphates
)
.plot_pairplot(df, trainset$quality, "(free.sulfur.dioxide, total.sulfur.dioxide, sulphates)")



# T-test for feature selection
# t.test(trainset$quality, trainset$sulphates)

# x <- trainset[trainset$quality == "bad", ]
# y <- trainset[trainset$quality == "good", ]
# t.test(x$alcohol,y$alcohol)
# bartlett.test(alcohol ~ quality, trainset_pca)
# t.test(alcohol ~ quality, trainset_pca, var.equal=FALSE, conf.level=0.95)
# t.test(alcohol ~ quality, trainset_pca, var.equal=TRUE, conf.level=0.95)

# library(lattice)
# histogram(~ alcohol | quality, data=trainset_pca, layout=c(1,2))
# boxplot(alcohol ~ quality, data = trainset_pca, names=c("bad","good"), ylab="Value")

# res.aov <- aov(weight ~ group, data = my_data)
# summary(res.aov)
