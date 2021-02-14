#' Exploratory Data Analysis
#'
#' add description.
#'

.plot_variable_by_class <- function(data, input, class) {
  data %>%
    ggplot(aes_string(x = input, fill = class, color = class)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.2)
}

.plot_variable_boxplot <- function(data, input, class) {
  outlier_bounds <- .detect_outliers(data[[input]])

  data %>%
    ggplot(aes_string(y = input, x = class, fill = class)) +
    geom_boxplot(outlier.shape = "cross") +
    # geom_jitter(aes_string(color = class), size = 0.4, alpha = 0.5) +
    geom_hline(yintercept = outlier_bounds$lower, linetype = 'dotted') +
    geom_hline(yintercept = outlier_bounds$upper, linetype = 'dotted') +
    labs(y = input)
}

.plot_class_barplot <- function(data, class) {
  data %>%
    ggplot(aes_string(x = class, fill = class, color = class)) +
    geom_bar()
}

.plot_pca <- function(dataset) {
  normalized <- normalize_dataset(dataset)
  pca <- prcomp(cov(normalized[1:11]))

  # Calculate eigenvalue to check how many PCs we can pick up
  eig <- get_eigenvalue(pca)
  print(eig)
  pca_plot <- fviz_eig(pca, addlabels = TRUE, ncp = 11)
  plot(pca_plot)
}

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(corrplot, ggplot2, dplyr, naniar, patchwork, factoextra)

# Local functions
source("./utils.R")

# Import datasets
combined <- read.csv("./dataset/winequality-combined.csv")
redwine <- read.csv("./dataset/winequality-red.csv")
whitewine <- read.csv("./dataset/winequality-white.csv")

# Summary report
summary(combined)

# Check Missing Values
miss_var_summary(combined)

# Distribuzione Dati Red & White
.plot_class_barplot(combined, "quality")

# Distribuzione Dati Red Only
.plot_class_barplot(redwine, "quality")

# Distribuzione Dati White Only
.plot_class_barplot(whitewine, "quality")

# Distribuzione Dati Red & White Configurazione 1: Multiclasse
config3 <- preprocess_dataset(combined, 3)
.plot_class_barplot(config3, "quality")

# Distribuzione Dati Red & White Configurazione 2: 3 Classi
config2 <- preprocess_dataset(combined, 2)
.plot_class_barplot(config2, "quality")

# Distribuzione Dati Red & White Configurazione 3: 2 Classi
config1 <- preprocess_dataset(combined, 1)
.plot_class_barplot(config1, "quality")

# Distribuzione Dati Red only Configurazione 1: Multiclasse
config3 <- preprocess_dataset(redwine, 3)
.plot_class_barplot(config3, "quality")

# Distribuzione Dati Red only Configurazione 2: 3 Classi
config2 <- preprocess_dataset(redwine, 2)
.plot_class_barplot(config2, "quality")

# Distribuzione Dati Red only Configurazione 3: 2 Classi
config1 <- preprocess_dataset(redwine, 1)
.plot_class_barplot(config1, "quality")

# Distribuzione Dati white only Configurazione 1: Multiclasse
config3 <- preprocess_dataset(whitewine, 3)
.plot_class_barplot(config3, "quality")

# Distribuzione Dati white only Configurazione 2: 3 Classi
config2 <- preprocess_dataset(whitewine, 2)
.plot_class_barplot(config2, "quality")

# Distribuzione Dati white only Configurazione 3: 2 Classi
config1 <- preprocess_dataset(whitewine, 1)
.plot_class_barplot(config1, "quality")

#Attribute Distribution By Class
config1_0<-config1 %>% filter(quality == 0)
config1_1<-config1 %>% filter(quality == 1)

#fixed.acidity global distribution
ggplot(combined, aes(x=fixed.acidity, title=fixed.acidity)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#fixed.acidity distribution by class
p1<-ggplot(config1_0, aes(x=fixed.acidity)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,800) +
  ggtitle("fixed.acidity quality 0")

p2<-ggplot(config1_1, aes(x=fixed.acidity)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,800) +
  ggtitle("fixed.acidity quality 1")

p1+p2

#volatile.acidity global distribution
ggplot(combined, aes(x=combined$volatile.acidity, title=volatile.acidity)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#volatile.acidity distribution by class
p1<-ggplot(config1_0, aes(x=volatile.acidity)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,800) +
  ggtitle("volatile.acidity quality 0")

p2<-ggplot(config1_1, aes(x=volatile.acidity)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,800) +
  ggtitle("volatile.acidity quality 1")

p1+p2

#citric.acid global distribution
ggplot(combined, aes(x=citric.acid, title=citric.acid)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#citric.acid distribution by class
p1<-ggplot(config1_0, aes(x=citric.acid)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,1100) +
  ggtitle("citric.acid quality 0")

p2<-ggplot(config1_1, aes(x=citric.acid)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,1100) +
  ggtitle("citric.acid quality 1")

p1+p2

#residual.sugar global distribution
ggplot(combined, aes(x=residual.sugar, title=residual.sugar)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#residual.sugar distribution by class
p1<-ggplot(config1_0, aes(x=residual.sugar)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,1300) +
  ggtitle("residual.sugar quality 0")

p2<-ggplot(config1_1, aes(x=residual.sugar)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,1300) +
  ggtitle("residual.sugar quality 1")

p1+p2

#chlorides global distribution
ggplot(combined, aes(x=chlorides, title=chlorides)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#chlorides distribution by class
p1<-ggplot(config1_0, aes(x=chlorides)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,1700) +
  ggtitle("chlorides quality 0")

p2<-ggplot(config1_1, aes(x=chlorides)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,1700) +
  ggtitle("chlorides quality 1")

p1+p2

#free.sulfur.dioxide global distribution
ggplot(combined, aes(x=free.sulfur.dioxide, title=free.sulfur.dioxide)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#free.sulfur.dioxide distribution by class
p1<-ggplot(config1_0, aes(x=free.sulfur.dioxide)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,900) +
  ggtitle("free.sulfur.dioxide quality 0")

p2<-ggplot(config1_1, aes(x=free.sulfur.dioxide)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,900) +
  ggtitle("free.sulfur.dioxide quality 1")

p1+p2

#total.sulfur.dioxide global distribution
ggplot(combined, aes(x=total.sulfur.dioxide, title=total.sulfur.dioxide)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#total.sulfur.dioxide distribution by class
p1<-ggplot(config1_0, aes(x=total.sulfur.dioxide)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,600) +
  ggtitle("total.sulfur.dioxide quality 0")

p2<-ggplot(config1_1, aes(x=total.sulfur.dioxide)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,600) +
  ggtitle("total.sulfur.dioxide quality 1")

p1+p2

#density global distribution
ggplot(combined, aes(x=density, title=density)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#density distribution by class
p1<-ggplot(config1_0, aes(x=density)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,900) +
  ggtitle("density quality 0")

p2<-ggplot(config1_1, aes(x=density)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,900) +
  ggtitle("density quality 1")

p1+p2

#pH global distribution
ggplot(combined, aes(x=pH, title=pH)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#pH distribution by class
p1<-ggplot(config1_0, aes(x=pH)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,500) +
  ggtitle("pH quality 0")

p2<-ggplot(config1_1, aes(x=pH)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,500) +
  ggtitle("pH quality 1")

p1+p2

#sulphates global distribution
ggplot(combined, aes(x=sulphates, title=sulphates)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#sulphates distribution by class
p1<-ggplot(config1_0, aes(x=sulphates)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,500) +
  ggtitle("sulphates quality 0")

p2<-ggplot(config1_1, aes(x=sulphates)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,500) +
  ggtitle("sulphates quality 1")

p1+p2

#alcohol global distribution
ggplot(combined, aes(x=alcohol, title=alcohol)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  geom_freqpoly(color="blue")

#alcohol distribution by class
p1<-ggplot(config1_0, aes(x=alcohol)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,400) +
  ggtitle("alcohol quality 0")

p2<-ggplot(config1_1, aes(x=alcohol)) +
  geom_histogram(alpha=0.7, color="red", fill="#FF6666") +
  ylim(0,400) +
  ggtitle("alcohol quality 1")

p1+p2

# Plot boxplot
.plot_variable_boxplot(config1, "pH", "quality")

# Distribuzione delle singole covariate
.plot_variable_by_class(config1, "pH", "quality")

# Plot correlation matrix
corrplot.mixed(cor(config1[names(config1) != "quality"]), tl.pos = "lt", tl.cex = .8, number.cex = .8)

# PCA Analysis
.plot_pca(config1)
