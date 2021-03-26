#' Exploratory Data Analysis
#'
#' add description.
#'

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(patchwork)

# Source scripts
source("../utils.R")

# Local Functions

.type_barplot <- function(data, title, y_lim = 4000) {
  data %>% ggplot(aes(x = quality, fill = quality)) +
    geom_bar(alpha = 0.7, color = "black") +
    ylim(0, y_lim) +
    ggtitle(title)
}

# Load dataset
whitewine <- read.csv("../../data/winequality-white.csv")

config1 <- relabeling(whitewine, 1) # white wine separate by two class
config2 <- relabeling(whitewine, 2) # white wine separate by three class
config3 <- relabeling(whitewine, 3) # white wine quality by different class

p1 <- .type_barplot(config1, "2 levels")
p2 <- .type_barplot(config2, "3 levels")
p3 <- .type_barplot(config3, "10 levels")

print(p3 + p2 + p1)

#source("./univariate.R")
#source("./outliers.R")
#source("./multivariate.R")
