#' Exploratory Data Analysis
#'
#' add description.
#'
.type_barplot <- function(data, color, title) {
  data %>% ggplot(aes(x = quality, fill = quality)) +
    geom_bar(alpha = 0.7, color = "black") +
    ylim(0, 5000) +
    ggtitle(title)
}

# Import dataset
whitewine <- read.csv("../../data/winequality-combined.csv")
whitewine <- filter(whitewine, type == "white") # only white wine

config0_whitewine <- relabeling(whitewine, 3) # white wine quality by different class
config1_whitewine <- relabeling(whitewine, 1) # white wine separate by two class
config2_whitewine <- relabeling(whitewine, 2) # white wine separate by three class

p4 <- .type_barplot(config0_whitewine, "#FFFFFF", "by 10 class")
p5 <- .type_barplot(config1_whitewine, "#FFFFFF", "by two class(x>6)")
p6 <- .type_barplot(config2_whitewine, "#FFFFFF", "by three class(x<=5, 5<x<7, x>=7)")
print(p4 + p6 + p5)

#source("./univariate.R")
#source("./outliers.R")
#source("./multivariate.R")
