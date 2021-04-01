#' Preliminar Analysis

# Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(patchwork)

# Source scripts
source("../utils.R")

.type_barplot <- function(data, title, y_lim = 4000) {
  data %>% ggplot(aes(x = quality, fill = quality)) +
    geom_bar(alpha = 0.7, color = "black") +
    ylim(0, y_lim) +
    ggtitle(title)
}

# Load dataset
whitewine <- read.csv("../../data/winequality-white.csv")

config1 <- relabeling(whitewine, 1) # 2 levels
config2 <- relabeling(whitewine, 2) # 3 levels
config3 <- relabeling(whitewine, 3) # 10 levels

p1 <- .type_barplot(config1, "2 levels")
p2 <- .type_barplot(config2, "3 levels")
p3 <- .type_barplot(config3, "10 levels")

print(p3 + p2 + p1)