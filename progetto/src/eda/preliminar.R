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

preliminar_path <- file.path("..", "..", "plots", "eda", "preliminar")
create_dir_if_not_exists(preliminar_path)
save <- TRUE

# Load dataset
redwine <- read.csv("../../data/winequality-red.csv")

config1 <- relabeling(redwine, 1) # 2 levels
config2 <- relabeling(redwine, 2) # 3 levels
config3 <- relabeling(redwine, 3) # 10 levels

p1 <- .type_barplot(config1, "2 levels")
p2 <- .type_barplot(config2, "3 levels")
p3 <- .type_barplot(config3, "10 levels")

p <- p3 + p2 / p1

print_or_save(p, file.path(preliminar_path, "class_hist.png"), save = save, wide = TRUE)

print_or_save(p1, file.path(preliminar_path, "class_hist1.png"), save = save)
print_or_save(p2, file.path(preliminar_path, "class_hist2.png"), save = save)
print_or_save(p3, file.path(preliminar_path, "class_hist3.png"), save = save)
