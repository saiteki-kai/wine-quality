.get_model <- function(name) {
  filename <- file.path("./results/models", paste0(name, "_model.RDS"))
  if (file.exists(filename)) {
    readRDS(filename)
  }
}

# Local functions
source("./utils.R")
source("models/dt.R")
source("models/nb.R")
source("models/svm.R")
source("models/nn.R")

# Prepare the dataset
dataset <- read.csv("./dataset/winequality-test.csv") %>%
  mutate(quality = factor(quality))

# Normalize the data
dataset <- normalize_dataset(dataset)

m1 <- .get_model("nb")
m2 <- .get_model("dt")
m3 <- .get_model("svm")
m4 <- .get_model("nn")

# Evaluate the model
res1 <- evaluate_model(m1, dataset)
res2 <- evaluate_model(m2, dataset)
res3 <- evaluate_model(m3, dataset)
res4 <- evaluate_model(m4, dataset)

# Plot AUCs ROC & PRC
n_classes <- length(unique(as.numeric(dataset$quality) - 1))
models <- list(nb_model = m1, dt_model = m2, svm_model = m3, nn_model = m4)
plot_roc_and_prc_all(dataset, models, n_classes)
