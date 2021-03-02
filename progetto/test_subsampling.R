library(caret)

source("./utils.R")

dataset <- read.csv('./dataset/winequality-combined.csv')

# Setup quality
dataset$quality <- ifelse(dataset$quality > 6, 'good', 'bad')
dataset$quality <- factor(dataset$quality)
dataset$type <- NULL

# Create Partition
set.seed(2969)
index <- createDataPartition(dataset$quality, p = 0.75, list = FALSE)
trainset <- dataset[index,]
testset <- dataset[-index,]


#Subsampling
set.seed(9560)
down_train <- downSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
names(down_train)[names(down_train) == "Class"] <- "quality"

set.seed(9560)
up_train <- upSample(x = trainset[, -ncol(trainset)], y = trainset$quality)
names(up_train)[names(up_train) == "Class"] <- "quality"

library(DMwR)
set.seed(9560)
smote_train <- SMOTE(quality ~ ., trainset, perc.over = 600,perc.under=100)

library(ROSE)
set.seed(9560)
rose_train <- ROSE(quality ~ ., data  = trainset)$data



train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

grid_radial <- expand.grid(
  sigma = 0.9,#c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4),
  C = 1.2#c(0.1,0.8,0.9,1,1.1,1.2,1.3,1.4)
)

set.seed(5627)
orig_fit <- train(
  quality ~ .,
  data = trainset,
  method = "svmRadial",
  tuneGrid = grid_radial,
  trControl = train_control
)

set.seed(5627)
down_outside <- train(
  quality ~ .,
  data = down_train,
  method = "svmRadial",
  tuneGrid = grid_radial,
  trControl = train_control
)

set.seed(5627)
up_outside <- train(
  quality ~ .,
  data = up_train,
  method = "svmRadial",
  tuneGrid = grid_radial,
  trControl = train_control
)

set.seed(5627)
smote_outside <- train(
  quality ~ .,
  data = smote_train,
  method = "svmRadial",
  tuneGrid = grid_radial,
  trControl = train_control
)

set.seed(5627)
rose_outside <- train(
  quality ~ .,
  data = rose_train,
  method = "svmRadial",
  tuneGrid = grid_radial,
  trControl = train_control
)

outside_models <- list(original = orig_fit,
                       down = down_outside,
                       up = up_outside,
                       SMOTE = smote_outside,
                       ROSE = rose_outside)

outside_resampling <- resamples(outside_models)

test_roc <- function(model, data) {
  library(pROC)
  y <- as.numeric(data$quality) - 1
  x <- as.numeric(predict(model, data[names(data) != "quality"]))-1
  roc_obj <- roc(y, x, levels = c("good", "bad"))
  ci(roc_obj)
  }

outside_test <- lapply(outside_models, test_roc, data = trainset)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
outside_test <- as.data.frame(outside_test)

print(outside_test)

summary(outside_resampling)


"             lower       ROC     upper
original 0.9033949 0.9151432 0.9268914
down     0.9047712 0.9115003 0.9182293
up       0.9799018 0.9834585 0.9870152
SMOTE    0.9757811 0.9791605 0.9825399
ROSE     0.7923760 0.8034289 0.8144819

###########################################

Models: original, down, up, SMOTE, ROSE
Number of resamples: 50

Accuracy
              Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
original 0.8459959 0.8603696 0.8647541 0.8659554 0.8726899 0.8911704    0
down     0.7434555 0.7718423 0.7905759 0.7908012 0.8070394 0.8541667    0
up       0.9411765 0.9492178 0.9527458 0.9529247 0.9566327 0.9654731    0
SMOTE    0.9711075 0.9759084 0.9779203 0.9782722 0.9807384 0.9863454    0
ROSE     0.7008197 0.7213115 0.7351129 0.7344559 0.7433265 0.7807377    0

Kappa
              Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
original 0.3974463 0.4727084 0.5013946 0.4986918 0.5216671 0.6084912    0
down     0.4866999 0.5437740 0.5812551 0.5816027 0.6140709 0.7083333    0
up       0.8823529 0.8984310 0.9054829 0.9058493 0.9132653 0.9309463    0
SMOTE    0.9416522 0.9514012 0.9554712 0.9561642 0.9611476 0.9724935    0
ROSE     0.4019708 0.4428966 0.4709909 0.4692996 0.4869860 0.5615049    0
"
