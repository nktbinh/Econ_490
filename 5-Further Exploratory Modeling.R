library(rpart)
library(rpart.plot)

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features
features <- c("gender","race","maristat","educat","k6")
rf.label <- as.factor(df_train$alcbin)
rpart.train.1 <- df_train[,features]
cv.10.folds <- createMultiFolds(rf.label, k = 2, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 2, repeats = 10,
                       index = cv.10.folds)

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(10000, rpart.train.1, rf.label, ctrl.1)
options(scipen = 999)
rpart.1.cv.1
rpart.1.cv.1$finalModel

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

table(df_train$race,df_train$alcbin)
