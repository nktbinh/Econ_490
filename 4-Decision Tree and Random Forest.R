library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
library(doSNOW)

# Decision Tree using package::party's CTREE
dec_tree <- function(df) {
  
  tree <- ctree(alcbin ~ age+race+educat+health+maristat+insured+race+gender+k6, data=df, 
                controls = ctree_control(
                  teststat="quad",
                  testtype="Univariate",
                  mincriterion=.95,
                  minsplit=10, 
                  minbucket=5,
                  maxdepth=0
                ))
  
  df$pred <- predict(tree, newdata=df, type="prob")
  df$pred <- unlist(lapply(df$pred, function(el)el[2]))
  
  return(df)
}
df_test1<-dec_tree(df_test)

ROCRpredCtree = prediction(df_test1$pred, df_test1$alcbin) #ROCR prediction function
ROCRperfCtree = performance(ROCRpredCtree, "tpr", "fpr") # Performance Function
auc = as.numeric(performance(ROCRpredCtree, "auc")@y.values)
auc

plot(ROCRperfCtree) # Plot ROC curve
plot(ROCRperfCtree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Add colors and threshold labels


# Decision Tree with package::rpart with Cross Validation to establish optimal parameter
numFolds = trainControl( method = "cv", number = 100 )
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 
# Perform the cross validation
train(alcbin ~ age+race+educat+health+maristat+insured+race+gender+k6, data = df_train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
binhTreeCV = rpart(alcbin ~ age+race+educat+health+maristat+insured+race+gender+k6, data = df_train, method="class", cp = 0.001)
prp(binhTreeCV)

# Make predictions
PredictCV = predict(binhTreeCV, newdata = df_test, type = "class")
table(df_test$alcbin, PredictCV)
(4541+1612)/nrow(df_test)


# RANDOM FOREST
rf.train.1 <- df_train[,c("gender","age","race","inc","health","maristat","insured","educat","k6")]
rf.label <- as.factor(df_train$alcbin)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# ------------------------------------------------------------------------
# CROSS - VALIDATION

# Leverage caret to create 20 folds
set.seed(2348)
rf.label <- as.factor(df_train$alcbin)
rf.train.1 <- df_train[,c("gender","race","maristat","educat","k6")]
cv.10.folds <- createMultiFolds(rf.label, k = 2, times = 10)

# Check stratification
table(rf.label)
table(rf.label[cv.10.folds[[77]]])

# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC (only Linus and Mac)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
rf.1.cv.1 <- train(x = rf.train.1, y = rf.label, method = "rf", tuneLength = 2,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1

