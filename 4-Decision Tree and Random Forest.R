library(rpart) # CART
library(rpart.plot) # CART plot
library(randomForest) 
library(caret) # Cross Validation
library(e1071) # Misc Statistical functions
library(doSNOW) # Multi-cores processing
require(party)


cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
tree <- ctree(alcbin ~ ., data=df_train, 
              controls = ctree_control(
                teststat="quad",
                testtype="Bonferroni",
                mincriterion=.95,
                minsplit=10, 
                minbucket=5,
                maxdepth=0
))
stopCluster(cl)
ctree_predTest <- Predict(tree, df_test)
table(df_test$alcbin,ctree_predTest)
mean(ctree_predTest==df_test$alcbin)

require(ROCR)
ctree_predProb <- predict(tree, newdata=df_test, type="prob")
ctree_predProb <- unlist(lapply(ctree_predProb, function(el)el[2]))

ROCRpredCtree = prediction(ctree_predProb, df_test$alcbin) 
ROCRperfCtree = performance(ROCRpredCtree, "tpr", "fpr") 
as.numeric(performance(ROCRpredCtree, "auc")@y.values)

plot(ROCRperfCtree, 
     colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7)) 


# Cross Validation with method Rpart
numFolds = trainControl( method = "cv", number = 100 )
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 

train(alcbin ~ age+race+educat+health+maristat+insured+race+gender+k6, 
      data = df_train, 
      method = "rpart", 
      trControl = numFolds, 
      tuneGrid = cpGrid )

# Create a new CART model
binhTreeCV = rpart(alcbin ~ ., 
                   data = df_train, 
                   method="class", 
                   cp = 0.001)
prp(binhTreeCV)

# Make predictions
PredictCV = predict(binhTreeCV, newdata = df_test, type = "class")
table(df_test$alcbin, PredictCV)


# RANDOM FOREST
# WARNING!!! - COMPUTATIONALLY INTENSIVE
rf.train.1 <- df_train[,-10]
rf.label <- as.factor(df_train$alcbin)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, 
                     y = rf.label, 
                     importance = TRUE, 
                     ntree = 1000)
rf.1
varImpPlot(rf.1)

# ------------------------------------------------------------------------
# Cross Validation with method rf

# Leverage caret to create 20 folds
require(caret)
set.seed(1234)
rf.label <- as.factor(df_train$alcbin)
rf.train.1 <- df_train[,1:9]
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

# Check stratification
table(rf.label)
table(rf.label[cv.5.folds[[34]]])

# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)


# Multi-core training
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(1234)
rf.1.cv.1 <- train(x = rf.train.1, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 100, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.1.cv.1

