library(mlr)
require(xgboost)
library(caret)

# One hot encoding
tr_labels<-df_train$alcbin
ts_labels<-df_test$alcbin
new_tr<-model.matrix(~.+0,data = df_train[,-9])
new_ts<-model.matrix(~.+0,data = df_test[,-9])
# convert factor to numeric
tr_labels <- as.numeric(tr_labels)-1
ts_labels <- as.numeric(ts_labels)-1
# 
dtrain<-xgb.DMatrix(data = new_tr, label = tr_labels)
dtest<-xgb.DMatrix(data = new_ts, label = ts_labels)

params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xgbcv <- xgb.cv(params = params, 
                data = dtrain, nrounds = 100, 
                nfold = 5, showsd = T, 
                stratified = T, print_every_n = 10,
                early_stop_round = 20, maximize = F)

xgb1 <- xgb.train(params = params, 
                  data = dtrain, nrounds = 100, 
                  watchlist = list(val=dtest,train=dtrain), 
                  print_every_n = 10, 
                  early_stop_round = 10, 
                  maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)


confusionMatrix(xgbpred, ts_labels) #0.6974
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:20]) 


#####
train.task <- makeClassifTask(data = df_train,target = "alcbin")
test.task <- makeClassifTask(data = df_test,target = "alcbin")

train.task <- createDummyFeatures(obj = train.task)
test.task <- createDummyFeatures(obj = test.task)

set.seed(1234)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 100L,
  eta=0.1
)

# Define Parameters for Tuning
xg_ps <- makeParamSet( 
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 3L,upper = 10L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)

# Search Strategy
ctrl <- makeTuneControlRandom(maxit = 10L) #do 10 iterations

# 5 Fold Cross Validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

# Set Parallel Backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

# Tune Parameters
xgb_tune <- tuneParams(learner = xgb_learner, 
                       task = train.task, 
                       resampling = set_cv, 
                       measures = acc, 
                       par.set = xg_ps, control = ctrl, show.info = T)

xgb_tune
xgb_tune$y # acc.test.mean = 0.7087

# Set hyperparameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

# train model
detach("package:caret", unload=TRUE)
xgmodel <- train(learner = xgb_new, task = train.task)

# Predict model
xg_pred <- predict(xgmodel, test.task)

# Confusion Matrix
require(caret)
confusionMatrix(xg_pred$data$response,xg_pred$data$truth) #0.7055

# Stop parallelization
parallelStop()
