library(Metrics)
library(randomForest)

set.seed(123)
model_rf<-randomForest(alcbin ~ age+race+inc+educat+health+maristat+insured+gender+k6,
                       data = df_train)

preds<-predict(model_rf,df_test[,-10])

# Feature Importance
randomForest::importance(model_rf)
model_rf2<-randomForest(alcbin ~ age+race+educat+maristat+gender+k6,
                        data = df_train)

preds2<-predict(model_rf2,df_test[,-10])
auc(preds2,df_test$alcbin)
