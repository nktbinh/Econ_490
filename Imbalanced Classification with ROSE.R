library(ROSE)
library(rpart)

tree_alcbin<-rpart(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, data=df_train)
tree_predTest<-predict(tree_alcbin,newdata=df_test)

accuracy.meas(df_test$alcbin,tree_predTest[,2])
roc.curve(df_test$alcbin, tree_predTest[,2], plotit = F) # auc = 0.606

# Oversampling
data_balanced_over <- ovun.sample(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, 
                                  data = df_train, method = "over",N = 24908)$data
table(data_balanced_over$alcbin)

# Undersampling
data_balanced_under <- ovun.sample(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, 
                                  data = df_train, method = "under",N = 16334)$data
table(data_balanced_under$alcbin)

# Combined
data_balanced_both <- ovun.sample(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, 
                                  data = df_train, method = "both", p=0.5, N=20261, seed = 1)$data
table(data_balanced_both$alcbin)

# ROSE
data.rose <- ROSE(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, 
                  data = df_train, seed = 1)$data
table(data.rose$alcbin)

# Decision Trees for all 4 sets
tree.rose <- rpart(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, data = data.rose)
tree.over <- rpart(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, data = data_balanced_over)
tree.under <- rpart(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, data = data_balanced_under)
tree.both <- rpart(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, data = data_balanced_both)

# Predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = df_test)
pred.tree.over <- predict(tree.over, newdata = df_test)
pred.tree.under <- predict(tree.under, newdata = df_test)
pred.tree.both <- predict(tree.both, newdata = df_test)

# AUC of these functions
roc.curve(df_test$alcbin, pred.tree.rose[,2]) #0.59
roc.curve(df_test$alcbin, pred.tree.over[,2]) #0.59
roc.curve(df_test$alcbin, pred.tree.under[,2]) #0.59
roc.curve(df_test$alcbin, pred.tree.both[,2]) #0.59
