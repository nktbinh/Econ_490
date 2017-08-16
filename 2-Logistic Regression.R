library(stringr) # Deal with string
library(tidyverse) # General Purpose Data Cleaning
library(forcats) # Deal with factors
library(caTools) # Randomized sample split
library(ROCR) # ROC curves and parameters
source("Common.R")

alchol <- data.frame(read_csv("alcohol2.csv"), check.names = FALSE)

# Split into training and testing sets
set.seed(1234)
split = sample.split(alchol$alcbin, SplitRatio = 0.7)
train = subset(alchol, split == TRUE)
test = subset(alchol, split == FALSE)

# Process data frame with utility function
df_train<-prepare_dataset(train)
df_test<-prepare_dataset(test)

# Logistic Regression
glm_alcbin = glm(alcbin ~ age+race+inc+health+educat+maristat+gender+insured+k6, 
                 data=df_train, family="binomial")
summary(glm_alcbin)
anova(glm_alcbin)

glm_predTrain = predict(glm_alcbin, type="response")
table(df_train$alcbin,glm_predTrain>0.5) 

# Visualize perfomance of scoring classifiers
glm_ROCRpredTrain = prediction(glm_predTrain, df_train$alcbin) 
glm_ROCRperfTrain = performance(glm_ROCRpredTrain, "tpr", "fpr") 
glm_aucTrain  = as.numeric(performance(glm_ROCRpredTrain, "auc")@y.values)
glm_aucTrain

plot(glm_ROCRperfTrain, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7)) 

# Apply predictions on the test set
glm_predTest=predict(glm_alcbin,df_test,type="response")
with(df_test,table(alcbin,glm_predTest >0.5))

glm_ROCRpredTest = prediction(glm_predTest, df_test$alcbin) 
glm_ROCRperfTest = performance(glm_ROCRpredTest, "tpr", "fpr") 
glm_auc = as.numeric(performance(glm_ROCRpredTest, "auc")@y.values)
glm_auc 

plot(glm_ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7)) 

# -------------------------------------------------------------

library(MASS)
library(class) # Various functions for classification

# Linear Discriminant Analysis
lda_alcbin=lda(alcbin ~ age+race+educat+health+maristat+insured+gender+k6,data=df_train)
lda_alcbin
plot(lda_alcbin)
lda_predTest=predict(lda_alcbin, df_test)
names(lda_predTest)
lda_class=lda_predTest$class
table(lda_class,df_test$alcbin)
mean(lda_class==df_test$alcbin) 

lda_ROCRpredTest = prediction(lda_predTest$posterior[,2], df_test$alcbin) 
lda_ROCRperfTest = performance(lda_ROCRpredTest, "tpr", "fpr") 
lda_auc = as.numeric(performance(lda_ROCRpredTest, "auc")@y.values)
lda_auc 

# Quadratic Discriminant Analysis
qda_alcbin=qda(alcbin ~ age+race+educat+health+maristat+insured+gender+k6,data=df_train)
qda_alcbin
qda_predTest = predict(qda_alcbin,df_test)
qda_class=predict(qda_alcbin,df_test)$class
table(qda_class,df_test$alcbin)
mean(qda_class==df_test$alcbin) 

qda_ROCRpredTest = prediction(qda_predTest$posterior[,2], df_test$alcbin) 
qda_ROCRperfTest = performance(qda_ROCRpredTest, "tpr", "fpr") 
qda_auc = as.numeric(performance(qda_ROCRpredTest, "auc")@y.values)
qda_auc 