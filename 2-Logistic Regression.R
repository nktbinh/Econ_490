library(caTools) # Randomized sample split
library(ROCR) # ROC curves and parameters
library(rpart) # CART
library(rpart.plot) # CART plots

# Split into training and testing sets
set.seed(1234)
split = sample.split(binh2, SplitRatio = 0.7)
train = subset(binh2, split == TRUE)
test = subset(binh2, split == FALSE)

# Function to prepare dataset
prepare_dataset <- function(df) {
  
  df<-df %>%
    mutate(alcbin=fct_recode(alcbin,
                             "1"="Heavy Alcohol Use",
                             "1"="Binge But Not Heavy Use",
                             "0"="Past Month But Not Binge",
                             "0"="No Alcohol Use Last Month"))
  df$alcbin<-relevel(df$alcbin, ref="0")
  df <- df[,c("gender","age","race","inc","health","maristat","insured","educat","k6","abudep","alcbin","daysalc")]
  df <- df[complete.cases(df),]
  
  return(df)
}
df_train<-prepare_dataset(train)

# Logistic Regression
alcbin_mod = glm(alcbin ~ age+race+educat+health+maristat+insured+race+gender+k6, data=df_train, family="binomial")
summary(alcbin_mod)

predictTrain = predict(alcbin_mod, type="response")
ROCRpredtrain = prediction(predictTrain, df_train$alcbin) # Prediction Function
ROCRperfTrain = performance(ROCRpredtrain, "tpr", "fpr") # Performance Function
auc = as.numeric(performance(ROCRpredtrain, "auc")@y.values)
auc

table(df_train$alcbin,predictTrain>0.5) #Confusion matrix with threshold 0.5
(10094+3161)/nrow(df_train) #Accuracy Rate

plot(ROCRperfTrain) # Plot ROC curve
plot(ROCRperfTrain, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Add colors and threshold labels

# Apply predictions on the test set
df_test<-prepare_dataset(test)

df_test$predicted.alcbin=predict(alcbin_mod,df_test,type="response")
ROCRpredTest = prediction(df_test$predicted.alcbin, df_test$alcbin) #ROCR prediction function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr") # Performance Function
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

with(df_test,table(alcbin,predicted.alcbin >=0.5))
(4673+1461)/nrow(df_test) #accuracty rate on test set

plot(ROCRperfTest) # Plot ROC curve
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Add colors and threshold labels
