library(ggplot2) # Visualisation tool
library(pROC) # ROC Tool
library(gridExtra) # Misc Functions for "Grid" Graphics
source("common.R")

loocv_pred <- log_reg(df_train,size=100)
summary(loocv_pred)
with(loocv_pred, table(alcbin,pred>0.3))

ROCRpred100pCV = prediction(loocv_pred$pred,loocv_pred$alcbin) 
ROCRperf100pCV = performance(ROCRpred100pCV, "tpr", "fpr") 
auc = as.numeric(performance(ROCRpred100pCV, "auc")@y.values)

# OR: AUC for the above parameter
auc(loocv_pred$alcbin, loocv_pred$pred)

# Function to plot the distribution of Binge/Heavy and Not cases on 
# the predicted survival probability
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$alcbin == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$alcbin == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$alcbin == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$alcbin == 0, "TN", v)
  
  df$pred_type <- v

  ggplot(data=df, aes(x=alcbin, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

plot_pred_type_distribution(loocv_pred,0.5)


# Calculate ROC for the above distribution
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$alcbin == 1) / sum(df$alcbin == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$alcbin == 0) / sum(df$alcbin == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$alcbin == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$alcbin == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

roc <- calculate_roc(loocv_pred,1,2,n=100)

# Plot ROC curve for the above distribution
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}

plot_roc(roc, 0.3, 1, 2)


