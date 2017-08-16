library(Boruta)

set.seed(123)
boruta.train<-Boruta(alcbin~., data = df_train, doTrace = 2, maxRuns = 25)
print(boruta.train)
# 24 iterations in 15 minutes, 7 important attributes, 1 tentative attribute: health

# Boruta variable importance chart
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# Decision on tentative attribute: health
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

# Confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
print(boruta.df)