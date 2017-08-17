read_data <- function(df_train, df_test,shuffle=FALSE) {
  
  df_full <- rbind(df_train, df_test)
  
  # homogenize levels
  df_train <- df_full[1:nrow(df_train),]
  df_test <- df_full[(nrow(df_train)+1):nrow(df_full),]
  
  if(shuffle) {
    df_train <- df_train[sample(nrow(df_train)),]
    df_test <- df_test[sample(nrow(df_test)),]
  }
  
  list(train = df_train, test = df_test)
}

kfolds <- function(N, k) {
  return(
    sort(
      rep(1:k, ceiling(N / k))[1:N]
    )
  )
}

hypergrid <- function(k, tt, minc, mins, minb, maxd, shuffle = FALSE) {
  G <- expand.grid(tt=tt, minc=minc, mins=mins, minb=minb, maxd=maxd, k=1:k,
                   stringsAsFactors=FALSE)
  G <- G[G$mins > G$minb,]
  
  if(shuffle) {
    G <- G[sample(nrow(G)),]
  }
  
  return(G)
}

gridsearch_ctree <- function(K, G, data, formula, cores=6, maxIter=NA) {
  library(doSNOW, quietly=TRUE)
  
  cl <- makeCluster(cores)
  registerDoSNOW(cl)
  
  if(is.na(maxIter)) {
    nIter <- nrow(G)
  } else {
    nIter <- maxIter
  }
  
  res <- foreach(i = 1:nIter, .packages="party", .combine="rbind") %dopar% {
    if(i %% 100 == 0) cat(sprintf("%d of %d",i,nrow(G)), file="state.txt")
    
    cfg <- G[i,]
    ctrl <- ctree_control(
      testtype = cfg$tt,
      mincriterion = cfg$minc,
      minsplit = cfg$mins,
      minbucket = cfg$minb,
      maxdepth = cfg$maxd
    )
    
    data_train <- data[K != cfg$k,]
    data_test <- data[K == cfg$k,]
    
    res <- data.frame(
      testtype = cfg$tt,
      mincriterion = cfg$minc,
      minsplit = cfg$mins,
      minbucket = cfg$minb,
      maxdepth = cfg$maxd,
      correct = NA,
      incorrect = NA,
      elapsed = NA,
      terminals = NA
    )
    
    try({
      time <- system.time(
        t <- ctree(formula, data_train, controls = ctrl)
      )
      predictions <- Predict(t, data_test)
      
      response <- all.vars(formula)[1]
      
      nc <- sum(predictions == data_test[,response])
      
      res <- data.frame(
        testtype = cfg$tt,
        mincriterion = cfg$minc,
        minsplit = cfg$mins,
        minbucket = cfg$minb,
        maxdepth = cfg$maxd,
        correct = nc,
        incorrect = length(predictions) - nc,
        elapsed = time["elapsed"],
        terminals = length(unique(where(t)))
      )
    })
    
    return(res)
  }
  
  stopCluster(cl)
  
  return(res)
}

###################################################
# prepare data
###################################################

data <- read_data(df_train, df_test, shuffle=TRUE)

k <- 100

K <- kfolds(nrow(data$train), k)
G <- hypergrid(k, 
               tt = c("Bonferroni","Univariate","Teststatistic"), 
               minc = c(0.9,0.95,0.99), 
               mins = c(60,40,20,10), 
               minb = c(30,20,10,5), 
               maxd = c(0,4,5,6),
               shuffle = TRUE
)

###################################################
# perform CV
###################################################

result <- NA

library(party)

for(i in 1:44) {
  subG <- G[((i-1)*900+1):(i*900),]
  print(sprintf("G from %d to %d",((i-1)*900+1),i*900))
  
  subResult <- gridsearch_ctree(K, subG, data$train,
                                alcbin ~ age+race+inc+health+educat+maristat+gender+insured+k6,
                                cores = 6, maxIter = NA
  )
  
  if(!is.na(result)) {
    result <- rbind(result, subResult)
  } else {
    result <- subResult
  }
  
  save.image("ctree.grid.search.RData")
}

###################################################
# analyze results
###################################################

load("ctree.grid.search.RData")

library(sqldf)

r <- sqldf("select testtype, mincriterion, minsplit, minbucket, maxdepth, 
           sum(correct) as N1, sum(incorrect) as N0, sum(elapsed) as Time, 
           sum(terminals) as Term, count(correct) as N from result group by testtype, 
           mincriterion, minsplit, minbucket, maxdepth")

r$err <- r$N0 / (r$N0 + r$N1)

r <- r[order(r$err),]
r$idx <- 1:nrow(r)
head(r)

r$testtype <- as.character(r$testtype)

v <- rep(NA,nrow(r))

require(party)

for(i in 1:nrow(r)) {
  t <- ctree(alcbin ~ age+race+inc+health+educat+maristat+gender+insured+k6, data$train,
             controls = ctree_control(
               teststat="quad",
               testtype=r[i,"testtype"],
               mincriterion=r[i,"mincriterion"],
               minsplit=r[i,"minsplit"], 
               minbucket=r[i,"minbucket"],
               maxdepth=r[i,"maxdepth"]
             )
  )
  
  v[i] <- sum(t@predict_response() != data$train$alcbin)/nrow(data$train)
}

r$err_all <- v

r_unique_trees <- sqldf("select min(idx) as idx from r group by err_all")

###################################################
# The Winning Tree
###################################################

t <- ctree(
  alcbin ~ age + race + inc + health + educat + maristat + gender + insured + k6, 
  data$train,
  controls = ctree_control(
    teststat="quad",
    testtype="Teststatistic",
    mincriterion=.95,
    minsplit=60, 
    minbucket=10,
    maxdepth=0
  )
)

plot(t)

# Train accuracy: 
sum(t@predict_response() == data$train$alcbin)/nrow(data$train)

# Test Accuracy
final_result <- data.frame(alcbin = df_test$alcbin, pred = as.numeric(as.character(Predict(t, df_test))))
with(final_result, table(alcbin,pred))
