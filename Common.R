# Theme for plotting
theme_blank <- function(...) {
  ret <- theme_bw(...)
  ret$line <- element_blank()
  ret$rect <- element_blank()
  ret$strip.text <- element_blank()
  ret$axis.text <- element_blank()
  ret$plot.title <- element_blank()
  ret$axis.title <- element_blank()
  ret$plot.margin <- structure(
    c(0, 0, -1, -1), 
    unit = "lines", 
    valid.unit = 3L, 
    class = "unit")
  ret
}

# Function to prepare dataset
prepare_dataset <- function(df) {
  
  df<-df %>%
    mutate(alcbin=fct_recode(alcbin,
                             "1" = "Heavy Alcohol Use",
                             "1" = "Binge But Not Heavy Use",
                             "0" = "Past Month But Not Binge",
                             "0" = "No Alcohol Use Last Month"))
  df$alcbin<-relevel(df$alcbin, ref="0")
  df_names<- c("age","inc","race","educat","health","maristat","insured","gender")
  for (nom in df_names){
    df[,nom]<-as.factor(df[,nom])
  }
  df$race<-relevel(df$race, ref = "White")
  df$inc<-relevel(df$inc, ref = "> $20,000")
  df$health<-relevel(df$health, ref = "Good/Excellent")
  df$maristat<-relevel(df$maristat, ref = "Not Married")
  df$insured<-relevel(df$insured, ref = "Yes")
  df$gender<-relevel(df$gender, ref = "Male")
  df <- df[,append(df_names,c("k6","alcbin"))]
  df <- df[complete.cases(df),]
  
  return(df)
}

# Function to perform glm regression with Leave-p-out cross validation
log_reg <- function(df, size=100) {
  N <- nrow(df)
  size=100
  
  df <- df[sample(N),]
  
  num <- floor(N/size)
  rest <- N - num * size
  ncv <- cumsum(c(rep(size,num), rest))
  
  predictions <- data.frame(alcbin = df$alcbin, pred = NA)
  
  for(n in ncv) {
    v <- rep(TRUE, N)
    v[(n-size+1):n] <- FALSE
    
    lr <- glm(alcbin ~ age+race+educat+health+maristat+insured+gender+k6, 
              data = df[v,], 
              family = binomial(logit))
    predictions[!v,"pred"] <- predict(lr, newdata=df[!v,], type="response")
  }
  return(predictions)
}
