library(stringr) # Deal with string
library(magrittr) # Pipe operations
library(tidyverse) # General Purpose Data Cleaning
library(forcats) # Deal with factors
library(randomForest)
library(GGally) # pair plots maker, add-on to ggplot2
library(cluster) # K-means cluster plot
library(caTools) # Randomized sample split

load("data.rda")
binh<-da36361.0001
rm(da36361.0001)

# Rename
old_names<-c("K6SCMON","K6SCMAX","OTHINS","PRVHLTIN","HEALTH2","IRSEX","EMPSTATY","INCOME","EDUCCAT2","IREDUC2","IRMARIT","SEXRACE","NEWRACE2","CATAGE",
             "DEPNDALC","ABUSEALC","TXNEDALC","ABODALC","ALCYRTOT","BINGEHVY","ALDAYPWK","ALCTRY","CIGTRY",
             "POVERTY2","WHODASC3","AMDELT","ADPSSOC","WRKOCUP2","UADPEOP")

new_names<-c("k6month","k6max","otherins","privateins","health","gender","empstat","inc","educat","educ","maristat","sexrace","race","age",
             "alcdep","alcabu","alctre","abudep","daysalc","alcbin","alcweek","alctry","cigtry","pov","who",
             "mde","feelsoc","typejob","alcsocial")

binh2<-binh%>%
  select(one_of(old_names))
colnames(binh2)<-new_names

# Count number of NAs in each columns
count<-list()
count_missing<-function (df){
  for (i in 1:length(df)){
    counti<-sum(is.na(df[,i]))
    count[[i]]<-counti
  }
  count<-data.frame(count,stringsAsFactors = F)
  colnames(count)<-colnames(df)
  count
}
count_missing(binh2)

# Deal with Employment Status (Method1)
binh2$empstat<-ifelse(binh2$empstat=="(1) Employed full time","full time",binh2$empstat)
binh2$empstat<-ifelse(binh2$empstat=="2","part time",binh2$empstat)
binh2$empstat<-ifelse(binh2$empstat=="3","unemployed",binh2$empstat)
binh2$empstat<-ifelse(binh2$empstat=="4","other",binh2$empstat)
binh2<-binh2%>%filter(!is.na(binh2$empstat))
# Method 2 
binh2<-binh2 %>%
  mutate(empstat=fct_recode(empstat,
                            "full time"="(1) Employed full time",
                            "part time"="(2) Employed part time",
                            "unemployed"="(3) Unemployed",
                            "other"="(4) Other (incl. not in labor force)"
  )) %>%
  filter(!is.na(binh2$empstat))
binh2%>%count(empstat)

# Deal with Health

# Deal with Psychological Distress Variable (k6)
k6<-vector(length=nrow(binh2))
for (i in 1:nrow(binh2)){
  if (binh2$k6month[i]>binh2$k6max[i]){
    k6[i]<-binh2$k6month[i]
  }else {
    k6[i]<-binh2$k6max[i]
  }
  k6
}
binh2$k6<-k6

# Deal with Income
binh2<-binh2 %>%
  mutate(inc=fct_recode(inc,
                            "< $20,000"="(1) Less than $20,000",
                            "$20,000 - $49,999"="(2) $20,000 - $49,999",
                            "$50,000 - $74,999"="(3) $50,000 - $74,999",
                            "> $75,000"="(4) $75,000 or More"))
binh2%>%count(inc)

# Deal with alcdep
binh2<-binh2 %>%
  mutate(alcdep=fct_recode(alcdep,
                           "No/Unknown"="(0) No/Unknown (Otherwise)",
                           "Yes"="(1) Yes (See comment above)"))
binh2%>%count(alcdep)

# Deal with abudep
binh2<-binh2 %>%
  mutate(abudep=fct_recode(abudep,
                           "No/Unknown"="(0) No/Unknown (ABUSEALC=0 and DEPNDALC=0)",
                           "Yes"="(1) Yes (ABUSEALC=1 or DEPNDALC=1)"))

# Deal with alcbin
binh2<-binh2 %>%
  mutate(alcbin=fct_recode(alcbin,
                           "Heavy Alcohol Use"="(1) Heavy Alcohol Use (HVYDRK2=1)",
                           "Binge But Not Heavy Use"="(2) 'Binge' But Not Heavy Use (BINGEDRK=1 & HVYDRK2=0)",
                           "Past Month But Not Binge"="(3) Past Month But Not 'Binge' (ALCMON=1 & BINGEDRK=0)",
                           "No Alcohol Use Last Month"="(4) Did Not Use Alcohol in Past Month (All source=0)"))


# Deal with educat
binh2<-binh2 %>%
  mutate(educat=fct_recode(educat,
                           "< high school"="(1) Less than high school (IREDUC2<=7 and AGE2>=7)",
                           "high school grad"="(2) High school graduate (IREDUC2=8 and AGE2>=7)",
                           "some college"="(3) Some college (IREDUC2=9-10 and AGE2>=7)",
                           "college graduate"="(4) College graduate (IREDUC2=11 and AGE2>=7)",
                           "NA"="(5) 12 to 17 year olds (AGE2<=6)"))%>%
  filter(!educat=="NA")
binh2%>%count(educat)

# Deal with maristat
binh2<-binh2 %>%
  mutate(maristat=fct_recode(maristat,
                             "Married"="(1) Married",
                             "Widowed"="(2) Widowed",
                             "Divorced/Separated"="(3) Divorced or Separated",
                             "Never Married"="(4) Never Been Married"))
binh2%>%count(maristat)

# Deal with sexrace
binh2<-binh2 %>%
  mutate(sexrace=fct_recode(sexrace,
                            "White Male"="(1) Male, White, Not Hisp (IRSEX=1 and NEWRACE2=1)",
                            "White Female"="(2) Female, White, Not Hisp (IRSEX=2 and NEWRACE2=1)",
                            "Black Male"="(3) Male, Black, Not Hisp (IRSEX=1 and NEWRACE2=2)",
                            "Black Female"="(4) Female, Black, Not Hisp (IRSEX=2 and NEWRACE2=2)",
                            "Hispanic Male"="(5) Male, Hispanic (IRSEX=1 and NEWRACE2=7)",
                            "Hispanic Female"="(6) Female, Hispanic (IRSEX=2 and NEWRACE2=7)",
                            "Male of Female, other races"="(7) Male or Female, Other Races (Otherwise)"))

# Deal with race
binh2<-binh2 %>%
  mutate(race=fct_recode(race,
                         "White"="(1) NonHisp White",
                         "Black/Afr American"="(2) NonHisp Black/Afr Am",
                         "Native Am/AK Native"="(3) NonHisp Native Am/AK Native",
                         "Native HI/Other Pac Isl"="(4) NonHisp Native HI/Other Pac Isl",
                         "Asian"="(5) NonHisp Asian",
                         "More than one race"="(6) NonHisp more than one race",
                         "Hispanic"="(7) Hispanic"))
binh2%>%count(race)

# Deal with gender
binh2<-binh2 %>%
  mutate(gender=fct_recode(gender,
                           "Female"="(2) Female",
                           "Male"="(1) Male"))





