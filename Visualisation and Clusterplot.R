# Visualise alcbin and abudep for different income and employment group
train_binge%>%
  mutate(alcbin = alcbin%>%fct_infreq()%>%fct_rev())%>%
  ggplot(aes(x=alcbin,fill=insured))+
  facet_wrap(~race+inc)+
  geom_bar()+
  coord_flip()+
  labs(fill="Insured")

train%>%
  mutate(abudep=abudep%>%fct_infreq()%>%fct_rev())%>%
  ggplot(aes(x=abudep,fill=insured))+
  facet_wrap(~race+inc)+
  geom_bar()+
  coord_flip()+
  labs(fill="Insured")

# Visualise drinking according to job sectors
binh3%>%
  filter(typebiz!="NA")%>%
  mutate(alcbin=alcbin%>%fct_infreq()%>%fct_rev())%>%
  ggplot(aes(x=alcbin,fill=typebiz))+
  geom_bar()+coord_flip()

# Clusterplot
set.seed(1234)
binh3<-binh2%>%filter(typejob!="NA")

isGoodCol <- function(col){
  sum(is.na(col)) == 0 && is.numeric(col) 
}
goodCols <- sapply(binh2, isGoodCol)
clusters <- kmeans(binh2[,"typejob"], centers=5)
labels <- clusters$cluster

binh2d<-prcomp(binh4[,goodCols],center=TRUE)
twoColumns<-binh2d$x[,1:2]
clusplot(twoColumns,labels,color=T,shade=T, labels=1)