library(tidyverse)

df=read.csv('data/raw/metaT_genus_rank.csv')
meta<-read.csv('data/raw/salazar_metadata.csv') %>% dplyr::select(site,Depth.nominal,Latitude)


Dq<-function(x,q){
  x<-x[x>0]
  Dq<-sum((x/sum(x))^q)^(1/(1-q))
  
  return(Dq)
}


df=df %>%
  merge(meta %>% dplyr::select(site,Depth.nominal),by='site')

meta$polar='non'
meta$polar[abs(meta$Latitude)>=60]<-'polar'
pol.epi=meta %>%
  filter(polar %in% "polar",Depth.nominal<200) %>%
  dplyr::select(site) %>%
  unlist()

non.epi=meta %>%
  filter(polar %in% "non",Depth.nominal<200) %>%
  dplyr::select(site) %>%
  unlist()


non<-df %>%
  filter(site %in% non.epi) %>%
  mutate(fr=fr*0+1) %>%
  dplyr::select(KO,fr,site) %>%
  group_by(site) %>%
  spread(key=KO,value=fr,fill = 0) %>%
  arrange(site) %>%
  ungroup() %>%
  dplyr::select(-site) %>%
  as.matrix()

pol<-df %>%
  filter(site %in% pol.epi) %>%
  mutate(fr=fr*0+1) %>%
  dplyr::select(KO,fr,site) %>%
  group_by(site) %>%
  spread(key=KO,value=fr,fill = 0) %>%
  arrange(site) %>%
  ungroup() %>%
  dplyr::select(-site) %>%
  as.matrix()


df.diversity<-data.frame(NULL)

#Randomly Sample 10 sites and calculate alpha,beta, and gamma diversity for polar and nonpolar regions
for (i in 1:1000){
  indx.non<-sample(1:nrow(non),10,replace = TRUE)
  indx.pol<-sample(1:nrow(pol),10,replace=TRUE)
  
  non.tmp<-non[indx.non,]
  pol.tmp<-pol[indx.pol,]
  
  gamma.non<-Dq(colSums(non.tmp),0)
  gamma.pol<-Dq(colSums(pol.tmp),0)
  
  alpha.non<-mean(apply(non.tmp,1,Dq,q=0))
  alpha.pol<-mean(apply(pol.tmp,1,Dq,q=0))
  
  
  beta.non<-gamma.non/alpha.non
  beta.pol<-gamma.pol/alpha.pol
  
  df.diversity<-data.frame(D=c(alpha.non,alpha.pol,beta.non,beta.pol,gamma.non,gamma.pol),
                           type=c('alpha','alpha','beta','beta','gamma','gamma'),
                           polar=c('non','polar','non','polar','non','polar')) %>%
    rbind(df.diversity)
  
}


#Figure 4A-C
ggplot(df.diversity,aes(x=polar,y=D))+geom_boxplot()+facet_wrap(.~type,scales = 'free')