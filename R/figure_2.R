library(tidyverse)
library(vegan)

meta<-read.csv('data/raw/salazar_metadata.csv') %>%
  arrange(site)
df_fr<-read.csv('data/raw/metaT_genus_rank.csv')

df_fr.rank<-df_fr %>%
  select(fr,rank) %>%
  group_by(rank) %>%
  summarize(fr.50=quantile(fr,0.5,na.rm = TRUE),
            fr.95=quantile(fr,0.975,na.rm = TRUE),
            fr.05=quantile(fr,0.025,na.rm = TRUE))


#Figure 2A
ggplot(df_fr.rank)+geom_line(aes(x=rank,y=fr.50),color='black')+
  geom_line(aes(x=rank,y=fr.05),color='black',linetype='dashed')+
  geom_line(aes(x=rank,y=fr.95),color='black',linetype='dashed') +
  ylab('fr')


df_fr$fr[is.na(df_fr$fr)]<-0 #substitute nan with 0's for redundancy analysis


offset_min<-function(x) {
  value.min<-abs(min(x,na.rm = TRUE))
  tmp<-x+value.min
  shift_0<-(10^min(log10(tmp[!tmp==0]),na.rm = TRUE))/10
  offset<-value.min+shift_0/10
  x<-x+offset
  return(x)
}

find_offset<-function(x) {
  if (sum(x<=0,na.rm = TRUE)>0){
    value.min<-abs(min(x,na.rm = TRUE))
    tmp<-x+value.min
    shift_0<-(10^min(log10(tmp[!tmp==0]),na.rm = TRUE))/10
    offset<-value.min+shift_0/10
  } else {
    offset<-0
  }
  return(offset)
}


boxcox.transform<-function(variable) {
  #evaluate lambdas -3 to 3 to determine which has the highest log-likelihood
  box.out<-MASS::boxcox(variable~1,
                        lambda=seq(-10,10,length.out =300),
                        plotit=FALSE) 
  #select lambda with lowest log-likelihood
  lambda<-box.out %>% 
    as.data.frame() %>%
    slice(which.max(y)) %>%
    dplyr::select(x) %>%
    as.numeric()
  
  
  return(lambda)
}

apply.boxcox.transform<-function(variable) {
  #evaluate lambdas -3 to 3 to determine which has the highest log-likelihood
  
  if(!is.na(sd(variable))){
    
    box.out<-MASS::boxcox(variable~1,
                          lambda=seq(-10,10,length.out =300),
                          plotit=FALSE) 
    #select lambda with lowest log-likelihood
    lambda<-box.out %>% 
      as.data.frame() %>%
      slice(which.max(y)) %>%
      dplyr::select(x) %>%
      as.numeric()
    
    variable<-(variable^lambda-1)/lambda
  }
  return(variable)
}



df_fr<-df_fr %>%
  filter(site %in% t(meta$site)) %>%
  dplyr::select(-rank,-D,-S) %>% 
  spread(key=KO,value=fr,fill = 0) %>%
  arrange(site)

#select variables that can be scaled to reanalysis models
meta<-meta %>%
  filter(site %in% df_fr$site) %>%
  arrange(site) %>%
  dplyr::select(Si,NO3,PO4,ChlorophyllA,Salinity,Oxygen,Depth.nominal)

#impute missing data
meta<-missForest::missForest(as.matrix(meta))$ximp %>%
  as.data.frame()

scale_factors<-meta %>% 
  dplyr::select(Si,NO3,PO4,ChlorophyllA,Salinity,Oxygen,Depth.nominal) %>%
  gather(key=variable,value=value) %>%
  group_by(variable) %>%
  mutate(value=offset_min(value)) %>%
  summarize(box.lambda=boxcox.transform(value))

scale_factors<-meta %>% 
  dplyr::select(Si,NO3,PO4,ChlorophyllA,Salinity,Oxygen,Depth.nominal) %>%
  gather(key=variable,value=value) %>%
  group_by(variable) %>%
  summarize(offset=find_offset(value)) %>%
  base::merge(scale_factors,by="variable")


scale_factors$var.m<-NaN
scale_factors$var.sd<-NaN
i=1
for (var in scale_factors$variable){
  
  indx.meta<-which(colnames(meta) %in% var)
  lambda<-scale_factors$box.lambda[i]
  meta[,indx.meta]<-meta[,indx.meta]+scale_factors$offset[i]
  meta[,indx.meta]<-(meta[,indx.meta]^lambda-1)/lambda
  vec.m=sapply(meta,mean,na.rm = TRUE)
  vec.sd=sapply(meta,sd,na.rm = TRUE)
  scale_factors$var.m[i]=vec.m[indx.meta]
  scale_factors$var.sd[i]=vec.sd[indx.meta]
  meta[,indx.meta]<-(meta[,indx.meta]-scale_factors$var.m[i])/scale_factors$var.sd[i]
  
  i=i+1
  
}

rda_KO<-as.matrix(df_fr[,-1])
rda.all.global<-rda(rda_KO~.,data=meta)

#summarize RDA results--no scaling
rda.sum<-summary(rda.all.global,scaling=0)
print(rda.sum$biplot) #look at loadings

factor.loadings<- data.frame(physchem=rownames(rda.sum$biplot),
                             loading=rda.sum$biplot[,1],significant="true")

factor.loadings$significant[abs(factor.loadings$loading)<sqrt(1/nrow(factor.loadings))]<-'false'

#Figure 2B
ggplot(factor.loadings,aes(x=reorder(physchem,-loading),y=loading,fill=significant))+
  geom_col()

saveRDS(rda.all.global,'data/processed/metaT_rda_obj.rds')
write.csv(scale_factors,'data/processed/metaT_scaling_factors_chemistry.csv',row.names = FALSE,quote=FALSE)
write.csv(rda_KO,'data/processed/rda_KO_matrix.csv',row.names = FALSE,quote=FALSE)


vif.cca(rda.all.global)
df_mu_sd<-read.csv('data/processed/fr_mu_sd.csv')

site.score.global=scores(rda.all.global,
                         choices=c(1:2),
                         display = c("sites"),
                         scaling=0)

#extract first factor scores
df_mu_sd$g1=site.score.global[,1]



#build model using original variables
best.models<-leaps::regsubsets(mu~.,df_mu_sd %>% select(mu,Si,NO3,PO4,Depth.nominal,Oxygen,ChlorophyllA,Salinity))
best.model.sum<-summary(best.models)
print(best.model.sum)
data.frame(
  Adj.R2 = which.max(best.model.sum$adjr2),
  CP = which.min(best.model.sum$cp),
  BIC = which.min(best.model.sum$bic)
)

#chemistry model; use best model
chem.lm<-lm(mu~Si+Oxygen+Depth.nominal,df_mu_sd)
#factor models
factor.lm<-lm(mu~g1,df_mu_sd) #mu


#compare performance between best subset and RDA for mean fr modeling
df_compare_model<-data.frame(fitted=chem.lm$fitted.values,
                             residual=chem.lm$residuals,
                             model='chem') %>%
  rbind(data.frame(fitted=factor.lm$fitted.values,
                   residual=factor.lm$residuals,
                   model='factor'))



#Figure 2C
ggplot(df_compare_model)+
  geom_density(aes(residual,y=..scaled..,fill=model),alpha=0.3) +
  ylab('relative density')


#mu
regression_mu.coef<-coef(summary(factor.lm))
summary(factor.lm)

write.csv(regression_mu.coef,'data/processed/regression_coef_mu.csv',row.names = FALSE,quote=FALSE)
