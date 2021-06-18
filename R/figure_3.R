library(ncdf4)
library(tidyverse)
library(vegan)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

chem1<-nc_open('data/processed/CHL_NO3_O2_2013_2018_5m.nc')
chem2<-nc_open('data/processed/PO4_Si_2013_2018_5m.nc')

Sal<-nc_open('data/processed/Salinity_2013_2018_5m.nc')

scale_factors<-read.csv('data/processed/metaT_scaling_factors_chemistry.csv')
regression_mu.coef<-read.csv('data/processed/regression_coef_mu.csv')
rda.all.global<-readRDS('data/processed/metaT_rda_obj.rds')
rda_KO<-read.csv('data/processed/rda_KO_matrix.csv') %>% 
  as.matrix()

lat<-ncvar_get(chem1,"latitude")
lon<-ncvar_get(chem1,"longitude")
time<-ncvar_get(chem1,"time")
depth<-5


for (i in 1:72){
  
  si<-ncvar_get(chem2,"si")[,,i] #%>% rowMeans(dims = 2)
  rownames(si)<-lon
  colnames(si)<-lat
  si<-as.data.frame(si) %>%
    rownames_to_column(var="lon") %>%
    gather(key=lat,value=Si,-lon)
  
  chl<-ncvar_get(chem1,"chl")[,,i]#%>% rowMeans(dims = 2)
  rownames(chl)<-lon
  colnames(chl)<-lat
  chl<-as.data.frame(chl) %>%
    rownames_to_column(var="lon") %>%
    gather(key=lat,value=ChlorophyllA,-lon)
  
  po4<-ncvar_get(chem2,"po4")[,,i] #%>% rowMeans(dims = 2)
  rownames(po4)<-lon
  colnames(po4)<-lat
  po4<-as.data.frame(po4) %>%
    rownames_to_column(var="lon") %>%
    gather(key=lat,value=PO4,-lon)
  
  o2<-ncvar_get(chem1,"o2")[,,i] #%>% rowMeans(dims = 2)
  rownames(o2)<-lon
  colnames(o2)<-lat
  o2<-as.data.frame(o2) %>%
    rownames_to_column(var="lon") %>%
    gather(key=lat,value=Oxygen,-lon)
  
  no3<-ncvar_get(chem1,"no3")[,,i] #%>% rowMeans(dims = 2)
  rownames(no3)<-lon
  colnames(no3)<-lat
  no3<-as.data.frame(no3) %>%
    rownames_to_column(var="lon") %>%
    gather(key=lat,value=NO3,-lon)
  
  S<-ncvar_get(Sal,"so_cglo")[,,i] #%>% rowMeans(dims = 2)
  rownames(S)<-lon
  colnames(S)<-lat
  S<-as.data.frame(S) %>%
    rownames_to_column(var="lon") %>%
    gather(key=lat,value=Salinity,-lon)
  
  
  df_global<-data.frame(lon=chl$lon,lat=chl$lat,
                        ChlorophyllA=chl$ChlorophyllA,
                        NO3=no3$NO3,
                        Oxygen=o2$Oxygen,
                        PO4=po4$PO4,
                        Salinity=S$Salinity,
                        Si=si$Si,
                        Depth.nominal=depth)
  
  j=1
  for (var in scale_factors$variable){
    
    indx.global<-which(colnames(df_global) %in% var)
    
    
    lambda<-scale_factors$box.lambda[j]
    
    
    df_global[,indx.global]<-df_global[,indx.global]+scale_factors$offset[j]
    df_global[,indx.global]<-(df_global[,indx.global]^lambda-1)/lambda
    
    
    df_global[,indx.global]<-(df_global[,indx.global]-scale_factors$var.m[j])/scale_factors$var.sd[j]
    
    j=j+1
    
  }
  
  df_global<-df_global[complete.cases(df_global),]  
  
  global.prediction<-predict(rda.all.global,newdata=df_global,type = 'lc',scaling=0)
  
  
  fr.prediction<-regression_mu.coef[1,1]+
    global.prediction[,1]*regression_mu.coef[2,1]
  
  global_fr<-data.frame(lon=df_global$lon,lat=df_global$lat,fr=fr.prediction,time=i)
  
  path<-'data/processed/fr_global_prediction_monthly/month_'
  filename<-paste(path,as.character(i),".csv",sep="")
  write.csv(global_fr,filename,row.names = FALSE,quote=FALSE)
  print(i) 
}
nc_close(chem1)
nc_close(chem2)
nc_close(Sal)

rm(list=ls())


dir='data/processed/fr_global_prediction_monthly/'

file_list=list.files(dir)
i=1
for (f in file_list){
  file.tmp=paste(dir,f,sep='')
  if (i==1){
    df=read.csv(file.tmp)
  } else {
    df=read.csv(file.tmp) %>%
      rbind(df)
  }
  
  
  
  i=i+1
}


df_mu<-df %>% group_by(lat,lon) %>% summarize(fr.q05=quantile(fr,0.025,na.rm=TRUE),
                                           fr.q95=quantile(fr,0.975,na.rm=TRUE),
                                           fr.q50=quantile(fr,0.50,na.rm=TRUE))
# df$rsd=df$fr.sd/abs(df$fr.m)

write.csv(df_mu,'data/processed/global_mu_fr.csv',row.names = FALSE,quote = FALSE)

global_mu$range<-global_mu$fr.q95-global_mu$fr.q05
global_mu$rsd<-global_mu$range/abs(global_mu$fr.q50)

#Figure 3A
global_mu.raster<-rasterFromXYZ(global_mu[,c(1,2,5)])
global_mu.plot<-as.data.frame(global_mu.raster,xy=TRUE)

colnames(global_mu.plot)[1:2]<-c("Latitude","Longitude")

# world <- map_data("world")
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_raster(data=global_mu.plot,aes(x=Longitude,y=Latitude,fill=fr.q50)) +
  geom_sf(data=world,color=NA) +
  xlab("Longitude") + ylab("Latitude")+
  # geom_map(
  #   data = world, map = world,
  #   aes(long, lat, map_id = region)
  # ) +
  scale_fill_distiller(palette="RdYlBu",oob=squish) +
  theme_bw()



#Figure 3B
global_mu.raster<-rasterFromXYZ(global_mu[,c(1,2,7)])
global_mu.plot<-as.data.frame(global_mu.raster,xy=TRUE)

colnames(global_mu.plot)[1:2]<-c("Latitude","Longitude")

# world <- map_data("world")
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_raster(data=global_mu.plot,aes(x=Longitude,y=Latitude,fill=rsd)) +
  geom_sf(data=world,color=NA) +
  xlab("Longitude") + ylab("Latitude")+
  # geom_map(
  #   data = world, map = world,
  #   aes(long, lat, map_id = region)
  # ) +
  scale_fill_distiller(palette="RdYlBu",oob=squish) +
  theme_bw()

