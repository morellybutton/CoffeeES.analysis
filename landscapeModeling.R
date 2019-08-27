#code to model distribution of yields under different landscape configurations

library(tidyverse)
library(arm)
library(MuMIn)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
d.F.new.14<-d.F.new %>% filter(year==2014)
d.F.new15 <- d.F.new %>% filter(year==2015&!is.na(logdiff))
d.F.new16 <- d.F.new %>% filter(year==2016&!is.na(logdiff))

#for 2014 yield
cand.set.14<-list()
cand.set.14[[1]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(elevation)+rescale(patcharea),data=d.F.new.14)
cand.set.14[[2]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(elevation)+rescale(patcharea)+rescale(tmax.anom.fruit),data=d.F.new.14)
cand.set.14[[3]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(elevation)+rescale(patcharea)+rescale(GapDry),data=d.F.new.14)
#include a model without interaction between BA.legume and Shannon.i with lowest possible delta AIC
cand.set.14[[4]]<-lm(Shrub.kg~rescale(Shannon.i)+rescale(BA.legume) +
                       rescale(elevation) + rescale(tmax.anom.fruit),data=d.F.new.14)

topmodels.avg<-model.avg(cand.set.14) 

tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))


#for 2015 yield difference
cand.set.15<-list()
cand.set.15[[1]]<-lm(logdiff~rescale(BA.legume) + rescale(propCBD) + rescale(coffee.area.ha)+
                       rescale(patcharea),data=d.F.new15)
cand.set.15[[2]]<-lm(logdiff~rescale(Shannon.i) + rescale(patcharea),data=d.F.new15)
cand.set.15[[3]]<-lm(logdiff~rescale(BA.legume) + rescale(coffee.area.ha)  +
                       rescale(patcharea),data=d.F.new15)
cand.set.15[[4]]<-lm(logdiff~rescale(coffee.area.ha)+rescale(patcharea) ,data=d.F.new15)
cand.set.15[[5]]<-lm(logdiff~rescale(coffee.area.ha)+rescale(patcharea) + rescale(propCBD),data=d.F.new15)
cand.set.15[[6]]<-lm(logdiff~rescale(coffee.area.ha)+rescale(Shannon.i) + rescale(patcharea),data=d.F.new15)
cand.set.15[[7]]<-lm(logdiff~rescale(BA.legume) + rescale(patcharea) + rescale(Shannon.i),data=d.F.new15)
cand.set.15[[8]]<-lm(logdiff~rescale(patcharea),data=d.F.new15)
cand.set.15[[9]]<-lm(logdiff~rescale(Shannon.i)+rescale(patcharea) + rescale(propCBD),data=d.F.new15)
cand.set.15[[10]]<-lm(logdiff~rescale(BA.legume)+rescale(patcharea) + rescale(propCBD),data=d.F.new15)
cand.set.15[[11]]<-lm(logdiff~rescale(patcharea) + rescale(propCBD),data=d.F.new15)
cand.set.15[[12]]<-lm(logdiff~rescale(BA.legume) + rescale(coffee.area.ha)  + rescale(patcharea)  + rescale(Shannon.i),data=d.F.new15)
cand.set.15[[13]]<-lm(logdiff~rescale(BA.legume)  + rescale(patcharea),data=d.F.new15)
cand.set.15[[14]]<-lm(logdiff~rescale(BA.legume) + rescale(propCBD) + rescale(coffee.area.ha)+
                        rescale(patcharea) + rescale(Shannon.i),data=d.F.new15)

topmodels.avg.15<-model.avg(cand.set.15) 
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

#for 2016 yield difference
cand.set.16<-list()
cand.set.16[[1]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit),data=d.F.new16)
cand.set.16[[2]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit) +rescale(GapDry),data=d.F.new16)
cand.set.16[[3]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit) + rescale(propCBB),data=d.F.new16)
cand.set.16[[4]]<-lm(logdiff~rescale(Shannon.i)+rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit),data=d.F.new16)

topmodels.avg.16<-model.avg(cand.set.16)
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

#keep elevation the same for each farmer (because that is outside their control)
#vary patch area (trade-off), either landscape of all "large patches" or "all small patches" 
#vary leguminous trees (complementary), either landscape without leguminous trees or all farms with max measured
#vary diversity of shade trees (trade-off), either landscape all low diversity or all high diversity

#produce figures for normal year, then log yield difference for "hot" year (2015), "dry" year (2016)

#modelling "normal" year
d.F.new.14$`rescale(patcharea)` <- rescale(d.F.new.14$patcharea)
d.F.new.14$`rescale(elevation)` <- rescale(d.F.new.14$elevation)
d.F.new.14$`rescale(GapDry)` <- rescale(d.F.new.14$GapDry)
d.F.new.14$`rescale(tmax.anom.fruit)` <- rescale(d.F.new.14$tmax.anom.fruit)

#add min and max patch area
d.F.new.14$min.patcharea <-
  d.F.new.14$`rescale(patcharea)`[match(d.F.new.14 %>% dplyr::select(patcharea) %>% min(), d.F.new.14$patcharea)]
  
d.F.new.14$max.patcharea<- 
  d.F.new.14$`rescale(patcharea)`[match(d.F.new.14 %>% dplyr::select(patcharea) %>% max(), d.F.new.14$patcharea)]

d.F.new.14$mean.patcharea<- 0
  
#add min and max BA legume
d.F.new.14$`rescale(BA.legume)` <- rescale(d.F.new.14$BA.legume)

d.F.new.14$min.BA.legume<-
  d.F.new.14$`rescale(BA.legume)`[match(d.F.new.14 %>% dplyr::select(BA.legume) %>% min(), d.F.new.14$BA.legume)]

d.F.new.14$max.BA.legume<-
  d.F.new.14$`rescale(BA.legume)`[match(d.F.new.14 %>% dplyr::select(BA.legume) %>% max(), d.F.new.14$BA.legume)]

d.F.new.14$mean.BA.legume<-0

#add min and max shade diversity
d.F.new.14$`rescale(Shannon.i)` <- rescale(d.F.new.14$Shannon.i)

d.F.new.14$min.Shannon.i<-
  d.F.new.14$`rescale(Shannon.i)`[match(d.F.new.14 %>% dplyr::select(Shannon.i) %>% min(), d.F.new.14$Shannon.i)]

d.F.new.14$max.Shannon.i<-
  d.F.new.14$`rescale(Shannon.i)`[match(d.F.new.14 %>% dplyr::select(Shannon.i) %>% max(), d.F.new.14$Shannon.i)]

d.F.new.14$mean.Shannon.i<-0
  
#create dataframe for min patch area and all shade variables for normal year
df.pa.min.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(elevation)`,min.patcharea,`rescale(tmax.anom.fruit)`,`rescale(GapDry)`) %>% 
  rename(patcharea=min.patcharea) 

#create dataframe for mean patch area and all shade variables for normal year
df.pa.mean.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                                `rescale(elevation)`,mean.patcharea,`rescale(tmax.anom.fruit)`,`rescale(GapDry)`) %>% 
  rename(patcharea=mean.patcharea) 

#create dataframe for max patch area and all shade variables for normal year
df.pa.max.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(elevation)`,max.patcharea,`rescale(tmax.anom.fruit)`,`rescale(GapDry)`) %>% 
  rename(patcharea=max.patcharea)

#max patcharea and variation in shade variables
df.pa.max.norm.1<-df.pa.max.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.max.norm.1<-df.pa.max.norm.1 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.max.norm.1<-left_join(df.pa.max.norm.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

#norm1<-ggplot(df.pa.max.norm.1,aes(elevation,Shrub.kg.mod)) + geom_bar(stat="identity",position = position_dodge(width = 0.9))
norm1<-ggplot(df.pa.max.norm.1,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

df.pa.max.norm.2<-df.pa.max.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.max.norm.2<-df.pa.max.norm.2 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.max.norm.2<-left_join(df.pa.max.norm.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm2<-ggplot(df.pa.max.norm.2,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

df.pa.max.norm.3<-df.pa.max.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.max.norm.3<-df.pa.max.norm.3 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.max.norm.3<-left_join(df.pa.max.norm.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm3<-ggplot(df.pa.max.norm.3,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)


#mean patcharea and variation in shade variables
df.pa.mean.norm.1<-df.pa.mean.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.mean.norm.1<-df.pa.mean.norm.1 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.mean.norm.1<-left_join(df.pa.mean.norm.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

#norm1<-ggplot(df.pa.max.norm.1,aes(elevation,Shrub.kg.mod)) + geom_bar(stat="identity",position = position_dodge(width = 0.9))
norm4<-ggplot(df.pa.mean.norm.1,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

df.pa.mean.norm.2<-df.pa.mean.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.mean.norm.2<-df.pa.mean.norm.2 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.mean.norm.2<-left_join(df.pa.mean.norm.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm5<-ggplot(df.pa.mean.norm.2,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

df.pa.mean.norm.3<-df.pa.mean.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.mean.norm.3<-df.pa.mean.norm.3 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.mean.norm.3<-left_join(df.pa.mean.norm.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm6<-ggplot(df.pa.mean.norm.3,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

#min patcharea and variation in shade variables
df.pa.min.norm.1<-df.pa.min.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.min.norm.1<-df.pa.min.norm.1 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.min.norm.1<-left_join(df.pa.min.norm.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

#norm1<-ggplot(df.pa.max.norm.1,aes(elevation,Shrub.kg.mod)) + geom_bar(stat="identity",position = position_dodge(width = 0.9))
norm7<-ggplot(df.pa.min.norm.1,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

df.pa.min.norm.2<-df.pa.min.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.min.norm.2<-df.pa.min.norm.2 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.min.norm.2<-left_join(df.pa.min.norm.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm8<-ggplot(df.pa.min.norm.2,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

df.pa.min.norm.3<-df.pa.min.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.min.norm.3<-df.pa.min.norm.3 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*BA.legume*Shannon.i)

df.pa.min.norm.3<-left_join(df.pa.min.norm.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm9<-ggplot(df.pa.min.norm.3,aes(Shrub.kg.mod)) + geom_freqpoly(binwidth=0.05) + theme_classic() + xlab("Modelled Yield [kg/shrub]") +
  theme(text=element_text(size=14)) + xlim(0,1.8)

# Create a figure by combining the different plots
figure <- ggpubr::ggarrange(norm1, norm2, norm3,norm4,norm5,norm6,norm7,norm8,norm9, labels = "AUTO")

# Annotate the figure by adding a common labels
ggpubr::annotate_figure(figure,
                top = ggpubr::text_grob(expression('Min Shade Diversity' %->% 'Max Shade Diversity'), face = "bold", size = 16),
                bottom = ggpubr::text_grob(expression('Max Basal Area of Leguminous Trees' %<-% 'Min Basal Area of Leguminous Trees'), face = "bold", size = 16),
                left = ggpubr::text_grob(expression('Min Patch Area' %->% 'Max Patch Area'), rot = 90, face = "bold", size = 16),
                fig.lab = "Normal Year", fig.lab.face = "bold", fig.lab.size = 20
)