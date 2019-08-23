#code to model distribution of yields under different landscape configurations

library(tidyverse)
library(AICcmodavg)
library(arm)

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

topmodels.avg<-MuMIn::model.avg(cand.set.14) 

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

topmodels.avg.15<-MuMIn::model.avg(cand.set.15) 
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

#for 2016 yield difference
cand.set.16<-list()
cand.set.16[[1]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit),data=d.F.new16)
cand.set.16[[2]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit) +rescale(GapDry),data=d.F.new16)
cand.set.16[[3]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit) + rescale(propCBB),data=d.F.new16)
cand.set.16[[4]]<-lm(logdiff~rescale(Shannon.i)+rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit),data=d.F.new16)

topmodels.avg.16<-MuMIn::model.avg(cand.set.16)
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

#keep elevation the same for each farmer (because that is outside their control)
#vary patch area (trade-off), either landscape of all "large patches" or "all small patches" 
#vary leguminous trees (complementary), either landscape without leguminous trees or all farms with max measured
#vary diversity of shade trees (trade-off), either landscape all low diversity or all high diversity

#produce figures for normal year, then log yield difference for "hot" year (2015), "dry" year (2016)

#modelling "normal" year
#add min and max patch area
d.F.new.14$min.patcharea<-d.F.new.14 %>% dplyr::select(patcharea) %>% min()
d.F.new.14$max.patcharea<-d.F.new.14 %>% dplyr::select(patcharea) %>% max()

#add min and max BA legume
d.F.new.14$min.BA.legume<-d.F.new.14 %>% dplyr::select(BA.legume) %>% min()
d.F.new.14$max.BA.legume<-d.F.new.14 %>% dplyr::select(BA.legume) %>% max()

#add min and max shade diversity
d.F.new.14$min.Shannon.i<-d.F.new.14 %>% dplyr::select(Shannon.i) %>% min()
d.F.new.14$max.Shannon.i<-d.F.new.14 %>% dplyr::select(Shannon.i) %>% max()

#create dataframe for min patch area and normal year
df.pa.min.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,Shannon.i,BA.legume,elevation,min.patcharea,labour,CN.ratio) %>% 
  rename(patcharea=min.patcharea)
