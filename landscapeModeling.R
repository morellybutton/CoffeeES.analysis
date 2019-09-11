#code to model distribution of yields under different landscape configurations

library(tidyverse)
library(arm)
library(MuMIn)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
d.F.new.14<-d.F.new %>% filter(year==2014)
d.F.new.15 <- d.F.new %>% filter(year==2015&!is.na(logdiff))
d.F.new.16 <- d.F.new %>% filter(year==2016&!is.na(logdiff))

#for 2014 yield
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))

#for 2015 yield difference
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

#for 2016 yield difference
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

#keep elevation the same for each farmer (because that is outside their control)
#vary patch area (trade-off), either landscape of all "large patches" or "all small patches" 
#vary leguminous trees (complementary), either landscape without leguminous trees or all farms with max measured
#vary diversity of shade trees (trade-off), either landscape all low diversity or all high diversity

#produce figures for normal year, then log yield difference for "hot" year (2015), "dry" year (2016)

#modelling all years
#normal
d.F.new.14$`rescale(patcharea)` <- rescale(d.F.new.14$patcharea)
d.F.new.14$`rescale(elevation)` <- rescale(d.F.new.14$elevation)
d.F.new.14$`rescale(GapDry)` <- rescale(d.F.new.14$GapDry)
d.F.new.14$`rescale(tmax.anom.fruit)` <- rescale(d.F.new.14$tmax.anom.fruit)

#hot year
d.F.new.15$`rescale(patcharea)` <- rescale(d.F.new.15$patcharea)
d.F.new.15$`rescale(propCBD)` <- rescale(d.F.new.15$propCBD)
d.F.new.15$`rescale(coffee.area.ha)` <- rescale(d.F.new.15$coffee.area.ha)

#dry year
d.F.new.16$`rescale(patcharea)` <- rescale(d.F.new.16$patcharea)
d.F.new.16$`rescale(GapDry)` <- rescale(d.F.new.16$GapDry)
d.F.new.16$`rescale(propCBB)` <- rescale(d.F.new.16$propCBB)
d.F.new.16$`rescale(tmax.anom.fruit)` <- rescale(d.F.new.16$tmax.anom.fruit)
d.F.new.16$`rescale(elevation)` <- rescale(d.F.new.16$elevation)

#add min and max patch area
d.F.new.14$min.patcharea <-
  d.F.new.14$`rescale(patcharea)`[match(d.F.new.14 %>% dplyr::select(patcharea) %>% min(), d.F.new.14$patcharea)]
d.F.new.14$max.patcharea<- 
  d.F.new.14$`rescale(patcharea)`[match(d.F.new.14 %>% dplyr::select(patcharea) %>% max(), d.F.new.14$patcharea)]
d.F.new.14$mean.patcharea<- 0

d.F.new.15$min.patcharea <-
  d.F.new.15$`rescale(patcharea)`[match(d.F.new.15 %>% dplyr::select(patcharea) %>% min(), d.F.new.15$patcharea)]
d.F.new.15$max.patcharea<- 
  d.F.new.15$`rescale(patcharea)`[match(d.F.new.15 %>% dplyr::select(patcharea) %>% max(), d.F.new.15$patcharea)]
d.F.new.15$mean.patcharea<- 0

d.F.new.16$min.patcharea <-
  d.F.new.16$`rescale(patcharea)`[match(d.F.new.16 %>% dplyr::select(patcharea) %>% min(), d.F.new.16$patcharea)]
d.F.new.16$max.patcharea<- 
  d.F.new.16$`rescale(patcharea)`[match(d.F.new.16 %>% dplyr::select(patcharea) %>% max(), d.F.new.16$patcharea)]
d.F.new.16$mean.patcharea<- 0

#add min and max BA legume
d.F.new.14$`rescale(BA.legume)` <- rescale(d.F.new.14$BA.legume)
d.F.new.14$min.BA.legume<-
  d.F.new.14$`rescale(BA.legume)`[match(d.F.new.14 %>% dplyr::select(BA.legume) %>% min(), d.F.new.14$BA.legume)]
d.F.new.14$max.BA.legume<-
  d.F.new.14$`rescale(BA.legume)`[match(d.F.new.14 %>% dplyr::select(BA.legume) %>% max(), d.F.new.14$BA.legume)]
d.F.new.14$mean.BA.legume<-0

d.F.new.15$`rescale(BA.legume)` <- rescale(d.F.new.15$BA.legume)
d.F.new.15$min.BA.legume<-
  d.F.new.15$`rescale(BA.legume)`[match(d.F.new.15 %>% dplyr::select(BA.legume) %>% min(), d.F.new.15$BA.legume)]
d.F.new.15$max.BA.legume<-
  d.F.new.15$`rescale(BA.legume)`[match(d.F.new.15 %>% dplyr::select(BA.legume) %>% max(), d.F.new.15$BA.legume)]
d.F.new.15$mean.BA.legume<-0

d.F.new.16$`rescale(BA.legume)` <- rescale(d.F.new.16$BA.legume)
d.F.new.16$min.BA.legume<-
  d.F.new.16$`rescale(BA.legume)`[match(d.F.new.16 %>% dplyr::select(BA.legume) %>% min(), d.F.new.16$BA.legume)]
d.F.new.16$max.BA.legume<-
  d.F.new.16$`rescale(BA.legume)`[match(d.F.new.16 %>% dplyr::select(BA.legume) %>% max(), d.F.new.16$BA.legume)]
d.F.new.16$mean.BA.legume<-0

#add min and max shade diversity
d.F.new.14$`rescale(Shannon.i)` <- rescale(d.F.new.14$Shannon.i)
d.F.new.14$min.Shannon.i<-
  d.F.new.14$`rescale(Shannon.i)`[match(d.F.new.14 %>% dplyr::select(Shannon.i) %>% min(), d.F.new.14$Shannon.i)]
d.F.new.14$max.Shannon.i<-
  d.F.new.14$`rescale(Shannon.i)`[match(d.F.new.14 %>% dplyr::select(Shannon.i) %>% max(), d.F.new.14$Shannon.i)]
d.F.new.14$mean.Shannon.i<-0
  
d.F.new.15$`rescale(Shannon.i)` <- rescale(d.F.new.15$Shannon.i)
d.F.new.15$min.Shannon.i<-
  d.F.new.15$`rescale(Shannon.i)`[match(d.F.new.15 %>% dplyr::select(Shannon.i) %>% min(), d.F.new.15$Shannon.i)]
d.F.new.15$max.Shannon.i<-
  d.F.new.15$`rescale(Shannon.i)`[match(d.F.new.15 %>% dplyr::select(Shannon.i) %>% max(), d.F.new.15$Shannon.i)]
d.F.new.15$mean.Shannon.i<-0

d.F.new.16$`rescale(Shannon.i)` <- rescale(d.F.new.16$Shannon.i)
d.F.new.16$min.Shannon.i<-
  d.F.new.16$`rescale(Shannon.i)`[match(d.F.new.16 %>% dplyr::select(Shannon.i) %>% min(), d.F.new.16$Shannon.i)]
d.F.new.16$max.Shannon.i<-
  d.F.new.16$`rescale(Shannon.i)`[match(d.F.new.16 %>% dplyr::select(Shannon.i) %>% max(), d.F.new.16$Shannon.i)]
d.F.new.16$mean.Shannon.i<-0

#create dataframe for min patch area and all shade variables for normal year
df.pa.min.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(elevation)`,min.patcharea,`rescale(tmax.anom.fruit)`,`rescale(GapDry)`) %>% 
  rename(patcharea=min.patcharea) 

df.pa.min.hot <- d.F.new.15 %>% dplyr::select(Plot,Shrub.kg,logdiff,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(propCBD)`,min.patcharea,`rescale(coffee.area.ha)`) %>% 
  rename(patcharea=min.patcharea) 

df.pa.min.dry <- d.F.new.16 %>% dplyr::select(Plot,Shrub.kg,logdiff,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                              `rescale(elevation)`,`rescale(propCBB)`,min.patcharea,`rescale(GapDry)`,`rescale(tmax.anom.fruit)`) %>% 
  rename(patcharea=min.patcharea) 

#create dataframe for mean patch area and all shade variables for normal year
df.pa.mean.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                                `rescale(elevation)`,mean.patcharea,`rescale(tmax.anom.fruit)`,`rescale(GapDry)`) %>% 
  rename(patcharea=mean.patcharea) 

df.pa.mean.hot <- d.F.new.15 %>% dplyr::select(Plot,Shrub.kg,logdiff,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                              `rescale(propCBD)`,mean.patcharea,`rescale(coffee.area.ha)`) %>% 
  rename(patcharea=mean.patcharea) 

df.pa.mean.dry <- d.F.new.16 %>% dplyr::select(Plot,Shrub.kg,logdiff,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                              `rescale(elevation)`,`rescale(propCBB)`,mean.patcharea,`rescale(GapDry)`,`rescale(tmax.anom.fruit)`) %>% 
  rename(patcharea=mean.patcharea) 

#create dataframe for max patch area and all shade variables for normal year
df.pa.max.norm <- d.F.new.14 %>% dplyr::select(Plot,Shrub.kg,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(elevation)`,max.patcharea,`rescale(tmax.anom.fruit)`,`rescale(GapDry)`) %>% 
  rename(patcharea=max.patcharea)

df.pa.max.hot <- d.F.new.15 %>% dplyr::select(Plot,Shrub.kg,logdiff,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(propCBD)`,max.patcharea,`rescale(coffee.area.ha)`) %>% 
  rename(patcharea=max.patcharea) 

df.pa.max.dry <- d.F.new.16 %>% dplyr::select(Plot,Shrub.kg,logdiff,`rescale(Shannon.i)`,`rescale(BA.legume)`,max.BA.legume,min.BA.legume,mean.BA.legume,min.Shannon.i,max.Shannon.i,mean.Shannon.i,
                                               `rescale(elevation)`,`rescale(propCBB)`,max.patcharea,`rescale(GapDry)`,`rescale(tmax.anom.fruit)`) %>% 
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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.max.norm.1<-left_join(df.pa.max.norm.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)


#norm1<-ggplot(df.pa.max.norm.1,aes(elevation,Shrub.kg.mod)) + geom_bar(stat="identity",position = position_dodge(width = 0.9))
norm1<-ggplot(df.pa.max.norm.1,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.max.norm.2<-left_join(df.pa.max.norm.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm2<-ggplot(df.pa.max.norm.2,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.max.norm.3<-left_join(df.pa.max.norm.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm3<-ggplot(df.pa.max.norm.3,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.mean.norm.1<-left_join(df.pa.mean.norm.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

#norm1<-ggplot(df.pa.max.norm.1,aes(elevation,Shrub.kg.mod)) + geom_bar(stat="identity",position = position_dodge(width = 0.9))
norm4<-ggplot(df.pa.mean.norm.1,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.mean.norm.2<-left_join(df.pa.mean.norm.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm5<-ggplot(df.pa.mean.norm.2,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.mean.norm.3<-left_join(df.pa.mean.norm.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm6<-ggplot(df.pa.mean.norm.3,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.min.norm.1<-left_join(df.pa.min.norm.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

#norm1<-ggplot(df.pa.max.norm.1,aes(elevation,Shrub.kg.mod)) + geom_bar(stat="identity",position = position_dodge(width = 0.9))
norm7<-ggplot(df.pa.min.norm.1,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.min.norm.2<-left_join(df.pa.min.norm.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm8<-ggplot(df.pa.min.norm.2,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

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
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.min.norm.3<-left_join(df.pa.min.norm.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

norm9<-ggplot(df.pa.min.norm.3,aes(Shrub.kg)) + geom_freqpoly(binwidth=0.1, aes(color="Measured")) + theme_classic() + xlab("Shrub Yield [kg]") +
  geom_freqpoly(aes(Shrub.kg.mod,color="Modelled"),binwidth=0.05) + theme(text=element_text(size=14),legend.title=element_blank()) + xlim(0,1.8) +
  scale_color_grey(start=0.6,end=0)

# Create a figure by combining the different plots
figure <- ggpubr::ggarrange(norm1, norm2, norm3,norm4,norm5,norm6,norm7,norm8,norm9, labels = "AUTO",common.legend=T)

# Annotate the figure by adding a common labels
ggpubr::annotate_figure(figure,
                top = ggpubr::text_grob(expression(bold('Min Shade Diversity' %->% 'Max Shade Diversity')),  size = 16),
                bottom = ggpubr::text_grob(expression(bold('Max Basal Area of Leguminous Trees' %<-% 'Min Basal Area of Leguminous Trees')), size = 16),
                left = ggpubr::text_grob(expression(bold('Min Patch Area' %->% 'Max Patch Area')), rot = 90, size = 16),
                fig.lab = "Normal Year", fig.lab.face = "bold", fig.lab.size = 20
)

#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/SuppFig_ModelledvsMeasuredYield.NormalYear.pdf",height=9,width=12)


#calculate logdiff for "normal", "hot" and "dry" years
#max patch area, min Shade diversity and min BA legume
df.pa.max.normb.1<-df.pa.max.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.max.normb.1<-df.pa.max.normb.1 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.max.normb.1<-left_join(df.pa.max.normb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

df.pa.max.normb.1 <- df.pa.max.normb.1 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.max.hot.1<-df.pa.max.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)


df.pa.max.hot.1<-df.pa.max.hot.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.max.hot.1<-left_join(df.pa.max.hot.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.max.dry.1<-df.pa.max.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.max.dry.1<-df.pa.max.dry.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
         tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
         tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
         tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
         tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
         tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
         tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
         tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.max.dry.1<-left_join(df.pa.max.dry.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo1<-ggplot(df.pa.max.normb.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.max.hot.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.max.dry.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#max patch area, mean BA.legume, mean Shannon.i
df.pa.max.norm.2 <- df.pa.max.norm.2 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.max.hot.2<-df.pa.max.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)


df.pa.max.hot.2<-df.pa.max.hot.2 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.max.hot.2<-left_join(df.pa.max.hot.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.max.dry.2<-df.pa.max.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.max.dry.2<-df.pa.max.dry.2 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.max.dry.2<-left_join(df.pa.max.dry.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo2<-ggplot(df.pa.max.norm.2,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.max.hot.2,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.max.dry.2,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#max patch area, max BA.legume, max Shannon.i
df.pa.max.normb.3<-df.pa.max.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.max.normb.3<-df.pa.max.normb.3 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i) %>% 
  mutate(Shrub.kg.mod=replace(Shrub.kg.mod,Shrub.kg.mod<0,0.001))


df.pa.max.normb.3<-left_join(df.pa.max.normb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

df.pa.max.normb.3 <- df.pa.max.normb.3 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.max.hot.3<-df.pa.max.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.max.hot.3<-df.pa.max.hot.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.max.hot.3<-left_join(df.pa.max.hot.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.max.dry.3<-df.pa.max.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.max.dry.3<-df.pa.max.dry.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.max.dry.3<-left_join(df.pa.max.dry.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo3<-ggplot(df.pa.max.normb.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.max.hot.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.max.dry.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#mean patch area, min Shade diversity and min BA legume
df.pa.mean.normb.1<-df.pa.mean.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.mean.normb.1<-df.pa.mean.normb.1 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.mean.normb.1<-left_join(df.pa.mean.normb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

df.pa.mean.normb.1 <- df.pa.mean.normb.1 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.mean.hot.1<-df.pa.mean.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)


df.pa.mean.hot.1<-df.pa.mean.hot.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.mean.hot.1<-left_join(df.pa.mean.hot.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.mean.dry.1<-df.pa.mean.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.mean.dry.1<-df.pa.mean.dry.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.mean.dry.1<-left_join(df.pa.mean.dry.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo4<-ggplot(df.pa.mean.normb.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.mean.hot.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.mean.dry.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#mean patch area, mean BA.legume, mean Shannon.i
df.pa.mean.norm.2 <- df.pa.mean.norm.2 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.mean.hot.2<-df.pa.mean.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)


df.pa.mean.hot.2<-df.pa.mean.hot.2 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.mean.hot.2<-left_join(df.pa.mean.hot.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.mean.dry.2<-df.pa.mean.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.mean.dry.2<-df.pa.mean.dry.2 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.mean.dry.2<-left_join(df.pa.mean.dry.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo5<-ggplot(df.pa.mean.norm.2,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.mean.hot.2,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.mean.dry.2,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#mean patch area, max BA.legume, max Shannon.i
df.pa.mean.normb.3<-df.pa.mean.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.mean.normb.3<-df.pa.mean.normb.3 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i) %>% 
  mutate(Shrub.kg.mod=replace(Shrub.kg.mod,Shrub.kg.mod<0,0.001))


df.pa.mean.normb.3<-left_join(df.pa.mean.normb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

df.pa.mean.normb.3 <- df.pa.mean.normb.3 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.mean.hot.3<-df.pa.mean.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.mean.hot.3<-df.pa.mean.hot.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.mean.hot.3<-left_join(df.pa.mean.hot.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.mean.dry.3<-df.pa.mean.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.mean.dry.3<-df.pa.mean.dry.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.mean.dry.3<-left_join(df.pa.mean.dry.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo6<-ggplot(df.pa.mean.normb.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.mean.hot.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.mean.dry.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#min patch area, min Shade diversity and min BA legume
df.pa.min.normb.1<-df.pa.min.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.min.normb.1<-df.pa.min.normb.1 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i)

df.pa.min.normb.1<-left_join(df.pa.min.normb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

df.pa.min.normb.1 <- df.pa.min.normb.1 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.min.hot.1<-df.pa.min.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.min.hot.1<-df.pa.min.hot.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.min.hot.1<-left_join(df.pa.min.hot.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.min.dry.1<-df.pa.min.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.min.dry.1<-df.pa.min.dry.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.min.dry.1<-left_join(df.pa.min.dry.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo7<-ggplot(df.pa.min.normb.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hot.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.min.dry.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#mean patch area, mean BA.legume, mean Shannon.i
df.pa.min.norm.2 <- df.pa.min.norm.2 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.min.hot.2<-df.pa.min.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)


df.pa.min.hot.2<-df.pa.min.hot.2 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.min.hot.2<-left_join(df.pa.min.hot.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.min.dry.2<-df.pa.min.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=mean.Shannon.i,BA.legume=mean.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.min.dry.2<-df.pa.min.dry.2 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.min.dry.2<-left_join(df.pa.min.dry.2,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo8<-ggplot(df.pa.min.norm.2,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hot.2,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.min.dry.2,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#min patch area, max BA.legume, max Shannon.i
df.pa.min.normb.3<-df.pa.min.norm %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,GapDry,patcharea,Shannon.i,tmax.anom.fruit,Plot,Shrub.kg)


df.pa.min.normb.3<-df.pa.min.normb.3 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*BA.legume*Shannon.i) %>% 
  mutate(Shrub.kg.mod2=replace(Shrub.kg.mod,Shrub.kg.mod<0,0.001))


df.pa.min.normb.3<-left_join(df.pa.min.normb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

df.pa.min.normb.3 <- df.pa.min.normb.3 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod2/Shrub.kg)) %>% ungroup()

df.pa.min.hot.3<-df.pa.min.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.min.hot.3<-df.pa.min.hot.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.min.hot.3<-left_join(df.pa.min.hot.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.min.dry.3<-df.pa.min.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.min.dry.3<-df.pa.min.dry.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.min.dry.3<-left_join(df.pa.min.dry.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

combo9<-ggplot(df.pa.min.normb.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hot.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.min.dry.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)


# Create a figure by combining the different plots
figure2 <- ggpubr::ggarrange(combo1, combo2, combo3,combo4,combo5,combo6,combo7,combo8,combo9, labels = "AUTO",common.legend=T)

# Annotate the figure by adding a common labels
ggpubr::annotate_figure(figure2,
                        top = ggpubr::text_grob(expression(bold('Min Shade Diversity' %->% 'Max Shade Diversity')),  size = 16),
                        bottom = ggpubr::text_grob(expression(bold('Min Basal Area of Leguminous Trees' %->% 'Max Basal Area of Leguminous Trees')), size = 16),
                        left = ggpubr::text_grob(expression(bold('Min Patch Area' %->% 'Max Patch Area')), rot = 90, size = 16),
                        fig.lab = "Minimize Shock Losses", fig.lab.face = "bold", fig.lab.size = 20
)

#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ModelledvsMeasuredYield.MinShockLosses.pdf",height=9,width=12)

#do figure with BA.legume and Shade diversity going in opposite directions and leave out mean values
#max patch area, min BA.legume and max Shade diversity
df.pa.max.norm.1 <- df.pa.max.norm.1 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.max.hotb.1<-df.pa.max.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.max.hotb.1<-df.pa.max.hotb.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.max.hotb.1<-left_join(df.pa.max.hotb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.max.dryb.1<-df.pa.max.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.max.dryb.1<-df.pa.max.dryb.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.max.dryb.1<-left_join(df.pa.max.dryb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

kombo1<-ggplot(df.pa.max.norm.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.max.hotb.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.max.dryb.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#max patch area, max BA.legume and min Shade diversity
df.pa.max.norm.3 <- df.pa.max.norm.3 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.max.hotb.3<-df.pa.max.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.max.hotb.3<-df.pa.max.hotb.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.max.hotb.3<-left_join(df.pa.max.hotb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.max.dryb.3<-df.pa.max.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.max.dryb.3<-df.pa.max.dryb.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.max.dryb.3<-left_join(df.pa.max.dryb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

kombo2<-ggplot(df.pa.max.norm.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.max.hotb.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.max.dryb.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)


#mean patch area, min BA.legume and max Shade diversity
df.pa.mean.norm.1 <- df.pa.mean.norm.1 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.mean.hotb.1<-df.pa.mean.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.mean.hotb.1<-df.pa.mean.hotb.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.mean.hotb.1<-left_join(df.pa.mean.hotb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.mean.dryb.1<-df.pa.mean.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.mean.dryb.1<-df.pa.mean.dryb.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.mean.dryb.1<-left_join(df.pa.mean.dryb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

kombo3<-ggplot(df.pa.mean.norm.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.mean.hotb.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.mean.dryb.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#mean patch area, max BA.legume and min Shade diversity
df.pa.mean.norm.3 <- df.pa.mean.norm.3 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.mean.hotb.3<-df.pa.mean.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.mean.hotb.3<-df.pa.mean.hotb.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.mean.hotb.3<-left_join(df.pa.mean.hotb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.mean.dryb.3<-df.pa.mean.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.mean.dryb.3<-df.pa.mean.dryb.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.mean.dryb.3<-left_join(df.pa.mean.dryb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

kombo4<-ggplot(df.pa.mean.norm.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.mean.hotb.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.mean.dryb.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#min patch area, min BA.legume and max Shade diversity
df.pa.min.norm.1 <- df.pa.min.norm.1 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.min.hotb.1<-df.pa.min.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.min.hotb.1<-df.pa.min.hotb.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.min.hotb.1<-left_join(df.pa.min.hotb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.min.dryb.1<-df.pa.min.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=max.Shannon.i,BA.legume=min.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.min.dryb.1<-df.pa.min.dryb.1 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.min.dryb.1<-left_join(df.pa.min.dryb.1,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

kombo5<-ggplot(df.pa.min.norm.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hotb.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.min.dryb.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

#mean patch area, max BA.legume and min Shade diversity
df.pa.min.norm.3 <- df.pa.min.norm.3 %>% group_by(Plot) %>% mutate(logdiff=log(Shrub.kg.mod/Shrub.kg)) %>% ungroup()

df.pa.min.hotb.3<-df.pa.min.hot %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,coffee.area.ha=`rescale(coffee.area.ha)`,propCBD=`rescale(propCBD)`) %>% 
  dplyr::select(BA.legume,coffee.area.ha,propCBD,patcharea,Shannon.i,Plot,Shrub.kg)

df.pa.min.hotb.3<-df.pa.min.hotb.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
           tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume + 
           tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*propCBD + 
           tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*coffee.area.ha + 
           tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*patcharea + 
           tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i) %>% ungroup()

df.pa.min.hotb.3<-left_join(df.pa.min.hotb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") 

df.pa.min.dryb.3<-df.pa.min.dry %>% dplyr::select(-`rescale(Shannon.i)`,-`rescale(BA.legume)`) %>% 
  rename(Shannon.i=min.Shannon.i,BA.legume=max.BA.legume,elevation=`rescale(elevation)`,propCBB=`rescale(propCBB)`,
         tmax.anom.fruit=`rescale(tmax.anom.fruit)`,GapDry=`rescale(GapDry)`) %>% 
  dplyr::select(BA.legume,elevation,tmax.anom.fruit,GapDry,propCBB,patcharea,Shannon.i,Plot,logdiff,Shrub.kg)

df.pa.min.dryb.3<-df.pa.min.dryb.3 %>% group_by(Plot) %>% 
  mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*BA.legume +
           tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*elevation + 
           tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*BA.legume*Shannon.i +
           tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*tmax.anom.fruit +
           tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*propCBB +
           tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*GapDry)

df.pa.min.dryb.3<-left_join(df.pa.min.dryb.3,d.F.new.14 %>% dplyr::select(Plot,elevation), by="Plot") %>%
  rename(elevation=elevation.y)

kombo6<-ggplot(df.pa.min.norm.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year")) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hotb.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5) + 
  geom_freqpoly(data=df.pa.min.dryb.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank()) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0)

# Create a figure by combining the different plots
figure3 <- ggpubr::ggarrange(kombo1, kombo2, kombo3,kombo4,kombo5,kombo6,labels = "AUTO",ncol=2,nrow=3,common.legend=T)

# Annotate the figure by adding a common labels
ggpubr::annotate_figure(figure3,
                        top = ggpubr::text_grob(expression(bold('Min Shade Diversity' %->% 'Max Shade Diversity')),  size = 16),
                        bottom = ggpubr::text_grob(expression(bold('Max Basal Area of Leguminous Trees' %<-% 'Min Basal Area of Leguminous Trees')), size = 16),
                        left = ggpubr::text_grob(expression(bold('Min Patch Area' %->% 'Max Patch Area')), rot = 90, size = 16),
                        fig.lab = "Maximise Yields", fig.lab.face = "bold", fig.lab.size = 20
)

#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ModelledvsMeasuredYield.MaxYields.pdf",height=9,width=9.5)

#combination of three options (max yield, min losses, min shock losses)
kombo7 <- ggplot(df.pa.max.norm.1,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.max.hotb.1,aes(logdiff.mod,color="Hot Year"),binwidth=0.5,size=1) + 
  geom_freqpoly(data=df.pa.max.dryb.1,aes(logdiff.mod,color="Dry Year"),binwidth=0.5,size=1) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0) + annotate("text",x=-6,y=25,label="Max Patch Area",size=5) + ggtitle("Maximise Yields (Normal Year)") +
  annotate("text",x=-5.65,y=23.5,label="Min Shade Diversity",size=5) + annotate("text",x=-4.95,y=22,label="Max BA Leguminous Trees",size=5) 
kombo8 <- ggplot(df.pa.min.norm.2,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hot.2,aes(logdiff.mod,color="Hot Year"),binwidth=0.5,size=1) + 
  geom_freqpoly(data=df.pa.min.dry.2,aes(logdiff.mod,color="Dry Year"),binwidth=0.5,size=1) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0) + annotate("text",x=-6.25,y=25,label="Min Patch Area",size=5) + ggtitle("Minimise Variability (All Years)") +
  annotate("text",x=-5.60,y=23.5,label="Mean Shade Diversity",size=5) + annotate("text",x=-4.95,y=22,label="Mean BA Leguminous Trees",size=5)
kombo9 <- ggplot(df.pa.min.normb.3,aes(logdiff)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Log Difference in Yield") +
  geom_freqpoly(data=df.pa.min.hot.3,aes(logdiff.mod,color="Hot Year"),binwidth=0.5,size=1) + 
  geom_freqpoly(data=df.pa.min.dry.3,aes(logdiff.mod,color="Dry Year"),binwidth=0.5,size=1) +
  geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0) + annotate("text",x=-6.1,y=25,label="Min Patch Area",size=5) + ggtitle("Minimise Losses (Shock Years)*") +
  annotate("text",x=-5.60,y=23.5,label="Max Shade Diversity",size=5) + annotate("text",x=-4.95,y=22,label="Max BA Leguminous Trees",size=5)

ggpubr::ggarrange(kombo7, kombo8, kombo9,labels = "AUTO",ncol=3,nrow=1,common.legend=T)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ContrastDifferentStrategies.pdf",height=5,width=16.5)


#calculate actual yields using (base year yield)*e^(logdiff) to get shock year yields.
#for maximising yields in normal year (max patch area, min shade diversity and max BA.legume)
#get mean coffee density and farm size
m_density<-d.F.new.14 %>% summarise(density=median(density,na.rm=T))
  
df.pa.max.hotb.1 <- df.pa.max.hotb.1 %>% group_by(Plot) %>% mutate(Shrub.kg.mod=Shrub.kg*exp(logdiff.mod)) %>% 
  mutate(yld.kg=Shrub.kg.mod*m_density$density)
  
df.pa.max.dryb.1 <- df.pa.max.dryb.1 %>% group_by(Plot) %>% mutate(Shrub.kg.mod=Shrub.kg*exp(logdiff.mod)) %>% 
  mutate(yld.kg=Shrub.kg.mod*m_density$density)

acnorm1<-ggplot(df.pa.max.norm.1,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + scale_color_viridis_c() +
  theme_classic() +  xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + xlim(0,1) + ylim(0,2) +
  annotate("text",label="Normal Year",x=0.2,y=1.9,size=6) + geom_abline(slope=2,intercept=0,linetype="dashed") +
  annotate("text",label="slope = 2",angle=45,x=0.25,y=0.6)

acnorm2<-  ggplot() + geom_point(data=df.pa.max.hotb.1,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + scale_color_viridis_c() +
  theme_classic() +  xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + xlim(0,1) + ylim(0,0.2) +
  annotate("text",label="Hot Year",x=0.15,y=0.19,size=6) + geom_abline(slope=0.1,intercept=0,linetype="dashed") +
  annotate("text",label="slope = 0.1",angle=25,x=0.75,y=0.085)

acnorm3<- ggplot() + geom_point(data=df.pa.max.dryb.1,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) +
  scale_color_viridis_c() + theme_classic() +  xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + xlim(0,1) + ylim(0,0.2) +
  annotate("text",label="Dry Year",x=0.15,y=0.19,size=6) + geom_abline(slope=0.2,intercept=0,linetype="dashed") +
  annotate("text",label="slope = 0.2",angle=45,x=0.5,y=0.11)

acnorm<-ggpubr::ggarrange(acnorm1, acnorm2, acnorm3,ncol=3,nrow=1,common.legend=T,legend="right")

ac.norm<-ggpubr::annotate_figure(acnorm,
                        fig.lab = "Maximise Yields", fig.lab.face = "bold", fig.lab.size = 20
)

#for minimising variability in yields (min patch area, mean shade diversity and mean BA.legume)
df.pa.min.hot.2 <- df.pa.min.hot.2 %>% group_by(Plot) %>% mutate(Shrub.kg.mod=Shrub.kg*exp(logdiff.mod)) %>% 
  mutate(yld.kg=Shrub.kg.mod*m_density$density)
df.pa.min.dry.2 <- df.pa.min.dry.2 %>% group_by(Plot) %>% mutate(Shrub.kg.mod=Shrub.kg*exp(logdiff.mod)) %>% 
  mutate(yld.kg=Shrub.kg.mod*m_density$density)

minvar1<-ggplot(df.pa.min.norm.2,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + xlim(0,1) + ylim(0,1) + scale_color_viridis_c() +
  theme_classic() + xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + geom_abline(slope=1,intercept=0,linetype="dashed") +
  annotate("text",label="Normal Year",x=0.2,y=0.9,size=6) + annotate("text",label="slope = 1",angle=45,x=0.75,y=0.8)

minvar2<-ggplot(df.pa.min.hot.2,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + xlim(0,1) + ylim(0,1) + scale_color_viridis_c() +
  theme_classic() + xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + geom_abline(slope=1,intercept=0,linetype="dashed") +
  annotate("text",label="Hot Year",x=0.15,y=0.9,size=6) + annotate("text",label="slope = 1",angle=45,x=0.5,y=0.55)

minvar3<-ggplot(df.pa.min.dry.2,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + xlim(0,1) + ylim(0,1) + scale_color_viridis_c() +
  theme_classic() + xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + geom_abline(slope=1,intercept=0,linetype="dashed") +
  annotate("text",label="Dry Year",x=0.15,y=0.9,size=6) + annotate("text",label="slope = 1",angle=45,x=0.5,y=0.55)

minvar<-ggpubr::ggarrange(minvar1, minvar2, minvar3,ncol=3,nrow=1,common.legend=T,legend="right")
min.var<-ggpubr::annotate_figure(minvar,
                                 fig.lab = "Minimise Variability", fig.lab.face = "bold", fig.lab.size = 20
)

#Minimise shock losses (min patch, max BA.legume, max shade diversity)
df.pa.min.hot.3 <- df.pa.min.hot.3 %>% group_by(Plot) %>% mutate(Shrub.kg.mod=Shrub.kg*exp(logdiff.mod)) %>% 
  mutate(yld.kg=Shrub.kg.mod*m_density$density)
df.pa.min.dry.3 <- df.pa.min.dry.3 %>% group_by(Plot) %>% mutate(Shrub.kg.mod=Shrub.kg*exp(logdiff.mod)) %>% 
  mutate(yld.kg=Shrub.kg.mod*m_density$density)

minlos1<-ggplot(df.pa.min.normb.3,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + xlim(0,1) + ylim(-0.5,1) + scale_color_viridis_c() +
  theme_classic() + xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + geom_hline(yintercept=0,linetype="dotted") +
  annotate("text",label="Normal Year",x=0.2,y=0.9,size=6) + annotate("text",label="y = 0", x=0.1,y=0.1)

minlos2<-ggplot(df.pa.min.hot.3,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + xlim(0,1) + ylim(0,2) + scale_color_viridis_c() +
  theme_classic() + xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + geom_abline(slope=2,intercept=0,linetype="dashed") +
  annotate("text",label="Hot Year",x=0.15,y=1.9,size=6) + annotate("text",label="slope = 2",angle=45,x=0.5,y=1.1)

minlos3<-ggplot(df.pa.min.dry.3,aes(Shrub.kg,Shrub.kg.mod,color=elevation)) + geom_point() + xlim(0,1) + ylim(0,6) + scale_color_viridis_c() +
  theme_classic() + xlab("Base Yield [kg/shrub]") + ylab("Modelled Yield [kg/shrub]") + geom_abline(slope=6,intercept=0,linetype="dashed") +
  annotate("text",label="Dry Year",x=0.15,y=5.9,size=6)  + annotate("text",label="slope = 6",angle=45,x=0.5,y=3.4)

minlos<-ggpubr::ggarrange(minlos1, minlos2, minlos3,ncol=3,nrow=1,common.legend=T,legend="right")
min.los<-ggpubr::annotate_figure(minlos,
                                 fig.lab = "Minimise Shock Losses", fig.lab.face = "bold", fig.lab.size = 20
)

ggpubr::ggarrange(ac.norm,min.var,min.los,common.legend=T,ncol=1,nrow=3,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ContrastDifferentStrategies.Yields.pdf",height=12,width=12)

#combination of three options (max yield, min losses, min shock losses)
#redo this figure but with actual yields instead
df.pa.max.norm.1<-df.pa.max.norm.1 %>% mutate(yld.kg=Shrub.kg.mod*m_density$density)
kombo10 <- ggplot(df.pa.max.norm.1,aes(yld.kg)) + geom_freqpoly(binwidth=50, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=df.pa.max.hotb.1,aes(yld.kg,color="Hot Year"),binwidth=50,size=1) + 
  geom_freqpoly(data=df.pa.max.dryb.1,aes(yld.kg,color="Dry Year"),binwidth=50,size=1) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0) + annotate("text",x=500,y=40,label="Max Patch Area",size=5) + ggtitle("Maximise Yields (Normal Year)") +
  annotate("text",x=500,y=37.5,label="Min Shade Diversity",size=5) + annotate("text",x=450,y=35,label="Max BA Leguminous Trees",size=5) 

df.pa.min.norm.2 <- df.pa.min.norm.2 %>% mutate(yld.kg=Shrub.kg.mod*m_density$density)
kombo11 <- ggplot(df.pa.min.norm.2,aes(yld.kg)) + geom_freqpoly(binwidth=50, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=df.pa.min.hot.2,aes(yld.kg,color="Hot Year"),binwidth=50,size=1) + 
  geom_freqpoly(data=df.pa.min.dry.2,aes(yld.kg,color="Dry Year"),binwidth=50,size=1) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-8,5) +
  scale_color_viridis_d(begin=0.8,end=0) + annotate("text",x=200,y=38,label="Min Patch Area",size=5) + ggtitle("Minimise Variability (All Years)") +
  annotate("text",x=200,y=35.5,label="Mean Shade Diversity",size=5) + annotate("text",x=200,y=33,label="Mean BA Leguminous Trees",size=5)

df.pa.min.normb.3 <- df.pa.min.normb.3 %>% mutate(yld.kg=Shrub.kg.mod2*m_density$density)
kombo12 <- ggplot(df.pa.min.normb.3,aes(yld.kg)) + geom_freqpoly(binwidth=0.5, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=df.pa.min.hot.3,aes(yld.kg,color="Hot Year"),binwidth=75,size=1) + 
  geom_freqpoly(data=df.pa.min.dry.3,aes(yld.kg,color="Dry Year"),binwidth=75,size=1) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-0.5,4000) +
  scale_color_viridis_d(begin=0.8,end=0) + annotate("text",x=2000,y=50,label="Min Patch Area",size=5) + ggtitle("Minimise Losses (Shock Years)*") +
  annotate("text",x=2000,y=47,label="Max Shade Diversity",size=5) + annotate("text",x=2000,y=44,label="Max BA Leguminous Trees",size=5)

ggpubr::ggarrange(kombo10, kombo11, kombo12,labels = "AUTO",ncol=3,nrow=1,common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ContrastDifferentStrategies.farmyields.pdf",height=5,width=16.5)

