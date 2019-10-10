#Analysis of coffee ES contributions to yield with TerraClim data

library(tidyverse)
#library(AICcmodavg)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")
#setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")

d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub_analysis_dataset.csv"))
d.F$site<-d.F$Plot
#remove NA values for yield
d.F <- d.F[!is.na(d.F$Shrub.kg),]

#make sure binary variables are factors
d.F$kebele<-factor(d.F$kebele)
d.F$compost.bin<-0
d.F[d.F$compost>0,"compost.bin"]<-1
d.F$compost.bin<-factor(d.F$compost.bin)
d.F$b.ffer<-factor(d.F$buffer)
d.F$wereda<-"Yayu"
d.F[d.F$kebele=="Badessa"|d.F$kebele=="Weyra","wereda"]<-"Doraani"
d.F$wereda<-factor(d.F$wereda)

#check which linking function I need, see how well values stay within dashed lines
d.F$Shrub.kg.1<- d.F$Shrub.kg + .00001
d.F <- d.F %>% group_by(Plot,Shrub.id,year) %>% mutate(prop.fdrop = fruit.drop/Tot.fruits) %>% mutate(prop.fdrop = replace(prop.fdrop,Tot.fruits==0,0)) %>% ungroup()

#identify consistently low yielding shrubs for each year
quart14 <- quantile(d.F %>% filter(year==2014) %>% pull(Shrub.kg))
quart15 <- quantile(d.F %>% filter(year==2015) %>% pull(Shrub.kg))
quart16 <- quantile(d.F %>% filter(year==2016) %>% pull(Shrub.kg))

low.yield14 <- d.F %>% filter(year==2014&Shrub.kg<quart14[3]) %>% select(Plot,Shrub.id)
low.yield14 <- low.yield14 %>% mutate(low.yield14=1)
low.yield14$year<-2014
low.yield15 <- d.F %>% filter(year==2015&Shrub.kg<quart15[3]) %>% select(Plot,Shrub.id)
low.yield15 <- low.yield15 %>% mutate(low.yield15=1)
low.yield15$year<-2015
low.yield16 <- d.F %>% filter(year==2016&Shrub.kg<quart16[3]) %>% select(Plot,Shrub.id)
low.yield16 <- low.yield16 %>% mutate(low.yield16=1)
low.yield16$year<-2016

d.F <- left_join(d.F,low.yield14,by=c("Plot","Shrub.id","year"))
d.F <- d.F %>% mutate(low.yield14=replace(low.yield14,is.na(low.yield14)&year==2014,0))
d.F <- left_join(d.F,low.yield15,by=c("Plot","Shrub.id","year"))
d.F <- d.F %>% mutate(low.yield15=replace(low.yield15,is.na(low.yield15)&year==2015,0))
d.F <- left_join(d.F,low.yield16,by=c("Plot","Shrub.id","year"))
d.F <- d.F %>% mutate(low.yield16=replace(low.yield16,is.na(low.yield16)&year==2016,0))
d.F <- d.F %>% group_by(Plot,Shrub.id,year) %>% mutate(low.yield=sum(low.yield14,low.yield15,low.yield16,na.rm=T))

#take sum of low yielding shrubs and only assign low.yield.bin 1 if sum is >=4
d.F.plot <- d.F %>% group_by(Plot,year) %>% summarise(Shrub.kg=median(Shrub.kg,na.rm=T),Tot.fruits=median(Tot.fruits,na.rm=T),fruitset=median(fruitset,na.rm=T),
                                                      propCBB=median(propCBB,na.rm=T),propCBD=median(propCBD,na.rm=T),fruit.drop=median(fruit.drop,na.rm=T),prop.fdrop=median(prop.fdrop,na.rm=T),
                                                      Tot.leaves=median(Tot.leaves,na.rm=T),leaf.drop=median(leaf.drop,na.rm=T),prop.ldrop=median(prop.ldrop,na.rm=T),
                                                      propLM=median(propLM, na.rm=T), propCLR=median(propCLR,na.rm=T), propWilt=median(propWilt,na.rm=T),
                                                      propHerb=median(propHerb,na.rm=T),low.yield=mean(low.yield,na.rm=T),low.yield14=sum(low.yield14,na.rm=T),low.yield15=sum(low.yield15,na.rm=T),
                                                      low.yield16=sum(low.yield16,na.rm=T),prop.legume=mean(BA.legume/BA.all,na.rm=T)) %>% mutate(prop.legume=replace(prop.legume,is.na(prop.legume),0)) %>%
  mutate(low.yield14.bin=0,low.yield15.bin=0,low.yield16.bin=0) %>% mutate(low.yield14.bin=replace(low.yield14.bin,low.yield14>=4,1),
                                                                                           low.yield15.bin=replace(low.yield15.bin,low.yield15>=4,1),
                                                                                           low.yield16.bin=replace(low.yield16.bin,low.yield16>=4,1)) %>% ungroup()

low.yield.plot<- d.F.plot %>% group_by(Plot) %>% summarise(low.yield1=sum(low.yield14.bin,low.yield15.bin,low.yield16.bin,na.rm=T),low.yield1415=sum(low.yield14.bin,low.yield15.bin,na.rm=T),low.yield1516=sum(low.yield15.bin,low.yield16.bin,na.rm=T),low.yield1416=sum(low.yield16.bin,low.yield14.bin,na.rm=T),low.yield.bin=0,low.yield1415.bin=0,low.yield1516.bin=0,low.yield1416.bin=0) %>% 
  mutate(low.yield.bin=replace(low.yield.bin,low.yield1==2,1),low.yield1415.bin=replace(low.yield1415.bin,low.yield1415==2,1),low.yield1516.bin=replace(low.yield1516.bin,low.yield1516==2,1),low.yield1416.bin=replace(low.yield1416.bin,low.yield1416==2,1)) %>% ungroup()

d.F.plot<-left_join(d.F.plot,low.yield.plot %>% select(Plot,low.yield.bin,low.yield1415.bin,low.yield1516.bin,low.yield1416.bin),by="Plot")
d.F.plot$ID<-1:nrow(d.F.plot)
d.F.plot <- left_join(d.F.plot,d.F %>% select(-ID,-Shrub.kg,-Tot.fruits,-fruitset,-propCBB,-propCBD,-fruit.drop,-Tot.leaves,-leaf.drop,-prop.ldrop,-prop.fdrop,-propLM,-propCLR,-propWilt,-propHerb,-Shrub.kg.1,-low.yield,-low.yield14,-low.yield15,-low.yield16),by=c("Plot","year"))
d.F.plot <- distinct(d.F.plot,ID,.keep_all=T)

#model difference between 2014 and 2016
y.ld14<-d.F.plot %>% filter(year=="2014") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.14=Shrub.kg)
y.ld16<-d.F.plot %>% filter(year=="2016") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.16=Shrub.kg)
y.ld15<-d.F.plot %>% filter(year=="2015") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.15=Shrub.kg)

combo<-left_join(y.ld14,y.ld15,by="Plot")
combo<-left_join(combo,y.ld16,by="Plot")
combo <- combo %>% group_by(Plot) %>%  mutate(diff.yld1415 = Shrub.kg.15-Shrub.kg.14,diff.yld1416 = Shrub.kg.16-Shrub.kg.14, logdiff1415=log(Shrub.kg.15/Shrub.kg.14),
                          logdiff1416=log(Shrub.kg.16/Shrub.kg.14),pdiff.yld1415=(Shrub.kg.15-Shrub.kg.14)/Shrub.kg.14,pdiff.yld1416=(Shrub.kg.16-Shrub.kg.14)/Shrub.kg.14) %>% 
  ungroup()

d.F.plot <- left_join(d.F.plot,combo %>% select(Plot,diff.yld1415,diff.yld1416,pdiff.yld1415,pdiff.yld1416,logdiff1415,logdiff1416),by="Plot")
d.F.plot.14 <- d.F.plot %>% filter(year==2014)
d.F.plot.1516 <- d.F.plot %>% filter(year!=2014)
d.F.plot.16 <- d.F.plot %>% filter(year==2016)
d.F.plot.15 <- d.F.plot %>% filter(year==2015)

d.F.plot.15<-d.F.plot.15 %>% rename(diff.yld=diff.yld1415,pdiff.yld=pdiff.yld1415,logdiff=logdiff1415)
d.F.plot.16<-d.F.plot.16 %>% rename(diff.yld=diff.yld1416,pdiff.yld=pdiff.yld1416,logdiff=logdiff1416)
diff.yld<-bind_rows(d.F.plot.15 %>% select(Plot,year,diff.yld,pdiff.yld,logdiff),d.F.plot.16 %>% select(Plot,year,diff.yld,pdiff.yld,logdiff))
d.F.plot.1516<-left_join(d.F.plot.1516,diff.yld,by=c("Plot","year"))
d.F.plot.1516 <- distinct(d.F.plot.1516,ID,.keep_all=T)

d.F.plot.14$diff.yld<-NA
d.F.new <- bind_rows(d.F.plot.14 %>% select(-diff.yld1415,-diff.yld1416,-pdiff.yld1415,-pdiff.yld1416,-logdiff1415,-logdiff1416),d.F.plot.1516 %>% 
                       select(-diff.yld1415,-diff.yld1416,-pdiff.yld1415,-pdiff.yld1416,-logdiff1415,-logdiff1416)) %>% arrange(Plot,year)

#remove H6
d.F.new <- d.F.new %>% filter(Plot!="H6") %>% mutate(logdiff=replace(logdiff,logdiff==-Inf,NA))
write.csv(d.F.new,paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

library(car)
library(MuMIn)
library(arm)
library(lattice)
library(lme4)
#library(plotly)

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#do difference in yields for all years together
options(na.action = "na.omit")

#model yield for 2014
d.F.new.14<-d.F.new %>% filter(year==2014)

pdf(paste0(getwd(),"/Analysis/ES/Plot.yld14_norm.pdf"),width=8,height=8)
qqp(d.F.new.14$Shrub.kg, "norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.logylddiff_norm.pdf"),width=8,height=8)
qqp(d.F.new$logdiff, "norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2015logylddiff_norm.pdf"),width=8,height=8)
qqp(d.F.new$logdiff[d.F.new$year=="2015"], "norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2016logylddiff_norm.pdf"),width=8,height=8)
qqp(d.F.new$logdiff[d.F.new$year=="2016"], "norm")
dev.off()

#do a corrplot
tmp14<-d.F.new.14 %>% dplyr::select(-X.1,-Plot,-ID,-X,-Shrub.id,-kebele,-wereda,-site)
corrplot(tmp14,color=TRUE)

#2014
#full equation from which below were derived
fm<-lm(Shrub.kg~rescale(GapDry)+rescale(Shannon.i)*rescale(BA.legume) + rescale(C.pct) + rescale(CN.ratio) +
           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) +
           rescale(tmax.anom.fruit)+rescale(elevation)*rescale(patcharea) ,data=d.F.new.14)
summary(fm)

fm14<-lm(Shrub.kg~rescale(GapDry)+rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
           rescale(propCLR) + rescale(propCBB)  +
           rescale(tmax.anom.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new.14)
summary(fm14)

fm14b<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
            rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  +
            rescale(elevation)*rescale(patcharea),data=d.F.new.14)
summary(fm14b)

#variables were removed in the following order: (N.B. pH was too closely correlated with elevation)
#1) elevation:patcharea, 2)coffee.area.ha, 3)propCBD, 4) C.pct, 5) CN.ratio
fm14c<-lm(Shrub.kg~rescale(GapDry)+rescale(Shannon.i)*rescale(BA.legume)  +
         rescale(propCLR) + rescale(propCBB)  +
         rescale(tmax.anom.fruit)+rescale(elevation) + rescale(patcharea),data=d.F.new.14)
summary(fm14c)

#2015&2016
#d.F.new1<-d.F.new %>% filter(!is.na(diff.yld))
#dm<-lmer(diff.yld~rescale(Shannon.i) + rescale(BA.legume) + rescale(GapDry) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(fruitset) +
#           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) + rescale(low.yield) +
#           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea) +
#           (1|year),data=d.F.new1, REML=FALSE)

#summary(dm)

#2015
d.F.new15 <- d.F.new %>% filter(year==2015&!is.na(logdiff))

#do a corrplot
tmp15<-d.F.new15 %>% dplyr::select(-X.1,-Plot,-ID,-X,-Shrub.id,-kebele,-wereda,-site)
corrplot(tmp15,color=TRUE)

#full equation from which below were derived
dm15<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume)+rescale(GapDry) + rescale(C.pct) + rescale(CN.ratio) + rescale(fruitset) +
           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha)+rescale(low.yield.bin) +
           rescale(tmax.anom.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)
summary(dm15)

dm15<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(Tot.P.ppm) + rescale(fruitset) +
           rescale(propCBD) + rescale(coffee.area.ha)+
           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)

summary(dm15)

dm15b<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(Tot.P.ppm) + rescale(fruitset) +
           rescale(propCBD)  + rescale(coffee.area.ha) +
           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)
summary(dm15b)

#variables were removed in the following order:
#1)BA.legume:Shannon.i, 2)propCLR, 3)GapDry, 4)CN.ratio, 5)propCBB, 6)tmax.anom.fruit,
#7)low.yield.bin, 8)C.pct, 9) fruitset
dm15c<-lm(logdiff~rescale(Shannon.i)+rescale(BA.legume)  +
           rescale(propCBD)  + rescale(coffee.area.ha) +
           rescale(elevation)*rescale(patcharea),data=d.F.new15)
summary(dm15c)

#2016
d.F.new16 <- d.F.new %>% filter(year==2016&!is.na(logdiff))
#do a corrplot
tmp16<-d.F.new16 %>% dplyr::select(-X.1,-Plot,-ID,-X,-Shrub.id,-kebele,-wereda,-site)
corrplot(tmp16,color=TRUE)

#full equation from which below were derived
dm16<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(GapDry) + rescale(C.pct) + rescale(CN.ratio) + rescale(fruitset) +
           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) + rescale(low.yield.bin) +
           rescale(tmax.anom.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)
summary(dm16)

dm16<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(GapDry) + rescale(Tot.P.ppm) +
           rescale(propCBB) + rescale(coffee.area.ha) +
           rescale(tmax.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new16)

summary(dm16)

dm16b<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(GapDry) + rescale(Tot.P.ppm) +
           rescale(propCBB) + rescale(coffee.area.ha) +
           rescale(tmax.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new16)
summary(dm16b)

#variables were removed in the following order:
#1)coffee.area.ha, 2)C.pct, 3)fruitset, 4)propCLR, 5)low.yield.bin, 6)propCBD, 7)CN.ratio
dm16c<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(GapDry) +
           rescale(propCBB)  +
           rescale(tmax.anom.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)
summary(dm16c)

#check heteroskedasticity
fm14<-fm14c
diagnos14 <- data.frame(Resid = resid(fm14, type = "pearson"), Fitted = fitted(fm14),Variable = d.F.new.14$Plot[!is.na(d.F.new.14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yld14_ResidualvFittedValues_all.v3.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos14)
dev.off()

#diagnos <- data.frame(Resid = resid(dm, type = "pearson"), Fitted = fitted(dm),Variable = d.F.new1$Plot )
#pdf(paste0(getwd(),"/Analysis/ES/Plot.ylddiff_ResidualvFittedValues_all.pdf"),width=8,height=8)
#xyplot(Resid ~ Fitted, data = diagnos)
#dev.off()

dm15<-dm15c
diagnos15 <- data.frame(Resid = resid(dm15, type = "pearson"), Fitted = fitted(dm15),Variable = d.F.new15$Plot)
pdf(paste0(getwd(),"/Analysis/ES/Plot.2015logylddiff_ResidualvFittedValues_all.v3.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos15)
dev.off()

dm16<-dm16c
diagnos16 <- data.frame(Resid = resid(dm16, type = "pearson"), Fitted = fitted(dm16),Variable = d.F.new16$Plot)
pdf(paste0(getwd(),"/Analysis/ES/Plot.2016logylddiff_ResidualvFittedValues_all.v3.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos16)
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yld14_qqplotResiduals_all.v3.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos14, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.ylddiff_qqplotResiduals_all.pdf"),width=8,height=8)
#qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
#       panel = function(x, ...) {
#         panel.qqmathline(x, ...)
#         panel.qqmath(x, ...)
#       })
#dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2015logylddiff_qqplotResiduals_all.v3.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos15, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2016logylddiff_qqplotResiduals_all.v3.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos16, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

options(na.action = "na.fail")
#2014
fm14.d<-dredge(fm14c)

#picked delta of 2 because delta of 6 was >24 models
dredg.m14<-subset(fm14.d,delta<6)
write.csv(dredg.m14,paste0(getwd(),"/Analysis/ES/Yld.2014_dredged03.csv"))

#2015&2016
#dm.d<-dredge(dm)

#picked delta 6 was >200 models
#dredg.m01<-subset(dm.d,delta<2)
#write.csv(dredg.m01,paste0(getwd(),"/Analysis/ES/Yld.diff_dredged01.csv"))

#2015
dm15.d<-dredge(dm15c)

#picked delta 2 because  6 was >200 models
dredg.m15<-subset(dm15.d,delta<2)
write.csv(dredg.m15,paste0(getwd(),"/Analysis/ES/Logyld.diff_dredged15.v3.csv"))

#2016
dm16.d<-dredge(dm16c)

#picked delta 2 because 6 was >50 models
dredg.m16<-subset(dm16.d,delta<6)
write.csv(dredg.m16,paste0(getwd(),"/Analysis/ES/Logyld.diff_dredged16.v3.csv"))

#for 2014 yield
cand.set.14<-list()
#delta 6 has 11 models
cand.set.14[[1]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(elevation)+rescale(patcharea),data=d.F.new.14)
cand.set.14[[2]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                      rescale(elevation)+rescale(patcharea)+rescale(tmax.anom.fruit),data=d.F.new.14)
cand.set.14[[3]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                      rescale(elevation)+rescale(patcharea)+rescale(GapDry),data=d.F.new.14)
#include a model without interaction between BA.legume and Shannon.i with lowest possible delta AIC
cand.set.14[[4]]<-lm(Shrub.kg~rescale(Shannon.i)+rescale(BA.legume) +
                       rescale(elevation) + rescale(tmax.anom.fruit),data=d.F.new.14)
#cand.set.14[[5]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
#                       rescale(elevation)+rescale(patcharea),data=d.F.new.14)
#cand.set.14[[6]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
#                       rescale(elevation),data=d.F.new.14)
#cand.set.14[[7]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
#                       rescale(elevation)*rescale(patcharea),data=d.F.new.14)
#cand.set.14[[8]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
#                       rescale(elevation)+rescale(patcharea) + rescale(labour),data=d.F.new.14)
#cand.set.14[[9]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
#                       rescale(elevation)*rescale(patcharea) + rescale(labour),data=d.F.new.14)
#cand.set.14[[10]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
#                       rescale(elevation) + rescale(labour),data=d.F.new.14)
#cand.set.14[[11]]<-lm(Shrub.kg~rescale(Shannon.i)+rescale(BA.legume) +
#                        rescale(elevation)+rescale(patcharea),data=d.F.new.14)


#for 2015 and 2016 yield difference
#cand.set<-list()
#delta 2 has 14 models reduced to 4
#cand.set[[1]]<-lmer(diff.yld~rescale(Shannon.i) + rescale(BA.legume) + rescale(coffee.area.ha) + rescale(low.yield) +
#                    rescale(elevation)*rescale(patcharea) +
#                    (1|year),data=d.F.new1, REML=FALSE)
#cand.set[[2]]<-lmer(diff.yld~rescale(BA.legume) + rescale(coffee.area.ha) + rescale(low.yield) +
#                    rescale(elevation)*rescale(patcharea) +
#                    (1|year),data=d.F.new1, REML=FALSE)
#include without interaction term for model averaging
#cand.set[[3]]<-lmer(diff.yld~rescale(BA.legume) + rescale(coffee.area.ha) + rescale(low.yield) +
#                      rescale(elevation) + rescale(patcharea) +
#                      (1|year),data=d.F.new1, REML=FALSE)

#for 2015 yield difference
cand.set.15<-list()
#delta 2 has 25 models
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
#cand.set.15[[15]]<-lm(logdiff~rescale(patcharea) + rescale(fruitset) + rescale(Shannon.i),data=d.F.new15)
#cand.set.15[[16]]<-lm(logdiff~rescale(BA.legume) + rescale(coffee.area.ha)+rescale(patcharea) + rescale(elevation) + rescale(tmax.fruit)  + rescale(propCBD),data=d.F.new15)
#cand.set.15[[17]]<-lm(logdiff~rescale(BA.legume) + rescale(patcharea) + rescale(propCBD),data=d.F.new15)
#cand.set.15[[18]]<-lm(logdiff~rescale(patcharea) + rescale(propCBD),data=d.F.new15)
#cand.set.15[[19]]<-lm(logdiff~rescale(BA.legume) + rescale(coffee.area.ha)+rescale(patcharea) + rescale(Shannon.i),data=d.F.new15)
#cand.set.15[[20]]<-lm(logdiff~rescale(coffee.area.ha) + rescale(patcharea) + rescale(propCBD) + rescale(tmax.fruit),data=d.F.new15)
#cand.set.15[[21]]<-lm(logdiff~rescale(patcharea) + rescale(tmax.fruit),data=d.F.new15)
#cand.set.15[[22]]<-lm(logdiff~rescale(BA.legume) + rescale(patcharea),data=d.F.new15)
#cand.set.15[[23]]<-lm(logdiff~rescale(coffee.area.ha)+rescale(patcharea) + rescale(elevation) + rescale(tmax.fruit),data=d.F.new15)
#cand.set.15[[24]]<-lm(logdiff~rescale(BA.legume) + rescale(coffee.area.ha)+rescale(patcharea) + rescale(Shannon.i) + rescale(propCBD),data=d.F.new15)
#cand.set.15[[25]]<-lm(logdiff~rescale(BA.legume) + rescale(patcharea) + rescale(tmax.fruit),data=d.F.new15)

#for 2016 yield difference
cand.set.16<-list()
#delta 2 has 9 models
cand.set.16[[1]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit),data=d.F.new16)
cand.set.16[[2]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit) +rescale(GapDry),data=d.F.new16)
cand.set.16[[3]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit) + rescale(propCBB),data=d.F.new16)
cand.set.16[[4]]<-lm(logdiff~rescale(Shannon.i)+rescale(BA.legume) + rescale(elevation) +rescale(tmax.anom.fruit),data=d.F.new16)
#cand.set.16[[5]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation)+rescale(patcharea) + rescale(propCBB),data=d.F.new16)
#cand.set.16[[6]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation)+rescale(patcharea) + rescale(propCBB) + rescale(tmax.fruit),data=d.F.new16)
#cand.set.16[[7]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation)+rescale(patcharea) + rescale(GapDry) + rescale(tmax.fruit),data=d.F.new16)
#cand.set.16[[8]]<-lm(logdiff~rescale(Shannon.i)*rescale(BA.legume) + rescale(elevation)+rescale(patcharea) + rescale(GapDry),data=d.F.new16)
#cand.set.16[[9]]<-lm(logdiff~rescale(Shannon.i)+rescale(BA.legume) + rescale(elevation)+rescale(patcharea),data=d.F.new16)

##create a vector of names to trace back models in set
Modnames.14 <- paste("mod", 1:length(cand.set.14), sep = " ")
#Modnames <- paste("mod", 1:length(cand.set), sep = " ")
Modnames.15 <- paste("mod", 1:length(cand.set.15), sep = " ")
Modnames.16 <- paste("mod", 1:length(cand.set.16), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set.14, modnames = Modnames.14, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_yld14.delta6.v3.csv"))

#res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
#write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_ylddiff.delta2.csv"))

res.table <-aictab(cand.set = cand.set.15, modnames = Modnames.15, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_logylddiff15.delta3.csv"))

res.table <-aictab(cand.set = cand.set.16, modnames = Modnames.16, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_logylddiff16.delta3.csv"))

#2014
topmodels.avg<-model.avg(cand.set.14) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.txt"))
summary(topmodels.avg)
sink() 

x14<-as.data.frame(summary(topmodels.avg)$importance)
x14$Comparison<-rownames(x14)
colnames(x14)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
#cand.set.14b<-list(cand.set.14[[1]],cand.set.14[[2]],cand.set.14[[3]])
#lose interaction of BA.legume and Shannon index because BA.legume not commone enough across plots
for(i in 1:nrow(x14)){
  if(x14$Comparison[i]=="rescale(BA.legume):rescale(Shannon.i)") vars[[i]]<-data.frame(cbind("rescale(Shannon.i):rescale(BA.legume)",modavg(cand.set.14,"rescale(Shannon.i):rescale(BA.legume)",uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,"rescale(Shannon.i):rescale(BA.legume)",uncond.se = "revised",modnames =  Modnames.14)$Uncond.SE,modavg(cand.set.14,"rescale(Shannon.i):rescale(BA.legume)",uncond.se = "revised",modnames =  Modnames.14)$Lower.CL,modavg(cand.set.14,"rescale(Shannon.i):rescale(BA.legume)",uncond.se = "revised",modnames =  Modnames.14)$Upper.CL),stringsAsFactors = F)
  else if(x14$Comparison[i]=="rescale(BA.legume)"|x14$Comparison[i]=="rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,parm=x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Upper.CL),stringsAsFactors = F)
  #else if(x14$Comparison[i]=="rescale(elevation):rescale(patcharea)") vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Upper.CL),stringsAsFactors = F)
  #else if(x14$Comparison[i]=="rescale(elevation)"|x14$Comparison[i]=="rescale(patcharea)") vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,parm=x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(elevation):rescale(patcharea)"))$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(elevation):rescale(patcharea)"))$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(elevation):rescale(patcharea)"))$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(elevation):rescale(patcharea)"))$Upper.CL),stringsAsFactors = F)
  else vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Upper.CL),stringsAsFactors = F)
}
vars.14<-do.call(rbind.data.frame,vars)
colnames(vars.14)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.14[nrow(vars.14)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Uncond.SE,modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Lower.CL,modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Upper.CL),stringsAsFactors = F)

vars.14[,2:5]<-sapply(vars.14[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp.14<-as.data.frame(t(topmodels.avg[[2]]))
tmp.14$Comparison <- rownames(tmp.14)
vars.14 <- vars.14 %>% mutate(Parameter=replace(Parameter,Parameter=="rescale(Shannon.i):rescale(BA.legume)","rescale(BA.legume):rescale(Shannon.i)"))
tmp.14[,4:7]<-vars.14[match(tmp.14$Comparison,vars.14$Parameter),2:5]

#add importance

tmp.14$Importance<-x14[match(tmp.14$Comparison,x14$Comparison),"Importance"]
write.csv(tmp.14,paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))

#tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))
tmp.14<-tmp.14[!is.na(tmp.14$full),]

#for delta 2
tmp.14$Comparison<-factor(tmp.14$Comparison,levels=tmp.14[order(tmp.14$Importance,decreasing=F),"Comparison"],
                       labels=c("Canopy Gap","Maximum Temperature\nAnomaly (Fruiting)","Patch Area","BA Legume:\nShade Diversity","Shade Diversity","Basal Area of\nLeguminous Trees","Elevation","(Intercept)"))

#order by importance
tmp.14<-tmp.14[!is.na(tmp.14$Importance),]

#add significance column
tmp.14$sig<-1
tmp.14<-tmp.14 %>% mutate(sig=replace(sig,Comparison=="Elevation"|Comparison=="Shade Diversity"|Comparison=="BA Legume:\nShade Diversity"|Comparison=="Patch Area",0))

g1<-ggplot(tmp.14, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield\n(2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_yld14.v3.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_results_yld14.v3.pdf",height=6,width=6)

p1<-g1+coord_flip()

#for 2015
topmodels.avg.15<-model.avg(cand.set.15) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.txt"))
summary(topmodels.avg.15)
sink() 

x15<-as.data.frame(summary(topmodels.avg.15)$importance)
x15$Comparison<-rownames(x15)
colnames(x15)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x15)){
  #  if(x15$Comparison[i]=="rescale(elevation):rescale(patcharea)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Upper.CL),stringsAsFactors = F)
  #  else if(x15$Comparison[i]=="rescale(elevation)"|x15$Comparison[i]=="rescale(patcharea)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,parm=x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Upper.CL),stringsAsFactors = F)
  #  else if(x15$Comparison[i]=="rescale(BA.legume):rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Upper.CL),stringsAsFactors = F)
  #  else if(x15$Comparison[i]=="rescale(BA.legume)"|x15$Comparison[i]=="rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,parm=x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Upper.CL),stringsAsFactors = F)
  #  else 
  vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Upper.CL),stringsAsFactors = F)
}
vars.15<-do.call(rbind.data.frame,vars)
colnames(vars.15)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.15[nrow(vars.15)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set.15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Uncond.SE,modavg(cand.set.15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Lower.CL,modavg(cand.set.15,"(Intercept)",uncond.se = "revised",modnames = Modnames.15)$Upper.CL),stringsAsFactors = F)

vars.15[,2:5]<-sapply(vars.15[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp.15<-as.data.frame(t(topmodels.avg.15[[2]]))
tmp.15$Comparison <- rownames(tmp.15)
tmp.15[,4:7]<-vars.15[match(tmp.15$Comparison,vars.15$Parameter),2:5]

#add importance
tmp.15$Importance<-x15[match(tmp.15$Comparison,x15$Comparison),"Importance"]
write.csv(tmp.15,paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
#tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.confint.csv"))
tmp.15<-tmp.15[!is.na(tmp.15$full),]

#for delta 6
tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
                       labels=c("Coffee Berry\nDisease\nIncidence","Shade Diversity","Basal Area of\nLeguminous Trees","Coffee Land Area","Patch Area","(Intercept)"))

#add in interaction between basal area and shade diversity
#tmp.15<-tmp.15 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#order by importance
tmp.15<-tmp.15[!is.na(tmp.15$Importance),]

#add significance column
tmp.15$sig<-1
tmp.15<-tmp.15 %>% mutate(sig=replace(sig,Comparison=="Patch Area",0))

g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Log Yield Difference\n(2015)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_logylddiff15.v3.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_results_yld15.v3.pdf",height=6,width=6)

p2<-g1+coord_flip()+  xlab("")


#for 2016
topmodels.avg.16<-model.avg(cand.set.16) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta2.v3.txt"))
summary(topmodels.avg.16)
sink() 

x16<-as.data.frame(summary(topmodels.avg.16)$importance)
x16$Comparison<-rownames(x16)
colnames(x16)<-c("Importance","Comparison")

x16 <- x16 %>% mutate(Comparison=replace(Comparison,Comparison=="rescale(BA.legume):rescale(Shannon.i)","rescale(Shannon.i):rescale(BA.legume)"))

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x16)){
  if(x16$Comparison[i]=="rescale(Shannon.i):rescale(BA.legume)") vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Upper.CL),stringsAsFactors = F)
  else if(x16$Comparison[i]=="rescale(BA.legume)"|x16$Comparison[i]=="rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,parm=x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(Shannon.i):rescale(BA.legume)"))$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(Shannon.i):rescale(BA.legume)"))$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(Shannon.i):rescale(BA.legume)"))$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(Shannon.i):rescale(BA.legume)"))$Upper.CL),stringsAsFactors = F)
  else vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Upper.CL),stringsAsFactors = F)
}
vars.16<-do.call(rbind.data.frame,vars)
colnames(vars.16)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.16[nrow(vars.16)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Uncond.SE,modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Lower.CL,modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Upper.CL),stringsAsFactors = F)

vars.16[,2:5]<-sapply(vars.16[,2:5],as.numeric)

#calculate confidence intervals from standard error

tmp.16<-as.data.frame(t(topmodels.avg.16[[2]]))
tmp.16$Comparison <- rownames(tmp.16)

tmp.16<-tmp.16 %>% mutate(Comparison=replace(Comparison,Comparison=="rescale(BA.legume):rescale(Shannon.i)","rescale(Shannon.i):rescale(BA.legume)"))

tmp.16[,4:7]<-vars.16[match(tmp.16$Comparison,vars.16$Parameter),2:5]
#tmp.16$Estimate<-as.numeric(vars.16[,1])
#tmp.16$Uncond.SE<-as.numeric(vars.16[,2])
#tmp.16$Lower.CL<-as.numeric(confint(cand.set.16[[1]])[,1])
#tmp.16$Upper.CL<-as.numeric(confint(cand.set.16[[1]])[,2])


#add importance
tmp.16$Importance<-x16[match(tmp.16$Comparison,x16$Comparison),"Importance"]
#tmp.16$Importance<-1
#tmp.16 <-tmp.16 %>% mutate(Importance=replace(Importance,Comparison=="(Intercept)",NA))
write.csv(tmp.16,paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
#tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.confint.csv"))
#tmp.16<-tmp.16[!is.na(tmp.16$Importance),]

#for delta 6
tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
                          labels=c("Coffee Berry\nBorer Incidence","Canopy Gap","BA Legume:\nShade Diversity","Shade Diversity","Basal Area of\nLeguminous Trees","Elevation","Maximum Temperature\nAnomaly (Fruiting)","Intercept"))

#add in interaction between basal area and shade diversity
#tmp.16<-tmp.16 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#order by importance
tmp.16<-tmp.16[!is.na(tmp.16$Importance),]

#add significance column
tmp.16$sig<-1
tmp.16<-tmp.16 %>% mutate(sig=replace(sig,Comparison=="Maximum Temperature\nAnomaly (Fruiting)"|Comparison=="Elevation"|Comparison=="BA Legume:\nShade Diversity",0))

g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Log Yield Difference\n(2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_logylddiff16.v3.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_results_yld16.v3.pdf",height=6,width=6)

p3<-g1+coord_flip() +  xlab("")

ggpubr::ggarrange(p1,p2,p3,ncol=3,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_combined.v3.pdf"),height=6,width=18)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_results_combined.v3.pdf",height=6,width=18)

#test validity of the model 2014
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))
d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#model yield for 2014
d.F.new.14<-d.F.new %>% filter(year==2014)
d.F.new.14$Shrub.kg.pred<-predict(topmodels.avg)

#check output of this command
d.F.new.14$z.elevation=rescale(d.F.new.14$elevation)
d.F.new.14$z.BA.legume=rescale(d.F.new.14$BA.legume)
d.F.new.14$z.Shannon.i=rescale(d.F.new.14$Shannon.i)
d.F.new.14$z.patcharea=rescale(d.F.new.14$patcharea)
d.F.new.14$z.tmax.anom.fruit=rescale(d.F.new.14$tmax.anom.fruit)
d.F.new.14$z.GapDry=rescale(d.F.new.14$GapDry)

df<-d.F.new.14 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*z.elevation +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume +
           tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i +
           tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*z.patcharea +
           tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*z.GapDry +
           tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*z.tmax.anom.fruit +
           tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*z.BA.legume*z.Shannon.i)

ggplot(df,aes(Shrub.kg.pred,Shrub.kg.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+
  xlab("Predicted Shrub Yield [kg]")+ylab("Modelled Shrub Yield [kg]")+
  ggtitle("2014")+theme_classic() +
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")

ggplot(df,aes(Shrub.kg,Shrub.kg.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+
  xlab("Observed Shrub Yield [kg]")+ylab("Modelled Shrub Yield [kg]")+
  ggtitle("2014")+theme_classic() +
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")

p14.3<-ggplot(df,aes(Shrub.kg,Shrub.kg.pred)) + geom_point() + geom_abline(slope=1,intercept=0,col="red") +
  ylim(0,1)+xlim(0,1)+
  xlab("Observed")+ylab("Predicted")+
  ggtitle("Model Assessment")+theme_bw() +
  theme(text = element_text(size = 12),legend.key = element_blank(),legend.position="bottom")
p14.3
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld2014.norm.pdf"),width=6,height=6)

#residuals vs fitted
p14.1<-ggplot(fm14c, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

#qqplot
p14.2 <- ggplot(fm14c, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm=T) +
  geom_abline(aes(intercept=0,slope=1), col = 'red')+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")+theme_bw()
  
p14.4<-ggpubr::ggarrange(p14.1,p14.2,p14.3,ncol=2,nrow=2,labels="auto")

ggpubr::ggarrange(p1,p14.4,ncol=2,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld2014.allgraphs.pdf"),width=12,height=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Modelled.yld2014.allgraphs.pdf",width=12,height=6)

#p14<-ggpubr::ggarrange(p1,p14.4,ncol=2,nrow=1)

#test validity of the model, 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

d.F.new15<-d.F.new %>% filter(year==2015)

d.F.new15$z.coffee.area.ha=rescale(d.F.new15$coffee.area.ha)
d.F.new15$z.BA.legume=rescale(d.F.new15$BA.legume)
d.F.new15$z.propCBD=rescale(d.F.new15$propCBD)
d.F.new15$z.patcharea=rescale(d.F.new15$patcharea)
d.F.new15$z.Shannon.i=rescale(d.F.new15$Shannon.i)

df15<-d.F.new15 %>% filter(!is.na(logdiff))
df15$logdiff.pred<-predict(topmodels.avg.15)

df15<-df15 %>% group_by(Plot,year) %>% mutate(logdiff.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+
                                                tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume + 
                                                tmp.15[tmp.15$Comparison=="rescale(propCBD)","Estimate"]*z.propCBD + 
                                                tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*z.coffee.area.ha + 
                                                tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*z.patcharea + 
                                                tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i)
 
ggplot(df15,aes(logdiff.pred,logdiff.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-6,1)+xlim(-6,1)+
  ylab("Modelled Log Difference in Yield")+xlab("Predicted Log Difference in Yield")+
  ggtitle("2015")+theme_classic() +
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")

ggplot(df15,aes(logdiff,logdiff.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-6,1)+xlim(-6,1)+
  xlab("Observed Log Difference in Yield")+ylab("Modelled Log Difference in Yield")+
  ggtitle("2015")+theme_classic() +
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")

p15.3<-ggplot(df15,aes(logdiff,logdiff.pred)) + geom_point() + geom_abline(slope=1,intercept=0,col='red') +
  ylim(-6,1)+xlim(-6,1)+
  xlab("Observed")+ylab("Predicted")+
  ggtitle("Model Assessment")+theme_bw() +
  theme(text = element_text(size = 12),legend.key = element_blank(),legend.position="bottom")

p15.3
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.logylddiff.2015.norm.pdf"),width=6,height=6)

#residuals vs fitted
p15.1<-ggplot(dm15c, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

#qqplot
p15.2 <- ggplot(dm15c, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm=T) +
  geom_abline(aes(intercept=0,slope=1), col = 'red')+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")+theme_bw()

p15.4<-ggpubr::ggarrange(p15.1,p15.2,p15.3,ncol=2,nrow=2,labels="auto")

p15<-ggpubr::ggarrange(p2,p15.4,ncol=2,nrow=1)
p15
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld2015.allgraphs.pdf"),width=12,height=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Modelled.yld2015.allgraphs.pdf",width=12,height=6)

#test validity of the model, 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

d.F.new16<-d.F.new %>% filter(year==2016)

#d.F.new16$z.coffee.area.ha=rescale(d.F.new16$coffee.area.ha)
d.F.new16$z.BA.legume=rescale(d.F.new16$BA.legume)
d.F.new16$z.elevation=rescale(d.F.new16$elevation)
d.F.new16$z.Shannon.i=rescale(d.F.new16$Shannon.i)
d.F.new16$z.propCBB=rescale(d.F.new16$propCBB)
d.F.new16$z.GapDry=rescale(d.F.new16$GapDry)
d.F.new16$z.tmax.anom.fruit=rescale(d.F.new16$tmax.anom.fruit)

df16<-d.F.new16 %>% filter(!is.na(logdiff))
df16$logdiff.pred<-predict(topmodels.avg.16)

df16<-df16 %>% group_by(Plot,year) %>% mutate(logdiff.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+
                                                tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i +
                                                tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume +
                                                tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*z.elevation + 
                                                tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*z.BA.legume*z.Shannon.i +
                                                tmp.16[tmp.16$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*z.tmax.anom.fruit +
                                                tmp.16[tmp.16$Comparison=="rescale(propCBB)","Estimate"]*z.propCBB +
                                                tmp.16[tmp.16$Comparison=="rescale(GapDry)","Estimate"]*z.GapDry)
      
ggplot(df16,aes(logdiff.pred,logdiff.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed") +
  #ylim(-3,1)+xlim(-3,1)+
  xlab("Predicted Log Difference in Yield")+ylab("Modelled Log Difference in Yield")+
  ggtitle("2016")+theme_classic() + 
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")

ggplot(df16,aes(logdiff,logdiff.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-4,1)+xlim(-4,1)+
  xlab("Observed Log Difference in Yield")+ylab("Modelled Log Difference in Yield")+
  ggtitle("2016")+theme_classic() +
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")

p16.3<-ggplot(df16,aes(logdiff,logdiff.pred)) + geom_point() + geom_abline(slope=1,intercept=0,col='red') +
  ylim(-4,1)+xlim(-4,1)+
  xlab("Observed")+ylab("Predicted")+
  ggtitle("Model Assessment")+theme_bw() +
  theme(text = element_text(size = 12),legend.key = element_blank(),legend.position="bottom")
p16.3
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.logylddiff.2016.norm.pdf"),width=6,height=6)

#residuals vs fitted
p16.1<-ggplot(dm16c, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

#qqplot
p16.2 <- ggplot(dm16c, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm=T) +
  geom_abline(aes(intercept=0,slope=1), col = 'red')+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")+theme_bw()

p16.4<-ggpubr::ggarrange(p16.1,p16.2,p16.3,ncol=2,nrow=2,labels="auto")

p16<-ggpubr::ggarrange(p3,p16.4,ncol=2,nrow=1)
p16
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld2016.allgraphs.pdf"),width=12,height=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Modelled.yld2016.allgraphs.pdf",width=12,height=6)

#3D figure of patcharea area and elevation
d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
d.F.new.14<-d.F.new %>% filter(year==2014)

elev<-seq(as.integer(min(d.F.new.14$elevation)),as.integer(max(d.F.new.14$elevation)),by=1)
patch<-seq(as.integer(min(d.F.new.14$patcharea)),as.integer(max(d.F.new.14$patcharea)),by=5)
z.elev<-attributes(scale(d.F.new.14$elevation))
z.patch<-attributes(scale(d.F.new.14$patcharea))

tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))

z.14<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) 
  }
}

colnames(z.14)<-patch
z.14$elevation<-elev

z_g.14<-gather(z.14,key="patch",value="yld",-elevation)

g1<-ggplot(z_g.14, aes( as.numeric(patch), elevation, z = yld)) +geom_raster(aes(fill=yld)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Shrub Yield\n(2014)") + theme(text=element_text(size=16))
g1
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld14.elev.vs.patcharea.pdf"),width=8,height=7)

#for 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

z.15<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.15[i,j] <- tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) 
  }
}
colnames(z.15)<-patch
z.15$elevation<-elev

z_g.15<-gather(z.15,key="patch",value="yld_diff",-elevation)

g2<-ggplot(z_g.15, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Log Yield\nDifference") + ggtitle("Shrub Yield Difference\n(2015)") + theme(text=element_text(size=16))
g2
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff15.elev.vs.patcharea.pdf"),width=8,height=7)

#for 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

z.16<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.16[i,j] <- tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) 
  }
}
colnames(z.16)<-patch
z.16$elevation<-elev

z_g.16<-gather(z.16,key="patch",value="yld_diff",-elevation)

g3<-ggplot(z_g.16, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Log Yield\nDifference") + ggtitle("Shrub Yield Difference\n(2016)") + theme(text=element_text(size=16))
g3
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff16.elev.vs.patcharea.pdf"),width=8,height=7)

#yield model for basal area leguminous trees and shade diversity
legume<-seq(as.integer(min(d.F.new15$BA.legume)),as.integer(max(d.F.new15$BA.legume)),by=0.30)
diversity<-seq(as.integer(min(d.F.new15$Shannon.i)),as.integer(max(d.F.new15$Shannon.i)),by=0.04)
z.legume<-attributes(scale(d.F.new15$BA.legume))
z.diversity<-attributes(scale(d.F.new15$Shannon.i))

#do for 2014 yields
s.14<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
      tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.14)<-diversity
s.14$legume<-legume

s_g.14<-gather(s.14,key="diversity",value="yld",-legume)

g4<-ggplot(s_g.14, aes( as.numeric(diversity), legume, z = yld)) +geom_raster(aes(fill=yld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Yield") + ggtitle("") + theme(text=element_text(size=16))

#do for 2015 yield difference
s.15<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.15[i,j] <- tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) #+
    #      tmp.15[tmp.15$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.15)<-diversity
s.15$legume<-legume

s_g.15<-gather(s.15,key="diversity",value="yld_diff",-legume)

g5<-ggplot(s_g.15, aes( as.numeric(diversity), legume, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Log Yield\nDifference") + ggtitle("") + theme(text=element_text(size=16))

#do for 2016 yield difference
s.16<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.16[i,j] <- tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
      tmp.16[tmp.16$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.16)<-diversity
s.16$legume<-legume

s_g.16<-gather(s.16,key="diversity",value="yld_diff",-legume)

g6<-ggplot(s_g.16, aes( as.numeric(diversity), legume, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Log Yield\nDifference") + ggtitle("") + theme(text=element_text(size=16))

#yields
c1<-ggpubr::ggarrange(g1,g4,ncol=1,nrow=2,common.legend = T,legend="right",font.label = list(size = 18),heights=c(1.1,1))
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.yield.elev.patcharea.pdf",height=7,width=4)

#yield differences
c2<-ggpubr::ggarrange(g2,g3,g5,g6,ncol=2,nrow=2,common.legend = T,legend="right",font.label = list(size = 18),heights=c(1.1,1),widths=c(1.05,1))

ggpubr::ggarrange(c1,c2,nrow=1,ncol=2,align="hv",widths=c(1.05,2))

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Model.yield.diffyld.elev.patcharea.pdf",height=7,width=12)

#TerraClim Figures
terra_clim<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies.csv"))

#anomalies around year of study, with harvesting dates
harvest<-tibble(c("2014-10-01","2015-10-01","2016-10-01"))
colnames(harvest)<-"harvest.date"
g1<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,precip_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Precipitation\nAnomaly [mm]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed") +
  theme(text=element_text(size=16)) + annotate("text",x=as.Date("2016-04-01"),y=200,label="Dry Year",size=8,fontface =2) +
  annotate("text",x=as.Date("2014-04-01"),y=200,label="Normal Year",size=8,fontface =2) +
  annotate("text",x=as.Date("2015-04-01"),y=200,label="Hot Year",size=8,fontface =2) 
  #geom_segment(aes(x = as.Date("2016-01-01"), y = 70, xend = as.Date("2016-10-01"), yend = 70), size=0.5,arrow = arrow(length = unit(0.5, "cm")))

g3<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,tmax_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Maximum Temperature\nAnomaly [C]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed")  +
  theme(text=element_text(size=16)) #+ annotate("text",x=as.Date("2015-01-01"),y=2.5,label="Hot Year",size=8) +
 # geom_segment(aes(x = as.Date("2015-01-01"), y = 2.15, xend = as.Date("2015-10-01"), yend = 2.15), size=0.5,arrow = arrow(length = unit(0.5, "cm")))

ggpubr::ggarrange(g1,g3,ncol=1,nrow=2,align="hv",heights=c(1.25,1),labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/TerraClim.Anom.Comparison.pdf",height=8,width=12)

#open ONI values
oni<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ONI.csv")
#add "month" value
oni <- oni %>% mutate(Date=as.Date(paste0(PeriodNum,"-01"),format="%Y-%m-%d")) %>% rename(oni=NOAA)

terra_clim<-left_join(terra_clim,oni %>% select(Date,oni),by="Date")

#assess the frequency of hot or dry years (plot standardised anomalies)
z1<-ggplot(terra_clim %>% filter(site=="B13"&year>=1986),aes(Date,tmax_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Maximum Temperature Anomalies") +
  stat_smooth(color="black") + theme(text=element_text(size=16),legend.title.align=0.5 ) + scale_fill_gradient2( low = "blue", mid = "white",
                                                                           high = "red", midpoint = 0, space = "Lab",
                                                                           guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") 
  
 
z3<-ggplot(terra_clim %>% filter(site=="B13"&year>=1986),aes(Date,precip_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Precipitation Anomalies") +
  stat_smooth(color="black") + theme(text=element_text(size=16),legend.title.align=0.5) + scale_fill_gradient2( low = "blue", mid = "white",
                                                                                                   high = "red", midpoint = 0, space = "Lab",
                                                                                                   guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") 

z2<-ggplot(terra_clim %>% filter(site=="B13"&year>=1986),aes(Date,vpd_anom_sigma_3mo)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Vapour Pressure Deficit Anomalies") +
  stat_smooth() + theme(text=element_text(size=16))

ggpubr::ggarrange(z3,z1,ncol=1,nrow=2,align="hv",labels="auto",common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/TerraClim.StdAnom.Comparison.pdf",height=9,width=12)
