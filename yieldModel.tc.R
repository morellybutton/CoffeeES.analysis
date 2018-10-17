#Analysis of coffee ES contributions to yield with TerraClim data

library(tidyverse)
library(AICcmodavg)

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
low.yield15 <- d.F %>% filter(year==2015&Shrub.kg<quart15[3]) %>% select(Plot,Shrub.id)
low.yield15 <- low.yield15 %>% mutate(low.yield15=1)
low.yield16 <- d.F %>% filter(year==2016&Shrub.kg<quart16[3]) %>% select(Plot,Shrub.id)
low.yield16 <- low.yield16 %>% mutate(low.yield16=1)

d.F <- left_join(d.F,low.yield14,by=c("Plot","Shrub.id"))
d.F <- d.F %>% mutate(low.yield14=replace(low.yield14,is.na(low.yield14),0))
d.F <- left_join(d.F,low.yield15,by=c("Plot","Shrub.id"))
d.F <- d.F %>% mutate(low.yield15=replace(low.yield15,is.na(low.yield15),0))
d.F <- left_join(d.F,low.yield16,by=c("Plot","Shrub.id"))
d.F <- d.F %>% mutate(low.yield16=replace(low.yield16,is.na(low.yield16),0))
d.F <- d.F %>% group_by(Plot,Shrub.id,year) %>% mutate(low.yield=sum(low.yield14,low.yield15,low.yield16,na.rm=T)/3)
  
d.F.plot <- d.F %>% group_by(Plot,year) %>% summarise(Shrub.kg=median(Shrub.kg,na.rm=T),Tot.fruits=median(Tot.fruits,na.rm=T),fruitset=median(fruitset,na.rm=T),
                                                      propCBB=median(propCBB,na.rm=T),propCBD=median(propCBD,na.rm=T),fruit.drop=median(fruit.drop,na.rm=T),prop.fdrop=median(prop.fdrop,na.rm=T),
                                                      Tot.leaves=median(Tot.leaves,na.rm=T),leaf.drop=median(leaf.drop,na.rm=T),prop.ldrop=median(prop.ldrop,na.rm=T),
                                                      propLM=median(propLM, na.rm=T), propCLR=median(propCLR,na.rm=T), propWilt=median(propWilt,na.rm=T),
                                                      propHerb=median(propHerb,na.rm=T),low.yield=mean(low.yield,na.rm=T),low.yield14=mean(low.yield14,na.rm=T),low.yield15=mean(low.yield15,na.rm=T),
                                                      low.yield16=mean(low.yield16,na.rm=T),prop.legume=mean(BA.legume/BA.all,na.rm=T)) %>% mutate(prop.legume=replace(prop.legume,is.na(prop.legume),0)) %>%
  mutate(low.yield14.bin=1,low.yield15.bin=1,low.yield16.bin=1,low.yield.bin=1) %>% mutate(low.yield14.bin=replace(low.yield14.bin,low.yield14<0.5,0),
                                                                                           low.yield15.bin=replace(low.yield15.bin,low.yield15<0.5,0),
                                                                                           low.yield16.bin=replace(low.yield16.bin,low.yield16<0.5,0),
                                                                                           low.yield.bin=replace(low.yield.bin,low.yield<0.5,0))

d.F.plot$ID<-1:nrow(d.F.plot)
d.F.plot <- left_join(d.F.plot,d.F %>% select(-ID,-Shrub.kg,-Tot.fruits,-fruitset,-propCBB,-propCBD,-fruit.drop,-Tot.leaves,-leaf.drop,-prop.ldrop,-prop.fdrop,-propLM,-propCLR,-propWilt,-propHerb,-Shrub.kg.1,-low.yield,-low.yield14,-low.yield15,-low.yield16),by=c("Plot","year"))
d.F.plot <- distinct(d.F.plot,ID,.keep_all=T)

#model difference between 2014 and 2016
y.ld14<-d.F.plot %>% filter(year=="2014") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.14=Shrub.kg)
y.ld16<-d.F.plot %>% filter(year=="2016") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.16=Shrub.kg)
y.ld15<-d.F.plot %>% filter(year=="2015") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.15=Shrub.kg)

combo<-left_join(y.ld14,y.ld15,by="Plot")
combo<-left_join(combo,y.ld16,by="Plot")
combo <- combo %>% mutate(diff.yld1415 = Shrub.kg.15-Shrub.kg.14,diff.yld1416 = Shrub.kg.16-Shrub.kg.14,
                          pdiff.yld1415=(Shrub.kg.15-Shrub.kg.14)/Shrub.kg.14,pdiff.yld1416=(Shrub.kg.16-Shrub.kg.14)/Shrub.kg.14) 

d.F.plot <- left_join(d.F.plot,combo %>% select(Plot,diff.yld1415,diff.yld1416,pdiff.yld1415,pdiff.yld1416),by="Plot")
d.F.plot.14 <- d.F.plot %>% filter(year==2014)
d.F.plot.1516 <- d.F.plot %>% filter(year!=2014)
d.F.plot.16 <- d.F.plot %>% filter(year==2016)
d.F.plot.15 <- d.F.plot %>% filter(year==2015)

d.F.plot.15<-d.F.plot.15 %>% rename(diff.yld=diff.yld1415,pdiff.yld=pdiff.yld1415)
d.F.plot.16<-d.F.plot.16 %>% rename(diff.yld=diff.yld1416,pdiff.yld=pdiff.yld1416)
diff.yld<-bind_rows(d.F.plot.15 %>% select(Plot,diff.yld,pdiff.yld),d.F.plot.16 %>% select(Plot,diff.yld,pdiff.yld))
d.F.plot.1516<-left_join(d.F.plot.1516,diff.yld,by="Plot")
d.F.plot.1516 <- distinct(d.F.plot.1516,ID,.keep_all=T)

d.F.plot.14$diff.yld<-NA
d.F.new <- bind_rows(d.F.plot.14 %>% select(-diff.yld1415,-diff.yld1416,-pdiff.yld1415,-pdiff.yld1416),d.F.plot.1516 %>% select(-diff.yld1415,-diff.yld1416,-pdiff.yld1415,-pdiff.yld1416))
#remove H6
d.F.new <- d.F.new %>% filter(Plot!="H6")
write.csv(d.F.new,paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

library(car)
library(MuMIn)
library(arm)
library(lattice)
library(lme4)
library(plotly)

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#do difference in yields for all years together
options(na.action = "na.omit")

#model yield for 2014
d.F.new.14<-d.F.new %>% filter(year==2014)

pdf(paste0(getwd(),"/Analysis/ES/Plot.yld14_norm.pdf"),width=8,height=8)
qqp(d.F.new.14$Shrub.kg, "norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.ylddiff_norm.pdf"),width=8,height=8)
qqp(d.F.new$diff.yld, "lnorm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2015ylddiff_norm.pdf"),width=8,height=8)
qqp(d.F.new$diff.yld[d.F.new$year=="2015"], "norm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2016ylddiff_norm.pdf"),width=8,height=8)
qqp(d.F.new$diff.yld[d.F.new$year=="2016"], "norm")
dev.off()

#2014
fm<-lm(Shrub.kg~rescale(labour) +  rescale(GapDry)+rescale(Shannon.i)*rescale(BA.legume) + rescale(Tot.P.ppm) + rescale(CN.ratio) +
           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) +
           rescale(tmax.anom.fruit)+rescale(elevation) + rescale(patcharea),data=d.F.new.14)
summary(fm)

fm14<-lm(Shrub.kg~rescale(GapDry)+rescale(Shannon.i)*rescale(BA.legume) + rescale(CN.ratio) +
           rescale(propCLR) + rescale(propCBB) +
           rescale(tmax.anom.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new.14)
summary(fm14)

#2015&2016
#d.F.new1<-d.F.new %>% filter(!is.na(diff.yld))
#dm<-lmer(diff.yld~rescale(Shannon.i) + rescale(BA.legume) + rescale(GapDry) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(fruitset) +
#           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) + rescale(low.yield) +
#           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea) +
#           (1|year),data=d.F.new1, REML=FALSE)

#summary(dm)

#2015
d.F.new15 <- d.F.new %>% filter(year==2015&!is.na(diff.yld))
dm15<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) + rescale(GapDry) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(fruitset) +
           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) + rescale(low.yield.bin) +
           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)

dm15<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) + rescale(Tot.P.ppm) + rescale(CN.ratio) +
         rescale(coffee.area.ha) + rescale(low.yield.bin) +
           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)

summary(dm15)

#2016
d.F.new16 <- d.F.new %>% filter(year==2016&!is.na(diff.yld))
dm16<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) + rescale(GapDry) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(fruitset) +
           rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + rescale(coffee.area.ha) + rescale(low.yield.bin) +
           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)
dm16<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(fruitset) +
          rescale(coffee.area.ha) + rescale(low.yield.bin) +
           rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)

summary(dm16)

#check heteroskedasticity
diagnos14 <- data.frame(Resid = resid(fm14, type = "pearson"), Fitted = fitted(fm14),Variable = d.F.new.14$Plot[!is.na(d.F.new.14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yld14_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos14)
dev.off()

#diagnos <- data.frame(Resid = resid(dm, type = "pearson"), Fitted = fitted(dm),Variable = d.F.new1$Plot )
#pdf(paste0(getwd(),"/Analysis/ES/Plot.ylddiff_ResidualvFittedValues_all.pdf"),width=8,height=8)
#xyplot(Resid ~ Fitted, data = diagnos)
#dev.off()

diagnos15 <- data.frame(Resid = resid(dm15, type = "pearson"), Fitted = fitted(dm15),Variable = d.F.new15$Plot)
pdf(paste0(getwd(),"/Analysis/ES/Plot.2015ylddiff_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos15)
dev.off()

diagnos16 <- data.frame(Resid = resid(dm16, type = "pearson"), Fitted = fitted(dm16),Variable = d.F.new16$Plot)
pdf(paste0(getwd(),"/Analysis/ES/Plot.2016ylddiff_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos16)
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yld14_qqplotResiduals_all.pdf"),width=8,height=8)
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

pdf(paste0(getwd(),"/Analysis/ES/Plot.2015ylddiff_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos15, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.2016ylddiff_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos16, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

options(na.action = "na.fail")
#2014
fm14.d<-dredge(fm14)

#picked delta of 2 because delta of 6 was >24 models
dredg.m14<-subset(fm14.d,delta<6)
write.csv(dredg.m14,paste0(getwd(),"/Analysis/ES/Yld.2014_dredged01.csv"))

#2015&2016
#dm.d<-dredge(dm)

#picked delta 6 was >200 models
#dredg.m01<-subset(dm.d,delta<2)
#write.csv(dredg.m01,paste0(getwd(),"/Analysis/ES/Yld.diff_dredged01.csv"))

#2015
dm15.d<-dredge(dm15)

#picked delta of  6 was 19 models
dredg.m15<-subset(dm15.d,delta<6)
write.csv(dredg.m15,paste0(getwd(),"/Analysis/ES/Yld.diff_dredged15.csv"))

#2016
dm16.d<-dredge(dm16)

#picked delta 6 with 21 models
dredg.m16<-subset(dm16.d,delta<6)
write.csv(dredg.m16,paste0(getwd(),"/Analysis/ES/Yld.diff_dredged16.csv"))

#for 2014 yield
cand.set.14<-list()
#delta 6 has 24 models reduced to 8
cand.set.14[[1]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(elevation)+rescale(patcharea),data=d.F.new.14)
cand.set.14[[2]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(tmax.anom.fruit)+rescale(elevation),data=d.F.new.14)
cand.set.14[[3]]<-lm(Shrub.kg~rescale(Shannon.i)*rescale(BA.legume) +
                      rescale(elevation),data=d.F.new.14)
cand.set.14[[4]]<-lm(Shrub.kg~rescale(GapDry)+rescale(Shannon.i) +
                       rescale(tmax.anom.fruit)+rescale(elevation),data=d.F.new.14)
cand.set.14[[5]]<-lm(Shrub.kg~rescale(Shannon.i) + rescale(CN.ratio) +
                       rescale(tmax.anom.fruit)+rescale(elevation),data=d.F.new.14)
cand.set.14[[6]]<-lm(Shrub.kg~rescale(Shannon.i) +
                       rescale(tmax.anom.fruit)+rescale(elevation),data=d.F.new.14)
cand.set.14[[7]]<-lm(Shrub.kg~rescale(Shannon.i)+rescale(BA.legume) +
                       rescale(tmax.anom.fruit)+rescale(elevation),data=d.F.new.14)
cand.set.14[[8]]<-lm(Shrub.kg~rescale(Shannon.i)+rescale(BA.legume) +
                       rescale(elevation)+rescale(patcharea),data=d.F.new.14)


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
#delta 6 has 26 models reduced to 7
cand.set.15[[1]]<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)
cand.set.15[[2]]<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(coffee.area.ha) + rescale(elevation)*rescale(patcharea),data=d.F.new15)
cand.set.15[[3]]<-lm(diff.yld~rescale(Shannon.i) + rescale(coffee.area.ha) + rescale(elevation)*rescale(patcharea),data=d.F.new15)
cand.set.15[[4]]<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new15)
cand.set.15[[5]]<-lm(diff.yld~rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new15)
cand.set.15[[6]]<-lm(diff.yld~rescale(Shannon.i) + rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new15)
cand.set.15[[7]]<-lm(diff.yld~rescale(coffee.area.ha) + rescale(elevation)*rescale(patcharea),data=d.F.new15)

#add in model for non-interaction involving BA.legume
cand.set.15[[8]]<-lm(diff.yld~rescale(Shannon.i)+rescale(BA.legume) +
                       rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new15)

#for 2016 yield difference
cand.set.16<-list()
#delta 6 has 23 models reduced to 6
cand.set.16[[1]]<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)
cand.set.16[[2]]<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(coffee.area.ha) + rescale(elevation)*rescale(patcharea),data=d.F.new16)
cand.set.16[[3]]<-lm(diff.yld~rescale(coffee.area.ha) +
                       rescale(low.yield.bin)+rescale(fruitset)+rescale(patcharea),data=d.F.new16)
cand.set.16[[4]]<-lm(diff.yld~rescale(Shannon.i) +
                       rescale(coffee.area.ha) + rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)
cand.set.16[[5]]<-lm(diff.yld~rescale(Shannon.i) +
                       rescale(coffee.area.ha) + rescale(elevation)*rescale(patcharea),data=d.F.new16)
cand.set.16[[6]]<-lm(diff.yld~rescale(coffee.area.ha) +
                      rescale(fruitset)+rescale(patcharea),data=d.F.new16)
#add 1st model without interaction effect between elevation and patcharea and Shannon index and BA legume
cand.set.16[[7]]<-lm(diff.yld~rescale(Shannon.i)*rescale(BA.legume) +
                       rescale(coffee.area.ha) + 
                       rescale(tmax.fruit)+rescale(elevation)+rescale(patcharea),data=d.F.new16)
cand.set.16[[8]]<-lm(diff.yld~rescale(Shannon.i)+rescale(BA.legume) +
                       rescale(coffee.area.ha) + 
                       rescale(tmax.fruit)+rescale(elevation)*rescale(patcharea),data=d.F.new16)

##create a vector of names to trace back models in set
Modnames.14 <- paste("mod", 1:length(cand.set.14), sep = " ")
#Modnames <- paste("mod", 1:length(cand.set), sep = " ")
Modnames.15 <- paste("mod", 1:length(cand.set.15), sep = " ")
Modnames.16 <- paste("mod", 1:length(cand.set.16), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set.14, modnames = Modnames.14, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_yld14.delta6.csv"))

#res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
#write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_ylddiff.delta2.csv"))

res.table <-aictab(cand.set = cand.set.15, modnames = Modnames.15, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_ylddiff15.delta6.csv"))

res.table <-aictab(cand.set = cand.set.16, modnames = Modnames.16, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_ylddiff16.delta6.csv"))

#2014
topmodels.avg<-model.avg(cand.set.14) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.txt"))
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
  if(x14$Comparison[i]=="rescale(BA.legume):rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Upper.CL),stringsAsFactors = F)
  else if(x14$Comparison[i]=="rescale(BA.legume)"|x14$Comparison[i]=="rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,parm=x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Upper.CL),stringsAsFactors = F)
  else vars[[i]]<-data.frame(cbind(x14$Comparison[i],modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Uncond.SE,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Lower.CL,modavg(cand.set.14,x14$Comparison[i],uncond.se = "revised",modnames =  Modnames.14)$Upper.CL),stringsAsFactors = F)
}
vars.14<-do.call(rbind.data.frame,vars)
colnames(vars.14)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.14[nrow(vars.14)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Mod.avg.beta,modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Uncond.SE,modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Lower.CL,modavg(cand.set.14,"(Intercept)",uncond.se = "revised",modnames = Modnames.14)$Upper.CL),stringsAsFactors = F)

vars.14[,2:5]<-sapply(vars.14[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp.14<-as.data.frame(t(topmodels.avg[[2]]))
tmp.14$Comparison <- rownames(tmp.14)
tmp.14[,4:7]<-vars.14[match(tmp.14$Comparison,vars.14$Parameter),2:5]

#add importance
tmp.14$Importance<-x14[match(tmp.14$Comparison,x14$Comparison),"Importance"]
write.csv(tmp.14,paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))

#tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))
tmp.14<-tmp.14[!is.na(tmp.14$full),]

#for delta 6
tmp.14$Comparison<-factor(tmp.14$Comparison,levels=tmp.14[order(tmp.14$Importance,decreasing=F),"Comparison"],
                       labels=c("Soil C:N","CanopyGap","Maximum Temperature\nAnomaly (Fruiting)","Patch Area","BA Legume:\nShade Diversity","Basal Area of\nLeguminous Trees","Shade Diversity","Elevation","(Intercept)"))

#add in interaction between basal area and shade diversity
tmp.14<-tmp.14 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#order by importance
tmp.14<-tmp.14[!is.na(tmp.14$Importance),]

g1<-ggplot(tmp.14, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  geom_point(data=tmp.14 %>% filter(Comparison=="BA Legume:\nShade Diversity"),aes(Comparison, Estimate), color="red") +
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Yield\nPer Shrub (2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 14)
        ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_yld14.pdf"),height=6,width=6)


#for 2015
topmodels.avg.15<-model.avg(cand.set.15) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta6.txt"))
summary(topmodels.avg.15)
sink() 

x15<-as.data.frame(summary(topmodels.avg.15)$importance)
x15$Comparison<-rownames(x15)
colnames(x15)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x15)){
  if(x15$Comparison[i]=="rescale(elevation):rescale(patcharea)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Upper.CL),stringsAsFactors = F)
  else if(x15$Comparison[i]=="rescale(elevation)"|x15$Comparison[i]=="rescale(patcharea)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,parm=x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15,exclude=list("rescale(elevation):rescale(patcharea)"))$Upper.CL),stringsAsFactors = F)
  else if(x15$Comparison[i]=="rescale(BA.legume):rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15)$Upper.CL),stringsAsFactors = F)
  else if(x15$Comparison[i]=="rescale(BA.legume)"|x15$Comparison[i]=="rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,parm=x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames =  Modnames.15,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Upper.CL),stringsAsFactors = F)
  else vars[[i]]<-data.frame(cbind(x15$Comparison[i],modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Mod.avg.beta,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Uncond.SE,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Lower.CL,modavg(cand.set.15,x15$Comparison[i],uncond.se = "revised",modnames = Modnames.15)$Upper.CL),stringsAsFactors = F)
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
write.csv(tmp.15,paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta6.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
#tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta6.confint.csv"))
tmp.15<-tmp.15[!is.na(tmp.15$full),]

#for delta 6
tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
                       labels=c("Maximum\nTemperature (Fruiting)","BA Legume:\nShade Diversity","Basal Area of\nLeguminous Trees","Elevation:Patch Area","Shade Diversity","Coffee Land Area","Elevation","Patch Area","(Intercept)"))

#add in interaction between basal area and shade diversity
tmp.15<-tmp.15 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#order by importance
tmp.15<-tmp.15[!is.na(tmp.15$Importance),]

g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  geom_point(data=tmp.15 %>% filter(Comparison=="BA Legume:\nShade Diversity"),aes(Comparison, Estimate), color="red") +
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Yield\nDifference Per Shrub (2015)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 14)
        ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_ylddiff15.pdf"),height=6,width=6)

#for 2016
topmodels.avg.16<-model.avg(cand.set.16) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta6.txt"))
summary(topmodels.avg.16)
sink() 

x16<-as.data.frame(summary(topmodels.avg.16)$importance)
x16$Comparison<-rownames(x16)
colnames(x16)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x16)){
  if(x16$Comparison[i]=="rescale(elevation):rescale(patcharea)") vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Upper.CL),stringsAsFactors = F)
  else if(x16$Comparison[i]=="rescale(elevation)"|x16$Comparison[i]=="rescale(patcharea)") vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,parm=x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16,exclude=list("rescale(elevation):rescale(patcharea)"))$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16,exclude=list("rescale(elevation):rescale(patcharea)"))$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16,exclude=list("rescale(elevation):rescale(patcharea)"))$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16,exclude=list("rescale(elevation):rescale(patcharea)"))$Upper.CL),stringsAsFactors = F)
  else if(x16$Comparison[i]=="rescale(BA.legume):rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16)$Upper.CL),stringsAsFactors = F)
  else if(x16$Comparison[i]=="rescale(BA.legume)"|x16$Comparison[i]=="rescale(Shannon.i)") vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,parm=x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames =  Modnames.16,exclude=list("rescale(BA.legume):rescale(Shannon.i)"))$Upper.CL),stringsAsFactors = F)
  else vars[[i]]<-data.frame(cbind(x16$Comparison[i],modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Uncond.SE,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Lower.CL,modavg(cand.set.16,x16$Comparison[i],uncond.se = "revised",modnames = Modnames.16)$Upper.CL),stringsAsFactors = F)
}
vars.16<-do.call(rbind.data.frame,vars)
colnames(vars.16)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.16[nrow(vars.16)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Mod.avg.beta,modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Uncond.SE,modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Lower.CL,modavg(cand.set.16,"(Intercept)",uncond.se = "revised",modnames = Modnames.16)$Upper.CL),stringsAsFactors = F)

vars.16[,2:5]<-sapply(vars.16[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp.16<-as.data.frame(t(topmodels.avg.16[[2]]))
tmp.16$Comparison <- rownames(tmp.16)
tmp.16[,4:7]<-vars.16[match(tmp.16$Comparison,vars.16$Parameter),2:5]

#add importance
tmp.16$Importance<-x16[match(tmp.16$Comparison,x16$Comparison),"Importance"]
write.csv(tmp.16,paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta6.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
#tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta2.confint.csv"))
tmp.16<-tmp.16[!is.na(tmp.16$full),]

#for delta 6
tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
                          labels=c("Low Yielding Plot","Fruitset","BA Legume:\nShade Diversity","Maximum\nTemperature (Fruiting)","Basal Area of\nLeguminous Trees","Elevation:Patch Area","Shade Diversity","Elevation","Coffee Land Area","Patch Area","(Intercept)"))

#add in interaction between basal area and shade diversity
tmp.16<-tmp.16 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#order by importance
tmp.16<-tmp.16[!is.na(tmp.16$Importance),]

g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  geom_point(data=tmp.16 %>% filter(Comparison=="BA Legume:\nShade Diversity"),aes(Comparison, Estimate), color="red") +
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Yield\nDifference Per Shrub (2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 14)
        ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_ylddiff16.pdf"),height=6,width=6)

#test validity of the model 2014
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))

d.F.new.14$z.GapDry=rescale(d.F.new.14$GapDry)
d.F.new.14$z.Shannon.i=rescale(d.F.new.14$Shannon.i)
d.F.new.14$z.BA.legume=rescale(d.F.new.14$BA.legume)
d.F.new.14$z.CN.ratio=rescale(d.F.new.14$CN.ratio)
d.F.new.14$z.elevation=rescale(d.F.new.14$elevation)
d.F.new.14$z.patcharea=rescale(d.F.new.14$patcharea)
d.F.new.14$z.tmax.anom.fruit=rescale(d.F.new.14$tmax.anom.fruit)

df<-d.F.new.14 %>% group_by(Plot) %>% 
  mutate(Shrub.kg.mod=tmp.14[tmp.14$Comparison=="(Intercept)","Estimate"]+tmp.14[tmp.14$Comparison=="rescale(GapDry)","Estimate"]*z.GapDry+tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i+tmp.14[tmp.14$Comparison=="rescale(CN.ratio)","Estimate"]*z.CN.ratio+
        tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*z.elevation + tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*z.patcharea + tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume +
         tmp.14[tmp.14$Comparison=="rescale(tmax.anom.fruit)","Estimate"]*z.tmax.anom.fruit + tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*z.BA.legume*z.Shannon.i,
         low.yield1=1) %>% mutate(low.yield1=replace(low.yield1,low.yield<0.5,0))

ggplot(df,aes(Shrub.kg,Shrub.kg.mod)) + geom_point(aes(color=factor(wereda))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,0.75)+xlim(0,0.75)+
  xlab("Observed Shrub Yield [kg]")+ylab("Modelled Shrub Yield [kg]")+
  ggtitle("2014")+theme_classic() + labs(color="Wereda")+
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld2014.norm.pdf"),width=6,height=6)

#test validity of the model, 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta6.confint.csv"))

d.F.new15$z.coffee.area.ha=rescale(d.F.new15$coffee.area.ha)
d.F.new15$z.BA.legume=rescale(d.F.new15$BA.legume)
#d.F.new15$z.low.yield=rescale(d.F.new15$low.yield)
d.F.new15$z.elevation=rescale(d.F.new15$elevation)
d.F.new15$z.patcharea=rescale(d.F.new15$patcharea)
d.F.new15$z.Shannon.i=rescale(d.F.new15$Shannon.i)
d.F.new15$z.tmax.fruit=rescale(d.F.new15$tmax.fruit)

df.15<-d.F.new15 %>% group_by(Plot,year) %>% mutate(diff.yld.mod=tmp.15[tmp.15$Comparison=="(Intercept)","Estimate"]+tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i +
                                                      tmp.15[tmp.15$Comparison=="rescale(elevation)","Estimate"]*z.elevation + tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*z.patcharea + 
                                                      tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume + tmp.15[tmp.15$Comparison=="rescale(tmax.fruit)","Estimate"]*z.tmax.fruit + 
                                                      tmp.15[tmp.15$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*z.BA.legume*z.Shannon.i + tmp.15[tmp.15$Comparison=="rescale(coffee.area.ha)","Estimate"]*z.coffee.area.ha + 
                                                      tmp.15[tmp.15$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*z.elevation*z.patcharea)


ggplot(df.15,aes(diff.yld,diff.yld.mod)) + geom_point(aes(color=factor(low.yield.bin))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-0.6,0.5)+xlim(-0.6,0.5)+
  xlab("Observed Difference in Yield [kg]")+ylab("Modelled Difference in Yield [kg]")+
  ggtitle("2015")+theme_classic() + labs(shape="Year",color="Low Yielding Plot")+
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff.2015.norm.pdf"),width=6,height=6)

#test validity of the model, 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta6.confint.csv"))

d.F.new16$z.coffee.area.ha=rescale(d.F.new16$coffee.area.ha)
d.F.new16$z.BA.legume=rescale(d.F.new16$BA.legume)
d.F.new16$z.low.yield.bin=rescale(d.F.new16$low.yield.bin)
d.F.new16$z.elevation=rescale(d.F.new16$elevation)
d.F.new16$z.patcharea=rescale(d.F.new16$patcharea)
d.F.new16$z.Shannon.i=rescale(d.F.new16$Shannon.i)
d.F.new16$z.tmax.fruit=rescale(d.F.new16$tmax.fruit)
d.F.new16$z.fruitset=rescale(d.F.new16$fruitset)

df.16<-d.F.new16 %>% group_by(Plot,year) %>% mutate(diff.yld.mod=tmp.16[tmp.16$Comparison=="(Intercept)","Estimate"]+tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i + tmp.16[tmp.16$Comparison=="rescale(fruitset)","Estimate"]*z.fruitset + 
                                                      tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*z.elevation + tmp.16[tmp.16$Comparison=="rescale(patcharea)","Estimate"]*z.patcharea + 
                                                      tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume + tmp.16[tmp.16$Comparison=="rescale(tmax.fruit)","Estimate"]*z.tmax.fruit + tmp.16[tmp.16$Comparison=="rescale(low.yield.bin)","Estimate"]*z.low.yield.bin + 
                                                      tmp.16[tmp.16$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*z.BA.legume*z.Shannon.i + tmp.16[tmp.16$Comparison=="rescale(coffee.area.ha)","Estimate"]*z.coffee.area.ha + 
                                                      tmp.16[tmp.16$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*z.elevation*z.patcharea)

ggplot(df.16,aes(diff.yld,diff.yld.mod)) + geom_point(aes(color=factor(low.yield.bin))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-0.6,0.5)+xlim(-0.6,0.5)+
  xlab("Observed Difference in Yield [kg]")+ylab("Modelled Difference in Yield [kg]")+
  ggtitle("2016")+theme_classic() + labs(shape="Year",color="Low Yielding Plot")+
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff.2016.norm.pdf"),width=6,height=6)

#comparison of difference in Tmax (fruiting), elevation and difference in yield - deadend
d.F.tmax <- d.F.new %>% filter(year==2014) %>% rename(tmax.flower.14=tmax.flower,tmax.fruit.14=tmax.fruit) %>% select(Plot,tmax.flower.14,tmax.fruit.14)

d.F.new<-left_join(d.F.new,d.F.tmax,by="Plot")
d.F.new <- d.F.new %>% group_by(Plot,year) %>% mutate(tmax.flower.diff=tmax.flower-tmax.flower.14,tmax.fruit.diff=tmax.fruit-tmax.fruit.14,low.yield1=1) %>%
  mutate(low.yield1=replace(low.yield1,low.yield<0.5,0))

ggplot(d.F.new,aes(elevation,tmax.fruit,group=year)) + geom_point() + facet_wrap(~year,ncol=3) + 
  stat_smooth(method="lm") + theme_classic() + xlab("Elevation [m]") + ylab("Maximum Temperature (Fruiting) [C]")
ggsave(paste0(getwd(),"/Analysis/ElNino/MaxTemp.Fruit.Elevation.pdf"),width=12,height=6)

#3D figure of patcharea area and elevation
d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
d.F.new.14<-d.F.new %>% filter(year==2014)

elev<-seq(as.integer(min(d.F.new.14$elevation)),as.integer(max(d.F.new.14$elevation)),by=1)
patch<-seq(as.integer(min(d.F.new.14$patcharea)),as.integer(max(d.F.new.14$patcharea)),by=5)
z.elev<-attributes(scale(d.F.new.14$elevation))
z.patch<-attributes(scale(d.F.new.14$patcharea))
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))

z.14<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) 
  }
}

colnames(z.14)<-patch
z.14$elevation<-elev

z_g.14<-gather(z.14,key="patch",value="yld",-elevation)

ggplot(z_g.14, aes( as.numeric(patch), elevation, z = yld)) +geom_raster(aes(fill=yld)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Shrub Yield") + ggtitle("Interaction of Elevation and Patch Area for 2014") + theme(text=element_text(size=14))
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld14.elev.vs.patcharea.pdf"),width=8,height=7)

#for 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta6.confint.csv"))
z.15<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.15[i,j] <- tmp.15[tmp.15$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) +
      tmp.15[tmp.15$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.15)<-patch
z.15$elevation<-elev

z_g.15<-gather(z.15,key="patch",value="yld_diff",-elevation)

ggplot(z_g.15, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Shrub Yield\nDifference") + ggtitle("Interaction of Elevation and Patch Area for 2015") + theme(text=element_text(size=14))
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff15.elev.vs.patcharea.pdf"),width=8,height=7)

#for 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta6.confint.csv"))

z.16<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.16[i,j] <- tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.16[tmp.16$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) +
      tmp.16[tmp.16$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.16)<-patch
z.16$elevation<-elev

z_g.16<-gather(z.16,key="patch",value="yld_diff",-elevation)

ggplot(z_g.16, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Shrub Yield\nDifference") + ggtitle("Influence of Elevation and Patch Area for 2016") + theme(text=element_text(size=14))
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff16.elev.vs.patcharea.pdf"),width=8,height=7)

library(ggpubr)
#explore link between coffee area and shrub yield
g1<-ggplot(d.F.new ,aes(coffee.area.ha,pdiff.yld)) + geom_point(aes(color=factor(low.yield1))) + theme_classic() +
  xlab("Coffee Area [ha]") + ylab("Difference in Yield [%]") + labs(color="Low Yielding Plot")
g2<-ggplot(d.F.new ,aes(coffee.area.ha,diff.yld)) + geom_point(aes(color=factor(low.yield1))) + stat_smooth(method="lm") +
  theme_classic() + xlab("Coffee Area [ha]") + ylab("Difference in Yield [kg]") + labs(color="Low Yielding Plot")
g3<-ggplot(d.F.new,aes(coffee.area.ha,low.yield)) + geom_point(aes(color=factor(low.yield1))) + stat_smooth(method="lm") +
  theme_classic() + xlab("Coffee Area [ha]") + ylab("Low Yielding Farm Score [0-1]") + labs(color="Low Yielding Plot")

ggarrange(g1,g2,g3,common.legend=T,ncol=3,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Comparison.diffyld.pdifyld.coffeearea.pdf"),width=9,height=3)

ggplot(d.F.new,aes(log(coffee.area.ha),Shrub.kg)) + geom_point(aes(color=factor(year))) + 
  stat_smooth(method="lm",aes(group=year,color=factor(year)),se=F) + theme_classic() +
  xlab("Log of Coffee Area [ha]") +ylab("Shrub Yield [kg]") + labs(color="Year")
ggsave(paste0(getwd(),"/Analysis/ES/Comparison.shrubyld.coffeearea.pdf"),width=4,height=3)

g1<-ggplot(d.F.new1 %>% filter(year==2015),aes(log(coffee.area.ha),GapDry))  + geom_point(aes(color=factor(low.yield1))) + 
  stat_smooth(method="lm") + theme_classic() +
  xlab("Log of Coffee Area [ha]") +ylab("Canopy Gap [%]") + labs(color="Low Yielding Plot")

g2<-ggplot(d.F.new1 %>% filter(year==2015),aes(log(coffee.area.ha),std.kg))  + geom_point(aes(color=factor(low.yield1))) + 
  stat_smooth(method="lm") + theme_classic() +
  xlab("Log of Coffee Area [ha]") +ylab("Standard Dev of Yield [kg]") + labs(color="Low Yielding Plot")

g3<-ggplot(d.F.new1 %>% filter(year==2015),aes(GapDry,std.kg))  + geom_point(aes(color=factor(low.yield1))) + 
  stat_smooth(method="lm") + theme_classic() +
  xlab("Canopy Gap [%]") +ylab("Standard Dev of Yield [kg]") + labs(color="Low Yielding Plot")
ggarrange(g1,g2,g3,common.legend=T,ncol=3,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Comparison.shrubsd.canopygap.coffeearea.pdf"),width=9,height=3)

#find where low yielding and large farmers are located
d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
d.F.new14<-d.F.new %>% filter(year==2014)
d.F.new15<-d.F.new %>% filter(year==2015)
d.F.new16<-d.F.new %>% filter(year==2016)

#library(car)
library(arm)
pdf(paste0(getwd(),"/Analysis/ES/Plot.LowYield_norm.pdf"),width=8,height=8)
car::qqp(d.F.new14$low.yield, "norm")
dev.off()

#see if elevation and patch area predict low yielding plots?
tm<-lm(low.yield~rescale(patcharea)+rescale(elevation)+rescale(Shannon.i)+rescale(BA.legume),data=d.F.new14)
xy<-summary(tm)

elev<-seq(as.integer(min(d.F.new14$elevation)),as.integer(max(d.F.new14$elevation)),by=1)
patch<-seq(as.integer(min(d.F.new14$patcharea)),as.integer(max(d.F.new14$patcharea)),by=5)
z.elev<-attributes(scale(d.F.new14$elevation))
z.patch<-attributes(scale(d.F.new14$patcharea))

#for lowyield
z.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.ly[i,j] <- coef(xy)[1,1]+coef(xy)[3,1]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + coef(xy)[2,1]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames( z.ly)<-patch
z.ly$elevation<-elev

z_g.ly<-gather(z.ly,key="patch",value="yld_diff",-elevation)

ggplot(z_g.ly, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_gradientn(colours = terrain.colors(20)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Probability Farm Low Yielding (2014)") + theme(text=element_text(size=14))
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.lowyield.elev.vs.patcharea.pdf"),width=8,height=7)



#for 2015&2016
#topmodels.avg<-model.avg(cand.set) 
#sink(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.delta2.txt"))
#summary(topmodels.avg)
#sink() 

#x1<-as.data.frame(summary(topmodels.avg)$importance)
#x1$Comparison<-rownames(x1)
#colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals

#vars<-list()
#for(i in 1:nrow(x1)){
#  if(x1$Comparison[i]=="rescale(elevation):rescale(patcharea)") vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
#  else if(x1$Comparison[i]=="rescale(elevation)"|x1$Comparison[i]=="rescale(patcharea)") vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,parm=x1$Comparison[i],uncond.se = "revised",modnames = Modnames,exclude=list("rescale(elevation):rescale(patcharea)"))$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames,exclude=list("rescale(elevation):rescale(patcharea)"))$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames,exclude=list("rescale(elevation):rescale(patcharea)"))$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames,exclude=list("rescale(elevation):rescale(patcharea)"))$Upper.CL),stringsAsFactors = F)
#  else vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
#}
#vars.1<-do.call(rbind.data.frame,vars)
#colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
#vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

#vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
#tmp<-as.data.frame(t(topmodels.avg[[2]]))
#tmp$Comparison <- rownames(tmp)
#tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
#tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.delta2.confint.csv"))

#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.delta2.confint.csv"))
#tmp<-tmp[!is.na(tmp$full),]

#for delta 2
#tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"],
#                       labels=c("Shade Diversity","Elevation:Patch Area","Basal Area of\nLeguminous Trees","Coffee Land Area","Low Yielding Plot","Elevation","Patch Area","(Intercept)"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]

#g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
#  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Yield\nDifference Per Shrub")+
#  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
#  theme(text = element_text(size = 14)
#    ,axis.text.x=element_text(angle = 45,hjust=1))
#g1+coord_flip()
#ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_ylddiff.pdf"),height=6,width=6)

#test validity of the model
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.delta6.confint.csv"))

d.F.new1$z.coffee.area.ha=rescale(d.F.new1$coffee.area.ha)
d.F.new1$z.Shannon.i=rescale(d.F.new1$Shannon.i)
d.F.new1$z.BA.legume=rescale(d.F.new1$BA.legume)
d.F.new1$z.low.yield=rescale(d.F.new1$low.yield)
d.F.new1$z.elevation=rescale(d.F.new1$elevation)
d.F.new1$z.patcharea=rescale(d.F.new1$patcharea)

df<-d.F.new1 %>% group_by(Plot,year) %>% mutate(diff.yld.mod=tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="rescale(coffee.area.ha)","Estimate"]*z.coffee.area.ha+tmp[tmp$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i+tmp[tmp$Comparison=="rescale(low.yield)","Estimate"]*z.low.yield+
                                                  tmp[tmp$Comparison=="rescale(elevation)","Estimate"]*z.elevation + tmp[tmp$Comparison=="rescale(patcharea)","Estimate"]*z.patcharea + tmp[tmp$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume+
                                                  tmp[tmp$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*z.elevation*z.patcharea,low.yield1=1) %>% mutate(low.yield1=replace(low.yield1,low.yield<0.5,0))

ggplot(df,aes(diff.yld,diff.yld.mod)) + geom_point(aes(color=factor(low.yield1),shape=factor(year))) + geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-0.6,0.5)+xlim(-0.6,0.5)+
  xlab("Observed Difference in Yield [kg]")+ylab("Modelled Difference in Yield [kg]")+
  ggtitle("2015 & 2016")+theme_classic() + labs(shape="Year",color="Low Yielding Plot")+
  theme(text = element_text(size = 14),legend.key = element_blank(),legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff.201516.norm.pdf"),width=6,height=6)


z14<-as.matrix(z)
x <-patch
y <-elev

yld_diff<-plot_ly() %>% 
  add_surface(z=~z1,x=patch,y=elev,type="surface",colorbar=list(title='Shrub Yield Difference')) %>%
  layout(scene=list(
    xaxis=list(title='Patch Size (ha)'),
    yaxis=list(title='Elevation (m)'),
    zaxis=list(title='')))

source("/users/Alex/Documents/Research/Africa/ECOLIMITS/Codes/CoffeeES.analysis/plotly_profile.R")
plotly_IMAGE(yld_diff, format = "png", out_file = paste0(getwd(),"/Analysis/ES/Ylddiff_elevation.patcharea.png"))
#if (!require("processx")) install.packages("processx")
orca(yld_diff, file = paste0(getwd(),"/Analysis/ES/Ylddiff_elevation.patcharea.png"))
