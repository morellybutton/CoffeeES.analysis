#visual comparison of yield outcomes

library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#calculate coefficient of variation
d.F.new <- d.F.new %>% group_by(Plot,year) %>% mutate(yld.cv=(Shrub.kg-avg.kg)/std.kg) %>% ungroup()
d.F.new %>% pull(yld.cv)

d.F.new.15<-d.F.new %>% filter(year==2015)
d.F.new.15<-d.F.new.15 %>% rename(Shrub.kg.15=Shrub.kg,logdiff15=logdiff,yld.cv15=yld.cv)

d.F.new.16<-d.F.new %>% filter(year==2016)
d.F.new.16<-d.F.new.16 %>% rename(Shrub.kg.16=Shrub.kg,logdiff16=logdiff,yld.cv16=yld.cv)

d.F.combo <- left_join(d.F.new %>% filter(year==2014),d.F.new.15 %>% select(Plot,Shrub.kg.15,logdiff15,yld.cv15), by="Plot")
d.F.combo <- left_join(d.F.combo,d.F.new.16 %>% select(Plot,Shrub.kg.16,logdiff16,yld.cv16), by="Plot")

g1<-ggplot(d.F.combo,aes(Shrub.kg,Shrub.kg.15)) + geom_point(aes(color=patcharea)) + geom_abline(intercept=0,slope=1,linetype="dashed") +
  theme_classic() + scale_colour_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + xlab("Normal Yields [kg/shrub]") + ylab("Hot Year Yields [kg/shrub]") +
  xlim(0,0.8) + ylim(0,0.8)

g2<-ggplot(d.F.combo,aes(Shrub.kg,logdiff15)) + geom_point(aes(color=patcharea))  +
  theme_classic() + scale_colour_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + xlab("Normal Yields [kg/shrub]") + ylab("Hot Year Yields [log diff]") +
  stat_smooth(method="lm")

g3<-ggplot(d.F.combo,aes(Shrub.kg,Shrub.kg.16)) + geom_point(aes(color=patcharea)) + geom_abline(intercept=0,slope=1,linetype="dashed") +
  theme_classic() + scale_colour_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + xlab("Normal Yields [kg/shrub]") + ylab("Dry Year Yields [kg/shrub]") +
  xlim(0,0.8) + ylim(0,0.8)

g4<-ggplot(d.F.combo,aes(Shrub.kg,logdiff16)) + geom_point(aes(color=patcharea))  +
  theme_classic() + scale_colour_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + xlab("Normal Yields [kg/shrub]") + ylab("Dry Year Yields [log diff]") +
  stat_smooth(method="lm")

g5<-ggplot(d.F.combo,aes(Shrub.kg,yld.cv15)) + geom_point(aes(color=patcharea))  +
  theme_classic() + scale_colour_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + 
  xlab("Average Yields [kg/shrub]") + ylab("Hot Year [Coefficient of Variation in Yields]") +
  stat_smooth(method="lm")

g6<-ggplot(d.F.combo,aes(Shrub.kg,yld.cv16)) + geom_point(aes(color=patcharea))  +
  theme_classic() + scale_colour_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + 
  xlab("Average Yields [kg/shrub]") + ylab("Dry Year [Coefficient of Variation in Yields]") +
  stat_smooth(method="lm")

ggpubr::ggarrange(g1,g2,g5,g3,g4,g6,ncol=3,nrow=2,common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/YieldComparisons.pdf",height=8,width=8)
rm(g1,g2,g5,g4,g6,g3)

#plot yields over 3 years
yld_summ <- d.F.new %>% group_by(year) %>% summarise(yld.kg=mean(Shrub.kg),sd=sd(Shrub.kg))
sm<-aov(Shrub.kg~factor(year),data=d.F.new)
summary(sm)
TukeyHSD(sm)

g1<-ggplot() + geom_point(data=d.F.new,aes(factor(year),Shrub.kg),color="light grey") + geom_line(data=d.F.new,aes(factor(year),Shrub.kg,group=Plot),color="light grey") +
  theme(legend.position="none") + ylab("Median Shrub Yield per Farm [kg]") + xlab("Year") +
  theme_classic() + geom_point(data=yld_summ,aes(x=factor(year),y=yld.kg),size=3) +
  geom_errorbar(data=yld_summ,aes(x=factor(year),ymin=yld.kg-sd,ymax=yld.kg+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))


ggsave(g1,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmYieldComparisons.pdf",height=5,width=7)

#do again for farm yield
d.F.new <- d.F.new %>% group_by(Plot,year) %>% mutate(yield.ha=Shrub.kg*density) %>% ungroup()

yld.ha_summ <- d.F.new %>% group_by(year) %>% summarise(yield.ha=mean(yield.ha))
yld.ha_summ.sd= d.F.new %>% group_by(year) %>% summarise(sd=sd(yield.ha))
yld.ha_summ <- left_join(yld.ha_summ,yld.ha_summ.sd,by="year")
sm1<-aov(yield.ha~factor(year),data=d.F.new)
summary(sm1)
TukeyHSD(sm1)

g1b<-ggplot() + geom_point(data=d.F.new,aes(factor(year),yield.ha),color="light grey") + 
  geom_line(data=d.F.new,aes(factor(year),yield.ha,group=Plot),color="light grey") +
  theme(legend.position="none") + ylab("Median Yield per Farm [kg/ha]") + xlab("Year") +
  theme_classic() + geom_point(data=yld.ha_summ,aes(x=factor(year),y=yield.ha),size=3) +
  geom_errorbar(data=yld.ha_summ,aes(x=factor(year),ymin=yield.ha-sd,ymax=yield.ha+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

ggpubr::ggarrange(g1,g1b,ncol=2,nrow=1)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmYield.ha.Comparisons.pdf",height=5,width=12)

#identify different modalities of yield.
#2014 is the highest, 2015 is second highest than 2016
#2014 high, 2015 low and 2016 high
#2014 low, 2015 high and 2016 low
#all yields are low
ggplot() + geom_point(data=d.F.new,aes(factor(year),Shrub.kg,color=factor(low.yield.bin))) + geom_line(data=d.F.new,aes(factor(year),Shrub.kg,group=Plot,color=factor(low.yield.bin))) +
  theme(legend.position="none") + ylab("Median Shrub Yield per Farm [kg]") + xlab("Year") +
  theme_classic() + geom_point(data=yld_summ,aes(x=factor(year),y=yld.kg),size=3) +
  geom_errorbar(data=yld_summ,aes(x=factor(year),ymin=yld.kg-sd,ymax=yld.kg+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

#calculate resistance and resilience values using Isbell et al (2015) Nature
#resistance = Yn/abs(Ye-Yn)
#where Yn = mean productivity during normal year and Ye is yield during climate shock
#resilience = abs(Ye-Yn)/abs(Y[e+1]-Yn)
#where Y[e+1] = productivity during the year after a climate event.

d.F.new.15 <- left_join(d.F.new.15,d.F.combo %>% rename(Shrub.kg.14=Shrub.kg) %>% select(Plot,Shrub.kg.14,Shrub.kg.16),by="Plot")
d.F.new.15 <- d.F.new.15 %>% group_by(Plot,year) %>% mutate(yld.ha.14=Shrub.kg.14*density,yld.ha.15=Shrub.kg.15*density,yld.ha.16=Shrub.kg.16*density) %>% ungroup()

d.F.new.15 <- d.F.new.15 %>% mutate(resist.15=Shrub.kg.14/abs(Shrub.kg.15-Shrub.kg.14),
                                    resist.16=Shrub.kg.14/abs(Shrub.kg.16-Shrub.kg.14),
                                    resist.15.ha=yld.ha.14/abs(yld.ha.15-yld.ha.14),
                                    resist.16.ha=yld.ha.14/abs(yld.ha.16-yld.ha.14),
                                    resilience=abs(Shrub.kg.15-Shrub.kg.14)/abs(Shrub.kg.16-Shrub.kg.14))
write.csv(d.F.new.15,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmResistanceComparisons.csv")

d.F.new.15 <- d.F.new.15 %>% mutate(group1=0,group2=0,group3=0) %>% 
  mutate(group1=replace(group1,Shrub.kg.14>Shrub.kg.15&Shrub.kg.15>Shrub.kg.16,1),
         group2=replace(group2,Shrub.kg.14>Shrub.kg.15&Shrub.kg.15<Shrub.kg.16,1),
         group3=replace(group3,Shrub.kg.14<Shrub.kg.15&Shrub.kg.15>Shrub.kg.16,1))
d.F.new <- left_join(d.F.new,d.F.new.15 %>% select(Plot,group1,group2,group3),by="Plot")
write.csv(d.F.new,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmResistanceGroups.csv")

yld_summ1 <- d.F.new %>% filter(group1==1&Plot!="H10") %>% group_by(year) %>% summarise(yld.kg=mean(Shrub.kg),sd=sd(Shrub.kg))
sm1<-aov(Shrub.kg~factor(year),data=d.F.new %>% filter(group1==1&Plot!="H10"))
summary(sm1)
TukeyHSD(sm1)

g2<-ggplot() + geom_point(data=d.F.new %>% filter(group1==1&Plot!="H10"),aes(factor(year),Shrub.kg),color="light grey") + 
  geom_line(data=d.F.new %>% filter(group1==1&Plot!="H10"),aes(factor(year),Shrub.kg,group=Plot),color="light grey") +
  theme(legend.position="none") + ylab("Shrub Yield [kg]") + xlab("Year") + ggtitle("Group 1") +
  theme_classic() + geom_point(data=yld_summ1,aes(x=factor(year),y=yld.kg),size=3) +
  geom_errorbar(data=yld_summ1,aes(x=factor(year),ymin=yld.kg-sd,ymax=yld.kg+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

yld_summ2 <- d.F.new %>% filter(group2==1&Plot!="H10") %>% group_by(year) %>% summarise(yld.kg=mean(Shrub.kg),sd=sd(Shrub.kg))
sm2<-aov(Shrub.kg~factor(year),data=d.F.new %>% filter(group2==1&Plot!="H10"))
summary(sm2)
TukeyHSD(sm2)

g3<-ggplot() + geom_point(data=d.F.new %>% filter(group2==1&Plot!="H10"),aes(factor(year),Shrub.kg),color="light grey") + 
  geom_line(data=d.F.new %>% filter(group2==1&Plot!="H10"),aes(factor(year),Shrub.kg,group=Plot),color="light grey") +
  theme(legend.position="none") + ylab("Shrub Yield [kg]") + xlab("Year") + ggtitle("Group 2") +
  theme_classic() + geom_point(data=yld_summ2,aes(x=factor(year),y=yld.kg),size=3) +
  geom_errorbar(data=yld_summ2,aes(x=factor(year),ymin=yld.kg-sd,ymax=yld.kg+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

yld_summ3 <- d.F.new %>% filter(group3==1&Plot!="H10") %>% group_by(year) %>% summarise(yld.kg=mean(Shrub.kg),sd=sd(Shrub.kg))
sm3<-aov(Shrub.kg~factor(year),data=d.F.new %>% filter(group3==1&Plot!="H10"))
summary(sm3)
TukeyHSD(sm3)

g4<-ggplot() + geom_point(data=d.F.new %>% filter(group3==1&Plot!="H10"),aes(factor(year),Shrub.kg),color="light grey") + 
  geom_line(data=d.F.new %>% filter(group3==1&Plot!="H10"),aes(factor(year),Shrub.kg,group=Plot),color="light grey") +
  theme(legend.position="none") + ylab("Shrub Yield [kg]") + xlab("Year") + ggtitle("Group 3") +
  theme_classic() + geom_point(data=yld_summ3,aes(x=factor(year),y=yld.kg),size=3) +
  geom_errorbar(data=yld_summ3,aes(x=factor(year),ymin=yld.kg-sd,ymax=yld.kg+sd),width=0.05,size=1) +
  theme(text = element_text(size = 16))

g5<-ggpubr::ggarrange(g2,g3,g4,ncol=1,nrow=3)
ggpubr::ggarrange(g1,g5,ncol=2,align="v",widths=c(1.5,1))

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmYieldComparisons.wgroups.pdf",height=6,width=10)

#plot resistance over time
resist.mean<-d.F.new.15 %>% summarise(resist.15=mean(resist.15,na.rm=T),resist.16=mean(resist.16,na.rm=T))
                                      #resist.15.ha=mean(resist.15.ha,na.rm=T),resist.16.ha=mean(resist.16.ha,na.rm=T))
resist.sd<-d.F.new.15 %>% summarise(resist.15=sd(resist.15,na.rm=T),resist.16=sd(resist.16,na.rm=T))
                                    #resist.15.ha=sd(resist.15.ha,na.rm=T),resist.16.ha=sd(resist.16.ha,na.rm=T))

tmp<-tibble(t(resist.mean))
colnames(tmp)<-"resist"
tmp$year<-c(2015,2016)
#tmp$conditions<-c("Heat","Drought")
tmp$sd<-t(resist.sd)

d.F.resist <- d.F.new.15 %>% select(Plot,year,resist.15,group1,group2,group3) %>% rename(resist=resist.15)
tmp1 <- d.F.new.15 %>% select(Plot,resist.16,group1,group2,group3) %>% rename(resist=resist.16) %>% mutate(year=2016)
d.F.resist<-bind_rows(d.F.resist,tmp1)

tm<-aov(resist~factor(year),data=d.F.resist)
summary(tm)
TukeyHSD(tm)

p1<-ggplot() + geom_point(data=d.F.resist,aes(factor(year),resist),color="light grey") + geom_line(data=d.F.resist,aes(factor(year),resist,group=Plot),color="light grey") +
  theme_classic() + ylab("Resistance") + xlab("Climate Shock") + theme(text = element_text(size = 16)) +
  geom_point(data=tmp,aes(factor(year),resist),size=3) + scale_colour_grey(start=0.9,end=0.7) +
  geom_errorbar(data=tmp,aes(x=factor(year),ymax=resist+sd,ymin=resist-sd),width=0.05) +
  geom_hline(yintercept=tmp %>% filter(year==2015) %>% pull(resist),linetype="dashed") +
  scale_x_discrete(labels=c("2015" = "Hot Year", "2016" = "Dry Year"))

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmResistanceComparisons.pdf",height=5,width=7)

resist.mean1<-d.F.new.15 %>% filter(group1==1) %>% summarise(resist.15=mean(resist.15,na.rm=T),resist.16=mean(resist.16,na.rm=T))
resist.sd1<-d.F.new.15 %>% filter(group1==1) %>% summarise(resist.15=sd(resist.15,na.rm=T),resist.16=sd(resist.16,na.rm=T))

tmp2<-tibble(t(resist.mean1))
colnames(tmp2)<-"resist"
tmp2$year<-c(2015,2016)
#tmp$conditions<-c("Heat","Drought")
tmp2$sd<-t(resist.sd1)

tm1<-aov(resist~factor(year),data=d.F.resist %>% filter(group1==1))
summary(tm1)
TukeyHSD(tm1)

p2<-ggplot() + geom_point(data=d.F.resist %>% filter(group1==1),aes(factor(year),resist),color="light grey") + 
  geom_line(data=d.F.resist %>% filter(group1==1),aes(factor(year),resist,group=Plot),color="light grey") +
  theme_classic() + ylab("Resistance") + xlab("Climate Shock") + theme(text = element_text(size = 16)) +
  ggtitle("Group 1") +
  geom_point(data=tmp2,aes(factor(year),resist),size=3) + scale_colour_grey(start=0.9,end=0.7) +
  geom_errorbar(data=tmp2,aes(x=factor(year),ymax=resist+sd,ymin=resist-sd),width=0.05) +
  #geom_hline(yintercept=tmp2 %>% filter(year==2015) %>% pull(resist),linetype="dashed") +
  scale_x_discrete(labels=c("2015" = "Hot Year", "2016" = "Dry Year"))

resist.mean2<-d.F.new.15 %>% filter(group2==1) %>% summarise(resist.15=mean(resist.15,na.rm=T),resist.16=mean(resist.16,na.rm=T))
resist.sd2<-d.F.new.15 %>% filter(group2==1) %>% summarise(resist.15=sd(resist.15,na.rm=T),resist.16=sd(resist.16,na.rm=T))

tmp3<-tibble(t(resist.mean2))
colnames(tmp3)<-"resist"
tmp3$year<-c(2015,2016)
#tmp$conditions<-c("Heat","Drought")
tmp3$sd<-t(resist.sd2)

tm2<-aov(resist~factor(year),data=d.F.resist %>% filter(group2==1))
summary(tm2)
TukeyHSD(tm2)

p3<-ggplot() + geom_point(data=d.F.resist %>% filter(group2==1),aes(factor(year),resist),color="light grey") + 
  geom_line(data=d.F.resist %>% filter(group2==1),aes(factor(year),resist,group=Plot),color="light grey") +
  theme_classic() + ylab("Resistance") + xlab("Climate Shock") + theme(text = element_text(size = 16)) +
  ggtitle("Group 2") +
  geom_point(data=tmp3,aes(factor(year),resist),size=3) + scale_colour_grey(start=0.9,end=0.7) +
  geom_errorbar(data=tmp3,aes(x=factor(year),ymax=resist+sd,ymin=resist-sd),width=0.05) +
  #geom_hline(yintercept=tmp2 %>% filter(year==2015) %>% pull(resist),linetype="dashed") +
  scale_x_discrete(labels=c("2015" = "Hot Year", "2016" = "Dry Year"))


resist.mean3<-d.F.new.15 %>% filter(group3==1) %>% summarise(resist.15=mean(resist.15,na.rm=T),resist.16=mean(resist.16,na.rm=T))
resist.sd3<-d.F.new.15 %>% filter(group3==1) %>% summarise(resist.15=sd(resist.15,na.rm=T),resist.16=sd(resist.16,na.rm=T))

tmp4<-tibble(t(resist.mean3))
colnames(tmp4)<-"resist"
tmp4$year<-c(2015,2016)
#tmp$conditions<-c("Heat","Drought")
tmp4$sd<-t(resist.sd3)

tm3<-aov(resist~factor(year),data=d.F.resist %>% filter(group3==1))
summary(tm3)
TukeyHSD(tm3)

p4<-ggplot() + geom_point(data=d.F.resist %>% filter(group3==1),aes(factor(year),resist),color="light grey") + 
  geom_line(data=d.F.resist %>% filter(group3==1),aes(factor(year),resist,group=Plot),color="light grey") +
  theme_classic() + ylab("Resistance") + xlab("Climate Shock") + theme(text = element_text(size = 16)) +
  ggtitle("Group 3") +
  geom_point(data=tmp4,aes(factor(year),resist),size=3) + scale_colour_grey(start=0.9,end=0.7) +
  geom_errorbar(data=tmp4,aes(x=factor(year),ymax=resist+sd,ymin=resist-sd),width=0.05) +
  #geom_hline(yintercept=tmp2 %>% filter(year==2015) %>% pull(resist),linetype="dashed") +
  scale_x_discrete(labels=c("2015" = "Hot Year", "2016" = "Dry Year"))

p5<-ggpubr::ggarrange(p2,p3,p4,ncol=1,nrow=3)
ggpubr::ggarrange(p1,p5,ncol=2,align="v",widths=c(1.5,1))

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmResistanceComparisons.wgroups.pdf",height=6,width=10)

#check out high resistance farms
test<-d.F.resist %>% filter(resist>3)
tm3<-aov(resist~factor(year),data=d.F.resist)
summary(tm3)

library(arm)
library(car)
library(MuMIn)
library(lattice)

#do a corrplot
tmp15<-d.F.new.15 %>% dplyr::select(-X.1,-Plot,-ID,-X,-Shrub.id,-kebele,-wereda,-site)
corrplot(tmp15,color=TRUE,details=TRUE)
#check correlation of fruitset and yields
cor(tmp15, use="complete.obs", method="pearson") 

#what variables could drive resilience or resistance?
#elevation, patcharea, soil conditions (CN.ratio, C.pct), coffee.area.ha, 
#BA.legume, Shade Diversity, buffer, canopy gap, being low yielding

pdf(paste0(getwd(),"/Analysis/ES/Plot.resilience.lnorm.pdf"),width=8,height=8)
qqp(d.F.new.15$resilience, "lnorm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.resistance15.lnorm.pdf"),width=8,height=8)
qqp(d.F.new.15$resist.15, "lnorm")
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.resistance16.lnorm.pdf"),width=8,height=8)
qqp(d.F.new.15$resist.16, "lnorm")
dev.off()

#for resilience
tmp1<-d.F.new.15 %>% filter(!is.na(resilience))
rm1<-glm(resilience~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) +
          rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer) +
          rescale(GapDry) + rescale(low.yield.bin), family=gaussian(link="log"),data=tmp1)
summary(rm1)

rm2<-glm(resilience~rescale(elevation)+rescale(patcharea) + rescale(C.pct) +
           rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer) +
          rescale(low.yield.bin), family=gaussian(link="log"),data=tmp1)
summary(rm2)

#for resistance 2015
tmp2<-d.F.new.15 %>% filter(!is.na(resist.15))
sm1<-glm(resist.15~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) +
          rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer) +
          rescale(GapDry) + rescale(low.yield.bin), family=gaussian(link="log"),data=tmp2)
summary(sm1)

sm2<-glm(resist.15~rescale(elevation)+rescale(patcharea)  +
           rescale(BA.legume)+rescale(Shannon.i) + rescale(b.ffer) +
           rescale(GapDry) + rescale(low.yield.bin), family=gaussian(link="log"),data=tmp2)
summary(sm2)

#for resistance 2016
tmp3<-d.F.new.15 %>% filter(!is.na(resist.16))
tm1<-glm(resist.16~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer) +
           rescale(GapDry) + rescale(low.yield.bin), family=gaussian(link="log"),data=tmp3)
summary(tm1)

tm2<-glm(resist.16~rescale(elevation)*rescale(patcharea) + rescale(C.pct) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) +
           rescale(low.yield.bin),
         family=gaussian(link="log"),data=tmp3)
summary(tm2)


#check heteroskedasticity
rm<-rm2
diagnos.r <- data.frame(Resid = resid(rm, type = "pearson"), Fitted = fitted(rm),Variable = tmp1 )
pdf(paste0(getwd(),"/Analysis/ES/Plot.ElNino.Resilience.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos.r)
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.ElNino.Resilience_qqplotResiduals_all.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos.r, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

sm<-sm2
diagnos.s <- data.frame(Resid = resid(sm, type = "pearson"), Fitted = fitted(sm),Variable = tmp2 )
pdf(paste0(getwd(),"/Analysis/ES/Plot.ElNino.Resistance15.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos.s)
dev.off()

tm<-tm2
diagnos.t <- data.frame(Resid = resid(tm, type = "pearson"), Fitted = fitted(tm),Variable = tmp3 )
pdf(paste0(getwd(),"/Analysis/ES/Plot.ElNino.Resistance16.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos.t)
dev.off()

options(na.action = "na.fail")

#resilience
#check for overdispersion
chat<-sum(residuals(rm,"pearson")^2)/rm$df.residual
chat 

rm.d<-dredge(rm)
#test.rm<-stepAIC(rm, trace = FALSE)

#picked delta of 2 because delta of 6 was >24 models
dredg.rm<-subset(rm.d,delta<2)
write.csv(dredg.rm,paste0(getwd(),"/Analysis/ES/Resilience_dredge.csv"))


#resistance 15
#check for overdispersion
chat<-sum(residuals(sm,"pearson")^2)/sm$df.residual
chat 

sm.d<-dredge(sm)

#picked delta of 2 because delta of 6 was >24 models
dredg.sm<-subset(sm.d,delta<2)
write.csv(dredg.sm,paste0(getwd(),"/Analysis/ES/Resistance2015_dredge.csv"))

#resistance 16
#check for overdispersion
chat<-sum(residuals(tm,"pearson")^2)/tm$df.residual
chat 

#result is empty
tm.d<-dredge(tm)

#picked delta of 2 because delta of 6 was >24 models
dredg.tm<-subset(tm.d,delta<2)
write.csv(dredg.tm,paste0(getwd(),"/Analysis/ES/Resistance2016_dredge.csv"))

#resilience
cand.set.rm<-list()
#delta 2 has 4 models plus an additional model without the interaction
cand.set.rm[[1]]<-glm(resilience~rescale(elevation)+rescale(patcharea) + rescale(C.pct) +
                        rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer), family=gaussian(link="log"),data=tmp1)
cand.set.rm[[2]]<-glm(resilience~rescale(elevation) +
                        rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer), family=gaussian(link="log"),data=tmp1)
cand.set.rm[[3]]<-glm(resilience~rescale(elevation)+rescale(C.pct) +
                        rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer), family=gaussian(link="log"),data=tmp1)
cand.set.rm[[4]]<-glm(resilience~rescale(elevation)+rescale(patcharea) +
                        rescale(BA.legume)*rescale(Shannon.i) + rescale(b.ffer), family=gaussian(link="log"),data=tmp1)
#cand.set.rm[[5]]<-glm(resilience~rescale(elevation) +
#                        rescale(BA.legume)+rescale(Shannon.i) + rescale(b.ffer), family=gaussian(link="log"),data=tmp1)

#resistance 15
cand.set.sm<-list()
#delta 2 had 9 models
cand.set.sm[[1]]<-glm(resist.15~rescale(elevation)+rescale(patcharea)  +
                      rescale(BA.legume) + rescale(b.ffer),family=gaussian(link="log"),data=tmp2)
cand.set.sm[[2]]<-glm(resist.15~rescale(elevation) +
                        rescale(BA.legume), family=gaussian(link="log"),data=tmp2)
cand.set.sm[[3]]<-glm(resist.15~rescale(elevation) +
                        rescale(BA.legume) + rescale(b.ffer), family=gaussian(link="log"),data=tmp2)
cand.set.sm[[4]]<-glm(resist.15~rescale(elevation)+rescale(patcharea)  +
                      rescale(BA.legume) + rescale(b.ffer) + rescale(GapDry), family=gaussian(link="log"),data=tmp2)
cand.set.sm[[5]]<-glm(resist.15~rescale(elevation) +
                        rescale(BA.legume) + rescale(GapDry), family=gaussian(link="log"),data=tmp2)
cand.set.sm[[6]]<-glm(resist.15~rescale(elevation)+rescale(patcharea)  +
                        rescale(BA.legume) + rescale(b.ffer) + rescale(Shannon.i), family=gaussian(link="log"),data=tmp2)
cand.set.sm[[7]]<-glm(resist.15~rescale(elevation) +
                        rescale(BA.legume) + rescale(b.ffer) + rescale(Shannon.i), family=gaussian(link="log"),data=tmp2)
cand.set.sm[[8]]<-glm(resist.15~rescale(elevation) + rescale(BA.legume) + rescale(b.ffer) + 
                        rescale(GapDry) + rescale(patcharea) + rescale(Shannon.i) , family=gaussian(link="log"),data=tmp2)
cand.set.sm[[9]]<-glm(resist.15~rescale(elevation) + rescale(b.ffer) +
                        rescale(BA.legume) + rescale(GapDry), family=gaussian(link="log"),data=tmp2)

#resistance 16
cand.set.tm<-list()
#delta 2 had 7 models and one for a non interaction
cand.set.tm[[1]]<-glm(resist.16~rescale(elevation) + 
                        rescale(BA.legume)*rescale(Shannon.i) +
                        rescale(low.yield.bin), family=gaussian(link="log"),data=tmp3)
cand.set.tm[[2]]<-glm(resist.16~rescale(elevation) + 
                        rescale(BA.legume)*rescale(Shannon.i), family=gaussian(link="log"),data=tmp3)
cand.set.tm[[3]]<-glm(resist.16~rescale(elevation)*rescale(patcharea) + 
                        rescale(BA.legume)*rescale(Shannon.i) + rescale(C.pct) +
                        rescale(low.yield.bin) + rescale(coffee.area.ha), family=gaussian(link="log"),data=tmp3)
cand.set.tm[[4]]<-glm(resist.16~rescale(elevation)*rescale(patcharea) + 
                        rescale(BA.legume)*rescale(Shannon.i) +
                        rescale(low.yield.bin) + rescale(coffee.area.ha), family=gaussian(link="log"),data=tmp3)
cand.set.tm[[5]]<-glm(resist.16~rescale(elevation) + 
                        rescale(BA.legume)*rescale(Shannon.i) + rescale(C.pct) +
                        rescale(low.yield.bin), family=gaussian(link="log"),data=tmp3)
cand.set.tm[[6]]<-glm(resist.16~rescale(elevation) + 
                        rescale(BA.legume)*rescale(Shannon.i) +
                        rescale(low.yield.bin) + rescale(coffee.area.ha), family=gaussian(link="log"),data=tmp3)
cand.set.tm[[7]]<-glm(resist.16~rescale(elevation) + rescale(C.pct) +
                        rescale(BA.legume)*rescale(Shannon.i) +
                        rescale(low.yield.bin) + rescale(coffee.area.ha), family=gaussian(link="log"),data=tmp3)
#cand.set.tm[[8]]<-glm(resist.16~rescale(elevation) + 
#                        rescale(BA.legume) + rescale(Shannon.i), family=gaussian(link="log"),data=tmp3)

##create a vector of names to trace back models in set
Modnames.rm <- paste("mod", 1:length(cand.set.rm), sep = " ")
#Modnames <- paste("mod", 1:length(cand.set), sep = " ")
Modnames.sm <- paste("mod", 1:length(cand.set.sm), sep = " ")
Modnames.tm <- paste("mod", 1:length(cand.set.tm), sep = " ")

##generate AICc table
res.table <-AICcmodavg::aictab(cand.set = cand.set.rm, modnames = Modnames.rm, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_Resilience.delta2.csv"))

res.table <-AICcmodavg::aictab(cand.set = cand.set.sm, modnames = Modnames.sm, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_Resistence15.delta2.csv"))

res.table <-AICcmodavg::aictab(cand.set = cand.set.tm, modnames = Modnames.tm, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_Resistence16.delta2.csv"))

#Resilience
topmodels.avg.rm<-model.avg(cand.set.rm) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_resilience.delta2.txt"))
summary(topmodels.avg.rm)
sink() 

x.rm<-as.data.frame(summary(topmodels.avg.rm)$coefmat.full[,5])
x.rm$Comparison<-rownames(x.rm)
colnames(x.rm)<-c("pvalue","Comparison")

#create figure of coefficients with confidence intervals
tmp.rm<-as.data.frame(t(topmodels.avg.rm[[2]]))
tmp.rm$Comparison <- rownames(tmp.rm)
tmp.rm$Lower.CL<-as.numeric(confint(topmodels.avg.rm, full = T)[,1])
tmp.rm$Upper.CL<-as.numeric(confint(topmodels.avg.rm, full = T)[,2])

#add importance
tmp.rm$pvalue<-x.rm[match(tmp.rm$Comparison,x.rm$Comparison),"pvalue"]
#write.csv(tmp.rm,paste0(getwd(),"/Analysis/ES/Model.Average_resilience.delta2.confint.csv"))
tmp.rm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resilience.delta2.confint.csv"))
tmp.rm<-tmp.rm[!is.na(tmp.rm$full),]

tmp.rm$Comparison<-factor(tmp.rm$Comparison,levels=tmp.rm[order(tmp.rm$pvalue,decreasing=T),"Comparison"],
                          labels=c("Patch Area","Soil C","Basal Area of\nLeguminous Trees","Shade Diversity","(Intercept)","BA Legume:\nShade Diversity","Elevation","Located in\nBuffer"))

#order by importance
tmp.rm<-tmp.rm %>% filter(Comparison!="(Intercept)")

#add significance column
tmp.rm$sig<-1
tmp.rm<-tmp.rm %>% mutate(sig=replace(sig,Comparison=="BA Legume:\nShade Diversity"|Comparison=="Elevation"|Comparison=="Located in\nBuffer",0))

g1<-ggplot(tmp.rm, aes(x = Comparison, y = full, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Farm Resilience")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_resilience.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_resilience.pdf",height=6,width=6)

p1<-g1+coord_flip()+  xlab("")

#Yield model for 2014
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model_yld14.delta2.v4.confint.csv"))
#tmp.14<-tmp.14[!is.na(tmp.14$full),]

#for delta 2
tmp.14$Comparison<-factor(tmp.14$Comparison,levels=tmp.14[order(tmp.14$pvalue,decreasing=T),"Comparison"],
                          labels=c("Basal Area of\nLeguminous Trees","Patch Area","BA Legume:\nShade Diversity","Shade Diversity","Elevation","(Intercept)"))

#order by importance
tmp.14<-tmp.14 %>% filter(Comparison!="(Intercept)")

#add significance column
tmp.14$sig<-1
tmp.14<-tmp.14 %>% mutate(sig=replace(sig,Comparison=="Elevation"|Comparison=="Shade Diversity"|Comparison=="BA Legume:\nShade Diversity"|Comparison=="Patch Area",0))

g1<-ggplot(tmp.14, aes(x = Comparison, y = Coefficients, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield\n(Normal Year)")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_results_yld14.v4.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_results_yld14.v4.pdf",height=6,width=6)

p1<-g1+coord_flip()


#Resistence 2015
topmodels.avg.sm<-model.avg(cand.set.sm) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_resistance15.delta2.txt"))
summary(topmodels.avg.sm)
sink() 

x.sm<-as.data.frame(summary(topmodels.avg.sm)$coefmat.full[,5])
x.sm$Comparison<-rownames(x.sm)
colnames(x.sm)<-c("pvalue","Comparison")

#create figure of coefficients with confidence intervals
tmp.sm<-as.data.frame(t(topmodels.avg.sm[[2]]))
tmp.sm$Comparison <- rownames(tmp.sm)
tmp.sm$Lower.CL<-as.numeric(confint(topmodels.avg.sm, full = T)[,1])
tmp.sm$Upper.CL<-as.numeric(confint(topmodels.avg.sm, full = T)[,2])

#add importance
tmp.sm$pvalue<-x.sm[match(tmp.sm$Comparison,x.sm$Comparison),"pvalue"]
#write.csv(tmp.sm,paste0(getwd(),"/Analysis/ES/Model.Average_resistance15.delta2.confint.csv"))
tmp.sm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resistance15.delta2.confint.csv"))
tmp.sm<-tmp.sm[!is.na(tmp.sm$full),]

tmp.sm$Comparison<-factor(tmp.sm$Comparison,levels=tmp.sm[order(tmp.sm$pvalue,decreasing=T),"Comparison"],
                          labels=c("Shade Diversity","Canopy Gap","Patch Area","Located in\nBuffer","Elevation","Basal Area of\nLeguminous Trees","(Intercept)"))

#order by importance
tmp.sm<-tmp.sm %>% filter(Comparison!="(Intercept)")

#add significance column
tmp.sm$sig<-1
tmp.sm<-tmp.sm %>% mutate(sig=replace(sig,Comparison=="Basal Area of\nLeguminous Trees"|Comparison=="Elevation",0))

g2<-ggplot(tmp.sm, aes(x = Comparison, y = full, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Farm Resistance\n(Hot Year)")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g2+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_resistance15.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_resistance15.pdf",height=6,width=6)

p2<-g2+coord_flip()+  xlab("")

#Resistence 2016
topmodels.avg.tm<-model.avg(cand.set.tm) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_resistance16.delta2.txt"))
summary(topmodels.avg.tm)
sink() 

x.tm<-as.data.frame(summary(topmodels.avg.tm)$coefmat.full[,5])
x.tm$Comparison<-rownames(x.tm)
colnames(x.tm)<-c("pvalue","Comparison")

#create figure of coefficients with confidence intervals
tmp.tm<-as.data.frame(t(topmodels.avg.tm[[2]]))
tmp.tm$Comparison <- rownames(tmp.tm)
tmp.tm$Lower.CL<-as.numeric(confint(topmodels.avg.tm, full = T)[,1])
tmp.tm$Upper.CL<-as.numeric(confint(topmodels.avg.tm, full = T)[,2])

#add importance
tmp.tm$pvalue<-x.tm[match(tmp.tm$Comparison,x.tm$Comparison),"pvalue"]
#write.csv(tmp.tm,paste0(getwd(),"/Analysis/ES/Model.Average_resistance16.delta2.confint.csv"))
tmp.tm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resistance16.delta2.confint.csv"))
tmp.tm<-tmp.tm[!is.na(tmp.tm$full),]

tmp.tm$Comparison<-factor(tmp.tm$Comparison,levels=tmp.tm[order(tmp.tm$pvalue,decreasing=T),"Comparison"],
                          labels=c("Patch Area","Shade Diversity","Basal Area of\nLeguminous Trees","Elevation:\nPatch Area","Soil C","(Intercept)","Coffee Land\nArea","Low Yielding\nFarm","Elevation","BA Legume:\nShade Diversity"))

#order by importance
tmp.tm<-tmp.tm %>% filter(Comparison!="(Intercept)")

#add significance column
tmp.tm$sig<-1
tmp.tm<-tmp.tm %>% mutate(sig=replace(sig,Comparison=="BA Legume:\nShade Diversity"|Comparison=="Elevation",0))

g3<-ggplot(tmp.tm, aes(x = Comparison, y = full, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Farm Resistance\n(Dry Year)")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")

g3+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_resistance16.pdf"),height=6,width=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_resistance16.pdf",height=6,width=6)

p3<-g3+coord_flip()+  xlab("")

ggpubr::ggarrange(p1,p2,p3,ncol=3,nrow=1)
#ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_resil.resist_combined.pdf"),height=6,width=18)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_averaged_resil.resist_combined.pdf",height=6,width=18)

#test validity of resilience model
tmp1$resilience.pred<-predict(topmodels.avg.rm,type="response")

p1.3<-ggplot(tmp1,aes(resilience,resilience.pred)) + geom_point() + geom_abline(slope=1,intercept=0,col='red') +
  ylim(0,8.15)+xlim(0,8.15)+
  xlab("Observed")+ylab("Predicted")+
  ggtitle("Model Assessment")+theme_bw() +
  theme(text = element_text(size = 12),legend.key = element_blank(),legend.position="bottom")

#residuals vs fitted
p1.1<-ggplot(rm2, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

#qqplot
p1.2 <- ggplot(rm2, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm=T) +
  geom_abline(aes(intercept=0,slope=1), col = 'red')+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")+theme_bw()

p1.4<-ggpubr::ggarrange(p1.1,p1.2,p1.3,ncol=2,nrow=2,labels="auto")

ggpubr::ggarrange(p1,p1.4,ncol=2,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.resilience.allgraphs.pdf"),width=12,height=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Modelled.resilience.allgraphs.pdf",width=12,height=6)

#test validity of resistance 2015 model
tmp2$resist15.pred<-predict(topmodels.avg.sm,type="response")

p2.3<-ggplot(tmp2,aes(resist.15,resist15.pred)) + geom_point() + geom_abline(slope=1,intercept=0,col='red') +
  ylim(0,8.15)+xlim(0,8.15)+
  xlab("Observed")+ylab("Predicted")+
  ggtitle("Model Assessment")+theme_bw() +
  theme(text = element_text(size = 12),legend.key = element_blank(),legend.position="bottom")

#residuals vs fitted
p2.1<-ggplot(sm2, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

#qqplot
p2.2 <- ggplot(sm2, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm=T) +
  geom_abline(aes(intercept=0,slope=1), col = 'red')+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")+theme_bw()

p2.4<-ggpubr::ggarrange(p2.1,p2.2,p2.3,ncol=2,nrow=2,labels="auto")

ggpubr::ggarrange(p2,p2.4,ncol=2,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.resistance15.allgraphs.pdf"),width=12,height=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Modelled.resistance15.allgraphs.pdf",width=12,height=6)

#test validity of resistance 2016 model
tmp3$resist16.pred<-predict(topmodels.avg.tm,type="response")

p3.3<-ggplot(tmp3,aes(resist.16,resist16.pred)) + geom_point() + geom_abline(slope=1,intercept=0,col='red') +
  ylim(0,8.15)+xlim(0,8.15)+
  xlab("Observed")+ylab("Predicted")+
  ggtitle("Model Assessment")+theme_bw() +
  theme(text = element_text(size = 12),legend.key = element_blank(),legend.position="bottom")

#residuals vs fitted
p3.1<-ggplot(tm2, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

#qqplot
p3.2 <- ggplot(tm2, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm=T) +
  geom_abline(aes(intercept=0,slope=1), col = 'red')+xlab("Theoretical Quantiles")+ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")+theme_bw()

p3.4<-ggpubr::ggarrange(p3.1,p3.2,p3.3,ncol=2,nrow=2,labels="auto")

ggpubr::ggarrange(p3,p3.4,ncol=2,nrow=1)
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.resistance16.allgraphs.pdf"),width=12,height=6)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Modelled.resistance16.allgraphs.pdf",width=12,height=6)

ggpubr::ggarrange(p1,p2,p3,ncol=3,nrow=1)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Modelled.yield&resistance.pdf",width=15,height=6)


#3D figure of patcharea area and elevation
elev<-seq(as.integer(min(d.F.combo$elevation)),as.integer(max(d.F.combo$elevation)),by=1)
patch<-seq(as.integer(min(d.F.combo$patcharea)),as.integer(max(d.F.combo$patcharea)),by=5)
z.elev<-attributes(scale(d.F.combo$elevation))
z.patch<-attributes(scale(d.F.combo$patcharea))

#produce graph of normal year yield
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model_yld14.delta2.v4.confint.csv"))

z.14<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(elevation)","Coefficients"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.14[tmp.14$Comparison=="rescale(patcharea)","Coefficients"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) 
  }
}
colnames(z.14)<-patch
z.14$elevation<-elev

z_g.14<-gather(z.14,key="patch",value="yld",-elevation)

g1<-ggplot(z_g.14, aes( as.numeric(patch), elevation, z = yld)) +geom_raster(aes(fill=yld)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(-0.5,1.0))+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Shrub Yield\n(Normal Year)") + theme(text=element_text(size=16))
g1
#ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yld14.elev.vs.patcharea.pdf"),width=8,height=7)

#tmp.rm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resilience.delta2.confint.csv"))

#z.rm<-data.frame()
#for(i in 1:length(elev)){
#  for(j in 1:length(patch)){
#    z.rm[i,j] <- tmp.rm[tmp.rm$Comparison=="rescale(elevation)","full"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.rm[tmp.rm$Comparison=="rescale(patcharea)","full"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) 
#  }
#}
#colnames(z.rm)<-patch
#z.rm$elevation<-elev

#z_g.rm<-gather(z.rm,key="patch",value="res",-elevation)

#g2<-ggplot(z_g.rm, aes( as.numeric(patch), elevation, z = res)) +geom_raster(aes(fill=res)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
#  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
#  labs(fill="Resilience") + ggtitle("Climate\nResilience") + theme(text=element_text(size=16))
#g2
#ggsave(paste0(getwd(),"/Analysis/ES/Modelled.resilience.elev.vs.patcharea.pdf"),width=8,height=7)

#resistance 2015
tmp.sm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resistance15.delta2.confint.csv"))

z.sm<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.sm[i,j] <- tmp.sm[tmp.sm$Comparison=="rescale(elevation)","full"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.sm[tmp.sm$Comparison=="rescale(patcharea)","full"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) 
  }
}
colnames(z.sm)<-patch
z.sm$elevation<-elev

z_g.sm<-gather(z.sm,key="patch",value="resist",-elevation)

g2<-ggplot(z_g.sm, aes( as.numeric(patch), elevation, z = resist)) +geom_raster(aes(fill=resist)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(-1,1))+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Resistance") + ggtitle("Resistance\n(Hot Year)") + theme(text=element_text(size=16))
g2
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.resistence15.elev.vs.patcharea.pdf"),width=8,height=7)

#resistance 2016
tmp.tm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resistance16.delta2.confint.csv"))

z.tm<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.tm[i,j] <- tmp.tm[tmp.tm$Comparison=="rescale(elevation)","full"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.tm[tmp.tm$Comparison=="rescale(patcharea)","full"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) +
      tmp.tm[tmp.tm$Comparison=="rescale(elevation):rescale(patcharea)","full"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.tm)<-patch
z.tm$elevation<-elev

z_g.tm<-gather(z.tm,key="patch",value="resist",-elevation)

g3<-ggplot(z_g.tm, aes( as.numeric(patch), elevation, z = resist)) +geom_raster(aes(fill=resist)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(-5,3))+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Resistance") + ggtitle("Resistance\n(Dry Year)") + theme(text=element_text(size=16))
g3
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.resistence16.elev.vs.patcharea.pdf"),width=8,height=7)

#models for basal area leguminous trees and shade diversity
legume<-seq(as.integer(min(d.F.combo$BA.legume)),as.integer(max(d.F.combo$BA.legume)),by=0.30)
diversity<-seq(as.integer(min(d.F.combo$Shannon.i)),as.integer(max(d.F.combo$Shannon.i)),by=0.04)
z.legume<-attributes(scale(d.F.combo$BA.legume))
z.diversity<-attributes(scale(d.F.combo$Shannon.i))

#do for 2014 yields
s.14<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Coefficients"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Coefficients"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
      tmp.14[tmp.14$Comparison=="rescale(Shannon.i):rescale(BA.legume)","Coefficients"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.14)<-diversity
s.14$legume<-legume

s_g.14<-gather(s.14,key="diversity",value="yld",-legume)

g4<-ggplot(s_g.14, aes( as.numeric(diversity), legume, z = yld)) +geom_raster(aes(fill=yld)) +
  scale_fill_viridis_c(limits=c(-0.5,1.0)) + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Yield") + ggtitle("") + theme(text=element_text(size=16))
g4

#do for resilience
#s.rm<-data.frame()
#for(i in 1:length(legume)){
#  for(j in 1:length(diversity)){
#    s.rm[i,j] <- tmp.rm[tmp.rm$Comparison=="rescale(BA.legume)","full"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.rm[tmp.rm$Comparison=="rescale(Shannon.i)","full"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
#      tmp.rm[tmp.rm$Comparison=="rescale(BA.legume):rescale(Shannon.i)","full"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
#  }
#}
#colnames(s.rm)<-diversity
#s.rm$legume<-legume

#s_g.rm<-gather(s.rm,key="diversity",value="res",-legume)

#g5<-ggplot(s_g.rm, aes( as.numeric(diversity), legume, z = res)) +geom_raster(aes(fill=res)) +
#  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
#  labs(fill="Resilience") + ggtitle("") + theme(text=element_text(size=16))
#g5

#do for resistance 2015
s.sm<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.sm[i,j] <- tmp.sm[tmp.sm$Comparison=="rescale(BA.legume)","full"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.sm[tmp.sm$Comparison=="rescale(Shannon.i)","full"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.sm)<-diversity
s.sm$legume<-legume

s_g.sm<-gather(s.sm,key="diversity",value="resist",-legume)

g5<-ggplot(s_g.sm, aes( as.numeric(diversity), legume, z = resist)) +geom_raster(aes(fill=resist)) +
  scale_fill_viridis_c(limits=c(-1,1)) + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Resistance") + ggtitle("") + theme(text=element_text(size=16))
g5

#do for resistance 2016
s.tm<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.tm[i,j] <- tmp.tm[tmp.tm$Comparison=="rescale(BA.legume)","full"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.tm[tmp.tm$Comparison=="rescale(Shannon.i)","full"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
      tmp.tm[tmp.tm$Comparison=="rescale(BA.legume):rescale(Shannon.i)","full"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.tm)<-diversity
s.tm$legume<-legume

s_g.tm<-gather(s.tm,key="diversity",value="resist",-legume)

g6<-ggplot(s_g.tm, aes( as.numeric(diversity), legume, z = resist)) +geom_raster(aes(fill=resist)) +
  scale_fill_viridis_c(limits=c(-5,3)) + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Resistance") + ggtitle("") + theme(text=element_text(size=16))
g6

g7<-ggpubr::ggarrange(g1,g4,ncol=1,nrow=2,common.legend = T,legend="right",font.label = list(size = 18),heights=c(1.1,1))
g8<-ggpubr::ggarrange(g2,g5,ncol=1,nrow=2,common.legend = T,legend="right",font.label = list(size = 18),heights=c(1.1,1))
g9<-ggpubr::ggarrange(g3,g6,ncol=1,nrow=2,common.legend = T,legend="right",font.label = list(size = 18),heights=c(1.1,1))
#g8<-ggpubr::ggarrange(g2,g3,g5,g6,ncol=2,nrow=2,common.legend = T,legend="right",font.label = list(size = 18),heights=c(1.1,1))

ggpubr::ggarrange(g7,g8,g9,nrow=1,ncol=3,align="hv",widths=c(1,1.1,1.1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Model.yld.resilience.resistance.landmanagement.pdf",height=6,width=12)

#modeling drivers of group1, group2, group3
d.F.new <- read.csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/FarmResistanceGroups.csv")
d.F.new.1<-d.F.new %>% filter(year==2014&!is.na(group1))

library(arm)
library(MuMIn)
#group1
m1<-glm(group1~rescale(elevation)*rescale(patcharea) + rescale(BA.legume)*rescale(Shannon.i),data=d.F.new.1,
        family=quasibinomial(link="logit"))
summary(m1)

m1b<-glm(group1~rescale(elevation) + rescale(patcharea) + rescale(BA.legume)*rescale(Shannon.i),data=d.F.new.1,
        family=quasibinomial(link="logit"))
summary(m1b)

model_global <- glm(group1~elevation*patcharea + BA.legume*Shannon.i,data=d.F.new.1,
                     family=quasibinomial(link="logit"))
model_1 <-  glm(group1~elevation+patcharea + BA.legume*Shannon.i,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_2 <-  glm(group1~BA.legume*Shannon.i,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_3 <-  glm(group1~BA.legume+Shannon.i,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_4 <-  glm(group1~elevation*patcharea,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_5 <-  glm(group1~elevation + patcharea,data=d.F.new.1,
                family=quasibinomial(link="logit"))

model_null <-  glm(group1~1, data=d.F.new.1,
                   family=quasibinomial(link="logit"))

anova(model_null,model_global, model_1,model_2,model_3,model_4,model_5, test="Chisq")

anova(model_global, model_2,model_3,model_4, test="Chisq")
#This calculates R2
1-model_global$deviance/model_global$null.deviance

#group2
m2<-glm(group2~rescale(elevation)*rescale(patcharea) + rescale(BA.legume)*rescale(Shannon.i),data=d.F.new.1,
        family=quasibinomial(link="logit"))
summary(m2)

m2b<-glm(group2~rescale(elevation)+rescale(patcharea) + rescale(BA.legume)*rescale(Shannon.i),data=d.F.new.1,
        family=quasibinomial(link="logit"))
summary(m2b)

model_globalb <- glm(group2~elevation*patcharea + BA.legume*Shannon.i,data=d.F.new.1,
                    family=quasibinomial(link="logit"))
model_1b <-  glm(group2~elevation+patcharea + BA.legume*Shannon.i,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_2b <-  glm(group2~BA.legume*Shannon.i,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_3b <-  glm(group2~BA.legume+Shannon.i,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_4b <-  glm(group2~elevation*patcharea,data=d.F.new.1,
                family=quasibinomial(link="logit"))
model_5b <-  glm(group2~elevation + patcharea,data=d.F.new.1,
                family=quasibinomial(link="logit"))

model_nullb <-  glm(group2~1, data=d.F.new.1,
                   family=quasibinomial(link="logit"))

anova(model_nullb,model_globalb, model_1b,model_2b,model_3b,model_4b,model_5b, test="Chisq")

anova(model_global, model_2,model_3,model_4, test="Chisq")
#This calculates R2
1-model_globalb$deviance/model_globalb$null.deviance

#group3
m3<-glm(group3~rescale(elevation)*rescale(patcharea) + rescale(BA.legume)*rescale(Shannon.i),data=d.F.new.1,
        family=quasibinomial(link="logit"))
summary(m3)

m3b<-glm(group3~rescale(elevation)*rescale(patcharea) +rescale(Shannon.i),data=d.F.new.1,
         family=quasibinomial(link="logit"))
summary(m3b)

model_globalc <- glm(group3~elevation*patcharea + BA.legume*Shannon.i,data=d.F.new.1,
                     family=quasibinomial(link="logit"))
model_1c <-  glm(group3~elevation+patcharea + BA.legume*Shannon.i,data=d.F.new.1,
                 family=quasibinomial(link="logit"))
model_2c <-  glm(group3~BA.legume*Shannon.i,data=d.F.new.1,
                 family=quasibinomial(link="logit"))
model_3c <-  glm(group3~BA.legume+Shannon.i,data=d.F.new.1,
                 family=quasibinomial(link="logit"))
model_4c <-  glm(group3~elevation*patcharea,data=d.F.new.1,
                 family=quasibinomial(link="logit"))
model_5c <-  glm(group3~elevation + patcharea,data=d.F.new.1,
                 family=quasibinomial(link="logit"))

model_nullc <-  glm(group3~1, data=d.F.new.1,
                    family=quasibinomial(link="logit"))

anova(model_nullc,model_globalc, model_1c,model_2c,model_3c,model_4c,model_5c, test="Chisq")

#This calculates R2
1-model_globalc$deviance/model_globalc$null.deviance

#landscape
patch<-seq(as.integer(min(d.F.new.1$patcharea)),as.integer(max(d.F.new.1$patcharea)),by=15)
#z.patch<-attributes(scale(d.F.new.1$patcharea))
elev<-seq(as.integer(min(d.F.new.1$elevation)),as.integer(max(d.F.new.1$elevation)),by=4)
#z.elev<-attributes(scale(d.F.new.1$elevation))
m_legume=mean(d.F.new.1$BA.legume)
m_diversity=mean(d.F.new.1$Shannon.i)

z.1<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(model_global, newdata=data.frame(patcharea=patch[j],
                                         elevation=elev[i],
                                         BA.legume=m_legume,Shannon.i=m_diversity), type="response", se.fit=TRUE)
    z.1[i,j]<-pi.hat$fit  
    }
}
colnames(z.1)<-patch
z.1$elevation<-elev

z_g.1<-gather(z.1,key="patch",value="prob",-elevation)

b1<-ggplot(z_g.1, aes( as.numeric(patch), elevation, z = prob)) +geom_raster(aes(fill=prob)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1),breaks=seq(0,1, by=0.5))+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Group 1") + theme(text=element_text(size=16))
b1

legume<-seq(as.integer(min(d.F.new.1$BA.legume)),as.integer(max(d.F.new.1$BA.legume)),by=0.30)
diversity<-seq(as.integer(min(d.F.new.1$Shannon.i)),as.integer(max(d.F.new.1$Shannon.i)),by=0.04)
#z.legume<-attributes(scale(d.F.new.1$BA.legume))
#z.diversity<-attributes(scale(d.F.new.1$Shannon.i))
m_patch=mean(d.F.new.1$patcharea)
m_elev=mean(d.F.new.1$elevation)

#do for group1
s.1<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    pi.hat = predict.glm(model_global, newdata=data.frame(patcharea=m_patch,
                                                 elevation=m_elev,
                                                 BA.legume=legume[i],
                                                 Shannon.i=diversity[j]), type="response", se.fit=TRUE)
    s.1[i,j]<-pi.hat$fit  
    }
}
colnames(s.1)<-diversity
s.1$legume<-legume

s_g.1<-gather(s.1,key="diversity",value="prob",-legume)

b2<-ggplot(s_g.1, aes( as.numeric(diversity), legume, z = prob)) +geom_raster(aes(fill=prob)) +
  scale_fill_viridis_c(limits=c(0,1),breaks=seq(0,1, by=0.5)) + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Probability") + ggtitle("") + theme(text=element_text(size=16))
b2

#group 2
z.2<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(model_globalb, newdata=data.frame(patcharea=patch[j],
                                                          elevation=elev[i],
                                                          BA.legume=m_legume,Shannon.i=m_diversity), type="response", se.fit=TRUE)
    z.2[i,j]<-pi.hat$fit  
  }
}
colnames(z.2)<-patch
z.2$elevation<-elev

z_g.2<-gather(z.2,key="patch",value="prob",-elevation)

b3<-ggplot(z_g.2, aes( as.numeric(patch), elevation, z = prob)) +geom_raster(aes(fill=prob)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1),breaks=seq(0,1, by=0.5))+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Group 2") + theme(text=element_text(size=16))
b3

s.2<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    pi.hat = predict.glm(model_globalb, newdata=data.frame(patcharea=m_patch,
                                                          elevation=m_elev,
                                                          BA.legume=legume[i],
                                                          Shannon.i=diversity[j]), type="response", se.fit=TRUE)
    s.2[i,j]<-pi.hat$fit  
  }
}
colnames(s.2)<-diversity
s.2$legume<-legume

s_g.2<-gather(s.2,key="diversity",value="prob",-legume)

b4<-ggplot(s_g.2, aes( as.numeric(diversity), legume, z = prob)) +geom_raster(aes(fill=prob)) +
  scale_fill_viridis_c(limits=c(0,1),breaks=seq(0,1, by=0.5)) + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Probability") + ggtitle("") + theme(text=element_text(size=16))
b4

#group 3
z.3<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(model_globalc, newdata=data.frame(patcharea=patch[j],
                                                           elevation=elev[i],
                                                           BA.legume=m_legume,Shannon.i=m_diversity), type="response", se.fit=TRUE)
    z.3[i,j]<-pi.hat$fit  
  }
}
colnames(z.3)<-patch
z.3$elevation<-elev

z_g.3<-gather(z.3,key="patch",value="prob",-elevation)

b5<-ggplot(z_g.3, aes( as.numeric(patch), elevation, z = prob)) +geom_raster(aes(fill=prob)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1),breaks=seq(0,1, by=0.5))+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Group 3") + theme(text=element_text(size=16))
b5

s.3<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    pi.hat = predict.glm(model_globalc, newdata=data.frame(patcharea=m_patch,
                                                           elevation=m_elev,
                                                           BA.legume=legume[i],
                                                           Shannon.i=diversity[j]), type="response", se.fit=TRUE)
    s.3[i,j]<-pi.hat$fit  
  }
}
colnames(s.3)<-diversity
s.3$legume<-legume

s_g.3<-gather(s.3,key="diversity",value="prob",-legume)

b6<-ggplot(s_g.3, aes( as.numeric(diversity), legume, z = prob)) +geom_raster(aes(fill=prob)) +
  scale_fill_viridis_c(limits=c(0,1),breaks=seq(0,1, by=0.5)) + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Probability") + ggtitle("") + theme(text=element_text(size=16))
b6

ggpubr::ggarrange(b1,b3,b5,b2,b4,b6,ncol=3,nrow=2,common.legend=T,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/GroupComparisons.pdf",height=7,width=12)
