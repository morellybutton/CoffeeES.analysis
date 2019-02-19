#script for producing final figures of paper

library(tidyverse)
library(AICcmodavg)
library(ggpubr)
setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

#create figure of yield collapse, histogram for each year
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
dF.2<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

g1<-ggplot(dF.1[dF.1$Shrub.kg>0&dF.1$year==2014,],aes(Shrub.kg,color="2014")) + geom_freqpoly(binwidth=0.1,size=1) + xlab("Yield [kg/shrub]") + ylab("Number of Farms")+
  geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year=="2015",],binwidth=0.05,aes(color="2015"),size=1)+geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year==2016,],binwidth=0.05,aes(color="2016"),size=1)+
  ggtitle("Shrub Yields of\nFresh Cherries")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    ,legend.title = element_blank())
#
#calculate annual harvests (total)
dF.1$est.yield<-dF.1$Shrub.kg*dF.1$density
g2<-ggplot(dF.1 %>% filter(year==2014),aes(est.yield,color="2014")) + geom_freqpoly(binwidth=75,size=1) + xlab("Yield [kg/ha]") + ylab("")+
  geom_freqpoly(data=dF.1 %>% filter(year==2015),binwidth=50,aes(est.yield,color="2015"),size=1)+geom_freqpoly(data=dF.1 %>% filter(year==2016),binwidth=50, aes(est.yield,color="2016"),size=1) +
  ggtitle("Per Hectare Yields\nof Fresh Cherries")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 14)
                        ,legend.key = element_blank()
                        ,legend.position="bottom"
                        ,legend.title = element_blank())
g3<-ggplot(dF.2,aes(income.2016,color="2016")) + geom_freqpoly(binwidth=100,size=1) + xlab("Income [US$]") + ylab("") +
  geom_freqpoly(data=dF.2,binwidth=100,aes(income.2015,color="2015"),size=1) + geom_freqpoly(data=dF.2,binwidth=100, aes(income.2014,color="2014"),size=1) +
  ggtitle("Total Coffee\nIncome")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 14)
                        ,legend.key = element_blank()
                        ,legend.position="bottom"
                        ,legend.title = element_blank())


ggarrange(g1,g2,g3,ncol=3,nrow=1,common.legend=T,labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/ShrubYieldIncome_Comparison.pdf",height=4,width=13)

#calculate median and max values for all farms
shrub.kg<-dF.1 %>% group_by(year) %>% add_tally() %>% select(year,Shrub.kg,n)  %>% summarise_all(funs(median,max,sd),na.rm=T) %>% select(-n_max,-n_sd) %>% 
  rename(shrub.median=Shrub.kg_median,shrub.max=Shrub.kg_max,n=n_median,shrub.sd=Shrub.kg_sd) %>% mutate(shrub.ci=1.96*shrub.sd/sqrt(n))
farm.kg<-dF.2 %>% select(yield.2014,yield.2015,yield.2016,harvest.kg.2014,harvest.kg.2015,harvest.kg.2016,income.2014,income.2015,income.2016) %>% 
  summarise_all(funs(min,median,max,sd),na.rm=T) %>% gather(key="variable",value="value") 
farm.kg$year<-2016
farm.kg$year[str_detect(farm.kg$variable,"2014")]<-2014
farm.kg$year[str_detect(farm.kg$variable,"2015")]<-2015
farm.kg$category<-"per.ha"
farm.kg$category[str_detect(farm.kg$variable,"harvest")]<-"total"
farm.kg$category[str_detect(farm.kg$variable,"income")]<-"income"
farm.kg$type<-"median"
farm.kg$type[str_detect(farm.kg$variable,"min")]<-"min"
farm.kg$type[str_detect(farm.kg$variable,"max")]<-"max"
farm.kg$type[str_detect(farm.kg$variable,"sd")]<-"sd"
farm.kg$variable1<-paste(farm.kg$category,farm.kg$type,sep=".")
farm.kg$nobs<-78
farm.ci<-farm.kg %>% filter(type=="sd") %>% mutate(ci=1.96*value/sqrt(nobs)) %>% select(-variable1,-variable,-type,-nobs,-value)
farm.ci$type<-"median"

farm.kg<-left_join(farm.kg,farm.ci,by=c("year","type","category"))

farm.kg<-farm.kg %>% select(year,value,type,category,ci,nobs) %>% spread(key="category",value="value")

farm.kg<-left_join(farm.kg,shrub.kg,by="year")
write.csv(farm.kg,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/SummaryofYields.csv")

#calculate median and maximum percent of income values
dF.summ<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

median(dF.summ$prop.income.2015,na.rm=T)
1.96*sd(dF.summ$prop.income.2015,na.rm=T)/sqrt(nrow(dF.summ))

median(dF.summ$prop.income.2016,na.rm=T)
1.96*sd(dF.summ$prop.income.2016,na.rm=T)/sqrt(nrow(dF.summ))

#calculate number of low yielding farms
dF.summ<-left_join(dF.summ %>% select(-low.yield.bin),dF.1 %>% filter(year==2014) %>% select(Plot,low.yield1415.bin,low.yield1516.bin,low.yield1416.bin,low.yield.bin),by="Plot")
dF.summ<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T)
dF.summ<-left_join(dF.summ %>% select(-low.yield14.bin),dF.1 %>% filter(year==2014) %>% select(Plot,low.yield14.bin),by="Plot")
dF.summ<-left_join(dF.summ %>% select(-low.yield15.bin),dF.1 %>% filter(year==2015) %>% select(Plot,low.yield15.bin),by="Plot")
dF.summ<-left_join(dF.summ %>% select(-low.yield16.bin),dF.1 %>% filter(year==2016) %>% select(Plot,low.yield16.bin),by="Plot")

low.yield<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T) %>% pull(low.yield.bin) %>% sum()
low.yield1415<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T) %>% pull(low.yield1415.bin) %>% sum()
low.yield1416<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T) %>% pull(low.yield1416.bin) %>% sum()
low.yield1516<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T) %>% pull(low.yield1516.bin) %>% sum()

dF.summ<-dF.summ  %>% group_by(Plot,Coffee.income) %>% mutate(sum.years=sum(low.yield1516.bin,low.yield1415.bin,low.yield1416.bin,na.rm=T),all.years=0) %>% 
  mutate(all.years=replace(all.years,sum.years==3,1)) %>% ungroup()

low.yield.all<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T) %>% pull(all.years) %>% sum()

#look at proportional increase in income relative to income quartile

TukeyHSD(aov(prop.income.2015~factor(Coffee.income.quartile),data=dF.summ))

#create column for farmers that are both vulnerable and consistently low yielding
dF.summ <- dF.summ %>% group_by(Plot,Coffee.income) %>% mutate(vul.low=0) %>% mutate(vul.low=replace(vul.low,vulnerable==1&low.yield.bin==1,1))
#how many?
dF.summ %>% pull(vul.low) %>% sum()

g1<-ggplot(dF.summ, aes(log.tot,Coffee.income.percent)) + geom_point(aes(shape=factor(vulnerable),color=factor(low.yield.bin))) +
  scale_shape_manual(values=c(1, 19)) +labs(colour="Low\nYielding",shape="Vulnerable")+
  scale_color_viridis_d()+theme_classic() +theme(text=element_text(size=14),legend.position="bottom") + 
   geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative Income Loss")


#see if diversification more common for low yielding plots
pv2<-glm(income.sources~all.years+patcharea,data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.1<-glm(income.sources~all.years+patcharea+elevation,data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.2<-glm(income.sources~all.years+elevation*patcharea,data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.3<-glm(income.sources~all.years+patcharea+poly(elevation,2),data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.4<-glm(income.sources~all.years+poly(elevation,2),data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.5<-glm(income.sources~all.years+elevation,data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.6<-glm(income.sources~all.years,data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
pv2.null<-glm(income.sources~1,data=dF.summ,family=inverse.gaussian(link = "1/mu^2"))
anova(pv2.null,pv2,pv2.1,pv2.2,pv2.3,pv2.4,pv2.5,pv2.6,test="Chisq")

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Diversify.landscape.lowyield.txt")
summary(pv2)
sink()
#to get r2 do 1-(Residual deviance/Null Deviance)
1-pv2$deviance/pv2$null.deviance

pv3<-glm(all.years~income.sources+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Diversify.landscape.lowyield.txt")
summary(pv3)
sink()

patch<-seq(as.integer(min(dF.summ$patcharea)),as.integer(max(dF.summ$patcharea)),by=5)
z.patch<-attributes(scale(dF.summ$patcharea))
low.yield<-seq(0,1,by=0.0025)

z.i<-data.frame()
for(i in 1:length(low.yield)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(pv2, data.frame(patcharea=patch[j],all.years=low.yield[i]),
                         type="response", se.fit=TRUE)
    z.i[i,j]<-pi.hat$fit  }
}
colnames(z.i)<-patch
z.i$all.years<-low.yield

z_g.i<-gather(z.i,key="patch",value="income.sources",-all.years)

g2<-ggplot(z_g.i, aes( as.numeric(patch), all.years, z = income.sources)) +geom_raster(aes(fill=income.sources)) +
  scale_fill_viridis_c()+ theme_classic() + ylab("Probability of Being Low Yielding") + xlab("Patch Area [ha]")+
  labs(fill="Number of\nIncome Sources") + theme(text=element_text(size=14),legend.position="bottom") + ggtitle("Diversified Livelihoods")

ggarrange(g1,g2,ncol=2,nrow=1,common.legend = F,labels="auto",align="v",widths=c(1.25,1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/PropCumDiffIncome.Vulnerable.PercentIncomeCoffee.pdf",height=5,width=9)


#see if elevation and patch area predict low yielding plots?
#dF.summ<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))
#dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#dF.summ<-left_join(dF.summ %>% select(-low.yield.bin),dF.1 %>% select(Plot,low.yield.bin,low.yield1415.bin,low.yield1516.bin,low.yield1416.bin,all.years),by="Plot")
#dF.summ<-dF.summ %>% distinct(Plot,.keep_all=T)


elev<-seq(as.integer(min(dF.summ$elevation)),as.integer(max(dF.summ$elevation)),by=1)
patch<-seq(as.integer(min(dF.summ$patcharea)),as.integer(max(dF.summ$patcharea)),by=5)
z.elev<-attributes(scale(dF.summ$elevation))
z.patch<-attributes(scale(dF.summ$patcharea))

#library(arm)
tm<-glm(all.years~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm.1<-glm(all.years~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm.2<-glm(all.years~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm,tm.1,tm.2)


tm2<-glm(low.yield.bin~elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
tm2.1<-glm(low.yield.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm2.2<-glm(low.yield.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm2,tm2.1,tm2.2)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.TwoYears.landscape.txt")
summary(tm2)
sink()

tm15.null<-glm(low.yield15.bin~1,data=dF.summ,family=quasibinomial(link="logit"))
tm15<-glm(low.yield15.bin~patcharea+elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm15.1<-glm(low.yield15.bin~patcharea*elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm15.2<-glm(low.yield15.bin~patcharea+poly(elevation,2)+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm15.3<-glm(low.yield15.bin~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm15.4<-glm(low.yield15.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm15.5<-glm(low.yield15.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
tm15.6<-glm(low.yield15.bin~patcharea+elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm15.7<-glm(low.yield15.bin~patcharea*elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm15.8<-glm(low.yield15.bin~patcharea+poly(elevation,2)+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm15.9<-glm(low.yield15.bin~elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm15.10<-glm(low.yield15.bin~elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm15.11<-glm(low.yield15.bin~poly(elevation,2)+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm15.null,tm15,tm15.1,tm15.2,tm15.3,tm15.4,tm15.5,tm15.6,tm15.7,tm15.8,tm15.9,tm15.10,tm15.11,test="Chisq")
anova(tm15.null,tm15,tm15.3,tm15.11,test="Chisq")

summary(tm15)
1-tm15$deviance/tm15$null.deviance

summary(tm15.3)
1-tm15.3$deviance/tm15.3$null.deviance

summary(tm15.5)
1-tm15.5$deviance/tm15.5$null.deviance

plot(allEffects(tm15))
plot(allEffects(tm15.3))
plot(allEffects(tm15.5))
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.2015.landscape.txt")
summary(tm15)
sink()

n1<-plot(allEffects(tm15)[[3]],ylab="Probability Low Yielding\n(2015)",xlab="",main="", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
n2<-plot(allEffects(tm15)[[2]],ylab="",xlab="",main="", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
n3<-plot(allEffects(tm15)[[1]],ylab="",xlab="Patch Area [ha]",main="", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
class(n1) <- class(n2) <- class(n3) <- "trellis"

n15<-gridExtra::grid.arrange(n1, n2, n3, ncol=3,nrow=1)


tm16.null<-glm(low.yield16.bin~1,data=dF.summ,family=quasibinomial(link="logit"))
tm16<-glm(low.yield16.bin~patcharea+elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm16.1<-glm(low.yield16.bin~patcharea*elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm16.2<-glm(low.yield16.bin~patcharea+poly(elevation,2)+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm16.3<-glm(low.yield16.bin~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm16.4<-glm(low.yield16.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm16.5<-glm(low.yield16.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
tm16.6<-glm(low.yield16.bin~patcharea+elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm16.7<-glm(low.yield16.bin~patcharea*elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm16.8<-glm(low.yield16.bin~patcharea+poly(elevation,2)+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm16.9<-glm(low.yield16.bin~elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm16.10<-glm(low.yield16.bin~elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm16.11<-glm(low.yield16.bin~poly(elevation,2)+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm16.null,tm16,tm16.1,tm16.2,tm16.3,tm16.4,tm16.5,tm16.6,tm16.7,tm16.8,tm16.9,tm16.10,tm16.11,test="Chisq")
anova(tm16.null,tm16.6,tm16.10,tm16.11,test="Chisq")

summary(tm16.6)
1-tm16.6$deviance/tm16.6$null.deviance

summary(tm16.11)
1-tm16.11$deviance/tm16.11$null.deviance

n4<-plot(allEffects(tm16.11)[[2]],ylab="Probability Low Yielding\n(2016)",xlab="Canopy Gap [%]",main="", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
n5<-plot(allEffects(tm16.11)[[1]],ylab="",xlab="Elevation [m]",main="" ,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
#n3<-plot(allEffects(tm16.11)[[1]],ylab="",xlab="",main="")
class(n4) <- class(n5)  <- "trellis"

n16<-gridExtra::grid.arrange(n4, n5, ncol=3,nrow=1)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.2016.landscape.txt")
summary(tm16.11)
sink()

tm14.null<-glm(low.yield14.bin~1,data=dF.summ,family=quasibinomial(link="logit"))
tm14<-glm(low.yield14.bin~patcharea+elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm14.1<-glm(low.yield14.bin~patcharea*elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm14.2<-glm(low.yield14.bin~patcharea+poly(elevation,2)+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm14.3<-glm(low.yield14.bin~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm14.4<-glm(low.yield14.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm14.5<-glm(low.yield14.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
tm14.6<-glm(low.yield14.bin~patcharea+elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm14.7<-glm(low.yield14.bin~patcharea*elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm14.8<-glm(low.yield14.bin~patcharea+poly(elevation,2)+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm14.9<-glm(low.yield14.bin~elevation+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))
tm14.10<-glm(low.yield14.bin~elevation+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
tm14.11<-glm(low.yield14.bin~poly(elevation,2)+poly(GapDry,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm14.null,tm14,tm14.1,tm14.2,tm14.3,tm14.4,tm14.5,tm14.6,tm14.7,tm14.8,tm14.9,tm14.10,tm14.11,test="Chisq")
anova(tm14.null,tm14,tm14.11,test="Chisq")

summary(tm14)
1-tm14$deviance/tm14$null.deviance

plot(allEffects(tm14))

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.2014.landscape.txt")
summary(tm14)
sink()

n6<-plot(allEffects(tm14)[[3]],ylab="Probability Low Yielding\n(2014)",xlab="",main="Canopy Gap Effect", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
n7<-plot(allEffects(tm14)[[2]],ylab="",xlab="",main="Elevation Effect", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
n8<-plot(allEffects(tm14)[[1]],ylab="",xlab="",main="Patch Area Effect", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
class(n6) <- class(n7) <- class(n8) <- "trellis"

n14<-gridExtra::grid.arrange(n6, n7, n8, ncol=3,nrow=1)

pdf("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Landscape.management.lowyielding.pdf",height=15,width=15)
gridExtra::grid.arrange(n14, n15, n16, ncol=1,nrow=3)
dev.off()

#do for two years
tm1415<-glm(low.yield1415.bin~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm1415.1<-glm(low.yield1415.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm1415.2<-glm(low.yield1415.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm1415,tm1415.1,tm1415.2)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.201415.landscape.txt")
summary(tm1415.1)
sink()

tm1516<-glm(low.yield1516.bin~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm1516.1<-glm(low.yield1516.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm1516.2<-glm(low.yield1516.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm1516,tm1516.1,tm1516.2)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.201516.landscape.txt")
summary(tm1516.2)
sink()

tm1416<-glm(low.yield1416.bin~patcharea+elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm1416.1<-glm(low.yield1416.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
tm1416.2<-glm(low.yield1416.bin~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))

anova(tm1416,tm1416.1,tm1416.2)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.201416.landscape.txt")
summary(tm1416.1)
sink()


#for lowyield (all years)
z.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm.2, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.ly[i,j]<-pi.hat$fit
  }
}
colnames(z.ly)<-patch
z.ly$elevation<-elev

z_g.ly<-gather(z.ly,key="patch",value="lowyld",-elevation)
t1<-ggplot(z_g.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms") + theme(text=element_text(size=14))

#for lowyield (2014)
z14.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm14.2, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z14.ly[i,j]<-pi.hat$fit
  }
}
colnames(z14.ly)<-patch
z14.ly$elevation<-elev

z_g14.ly<-gather(z14.ly,key="patch",value="lowyld",-elevation)
t3<-ggplot(z_g14.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms\n(2014)") + theme(text=element_text(size=14))

#for lowyield (2015)
z15.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm15.2, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z15.ly[i,j]<-pi.hat$fit
  }
}
colnames(z15.ly)<-patch
z15.ly$elevation<-elev

z_g15.ly<-gather(z15.ly,key="patch",value="lowyld",-elevation)
t4<-ggplot(z_g15.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms\n(2015)") + theme(text=element_text(size=14))

#for lowyield (2014&2015)
z1415.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm1415.1, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z1415.ly[i,j]<-pi.hat$fit
  }
}
colnames(z1415.ly)<-patch
z1415.ly$elevation<-elev

z_g1415.ly<-gather(z1415.ly,key="patch",value="lowyld",-elevation)
t5<-ggplot(z_g1415.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms\n(2014 & 2015)") + theme(text=element_text(size=14))

#for lowyield (2015&2016)
z1516.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm1516.2, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z1516.ly[i,j]<-pi.hat$fit
  }
}
colnames(z1516.ly)<-patch
z1516.ly$elevation<-elev

z_g1516.ly<-gather(z1516.ly,key="patch",value="lowyld",-elevation)
t6<-ggplot(z_g1516.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms\n(2015 & 2016)") + theme(text=element_text(size=14))

ggarrange(t3,t4,t5,t6,common.legend = T,labels="auto",ncol=2,nrow=2,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Landscape.drivers.resilience.annual.pdf",width=8,height=7)

#vulnerable farmers
pv1<-glm(vulnerable~poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
pv<-glm(vulnerable~elevation*patcharea,data=dF.summ,family=quasibinomial(link="logit"))
pv.2<-glm(vulnerable~elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))

anova(pv1,pv,pv.2)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.landscape.txt")
summary(pv1)
sink()

pv<-glm(vulnerable~elevation*patcharea,data=dF.summ,family=quasibinomial(link="logit"))
pv.2<-glm(vulnerable~elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))

z.v<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(pv1, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.v[i,j]<-pi.hat$fit  }
}
colnames(z.v)<-patch
z.v$elevation<-elev

z_g.v<-gather(z.v,key="patch",value="vulnerability",-elevation)

t2<-ggplot(z_g.v, aes( as.numeric(patch), elevation, z = vulnerability)) +geom_raster(aes(fill=vulnerability)) +
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + theme(text=element_text(size=14)) + ggtitle("Vulnerable Farmers")
ggarrange(t1,t2,common.legend = T,labels="auto",ncol=2,nrow=1,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Landscape.drivers.resilience.vulnerability.pdf",width=11,height=5)

#model farms that are both vulnerable and consistently low yielding
vl<-glm(vul.low~elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
vl.1<-glm(vul.low~elevation*patcharea,data=dF.summ,family=quasibinomial(link="logit"))
vl.2<-glm(vul.low~poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))

anova(vl,vl.1,vl.2)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/VulnerabilityandLowYielding.landscape.txt")
summary(vl.2)
sink()

z.vly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(vl.2, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.vly[i,j]<-pi.hat$fit
  }
}
colnames(z.vly)<-patch
z.vly$elevation<-elev

z_g.vly<-gather(z.vly,key="patch",value="lowyld",-elevation)
t3<-ggplot(z_g.vly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding & Vulnerable") + theme(text=element_text(size=14))




pv2<-glm(vulnerable~Coffee.income.percent,data=dF.summ,family=quasibinomial(link="logit"))
#sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.landscape.txt")
summary(pv2)
sink()

pv3<-glm(log.tot~poly(elevation,2)+patcharea,data=dF.summ,family=gaussian(link = "identity"))
pv4<-glm(log.tot~elevation+patcharea,data=dF.summ,family=gaussian(link = "identity"))
pv5<-glm(log.tot~elevation*patcharea,data=dF.summ,family=gaussian(link = "identity"))

anova(pv3,pv4,pv5)

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.logratio.landscape.txt")
summary(pv3)
sink()


z.t<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(pv3, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.t[i,j]<-pi.hat$fit  }
}
colnames(z.t)<-patch
z.t$elevation<-elev

z_t.v<-gather(z.t,key="patch",value="income.log.ratio",-elevation)

ggplot(z_t.v, aes( as.numeric(patch), elevation, z = income.log.ratio)) +geom_raster(aes(fill=income.log.ratio)) +
  scale_fill_viridis_c(direction = -1)+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Log Ratio") + theme(text=element_text(size=14)) + ggtitle("Income Log Ratio")

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Landscape.drivers.logratio.pdf",width=6,height=5)

pv6<-glm(Coffee.income.percent~elevation*patcharea,data=dF.summ,family=gaussian)
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.coffeeincomeperc.landscape.txt")
summary(pv6)
sink()

pv7<-glm(Coffee.income.percent~elevation+patcharea,data=dF.summ,family=gaussian)
pv8<-glm(Coffee.income.percent~poly(elevation,2)+patcharea,data=dF.summ,family=gaussian)
anova(pv6,pv7,pv8)

z.p<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(pv4, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.p[i,j]<-pi.hat$fit  }
}
colnames(z.p)<-patch
z.p$elevation<-elev

z_p.v<-gather(z.p,key="patch",value="perc.income",-elevation)


pv5<-glm(Coffee.income.quartile~elevation+patcharea,data=dF.summ,family=gaussian)
#sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.landscape.txt")
summary(pv5)
sink()


#shade diversity and leguminous trees do not predict low yielding farms, but canopy gap?
sm<-glm(all.years~poly(GapDry,2)+elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(sm)
sm.1<-glm(all.years~poly(GapDry,2)+poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(sm.1)
sm.2<-glm(all.years~poly(GapDry,2)+elevation*patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(sm.2)
sm.3<-glm(all.years~GapDry*patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
summary(sm.3)
sm.4<-glm(all.years~poly(GapDry,2)+elevation,data=dF.summ,family=quasibinomial(link="logit"))
summary(sm.4)
sm.5<-glm(all.years~patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
sm.6<-glm(all.years~poly(GapDry,2)+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
sm.7<-glm(all.years~poly(GapDry,2)+elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(sm.7)

s.null<-glm(all.years~1,data=dF.summ,family=quasibinomial(link="logit"))

anova(s.null,sm,sm.1,sm.2,sm.3,sm.4,sm.5,sm.6,test="Chisq")
anova(s.null,sm,sm.1,sm.4,sm.5,test="Chisq")
#R2 of sm.1
1-sm.4$deviance/sm.4$null.deviance

library(effects)

plot(allEffects(sm))
plot(allEffects(sm.1))
plot(allEffects(sm.2))
plot(allEffects(sm.3))
plot(allEffects(sm.4))
plot(allEffects(sm.7))

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.AllYears.landscape.txt")
summary(sm.4)
sink()

elev<-seq(as.integer(min(dF.summ$elevation)),as.integer(max(dF.summ$elevation)),by=5)
shade<-seq(as.integer(min(dF.summ$GapDry)),as.integer(max(dF.summ$GapDry)),by=0.25)

z.s<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(shade)){
    pi.hat = predict.glm(sm.4, data.frame(GapDry=shade[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.s[i,j]<-pi.hat$fit  }
}
colnames(z.s)<-shade
z.s$elevation<-elev

z_s.v<-gather(z.s,key="shade",value="low.yld",-elevation)

t1<-ggplot(z_s.v, aes( as.numeric(shade), elevation, z = low.yld)) +geom_raster(aes(fill=low.yld)) +
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Canopy Gap [%]")+
  labs(fill="Probability") + theme(text=element_text(size=14)) + ggtitle("Low Yielding Farms")

#shade as a predictor of vulnerability
vm<-glm(vulnerable~poly(GapDry,2)+elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(vm)
vm.1<-glm(vulnerable~poly(GapDry,2)+poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.1)
vm.2<-glm(vulnerable~poly(GapDry,2)+elevation*patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.2)
vm.3<-glm(vulnerable~GapDry*patcharea+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.3)
vm.4<-glm(vulnerable~poly(GapDry,2)+poly(elevation,2),data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.4)
vm.5<-glm(vulnerable~GapDry+elevation+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.5)
vm.6<-glm(vulnerable~poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.6)
vm.7<-glm(vulnerable~poly(elevation,2)+patcharea+GapDry,data=dF.summ,family=quasibinomial(link="logit"))
summary(vm.7)

anova(vm,vm.1,vm.2,vm.3,vm.4,vm.5,vm.6,vm.7,test="Chisq")
anova(vm.1,vm.4,test="Chisq")

plot(allEffects(vm.1))
plot(allEffects(vm.4))
plot(allEffects(vm.6))
plot(allEffects(vm.7))

#R2 of sm.1
1-vm.4$deviance/vm.4$null.deviance

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerable.landscape.txt")
summary(vm.4)
sink()

z.sv<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(shade)){
    pi.hat = predict.glm(vm.4, data.frame(GapDry=shade[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.sv[i,j]<-pi.hat$fit  }
}
colnames(z.sv)<-shade
z.sv$elevation<-elev

z_sv.v<-gather(z.sv,key="shade",value="vul",-elevation)

t2<-ggplot(z_sv.v, aes(as.numeric(shade), elevation, z = vul)) +geom_raster(aes(fill=vul)) +
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Canopy Gap [%]")+
  labs(fill="Probability") + theme(text=element_text(size=14)) + ggtitle("Vulnerable Farmers")
ggarrange(t1,t2,ncol=2,nrow=1,common.legend = T,labels="auto",legend="right")

ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Landscape.management.lowvul.pdf",height=5,width=10)

#shade as a predictor of income loss
im<-glm(log.tot~poly(GapDry,2)+elevation+patcharea,data=dF.summ,family=gaussian(link="identity"))
summary(im)
im.1<-glm(log.tot~poly(GapDry,2)+poly(elevation,2)+patcharea,data=dF.summ,family=gaussian(link="identity"))
summary(im.1)
im.2<-glm(log.tot~poly(GapDry,2)+elevation*patcharea,data=dF.summ,family=gaussian(link="identity"))
summary(im.2)
im.2<-glm(log.tot~GapDry*patcharea+poly(elevation,2),data=dF.summ,family=gaussian(link="identity"))
summary(im.2)
im.3<-glm(log.tot~poly(GapDry,2)+poly(elevation,2),data=dF.summ,family=gaussian(link="identity"))
summary(im.3)
im.4<-glm(log.tot~GapDry+elevation+patcharea,data=dF.summ,family=gaussian(link="identity"))
summary(im.4)
im.5<-glm(log.tot~poly(elevation,2)+patcharea,data=dF.summ,family=gaussian(link="identity"))
summary(im.5)
im.null<-glm(log.tot~1,data=dF.summ,family=gaussian(link="identity"))

anova(im.null,im,im.1,im.2,im.3,im.4,im.5,test="Chisq")
anova(im,im.1,im.2,im.3,im.4,im.5,test="Chisq")
#R2 of sm.1
1-im.1$deviance/im.1$null.deviance

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerable.landscape.txt")
summary(im.1)
sink()

p1<-plot(allEffects(im.1)[[1]],ylab="Income Log Ratio",xlab="Canopy Gap [%]",main="Canopy Gap Effect")
p2<-plot(allEffects(im.1)[[2]],ylab="Income Log Ratio",xlab="Elevation [m]",main="Elevation Effect")
p3<-plot(allEffects(im.1)[[3]],ylab="Income Log Ratio",xlab="Patch Area [ha]",main="Patch Area Effect")
class(p1) <- class(p2) <- class(p3) <- "trellis"

pdf("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Landscape.management.incomeratio.pdf",height=5,width=15)
gridExtra::grid.arrange(p1, p2, p3, ncol=3,nrow=1)
dev.off()


#does patch area relate to canopy gap? nope
dm<-glm(GapDry~patcharea*elevation,data=dF.summ,family=gaussian(link="identity"))
summary(dm)
dm.1<-glm(GapDry~patcharea+elevation,data=dF.summ,family=gaussian(link="identity"))
summary(dm.1)
dm.2<-glm(GapDry~patcharea+poly(elevation,2),data=dF.summ,family=gaussian(link="identity"))
summary(dm.2)


#coffee land area predict low yielding farms
clm<-glm(low.yield.bin~arm::rescale(coffee.area.ha),data=dF.summ,family=quasibinomial(link="logit"))
summary(clm)

#landscape features predict low yielding and vulnerable
vlm<-glm(vul.low~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))
summary(vlm)

#dependence on coffee income as prediction of low yielding
plm<-glm(low.yield.bin~Coffee.income.percent,data=dF.summ,family=quasibinomial(link="logit"))
summary(plm)

#TerraClim Figures
terra_clim<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies.csv"))

#anomalies around year of study, with harvesting dates
harvest<-data_frame(c("2014-10-01","2015-10-01","2016-10-01"))
colnames(harvest)<-"harvest.date"
g1<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,precip_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Precipitation\nAnomaly [mm]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed",color="red")

g2<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Vapour Pressure Deficit\nAnomaly [kPa]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed",color="red")

g3<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,tmax_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Maximum Temperature\nAnomaly [C]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed",color="red")
ggarrange(g1,g2,g3,ncol=1,nrow=3,align="hv",labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/TerraClim.Anom.Comparison.pdf",height=10,width=10)

#need to compare measures to one large metstation in Doraani
library(lubridate)
met_ppt<-read_csv(paste0(getwd(),"/MetData/ECO_12_monthlyppt.csv"))
met_summ<-read_csv(paste0(getwd(),"/MetData/ECO_12_summary.csv"))
met_summ$month <- as.Date(paste(year(met_summ$day),month(met_summ$day),"01",sep="-"),format="%Y-%m-%d")
met_comp <- met_summ %>% group_by(month) %>% summarise(max_temp=mean(Tmax,na.rm=T),min_temp=mean(Tmin,na.rm=T),vpd=mean(VPDmax,na.rm=T))

#combining satellite and ground measurements for comparison
sat_anom<-terra_clim %>% filter(site=="B13")

met_comp<-met_comp %>% rename(Date=month,g.max_temp=max_temp,g.min_temp=min_temp,g.vpd=vpd)
met_comp<-left_join(met_comp,sat_anom %>% select(Date,vpd,tmax),by="Date")
met_ppt<-met_ppt %>% rename(Date=month)
met_ppt<-left_join(met_ppt,sat_anom %>% select(Date,ppt),by="Date")

#plot the measurements
lm_eqn<-lm(tmax~g.max_temp,data=met_comp)
g1<-met_comp %>% ggplot() + geom_point(aes(g.max_temp,tmax)) + theme_classic() + ylab("TerraClim Max T [C]") +
  xlab("Measured Max T [C]") + geom_smooth(aes(g.max_temp,tmax),method="lm") + 
  annotate("text",x=23,y=30,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn)$adj.r.squared,2)),parse=T)

#met_comp<-met_comp %>% mutate(g.vpd=g.vpd/10)
lm_eqn3<-lm(vpd~g.vpd,data=met_comp)
g3<-met_comp %>% ggplot() + geom_point(aes(g.vpd/10,vpd)) + theme_classic() + ylab("TerraClim VPD [kPa]") +
  xlab("Measured VPD [kPa]") + geom_smooth(aes(g.vpd/10,vpd),method="lm") + 
  annotate("text",x=1.0,y=2.0,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn3)$adj.r.squared,2)),parse=T)
#
lm_eqn4<-lm(ppt~Tppt,data=met_ppt)
g4<-met_ppt %>% ggplot() + geom_point(aes(Tppt,ppt)) + theme_classic() + ylab("TerraClim Precipitation [mm]") +
  xlab("Measured Precipitation [mm]") + geom_smooth(aes(Tppt,ppt),method="lm") +
  annotate("text",x=50,y=350,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn4)$adj.r.squared,2)),parse=T)

ggarrange(g1,g3,g4,ncol=3,nrow=1,labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/TerraClimvsGroundMeasures.pdf",width=9, height=3)

