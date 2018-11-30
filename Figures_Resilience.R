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
  ggtitle("Shrub Yields of Fresh Cherries")+
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
  ggtitle("Per Hectare Yields of Fresh Cherries")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 14)
                        ,legend.key = element_blank()
                        ,legend.position="bottom"
                        ,legend.title = element_blank())
g3<-ggplot(dF.2,aes(income.2016,color="2016")) + geom_freqpoly(binwidth=100,size=1) + xlab("Income [US$]") + ylab("") +
  geom_freqpoly(data=dF.2,binwidth=100,aes(income.2015,color="2015"),size=1) + geom_freqpoly(data=dF.2,binwidth=100, aes(income.2014,color="2014"),size=1) +
  ggtitle("Total Coffee Income")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 14)
                        ,legend.key = element_blank()
                        ,legend.position="bottom"
                        ,legend.title = element_blank())


ggarrange(g1,g2,g3,ncol=3,nrow=1,common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/ShrubYieldIncome_Comparison.pdf",height=4,width=13)

#log ratio vs dependence on coffee income
dF.summ<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

g1<-ggplot(dF.summ %>% filter(low.yield.bin==1), aes(log.tot,Coffee.income.percent)) + geom_point(aes(color=factor(vulnerable))) +
  #xlim(-1,1.5) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=75),fill="orange",alpha=1/500) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=75,ymax=Inf),fill="red",alpha=1/500) +
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative Income Loss\n(Low Yielding Farms)") +
  labs(color="Vulnerable\nFarms") + scale_color_grey()

g2<-ggplot(dF.summ %>% filter(low.yield.bin==0), aes(log.tot,Coffee.income.percent)) + geom_point(aes(color=factor(vulnerable))) +
  #xlim(-1,1.5) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=75),fill="orange",alpha=1/500) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=75,ymax=Inf),fill="red",alpha=1/500) +
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative Income Loss\n(Remaining Farms)") +
  labs(color="Vulnerable\nFarms") + scale_color_grey()
ggarrange(g1,g2,ncol=2,nrow=1,common.legend = T,labels="auto")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/PropCumDiffIncome.Vulnerable.PercentIncomeCoffee.pdf",height=5,width=10)


#see if elevation and patch area predict low yielding plots?
dF.summ<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

elev<-seq(as.integer(min(dF.summ$elevation)),as.integer(max(dF.summ$elevation)),by=1)
patch<-seq(as.integer(min(dF.summ$patcharea)),as.integer(max(dF.summ$patcharea)),by=5)
z.elev<-attributes(scale(dF.summ$elevation))
z.patch<-attributes(scale(dF.summ$patcharea))

#library(arm)
tm<-glm(low.yield.bin~patcharea*elevation,data=dF.summ,family=quasibinomial(link="logit"))

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.AllYears.landscape.txt")
summary(tm)
sink()

#shade diversity and leguminous trees do not predict low yielding farms
sm<-glm(low.yield.bin~arm::rescale(Shannon.i)*arm::rescale(BA.legume),data=dF.summ,family=quasibinomial(link="logit"))
summary(sm)

#coffee land area predict low yielding farms
clm<-glm(low.yield.bin~arm::rescale(coffee.area.ha),data=dF.summ,family=quasibinomial(link="logit"))
summary(clm)

#for lowyield and vulnerability
z.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm, data.frame(patcharea=patch[j],elevation=elev[i]),
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

pv1<-glm(vulnerable~poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.landscape.txt")
summary(pv1)
sink()

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

