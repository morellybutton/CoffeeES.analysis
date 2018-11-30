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

#create yield model figures, 2014 and yield diff for 2015 & 2016

#2014
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))
tmp.14<-tmp.14[!is.na(tmp.14$full),]

tmp.14$Comparison<-factor(tmp.14$Comparison,levels=tmp.14[order(tmp.14$Importance,decreasing=F),"Comparison"],
                          labels=c("Soil C:N","CanopyGap","Maximum Temperature\nAnomaly (Fruiting)","Patch Area","BA Legume:\nShade Diversity","Basal Area\nof Leguminous\nTrees","Shade Diversity","Elevation","(Intercept)"))

#add in interaction between basal area and shade diversity
tmp.14<-tmp.14 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#calculate average error and then confidence interval for interaction term
int_se<-cbind("BA Legume:\nShade Diversity",tmp.14 %>% filter(Comparison=="BA Legume:\nShade Diversity") %>% pull(Estimate),tmp.14 %>% summarise(Uncond.SE=mean(Uncond.SE,na.rm=T)))
names(int_se) <- c("Comparison","Estimate","Uncond.SE")
int_se <- int_se %>% mutate(Lower.CL=Estimate-1.96*Uncond.SE,Upper.CL=Estimate+1.96*Uncond.SE)

#add significance column
tmp.14$sig<-0
tmp.14<-tmp.14 %>% mutate(sig=replace(sig,Comparison=="CanopyGap"|Comparison=="Soil C:N"|Comparison=="Basal Area\nof Leguminous\nTrees",1))

#order by importance
tmp.14<-tmp.14[!is.na(tmp.14$Importance),]

g1<-ggplot(tmp.14, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + 
  geom_point(shape=15,size=5,aes(color=factor(sig)))+
  geom_point(data=int_se,aes(Comparison, Estimate), color="red",shape=15,size=5) +
  geom_errorbar(data=int_se,width=0.2,color="red",linetype="dashed") +
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield\n(2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 12),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Yieldmodel_2014.pdf",height=5,width=5)

#for 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.confint.csv"))
tmp.15<-tmp.15[!is.na(tmp.15$full),]

#for delta 2
tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
                          labels=c("Maximum\nTemperature (Fruiting)","Shade Diversity","Coffee Berry\nDisease\nIncidence","Basal Area\nof Leguminous\nTrees","Coffee Land Area","Patch Area","(Intercept)"))

#add in interaction between basal area and shade diversity
#tmp.15<-tmp.15 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#add significance column
tmp.15$sig<-1
tmp.15<-tmp.15 %>% mutate(sig=replace(sig,Comparison=="Patch Area",0))

#calculate average error and then confidence interval for interaction term
#int_se.15<-cbind("BA Legume:\nShade Diversity",tmp.15 %>% filter(Comparison=="BA Legume:\nShade Diversity") %>% pull(Estimate),tmp.15 %>% summarise(Uncond.SE=mean(Uncond.SE,na.rm=T)))
#names(int_se.15) <- c("Comparison","Estimate","Uncond.SE")
#int_se.15 <- int_se.15 %>% mutate(Lower.CL=Estimate-1.96*Uncond.SE,Upper.CL=Estimate+1.96*Uncond.SE)

#order by importance
tmp.15<-tmp.15[!is.na(tmp.15$Importance),]

g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield Difference\n(2015)")+
  #xlab("Variable [ranked by importance]")+
  #geom_point(data=int_se.15,aes(Comparison, Estimate), color="red",shape=15,size=5) +
  #geom_errorbar(data=int_se.15,width=0.2,color="red",linetype="dashed") +
  xlab("") +
  ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +scale_color_grey() +
  theme(text = element_text(size = 12)
        ,axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Yielddiffmodel_2015.pdf",height=5,width=5)

#for 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.confint.csv"))
#tmp.16<-tmp.16[!is.na(tmp.16$full),]

#for delta 6
tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
                          labels=c("Shade Diversity","Basal Area\nof Leguminous\nTrees","Elevation","Patch Area","BA Legume:\nShade Diversity","Intercept"))

#add in interaction between basal area and shade diversity
#tmp.16<-tmp.16 %>% mutate(Estimate=replace(Estimate,is.na(Estimate),subset[is.na(Estimate)]))

#add significance column
tmp.16$sig<-0
tmp.16<-tmp.16 %>% mutate(sig=replace(sig,Comparison=="Basal Area\nof Leguminous\nTrees"|Comparison=="Shade Diversity",1))

#calculate average error and then confidence interval for interaction term
#int_se.16<-cbind("BA Legume:\nShade Diversity",tmp.16 %>% filter(Comparison=="BA Legume:\nShade Diversity") %>% pull(Estimate),tmp.16 %>% summarise(Uncond.SE=mean(Uncond.SE,na.rm=T)))
#names(int_se.16) <- c("Comparison","Estimate","Uncond.SE")
#int_se.16 <- int_se.16 %>% mutate(Lower.CL=Estimate-1.96*Uncond.SE,Upper.CL=Estimate+1.96*Uncond.SE)

#order by importance
tmp.16<-tmp.16[!is.na(tmp.16$Importance),]

g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield Difference\n(2016)")+
  #xlab("Variable [ranked by importance]")+
  #geom_point(data=int_se.16,aes(Comparison, Estimate), color="red",shape=15,size=5) +
  #geom_errorbar(data=int_se.16,width=0.2,color="red",linetype="dashed") +
  xlab("")+ scale_color_grey() + 
  ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 12)
        ,axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Yielddiffmodel_2016.pdf",height=5,width=5)

#Surface plots of elevation vs patch area
library(ggpubr)
d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
d.F.new14<-d.F.new %>% filter(year==2014)
d.F.new15 <- d.F.new %>% filter(year==2015)
d.F.new16 <- d.F.new %>% filter(year==2016)

#3D figure of patcharea area and elevation
elev<-seq(as.integer(min(d.F.new15$elevation)),as.integer(max(d.F.new15$elevation)),by=1)
patch<-seq(as.integer(min(d.F.new15$patcharea)),as.integer(max(d.F.new15$patcharea)),by=5)
z.elev<-attributes(scale(d.F.new15$elevation))
z.patch<-attributes(scale(d.F.new15$patcharea))

#for 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.confint.csv"))

z.15<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.15[i,j] <- tmp.15[tmp.15$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) #+ tmp.15[tmp.15$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + 
      #tmp.15[tmp.15$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])# 
    }
}
colnames(z.15)<-patch
z.15$elevation<-elev

z_g.15<-gather(z.15,key="patch",value="yld_diff",-elevation)


g2<-ggplot(z_g.15, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Log Yield\nDifference") + ggtitle("Shrub Yield Difference (2015)") + theme(text=element_text(size=12))
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.elev.patcharea.2015.pdf",height=5,width=5)

#for 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.confint.csv"))
z.16<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.16[i,j] <- tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.16[tmp.16$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])# +
      #tmp.16[tmp.16$Comparison=="rescale(elevation):rescale(patcharea)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.16)<-patch
z.16$elevation<-elev

z_g.16<-gather(z.16,key="patch",value="yld_diff",-elevation)

g3<-ggplot(z_g.16, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Log Yield\nDifference") + ggtitle("Shrub Yield Difference (2016)") + theme(text=element_text(size=12))

#do for 2014 yields
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

g1<-ggplot(z_g.14, aes( as.numeric(patch), elevation, z = yld)) +geom_raster(aes(fill=yld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Shrub Yield (2014)") + theme(text=element_text(size=12))

#do for Leguminous Trees vs Shade Diversity
legume<-seq(as.integer(min(d.F.new15$BA.legume)),as.integer(max(d.F.new15$BA.legume)),by=0.30)
diversity<-seq(as.integer(min(d.F.new15$Shannon.i)),as.integer(max(d.F.new15$Shannon.i)),by=0.04)
z.legume<-attributes(scale(d.F.new15$BA.legume))
z.diversity<-attributes(scale(d.F.new15$Shannon.i))

#do for 2014 yields
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.confint.csv"))
s.14<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.14[tmp.14$Comparison=="rescale(Shannon.i)","Estimate"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
      tmp.14[tmp.14$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.14)<-diversity
s.14$legume<-legume

s_g.14<-gather(s.14,key="diversity",value="yld",-legume)

g4<-ggplot(s_g.14, aes( as.numeric(diversity), legume, z = yld)) +geom_raster(aes(fill=yld)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Yield") + ggtitle("Shrub Yield (2014)") + theme(text=element_text(size=12))

#do for 2015 yield difference
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.confint.csv"))
s.15<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.15[i,j] <- tmp.15[tmp.15$Comparison=="rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.15[tmp.15$Comparison=="rescale(Shannon.i)","Estimate"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) #+
#      tmp.15[tmp.15$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.15)<-diversity
s.15$legume<-legume

s_g.15<-gather(s.15,key="diversity",value="yld_diff",-legume)

g5<-ggplot(s_g.15, aes( as.numeric(diversity), legume, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Log Yield\nDifference") + ggtitle("Shrub Yield Difference (2015)") + theme(text=element_text(size=12))

#do for 2016 yield difference
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta6.confint.csv"))
s.16<-data.frame()
for(i in 1:length(legume)){
  for(j in 1:length(diversity)){
    s.16[i,j] <- tmp.16[tmp.16$Comparison=="rescale(BA.legume)","Estimate"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]]) + tmp.16[tmp.16$Comparison=="rescale(Shannon.i)","Estimate"]*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]]) +
      tmp.16[tmp.16$Comparison=="rescale(BA.legume):rescale(Shannon.i)","subset"]*(legume[i]-z.legume[[2]])/(2*z.legume[[3]])*(diversity[j]-z.diversity[[2]])/(2*z.diversity[[3]])
  }
}
colnames(s.16)<-diversity
s.16$legume<-legume

s_g.16<-gather(s.16,key="diversity",value="yld_diff",-legume)

g6<-ggplot(s_g.16, aes( as.numeric(diversity), legume, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Basal Area\nLeguminous Trees [m2]") + xlab("Shade Diversity [H]")+
  labs(fill="Log Yield\nDifference") + ggtitle("Shrub Yield Difference (2015)") + theme(text=element_text(size=12))

#yields
c1<-ggarrange(g1,g4,ncol=1,nrow=2,common.legend = T,legend="right",font.label = list(size = 13))
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.yield.elev.patcharea.pdf",height=7,width=4)

#yield differences
c2<-ggarrange(g2,g3,g5,g6,ncol=2,nrow=2,common.legend = T,legend="right",font.label = list(size = 13))

ggarrange(c1,c2,nrow=1,ncol=2,widths=c(1,2))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.yield.diffyld.elev.patcharea.pdf",height=6,width=12)

#take difference between 2015 and 2016
#z_g.combo<-left_join(z_g.15,z_g.16,by=c("elevation","patch"))
#z_g.combo<-z_g.combo %>% group_by(elevation,patch) %>% mutate(yld_diff.combo=mean(yld_diff.x,yld_diff.y,na.rm=T))

#g1<-ggplot(z_g.combo, aes( as.numeric(patch), elevation, z = yld_diff.combo)) +geom_raster(aes(fill=yld_diff.combo)) +
#  scale_fill_gradientn(colours = rev(terrain.colors(20))) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
#  labs(fill="Yield/Difference") + ggtitle("Mean Yield Difference") + theme(text=element_text(size=12))


#ggarrange(g2,g3,g1,ncol=3,nrow=1,common.legend=T)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.elev.patcharea.pdf",height=5,width=14)


#see if elevation and patch area predict low yielding plots?
library(arm)
tm<-glm(low.yield.bin~patcharea*elevation,data=d.F.new14,family=quasibinomial(link="logit"))

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/LowYield.AllYears.landscape.txt")
summary(tm)
sink()

#shade diversity and leguminous trees do not predict low yielding farms
sm<-glm(low.yield.bin~arm::rescale(Shannon.i)*arm::rescale(BA.legume),data=d.F.new14,family=quasibinomial(link="logit"))
summary(sm)

#coffee land area predict low yielding farms
clm<-glm(low.yield.bin~arm::rescale(coffee.area.ha),data=d.F.new14,family=quasibinomial(link="logit"))
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

pv1<-glm(vulnerable~poly(elevation,2)+patcharea,data=d.F.new14,family=quasibinomial(link="logit"))
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


#do for 2014 
tm.14<-glm(low.yield14.bin~elevation+patcharea,data=d.F.new14,family="binomial")
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Resilience/LowYield.2014.landscape.txt")
summary(tm.14)
sink()

#for lowyield
z14.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm.14, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z14.ly[i,j]<-pi.hat$fit
  }
}
colnames(z14.ly)<-patch
z14.ly$elevation<-elev

z_g14.ly<-gather(z14.ly,key="patch",value="lowyld",-elevation)
t2<-ggplot(z_g14.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c(direction=-1) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms\n(2014)") + theme(text=element_text(size=14))

#for 2015 - significant
tm.15<-glm(low.yield15.bin~patcharea+elevation,data=d.F.new15,family="binomial")

sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Resilience/LowYield.2015.landscape.txt")
summary(tm.15)
sink()

#for lowyield
z15.ly<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(tm.15, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z15.ly[i,j]<-pi.hat$fit
  }
}
colnames(z15.ly)<-patch
z15.ly$elevation<-elev

z_g15.ly<-gather(z15.ly,key="patch",value="lowyld",-elevation)
t3<-ggplot(z_g15.ly, aes( as.numeric(patch), elevation, z = lowyld)) +geom_raster(aes(fill=lowyld)) +
  scale_fill_viridis_c(direction=-1) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Low Yielding Farms\n(2015)") + theme(text=element_text(size=14))

#for 2016 - not significant
tm.16<-glm(low.yield16.bin~patcharea+elevation,data=d.F.new16,family="binomial")
summary(tm.16)

ggarrange(t1,t2,t3,ncol=3,nrow=1,common.legend = T,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Modelled.lowyield.elev.vs.patcharea.pdf",width=14,height=5)

