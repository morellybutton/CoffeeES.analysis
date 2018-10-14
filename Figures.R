#script for producing final figures of paper

library(tidyverse)
library(AICcmodavg)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

#create figure of yield collapse, histogram for each year

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

ggplot(dF.1[dF.1$Shrub.kg>0&dF.1$year==2014,],aes(Shrub.kg,color="2014")) + geom_freqpoly(binwidth=0.1,size=1) + xlab("Estimated Shrub Yield [kg/shrub]") + ylab("Number of Farms")+
  geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year=="2015",],binwidth=0.05,aes(color="2015"),size=1)+geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year==2016,],binwidth=0.05,aes(color="2016"),size=1)+
  ggtitle("Estimated Yields of Fresh Cherries")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 16)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    ,legend.title = element_blank())
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/ShrubYield_Comparison.pdf",height=4,width=6)

#create yield model figures, 2014 and yield diff for 2015 & 2016
d.F.new14<-d.F.new %>% filter(year==2014)
d.F.new15 <- d.F.new %>% filter(year==2015)
d.F.new16 <- d.F.new %>% filter(year==2016)

#2014
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta2.confint.csv"))
tmp.14<-tmp.14[!is.na(tmp.14$full),]

tmp.14$Comparison<-factor(tmp.14$Comparison,levels=tmp.14[order(tmp.14$Importance,decreasing=F),"Comparison"],
                          labels=c("Basal Area\nof Leguminous\nTrees","Soil C:N","Patch Area","CanopyGap","Maximum\nTemperature\nAnomaly","Shade Diversity","Elevation","(Intercept)"))
#add significance column
tmp.14$sig<-0
tmp.14<-tmp.14 %>% mutate(sig=replace(sig,Comparison=="CanopyGap"|Comparison=="Soil C:N"|Comparison=="Basal Area\nof Leguminous\nTrees",1))
#order by importance
tmp.14<-tmp.14[!is.na(tmp.14$Importance),]

g1<-ggplot(tmp.14, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield\n(2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() + scale_color_grey() +
  theme(text = element_text(size = 12),axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Yieldmodel_2014.pdf",height=4,width=4)

#for 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta2.confint.csv"))
tmp.15<-tmp.15[!is.na(tmp.15$full),]

#for delta 2
tmp.15$Comparison<-factor(tmp.15$Comparison,levels=tmp.15[order(tmp.15$Importance,decreasing=F),"Comparison"],
                          labels=c("Basal Area\nof Leguminous\nTrees","Elevation:\nPatch Area","Elevation","Patch Area","Coffee\nLand Area","Low\nYielding\nPlot","(Intercept)"))

#add significance column
tmp.15$sig<-0
tmp.15<-tmp.15 %>% mutate(sig=replace(sig,Comparison=="Elevation"|Comparison=="Basal Area\nof Leguminous\nTrees",1))

#order by importance
tmp.15<-tmp.15[!is.na(tmp.15$Importance),]

g1<-ggplot(tmp.15, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2,aes(color=factor(sig))) + geom_point(shape=15,size=5,aes(color=factor(sig)))+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield Difference\n(2015)")+
  #xlab("Variable [ranked by importance]")+
  xlab("") +
  ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +scale_color_grey() +
  theme(text = element_text(size = 12)
        ,axis.text.x=element_text(angle = 45,hjust=1),legend.position="none")
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Yielddiffmodel_2015.pdf",height=4,width=4)

#for 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta2.confint.csv"))
tmp.16<-tmp.16[!is.na(tmp.16$full),]

#for delta 2
tmp.16$Comparison<-factor(tmp.16$Comparison,levels=tmp.16[order(tmp.16$Importance,decreasing=F),"Comparison"],
                          labels=c("Elevation","Incidence\nof CLR","Maximum\nTemperature\n(Fruiting)","Patch Area","Fruitset","Coffee\nLand Area","Low\nYielding\nPlot","(Intercept)"))

#order by importance
tmp.16<-tmp.16[!is.na(tmp.16$Importance),]

g1<-ggplot(tmp.16, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point(shape=15,size=5)+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shrub Yield Difference\n(2016)")+
  #xlab("Variable [ranked by importance]")+
  xlab("") +
  ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 12)
        ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Yielddiffmodel_2016.pdf",height=4,width=4)

#Surface plots of elevation vs patch area
library(ggpubr)
#3D figure of patcharea area and elevation
elev<-seq(as.integer(min(d.F.new15$elevation)),as.integer(max(d.F.new15$elevation)),by=1)
patch<-seq(as.integer(min(d.F.new15$patcharea)),as.integer(max(d.F.new15$patcharea)),by=5)
z.elev<-attributes(scale(d.F.new15$elevation))
z.patch<-attributes(scale(d.F.new15$patcharea))

#for 2015
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff15.delta2.confint.csv"))

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

g2<-ggplot(z_g.15, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_gradientn(colours = terrain.colors(20)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield/Difference") + ggtitle("Shrub Yield Difference (2015)") + theme(text=element_text(size=12))
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.elev.patcharea.2015.pdf",height=5,width=5)

#for 2016
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff16.delta2.confint.csv"))
z.16<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.16[i,j] <- tmp.16[tmp.16$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.16[tmp.16$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.16)<-patch
z.16$elevation<-elev

z_g.16<-gather(z.16,key="patch",value="yld_diff",-elevation)

g3<-ggplot(z_g.16, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_gradientn(colours = terrain.colors(20)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield/Difference") + ggtitle("Shrub Yield Difference (2016)") + theme(text=element_text(size=12))

#for 2014
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta2.confint.csv"))
z.14<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.14[i,j] <- tmp.14[tmp.14$Comparison=="rescale(elevation)","Estimate"]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + tmp.14[tmp.14$Comparison=="rescale(patcharea)","Estimate"]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.14)<-patch
z.14$elevation<-elev

z_g.14<-gather(z.14,key="patch",value="yld_diff",-elevation)

g1<-ggplot(z_g.14, aes( as.numeric(patch), elevation, z = yld_diff)) +geom_raster(aes(fill=yld_diff)) +
  scale_fill_gradientn(colours = terrain.colors(20)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield/Difference") + ggtitle("Shrub Yield (2014)") + theme(text=element_text(size=12))

ggarrange(g1,g2,g3,ncol=3,nrow=1,common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Model.elev.patcharea.pdf",height=5,width=14)

