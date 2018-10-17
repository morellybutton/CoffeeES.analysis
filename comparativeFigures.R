#code to create comparative figures of yield and disease incidence across years

library(tidyverse)
library(gridExtra)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")

#Load plot yields
df<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))
#load modified microclimate values
#numb<-read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))
#numb <- numb %>% filter(year!=2014) %>% select(Plot,year,ah.fruit,ah.flower) %>% group_by(Plot,year) %>%
#  summarise(ah.fruit.mod=mean(ah.fruit,na.rm=T),ah.flower.mod=mean(ah.flower,na.rm=T)) %>% ungroup()

#df<-left_join(df,numb,by=c("Plot","year"))

#df<- df %>% mutate(ah.flower=replace(ah.flower,is.na(ah.flower),ah.flower.mod[is.na(ah.flower)]),ah.fruit=replace(ah.fruit,is.na(ah.fruit),ah.fruit.mod[is.na(ah.fruit)]))

df.comp14<- df %>% filter(year=="2014") %>% select(Plot,kebele,Shrub.kg,fruitset,propCBB,propCBD,prop.ldrop,prop.fdrop,propCLR,propHerb,vpd.fruit,vpd.flower,tmax.flower,tmax.fruit,p_et.flower,p_et.fruit)
df.comp15<- df %>% filter(year=="2015") %>% select(Plot,kebele,Shrub.kg,fruitset,propCBB,propCBD,prop.ldrop,prop.fdrop,propCLR,propHerb,vpd.fruit,vpd.flower,tmax.flower,tmax.fruit,p_et.flower,p_et.fruit)
df.comp16<- df %>% filter(year=="2016") %>% select(Plot,kebele,Shrub.kg,fruitset,propCBB,propCBD,prop.ldrop,prop.fdrop,propCLR,propHerb,vpd.fruit,vpd.flower,tmax.flower,tmax.fruit,p_et.flower,p_et.fruit)

dF <- df.comp14 %>% gather(key="variable",value="i2014",c(-Plot,-kebele))

d.F <- df.comp15 %>% gather(key="variable",value="i2015",c(-Plot,-kebele))
dF <-left_join(dF,d.F %>% select(Plot,variable,i2015),by=c("Plot","variable"))

d.F <- df.comp16 %>% gather(key="variable",value="i2016",c(-Plot,-kebele))
dF <-left_join(dF,d.F %>% select(Plot,variable,i2016),by=c("Plot","variable"))
rm(d.F,df.comp14,df.comp15,df.comp16)

dF <- dF %>% mutate(wereda="Yayu") %>% mutate(wereda=replace(wereda,kebele=="Badessa"|kebele=="Weyra","Doraani"))

#plot comparisons between 2014 and 2015
#plot comparisons, yield
g1<-ggplot(dF[dF$variable=="Shrub.kg",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1.5)+xlim(0,1.5)+ xlab("Median per Shrub\nYield 2014 [kg shrub-1]")+ylab("Median per Shrub\nYield  2015 [kg shrub-1]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g2<-ggplot(dF[dF$variable=="propCBB",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Borer\nIncidence 2014")+ylab("Coffee Berry Borer\nIncidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g3<-ggplot(dF[dF$variable=="propCBD",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Disease\nIncidence 2014")+ylab("Coffee Berry Disease\nIncidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g4<-ggplot(dF[dF$variable=="prop.fdrop",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Berries\nDropped 2014")+ylab("Proportion of Berries\nDropped 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g5<-ggplot(dF[dF$variable=="prop.ldrop",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Leaves\nDropped 2014")+ylab("Proportion of Leaves\nDropped 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g6<-ggplot(dF[dF$variable=="propCLR",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Leaf Rust\nIncidence 2014")+ylab("Coffee Leaf Rust\nIncidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g7<-ggplot(dF[dF$variable=="propHerb",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Herbivory Incidence 2014")+ylab("Herbivory Incidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g8<-grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/ComparativeFigures.2014v2015.pdf"),g8,height=6,width=12)

#plot comparisons between 2014 and 2016
#plot comparisons, yield
g1<-ggplot(dF[dF$variable=="Shrub.kg",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1.5)+xlim(0,1.5)+ xlab("Median per Shrub\nYield 2014 [kg shrub-1]")+ylab("Median per Shrub\nYield 2016 [kg shrub-1]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g2<-ggplot(dF[dF$variable=="propCBB",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,0.5)+xlim(0,0.5)+ 
  xlab("Coffee Berry Borer\nIncidence 2014")+ylab("Coffee Berry Borer\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g3<-ggplot(dF[dF$variable=="propCBD",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Disease\nIncidence 2014")+ylab("Coffee Berry Disease\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g4<-ggplot(dF[dF$variable=="prop.fdrop",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Berries\nDropped 2014")+ylab("Proportion of Berries\nDropped 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g5<-ggplot(dF[dF$variable=="prop.ldrop",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Leaves\nDropped 2014")+ylab("Proportion of Leaves\nDropped 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g6<-ggplot(dF[dF$variable=="propCLR",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Leaf Rust\nIncidence 2014")+ylab("Coffee Leaf Rust\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g7<-ggplot(dF[dF$variable=="propHerb",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Herbivory Incidence 2014")+ylab("Herbivory Incidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g8<-grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/ComparativeFigures.2014v2016.pdf"),g8,height=6,width=12)

#comparison between 2015 and 2016
g1<-ggplot(dF[dF$variable=="Shrub.kg",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ xlab("Median per Shrub\nYield 2015 [kg shrub-1]")+ylab("Median per Shrub\nYield 2016 [kg shrub-1]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g2<-ggplot(dF[dF$variable=="fruitset",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Fruitset 2015")+ylab("Fruitset 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g3<-ggplot(dF[dF$variable=="prop.fdrop",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Berries\nDropped 2015")+ylab("Proportion of Berries\nDropped 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g4<-ggplot(dF[dF$variable=="prop.ldrop",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Leaves\nDropped 2015")+ylab("Proportion of Leaves\nDropped 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g5<-ggplot(dF[dF$variable=="propCLR",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Leaf Rust\nIncidence 2015")+ylab("Coffee Leaf Rust\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g6<-ggplot(dF[dF$variable=="ah.flower",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(9,15)+xlim(9,15)+ 
  xlab("Absolute Humidity\nDuring Flowering 2015")+ylab("Absolute Humidity\nDuring Flowering 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g7<-ggplot(dF[dF$variable=="ah.fruit",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(10,20)+xlim(10,20)+ 
  xlab("Absolute Humidity\nDuring Berry Development 2015")+ylab("Absolute Humidity\nDuring Berry Development 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g8<-ggplot(dF[dF$variable=="tmax.fruit",],aes(i2015,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(20,35)+xlim(20,35)+ 
  xlab("Maximum Temperature During\nBerry Development 2015")+ylab("Maximum Temperature During\nBerry Development 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g9<-grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/ComparativeFigures.2015v2016.pdf"),g9,height=6,width=12)


#plot comparisons between 2014 and 2015 and 2014 and 2016
#plot comparisons, yield
ggplot(dF[dF$variable=="Shrub.kg",],aes(i2014,i2015))+geom_point(aes(color="2015")) +geom_abline(slope=1,intercept=0,linetype="dashed") + geom_point(data=dF[dF$variable=="Shrub.kg",],aes(i2014,i2016,color="2016"))+
  ylim(0,1.5)+xlim(0,1.5)+scale_color_discrete(name="year")+stat_smooth(data=dF[dF$variable=="Shrub.kg",],aes(i2014,i2015,color="2015"),method="lm") +
  xlab("Median per Shrub\nYield 2014 [kg shrub-1]")+ylab("Median per Shrub\nYield [kg shrub-1]") +stat_smooth(data=dF[dF$variable=="Shrub.kg",],aes(i2014,i2016,color="2016"),method="lm") +
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_blank()
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/ComparativeFigure.yield.2014v2015.2016.pdf"))


#plot barplots of disease measures
g1<-ggplot(output,aes(wereda,propCBD,fill=factor(year))) + geom_bar(stat="identity",position="dodge")+
  xlab("Wereda") + ylab("Mean CBD [%]")+scale_fill_discrete(name="Year")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 18)
    ,legend.key = element_blank()
    ,legend.position="none")

g2<-ggplot(output,aes(wereda,propCBB,fill=factor(year))) + geom_bar(stat="identity",position="dodge")+
  xlab("Wereda") + ylab("Mean CBB [%]")+scale_fill_discrete(name="Year")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 18)
    ,legend.key = element_blank()
    ,legend.position="none")
g3<-ggplot(output,aes(wereda,propCLR,fill=factor(year))) + geom_bar(stat="identity",position="dodge")+
  xlab("Wereda") + ylab("Mean CLR [%]")+scale_fill_discrete(name="Year")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 18)
    ,legend.key = element_blank()
    ,legend.position="bottom")

g4<-grid.arrange(g1,g2,g3,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ES/Variability.disease.measures.by.wereda.pdf"),g4,width=7,height=10)

