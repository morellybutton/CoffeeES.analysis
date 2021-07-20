#code to create comparative figures of yield and disease incidence across years

library(tidyverse)
#library(gridExtra)

folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Yayu"
#pubs folder
ptemp<-"Publications/2021/CoffeeLandscapes/"
setwd(paste0(folder_names,dtemp))


#Load plot yields
df<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))

#add metdata
met_data <- read_csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))
#calculate water stress (fruiting) and temperature averages (flowering)
met_summ <- met_data %>% rename(date=month) %>% mutate(year=lubridate::year(date), month=lubridate::month(date)) %>% 
  mutate(season="flowering") %>% mutate(season=replace(season,month>4&month<10,"fruiting")) %>% 
  group_by(Plot,year,season) %>% summarise(tmax=mean(Tmax,na.rm=T),tmin=mean(Tmin,na.rm=T),tavg=mean(Tavg,na.rm=T),vpdmax=mean(maxVPD,na.rm=T),
                                           stress=mean(stress, na.rm=T))

met_flower <- met_summ %>% filter(season=="flowering") %>% rename(tmax_flower=tmax,tmin_flower=tmin,tavg_flower=tavg,
                                                                  vpdmax_flower=vpdmax,stress_flower=stress)
met_fruit <- met_summ %>% filter(season=="fruiting") %>% rename(tmax_fruit=tmax,tmin_fruit=tmin,tavg_fruit=tavg,
                                                                  vpdmax_fruit=vpdmax,stress_fruit=stress)

df <- left_join(df,met_flower %>% select(-season), by = c("Plot","year"))
df <- left_join(df,met_fruit %>% select(-season), by = c("Plot","year"))

df.comp14<- df %>% filter(year=="2014") %>% select(Plot,kebele,Shrub.kg,fruitset,propCBB,propCBD,prop.ldrop,prop.fdrop,propCLR,iCLR,tmax.anom.flower,tmax.anom.fruit,tmax_flower,tmax_fruit,vpdmax_flower,vpdmax_fruit,stress_flower,stress_fruit)
df.comp15<- df %>% filter(year=="2015") %>% select(Plot,kebele,Shrub.kg,fruitset,propCBB,propCBD,prop.ldrop,prop.fdrop,propCLR,iCLR,tmax.anom.flower,tmax.anom.fruit,tmax_flower,tmax_fruit,vpdmax_flower,vpdmax_fruit,stress_flower,stress_fruit)
df.comp16<- df %>% filter(year=="2016") %>% select(Plot,kebele,Shrub.kg,fruitset,propCBB,propCBD,prop.ldrop,prop.fdrop,propCLR,iCLR,tmax.anom.flower,tmax.anom.fruit,tmax_flower,tmax_fruit,vpdmax_flower,vpdmax_fruit,stress_flower,stress_fruit)

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
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g2<-ggplot(dF[dF$variable=="tmax_flower",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(20,50)+xlim(20,50)+ xlab("Mean Maximum Temperature\nDuring Flowering 2014 [C]")+ylab("Mean Maximum Temperature\nDuring Flowering  2015 [C]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g3<-ggplot(dF[dF$variable=="stress_flower",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-300,0)+xlim(-300,0) + xlab("Mean Monthly Water Stress\nDuring Flowering 2014 [mm]")+ylab("Mean Monthly Water Stress\nDuring Flowering 2015 [mm]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")


g4<-ggplot(dF[dF$variable=="propCBB",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Borer\nIncidence 2014")+ylab("Coffee Berry Borer\nIncidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g5<-ggplot(dF[dF$variable=="propCBD",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Disease\nIncidence 2014")+ylab("Coffee Berry Disease\nIncidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g6<-ggplot(dF[dF$variable=="prop.fdrop",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Berries\nDropped 2014")+ylab("Proportion of Berries\nDropped 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g7<-ggplot(dF[dF$variable=="propCLR",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Leaf Rust\nIncidence 2014")+ylab("Coffee Leaf Rust\nIncidence 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g8<-ggplot(dF[dF$variable=="iCLR",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,4)+xlim(0,4)+ 
  xlab("CLR Intensity\n2014")+ylab("CLR Intensity\n2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g9<-ggplot(dF[dF$variable=="prop.ldrop",],aes(i2014,i2015))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Leaves\nDropped 2014")+ylab("Proportion of Leaves\nDropped 2015")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")



g10<-ggpubr::ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,ncol=3,nrow=3,labels="auto")
ggsave(paste0(getwd(),"/Analysis/ES/ComparativeFigures.2014v2015.pdf"),g10,height=10,width=11)
ggsave(paste0(folder_names,ptemp,"/ComparativeFigures.2014v2015.tiff"),g10,height=10,width=11)

#plot comparisons between 2014 and 2016
#plot comparisons, yield
g1<-ggplot(dF[dF$variable=="Shrub.kg",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1.5)+xlim(0,1.5)+ xlab("Median per Shrub\nYield 2014 [kg shrub-1]")+ylab("Median per Shrub\nYield  2016 [kg shrub-1]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g2<-ggplot(dF[dF$variable=="tmax_flower",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(20,50)+xlim(20,50)+ xlab("Mean Maximum Temperature\nDuring Flowering 2014 [C]")+ylab("Mean Maximum Temperature\nDuring Flowering  2016 [C]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g3<-ggplot(dF[dF$variable=="stress_flower",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(-200,0)+xlim(-200,0) + xlab("Mean Monthly Water Stress\nDuring Flowering 2014 [mm]")+ylab("Mean Monthly Water Stress\nDuring Flowering 2016 [mm]")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=12)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")


g4<-ggplot(dF[dF$variable=="propCBB",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Borer\nIncidence 2014")+ylab("Coffee Berry Borer\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g5<-ggplot(dF[dF$variable=="propCBD",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Berry Disease\nIncidence 2014")+ylab("Coffee Berry Disease\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g6<-ggplot(dF[dF$variable=="prop.fdrop",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Berries\nDropped 2014")+ylab("Proportion of Berries\nDropped 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g7<-ggplot(dF[dF$variable=="propCLR",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Coffee Leaf Rust\nIncidence 2014")+ylab("Coffee Leaf Rust\nIncidence 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g8<-ggplot(dF[dF$variable=="iCLR",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,4)+xlim(0,4)+ 
  xlab("CLR Intensity\n2014")+ylab("CLR Intensity\n2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")

g9<-ggplot(dF[dF$variable=="prop.ldrop",],aes(i2014,i2016))+geom_point(aes(color=wereda)) +geom_abline(slope=1,intercept=0,linetype="dashed") +
  ylim(0,1)+xlim(0,1)+ 
  xlab("Proportion of Leaves\nDropped 2014")+ylab("Proportion of Leaves\nDropped 2016")+
  theme(
    panel.background=element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key = element_rect(colour = "white", fill = NA)
    ,legend.justification=c(0,1), legend.position=c(0,1)
    ,text = element_text(size=14)
    ,legend.background = element_blank())+scale_color_discrete(name="Wereda")



g10<-ggpubr::ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,ncol=3,nrow=3,labels="auto")
ggsave(paste0(getwd(),"/Analysis/ES/ComparativeFigures.2014v2016.pdf"),g10,height=10,width=11)
ggsave(paste0(folder_names,ptemp,"/ComparativeFigures.2014v2016.tiff"),g10,height=10,width=11)

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

