#explore influence of patch area and elevation on micro-climate for each month.
#calculated using field micro-climate data

library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(lme4)


folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Yayu"
#pubs folder
ptemp<-"Publications/2021/CoffeeLandscapes/"
setwd(paste0(folder_names,dtemp))

#load metdata
met_data <- read_csv(paste0(getwd(),"/MetData/MonthlyStress_estimates.csv"))
plot_data <- read_csv(paste0(getwd(),"/plotnums.csv"))

plot_data <- plot_data %>% mutate(Plot=name)
met_data <- left_join(met_data,plot_data %>% select(Plot,elevation,PatchSize,PatchArea,GapJuly_15,Gap_Nov14,Kebele,PlotNum,plotsize),by="Plot")
#remove suspicious FC1 RH
met_data <- met_data %>% mutate(meanRH=replace(meanRH,Plot=="FC1"&month>"2015-07-01",NA)) %>% 
  mutate(year=year(month),month.no=month(month),season=NA) %>% mutate(season=replace(season,month.no<4,"flowering"),
                                                                      season=replace(season,month.no>3&month.no<11,"fruiting")) %>% 
  ungroup()
u_met<-met_data %>% group_by(month.no,year) %>% select(month.no,year,Tmax,Tmin,Tavg,maxVPD) %>% summarise_all(mean) 
names(u_met) <-c(names(u_met[,1:2]),paste0("u_",names(u_met[,3:ncol(u_met)])))
sd_met<-met_data %>% group_by(month.no,year) %>% select(month.no,year,Tmax,Tmin,Tavg,maxVPD) %>% summarise_all(sd)
names(sd_met) <-c(names(sd_met[,1:2]),paste0("sd_",names(sd_met[,3:ncol(u_met)])))

met_data<- left_join(met_data,u_met,by=c("month.no","year"))
met_data<- left_join(met_data,sd_met,by=c("month.no","year"))

#calculate anomaly
met_data<-met_data %>% mutate(tmax_anom=Tmax-u_Tmax,tmin_anom=Tmin-u_Tmin,tavg_anom=Tavg-u_Tavg,vpd_anom=maxVPD-u_maxVPD) %>% 
  mutate(tmax_anom_sigma=tmax_anom/sd_Tmax,tmin_anom_sigma=tmin_anom/sd_Tmin,tavg_anom_sigma=tavg_anom/sd_Tavg,vpd_anom_sigma=vpd_anom/sd_maxVPD)

ta_all_mx<-lmer(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea)+(month.no|year),data=met_data,REML=F)
summary(ta_all_mx)

ta_all<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data %>% filter(year<=2016))
summary(ta_all)

MuMIn::r.squaredGLMM(ta_all)

ta_14<-lm(tavg_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2014))
summary(ta_14)

ta_15<-lm(tavg_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2015))
summary(ta_15)

ta_16<-lm(tavg_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2016))
summary(ta_16)


elev<-seq(as.integer(min(plot_data$elevation)),as.integer(max(plot_data$elevation)),by=1)
patch<-seq(as.integer(min(plot_data$PatchArea[!is.na(plot_data$PatchArea)])),as.integer(max(plot_data$PatchArea[!is.na(plot_data$PatchArea)])),by=5)
z.elev<-attributes(scale(plot_data$elevation))
z.patch<-attributes(scale(plot_data$PatchArea))

z.ta<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.ta[i,j] <- coef(ta_all)[1]+coef(ta_all)[2]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + coef(ta_all)[3]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) +
      coef(ta_all)[4]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.ta)<-patch
z.ta$elevation<-elev

z_g.ta<-gather(z.ta,key="patch",value="tavg_anom",-elevation)
ggplot(z_g.ta, aes( as.numeric(patch), elevation, z = tavg_anom)) +geom_raster(aes(fill=tavg_anom)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Average\nTemperature\nAnomaly") +  theme(text=element_text(size=14))
ggsave(paste0(getwd(),"/Analysis/ES/AvgTempAnom.elev.vs.patcharea.pdf"),width=8,height=7)


tmx_all<-lm(tmax_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year<=2016))
summary(tmx_all)

tmx_14<-lm(tmax_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2014))
summary(tmx_14)

tmx_15<-lm(tmax_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2015))
summary(tmx_15)

tmx_16<-lm(tmax_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2016))
summary(tmx_16)


vpd_all<-lm(vpd_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year<=2016))
summary(vpd_all)

vpd_14<-lm(vpd_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2014))
summary(vpd_14)

vpd_15<-lm(vpd_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2015))
summary(vpd_15)

vpd_16<-lm(vpd_anom~arm::rescale(elevation)+arm::rescale(PatchArea),data=met_data %>% filter(year==2016))
summary(vpd_16)

met_data.15<-met_data %>% filter(year==2015)
met_data.16<-met_data %>% filter(year==2016)

#check landscape variables on anomalies
tmx.15<-lm(tmax_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.15)
summary(tmx.15)

tmxz.15<-lm(tmax_anom_sigma~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.15)
summary(tmxz.15)

tmn.15<-lm(tmin_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.15)
summary(tmn.15)

ta.15<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.15)
summary(ta.15)

taz.15<-lm(tavg_anom_sigma~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.15)
summary(taz.15)

vpd.15<-lm(vpd_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.15)
summary(vpd.15)

#for 2016
tmx.16<-lm(tmax_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.16)
summary(tmx.16)

tmxz.16<-lm(tmax_anom_sigma~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.16)
summary(tmxz.16)

tmn.16<-lm(tmin_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.16)
summary(tmn.16)

ta.16<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.16)
summary(ta.16)

taz.16<-lm(tavg_anom_sigma~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.16)
summary(taz.16)

vpd.16<-lm(vpd_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_data.16)
summary(vpd.16)


met_season<-met_data %>% filter(!is.na(season)) %>% group_by(Plot,year,season,PatchArea,elevation) %>% 
  summarise_all(mean) %>% ungroup()

met_fruit.15<-met_season %>% filter(year==2015&season=="fruiting")
met_fruit.16<-met_season %>% filter(year==2016&season=="fruiting")

tm.15<-lm(tmax_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.15)
summary(tm.15)

ta.15<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.15)
summary(ta.15)

tmn.15<-lm(tmin_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.15)
summary(tmn.15)

vpd.15<-lm(vpd_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.15)
summary(vpd.15)

tm.16<-lm(tmax_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.16)
summary(tm.16)

ta.16<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.16)
summary(ta.16)

tmn.16<-lm(tmin_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.16)
summary(tmn.16)

vpd.16<-lm(vpd_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_fruit.16)
summary(vpd.16)

met_flower.15<-met_season %>% filter(year==2015&season=="flowering")
met_flower.16<-met_season %>% filter(year==2016&season=="flowering")

tmf.15<-lm(tmax_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_flower.15)
summary(tmf.15)

taf.15<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_flower.15)
summary(taf.15)

tmnf.15<-lm(tmin_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_flower.15)
summary(tmnf.15)

taf.16<-lm(tavg_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_flower.16)
summary(taf.16)

tmnf.16<-lm(tmin_anom~arm::rescale(elevation)*arm::rescale(PatchArea),data=met_flower.16)
summary(tmnf.16)

#include box around ENSO index > 1.5 (to signify "Strong") and > 2.0 (to signify "Very Strong")
# > 1.5 is 2015-JJA,JAS, 2016-FMA, > 2.0 is 2015-ASO,SON,OND, 2016-NDJ,DJF,JFM
strong = data.frame(x1=c(as.Date("2015-06-01"),as.Date("2016-02-01")), x2=c(as.Date("2015-08-01"),as.Date("2016-03-01")), y1=c(-Inf,-Inf), y2=c(Inf,Inf))
v_strong = data.frame(x1=as.Date("2015-08-01"), x2=as.Date("2016-02-01"), y1=-Inf, y2=Inf)
wet_start = data.frame(x1=c(as.Date("2015-06-01"),as.Date("2016-06-01"),as.Date("2017-06-01")))
wet_end = data.frame(x2=c(as.Date("2014-09-01"),as.Date("2015-09-01"),as.Date("2016-09-01"),as.Date("2017-09-01")))
#water stress
g1<-met_summ %>% ggplot() + 
  #geom_point(aes(month,stress.mm,color=factor(distance))) + 
  geom_line(aes(month,stress.mm,color=factor(category))) + geom_hline(yintercept=0,linetype="dashed") +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  geom_text(aes(x=min(wet_start$x1),label="Wet Season Start\n",y=250),color="blue",angle=90,text=element_text(size=11))+
  geom_text(aes(x=min(wet_end$x2),label="Wet Season End\n",y=250),color="black",angle=90,text=element_text(size=11))+
  scale_color_discrete(name="category")+
  theme_classic() + xlab("") + ylab("Water Stress [mm]")+theme(legend.position="none")

g2<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxVPD,color=factor(distance))) + 
  geom_line(aes(month,maxVPD,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="category")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Maximum VPD [hPa]")+theme(legend.position="none")

g3<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,maxT,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="category")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("Date") + ylab("Maximum Temperature [C]")+theme(legend.position="bottom")

g4<-grid.arrange(g1,g2,g3,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.MicroClimate.Measures.pdf"),g4,height=9,width=6)

#create figure of ppt
met_ppt<-read_csv(paste0(getwd(),"/MetData/ECO_12_monthlyppt.csv"))
#harmat<-data.frame(x1=as.Date("2015-01-01"), x2=as.Date("2016-01-01"), x3=as.Date("2017-01-01"))
g4<-met_ppt %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,Tppt)) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Monthly Precipitation [mm]") +
  # geom_text(aes(x=harmat$x1,y=250,label="Harmattan"),color="grey",size=3.5)+geom_text(aes(x=harmat$x2,y=250,label="Harmattan"),color="grey",size=3.5)+
  #geom_text(aes(x=harmat$x3,y=250,label="Harmattan"),color="grey",size=3.5)+
  #geom_text(aes(x=wet_start$x1[2],y=200,label="\nFirst Wet Season"),color="blue",angle=90,size=3)+
  geom_text(aes(x=wet_start$x1[1],y=350,label="\n Wet Season"),color="blue",angle=90,size=3) 

g1<-met_summ %>% ggplot() + 
  #geom_point(aes(month,stress.mm,color=factor(distance))) + 
  geom_line(aes(month,stress.mm,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  #geom_text(aes(x=min(wet_start$x1),label="Wet Season Start\n",y=50),color="blue",angle=90,text=element_text(size=11))+
  #geom_text(aes(x=min(wet_end$x2),label="Wet Season End\n",y=50),color="black",angle=90,text=element_text(size=11))+
  scale_color_discrete(name="Category")+
  theme_classic() + xlab("") + ylab("Water Stress [mm]")+theme(legend.position="none")

g5<-grid.arrange(g4,g1,g2,g3,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.MicroClimate.Measures.wppt.pdf"),g5,height=10,width=8)

g6<-met_ppt %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,Tppt)) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Monthly Precipitation [mm]") +
  #geom_text(aes(x=harmat$x1,y=250,label="Harmattan"),color="black",size=5)+geom_text(aes(x=harmat$x2,y=250,label="Harmattan"),color="black",size=5)+
  #geom_text(aes(x=harmat$x3,y=250,label="Harmattan"),color="black",size=5)+
  #geom_text(aes(x=wet_start$x1[2],y=200,label="\nFirst Wet Season"),color="blue",angle=90,size=4)+
  geom_text(aes(x=wet_start$x1[1],y=350,label="\n Wet Season"),color="blue",angle=90,size=4)+theme(text = element_text(size=14))
g7<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,maxT,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="category")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("Date") + ylab("Maximum Temperature [C]")+theme(legend.position="bottom")+theme(text = element_text(size=14))

g8<-ggarrange(g6,g7,ncol=1,nrow=2,common.legend = TRUE, legend = "bottom")
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.MaxT.wppt.jpeg"),g8)

met_month<-read_csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
met_month<-left_join(met_month,met_data %>% select(Plot,PatchSize,elev.class),by="Plot")
met_month <- met_month %>% mutate(category=paste(elev.class,PatchSize,sep="-"))

met_month <- met_month %>% mutate(ah=replace(ah,Plot=="FC1"&month>"2015-07-01",NA))

#look at soil measures [NOT CALCULATED YET]
met_ssumm <- met_month %>% group_by(category,month) %>% summarise(ah=mean(ah,na.rm=T),vwc=mean(vwc,na.rm=T),stmax=mean(stmax,na.rm=T),stmin=mean(stmin,na.rm=T)) 
#remove -Inf values
met_ssumm <- met_ssumm %>% filter(stmax!="-Inf")

g4<-met_ppt %>% ggplot() + 
  #geom_point(aes(month,maxT,color=factor(distance))) + 
  geom_line(aes(month,Tppt)) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Monthly Precipitation\n[mm]") +
  #geom_text(aes(x=harmat$x1,y=250,label="Harmattan"),color="grey",size=3.5)+geom_text(aes(x=harmat$x2,y=250,label="Harmattan"),color="grey",size=3.5)+
  #geom_text(aes(x=harmat$x3,y=250,label="Harmattan"),color="grey",size=3.5)+
  #geom_text(aes(x=wet_start$x1[2],y=200,label="\nFirst Wet Season"),color="blue",angle=90,size=3)+
  geom_text(aes(x=wet_start$x1[1],y=350,label="\nWet Season"),color="blue",angle=90,size=3) 

g6<-met_ssumm %>% ggplot() +
  geom_line(aes(month,ah,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="category")+
  theme_classic() + xlab("") + ylab("Absolute Humidity\n[kg/m3]")+theme(legend.position = "none")

g7<-met_ssumm %>% ggplot() +
  geom_line(aes(month,vwc,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="category")+
  theme_classic() + xlab("") + ylab("Volumetric Water\nContent")+theme(legend.position = "none")

g8<-met_ssumm %>% ggplot() +
  geom_line(aes(month,stmax,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="category")+
  theme_classic() + xlab("") + ylab("Maximum Soil\nTemperature [C]")+theme(legend.position = "bottom")

g9<-grid.arrange(g4,g6,g7,g8,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.Soil.Measures.wppt.pdf"),g9,height=10,width=8)

g2<-met_summ %>% ggplot() + 
  #geom_point(aes(month,maxVPD,color=factor(distance))) + 
  geom_line(aes(month,maxVPD,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + scale_color_discrete(name="category")+
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  theme_classic() + xlab("") + ylab("Maximum VPD\n[hPa]")+theme(legend.position="none")

g7<-met_ssumm %>% ggplot() +
  geom_line(aes(month,vwc,color=factor(category))) +
  geom_rect(data=strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) + 
  geom_vline(xintercept=wet_start$x1,linetype="dashed",color="blue")+geom_vline(xintercept=wet_end$x2,linetype="dotted")+
  scale_color_discrete(name="category")+
  theme_classic() + xlab("") + ylab("Soil Volumetric\nWater Content [0/1]")+theme(legend.position = "bottom")

ggarrange(g4,g6,g2,g7,common.legend = TRUE, legend = "bottom",nrow = 4)

ggsave(paste0(getwd(),"/Analysis/ElNino/Seasonal.Measures.wppt.final.pdf"),height=10,width=8)


#calculate anomalies from monthly met data
met_month<-read_csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
met_month<-met_month %>% mutate(g_month=month(month))
met_month<-left_join(met_month,met_data %>% filter(month=="2014-11-01") %>% select(Plot,PatchSize,elev.class),by="Plot")
met_month <- met_month %>% mutate(category=paste(elev.class,PatchSize,sep="-"))

#take mean outside of enso months
mean_data <- met_month %>% filter(month<v_strong$x1|month>v_strong$x2) %>% group_by(g_month,category) %>% summarise(m_ah=mean(ah,na.rm=T),m_tavg=mean(tavg,na.rm=T),m_tmin=mean(tmin,na.rm=T),m_tmax=mean(tmax,na.rm=T),m_vpd=mean(vpd,na.rm=T))

g1<-ggplot(mean_data,aes(factor(g_month),m_ah,group=category)) + geom_line(aes(color=category)) + theme_classic() +
  xlab("Month") + ylab("Monthly Mean of AH [kg/m3]")

g2<-ggplot(mean_data,aes(factor(g_month),m_tavg,group=category)) + geom_line(aes(color=category)) + theme_classic() +
  xlab("Month") + ylab("Monthly Mean Temperature [C]")

g3<-ggplot(mean_data,aes(factor(g_month),m_tmin,group=category)) + geom_line(aes(color=category)) + theme_classic() +
  xlab("Month") + ylab("Monthly Minimum Temperature [C]")

g4<-ggplot(mean_data,aes(factor(g_month),m_tmax,group=category)) + geom_line(aes(color=category)) + theme_classic() +
  xlab("Month") + ylab("Monthly Maximum Temperature [C]")

g5<-ggplot(mean_data,aes(factor(g_month),m_vpd,group=category)) + geom_line(aes(color=category)) + theme_classic() +
  xlab("Month") + ylab("Monthly Vapour Pressure Deficit [hPa]")

ggarrange(g1,g2,g3,g4,g5,common.legend = TRUE, legend = "bottom",nrow = 2,ncol=3)
ggsave(paste0(getwd(),"/Analysis/ElNino/Monthly.means.bycategory.pdf"),height=6,width=10)

met_month <- left_join(met_month,mean_data,by=c("g_month","category"))
met_month <- met_month %>% mutate(anom_ah=ah-m_ah,anom_tavg=tavg-m_tavg,anom_tmin=tmin-m_tmin,anom_tmax=tmax-m_tmin,anom_vpd=vpd-m_vpd)

met_elnino <- met_month %>% filter(month>strong$x1[1]&month<strong$x2[2]) %>% group_by(category,month) 
#%>% summarise(anom_ah=mean(anom_ah,na.rm=T),anom_tavg=mean(anom_tavg,na.rm=T),anom_tmin=mean(anom_tmin,na.rm=T),
#            anom_tmax=mean(anom_tmax,na.rm=T),anom_vpd=mean(anom_vpd,na.rm=T))
#g1<-ggplot(met_elnino,aes(factor(month),anom_ah,group=category)) + geom_boxplot(aes(color=category,group=month)) + theme_classic() +
#  xlab("Month") + ylab("Anomaly AH [kg/m3]")

#g2<-ggplot(mean_data,aes(factor(g_month),m_tavg,group=category)) + geom_line(aes(color=category)) + theme_classic() +
#  xlab("Month") + ylab("Monthly Mean Temperature [C]")

#g3<-ggplot(mean_data,aes(factor(g_month),m_tmin,group=category)) + geom_line(aes(color=category)) + theme_classic() +
#  xlab("Month") + ylab("Monthly Minimum Temperature [C]")

#g4<-ggplot(mean_data,aes(factor(g_month),m_tmax,group=category)) + geom_line(aes(color=category)) + theme_classic() +
#  xlab("Month") + ylab("Monthly Maximum Temperature [C]")

#g5<-ggplot(mean_data,aes(factor(g_month),m_vpd,group=category)) + geom_line(aes(color=category)) + theme_classic() +
#  xlab("Month") + ylab("Monthly Vapour Pressure Deficit [hPa]")

write_csv(met_elnino,paste0(getwd(),"/MetData/Monthly_anomalies_enso.csv"))

#open terra climate data
#extract name of all .csvs
#terraclim data
terra_clim<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies.csv"))

#plot met data for each metstation location - useless
ggplot(terra_clim,aes(Date,vpd)) + geom_line() + theme_classic() + facet_wrap(~site,ncol=3)

ggplot(terra_clim,aes(Date,ppt,group=site)) + geom_line(aes(color=site)) + theme_classic()# + facet_wrap(~plot,ncol=1)

#ggplot(terra_clim,aes(Date,min_temp)) + geom_line() + theme_classic() + facet_wrap(~plot,ncol=1)

ggplot(terra_clim,aes(Date,tmax,group=site)) + geom_line(aes(color=site)) + theme_classic()# + facet_wrap(~plot,ncol=1)

ggplot(terra_clim,aes(Date,pet)) + geom_line(aes(color=site)) + theme_classic()

#need to compare measures to one large metstation in Doraani
met_ppt<-read_csv(paste0(getwd(),"/MetData/ECO_12_monthlyppt.csv"))
met_summ<-read_csv(paste0(getwd(),"/MetData/ECO_12_summary.csv"))
met_summ$month <- as.Date(paste(year(met_summ$day),month(met_summ$day),"01",sep="-"),format="%Y-%m-%d")
met_comp <- met_summ %>% group_by(month) %>% summarise(max_temp=mean(Tmax,na.rm=T),min_temp=mean(Tmin,na.rm=T),vpd=mean(VPDmax,na.rm=T))

g1<-ggplot(terra_clim %>% filter(site=="B19"&year>=1986),aes(Date,ppt)) + geom_line() + geom_line(data=met_ppt,aes(month,Tppt),color="green") +
  xlab("") + ylab("Precipitation [mm]") + theme_classic()

g2<-ggplot(terra_clim %>% filter(site=="B19"&Date>"2014-01-01"),aes(Date,ppt)) + geom_line() + geom_line(data=met_ppt,aes(month,Tppt),color="green") +
  xlab("Month") + ylab("Monthly Precipitation [mm]") + theme_classic()

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
#ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.PrecipComparison.pdf"))

#plot with anomalies
g2<-ggplot(terra_clim %>% filter(site=="B13"&year>=1986),aes(Date,precip_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2014-01-01"),xmax=as.Date("2016-10-01"),ymin=-Inf,ymax=Inf),fill='yellow',alpha=1/500) +
  ylab("Precipitation Anomaly [mm]") + xlab("Date")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.PrecipComparison2.pdf"))


g1<-ggplot(terra_clim %>% filter(site=="B19"&year>=1986),aes(Date,tmax)) + geom_line() + geom_line(data=met_comp,aes(month,max_temp),color="green") +
  xlab("") + ylab("Maximum Temperature [C]") + theme_classic()
g2<-ggplot(terra_clim %>% filter(site=="B19"&year>=1986),aes(Date,tmax_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2014-01-01"),xmax=as.Date("2016-10-01"),ymin=-Inf,ymax=Inf),fill='yellow',alpha=1/500) +
  ylab("Maximum Temperature Anomaly [C]") + xlab("Date")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.MaxTComparison2.pdf"))

g1<-ggplot(terra_clim %>% filter(site=="B19"&year>=1986),aes(Date,vpd)) + geom_line() + geom_line(data=met_comp,aes(month,vpd/10),color="green") +
  xlab("") + ylab("Vapour Pressure Deficit [kPa]") + theme_classic()
g2<-ggplot(terra_clim %>% filter(site=="B19"&year>=1986),aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2014-01-01"),xmax=as.Date("2016-10-01"),ymin=-Inf,ymax=Inf),fill='yellow',alpha=1/500) +
  ylab("Vapour Pressure Deficit Anomaly [kPa]") + xlab("Date")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.VPDComparison2.pdf"))


#anomalies around year of study, with harvesting dates
harvest<-data_frame(c("2014-10-01","2015-10-01","2016-10-01"))
colnames(harvest)<-"harvest.date"
g1<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,precip_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Precipitation Anomaly [mm]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed",color="red")

g2<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Vapour Pressure Deficit Anomaly [kPa]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed",color="red")

g3<-ggplot(terra_clim %>% filter(site=="B13"&year>=2014&year<2017),aes(Date,tmax_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(inherit.aes = F,mapping=aes(xmin=as.Date("2015-06-01"),xmax=as.Date("2016-03-01"),ymin=-Inf,ymax=Inf),fill='lightgrey',alpha=1/50) +
  ylab("Maximum Temperature Anomaly [C]") + xlab("Date") + geom_vline(data=harvest,aes(xintercept=as.Date(harvest.date)),linetype="dashed",color="red")
ggarrange(g1,g2,g3,ncol=1,nrow=3,align="hv")
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.Anom.Comparison.pdf"))

#include box around ENSO index > 1.5 (to signify "Strong") and > 2.0 (to signify "Very Strong")
# > 1.5 is 2015-JJA,JAS, 2016-FMA, > 2.0 is 2015-ASO,SON,OND, 2016-NDJ,DJF,JFM
strong2 = data.frame(x1=c(as.Date("1965-07-01"),as.Date("1972-08-01"),as.Date("1982-08-01"),as.Date("1987-06-01"),as.Date("1991-10-01"),as.Date("1997-06-01"),as.Date("2009-11-01"),as.Date("2015-06-01")), 
                     x2=c(as.Date("1965-12-01"),as.Date("1973-01-01"),as.Date("1983-03-01"),as.Date("1987-09-01"),as.Date("1992-03-01"),as.Date("1998-03-01"),as.Date("2010-01-01"),as.Date("2016-03-01")), y1=-Inf, y2=Inf)
v_strong2 = data.frame(x1=c(as.Date("1965-09-01"),as.Date("1972-11-01"),as.Date("1982-09-01"),as.Date("1997-08-01"),as.Date("2015-08-01")), 
                       x2=c(as.Date("1965-11-01"),as.Date("1972-12-01"),as.Date("1983-02-01"),as.Date("1998-01-01"),as.Date("2016-02-01")), y1=-Inf, y2=Inf)
g2<-ggplot(sat_anom,aes(Date,vpd_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(data=strong2,inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong2, inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) +
  xlab("") + ylab("Vapour Pressure Deficity Anomaly [kPa]")
g1<-ggplot(sat_anom,aes(Date,vpd)) + geom_line() + theme_classic() + geom_line(data=met_comp,aes(month,vpd/10),color="green") +
  xlab("") + ylab("VPD [kPa]")
ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3),align="h")
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.VPDComparison.pdf"))

g1<-ggplot(sat_anom,aes(Date,precip)) + geom_line() + geom_line(data=met_ppt,aes(month,Tppt),color="green") +
  xlab("") + ylab("") + theme_classic()

g2<-ggplot(sat_anom,aes(Date,ppt_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(data=strong2,inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong2, inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) +
  xlab("") + ylab("Monthly Precipitation Anomaly [mm]")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.PrecipComparison.pdf"))

g1<-ggplot(sat_anom,aes(Date,max_temp)) + geom_line() + geom_line(data=met_comp,aes(month,max_temp),color="green") +
  xlab("") + ylab("") + theme_classic()

g2<-ggplot(sat_anom,aes(Date,max_anom)) + geom_bar(stat="identity") + theme_classic() +
  geom_rect(data=strong2,inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='orange', alpha=0.2) + 
  geom_rect(data=v_strong2, inherit.aes = F, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill='red', alpha=0.2) +
  xlab("") + ylab("Monthly Maximum Temperature Anomaly [C]")

ggarrange(g1,g2,ncol=1,nrow=2,heights=c(1,3))
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClim.MaxTComparison.pdf"))

#combining satellite and ground measurements for comparison

met_comp<-met_comp %>% rename(Date=month,g.max_temp=max_temp,g.min_temp=min_temp,g.vpd=vpd)
met_comp<-left_join(met_comp,sat_anom %>% select(Date,vpd,tmax),by="Date")
met_ppt<-met_ppt %>% rename(Date=month)
met_ppt<-left_join(met_ppt,sat_anom %>% select(Date,precip),by="Date")


#plot the measurements
lm_eqn<-lm(max_temp~g.max_temp,data=met_comp)
g1<-met_comp %>% ggplot() + geom_point(aes(g.max_temp,max_temp)) + theme_classic() + ylab("TerraClim Max T [C]") +
  xlab("Measured Max T [C]") + geom_smooth(aes(g.max_temp,max_temp),method="lm") + 
  annotate("text",x=23,y=30,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn)$adj.r.squared,2)),parse=T)
       
  
lm_eqn2<-lm(min_temp~g.min_temp,data=met_comp)
g2<-met_comp %>% ggplot() + geom_point(aes(g.min_temp,min_temp)) + theme_classic() + ylab("TerraClim Min T [C]") +
  xlab("Measured Min T [C]") + geom_smooth(aes(g.min_temp,min_temp),method="lm") + 
  annotate("text",x=15,y=17,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn2)$adj.r.squared,2)),parse=T)


lm_eqn3<-lm(vpd~g.vpd/10,data=met_comp)
g3<-met_comp %>% ggplot() + geom_point(aes(g.vpd/10,vpd)) + theme_classic() + ylab("TerraClim VPD [kPa]") +
  xlab("Measured VPD [kPa]") + geom_smooth(aes(g.vpd/10,vpd),method="lm") + 
  annotate("text",x=1.0,y=2.0,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn3)$adj.r.squared,2)),parse=T)

lm_eqn4<-lm(precip~Tppt,data=met_ppt)
g4<-met_ppt %>% ggplot() + geom_point(aes(Tppt,precip)) + theme_classic() + ylab("TerraClim Precipitation [mm]") +
  xlab("Measured Precipitation [mm]") + geom_smooth(aes(Tppt,precip),method="lm") +
  annotate("text",x=50,y=350,label=paste0("italic(R) ^ 2 ==",signif(summary(lm_eqn4)$adj.r.squared,2)),parse=T)

ggarrange(g1,g2,g3,g4,ncol=2,nrow=2)
ggsave(paste0(getwd(),"/Analysis/ElNino/TerraClimvsGroundMeasures.pdf"))

