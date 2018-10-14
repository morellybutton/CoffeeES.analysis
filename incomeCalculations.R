library(gridExtra)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")
#setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")
#load input data for model
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot.mod_analysis_dataset.csv"))
dF.hhold<-data.frame(read.csv(paste0(getwd(),"/HouseholdSurvey/household_data.csv")),stringsAsFactors = F)

#currency conversion rate birr to usd
usd=27.21

#add wereda
dF.1$wereda<-"Doraani"
dF.1[dF.1$kebele!="Badessa"&dF.1$kebele!="Weyra","wereda"]<-"Yayu"

#calculate annual harvests (total)
dF.1$est.yield<-dF.1$Shrub.kg*dF.1$density

ggplot(dF.1[dF.1$est.yield>0&dF.1$year==2014,],aes(est.yield)) + geom_freqpoly(binwidth=60) + xlab("Estimated Farm Yield [kg/ha]") + ylab("Number of Farms")+
  ggtitle("Estimated Yields of Fresh Cherries for 2014")+theme_classic()+theme(text = element_text(size = 14)
    ,legend.key = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Observed.yields.2014.pdf"),height=5,width=7)

ggplot(dF.1[dF.1$Shrub.kg>0&dF.1$year==2014,],aes(Shrub.kg,color="2014")) + geom_freqpoly(binwidth=0.1) + xlab("Estimated Shrub Yield [kg/shrub]") + ylab("Number of Farms")+
  geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year=="2015",],binwidth=0.05,aes(color="2015"))+geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year==2016,],binwidth=0.05,aes(color="2016"))+
  ggtitle("Estimated Yields of Fresh Cherries")+
  #scale_fill_discrete(aes(name="Year"))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 16)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    ,legend.title = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Observed.yields.shrub.histo.pdf"),height=5,width=7)

df.14<-dF.1 %>% filter(year==2014)
df.15<-dF.1 %>% filter(year==2015) %>% mutate(est.yield.15=est.yield)
df.16<-dF.1 %>% filter(year==2016) %>% mutate(est.yield.16=est.yield)

df.14<-left_join(df.14,df.15 %>% select(Plot,est.yield.15),by="Plot")
df.14<-left_join(df.14,df.16 %>% select(Plot,est.yield.16),by="Plot")

ggplot(df.14,aes(est.yield,est.yield.15)) + geom_point(aes(color="2015")) + stat_smooth(method="lm",aes(color="2015")) +
  geom_point(data=df.14,aes(est.yield,est.yield.16,color="2016")) + stat_smooth(data=df.14,method="lm",aes(est.yield,est.yield.16,color="2016"))+
  xlim(0,2000) + ylim(0,2000) + geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Estimated Yield in 2014 [kg/ha]") + 
  ylab("Estimated Yield in 2015 [kg/ha]")+scale_colour_discrete(name="Year")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Observed.yields.compared.pdf"),height=7,width=7)

#calculate current income values using 15 birr/kg (coffee area (ha) * average shrub kg * density of shrubs/ha * price/kg)
dF.1$est.coffee.income.15<-dF.1$coffee.area.ha*dF.1$Shrub.kg*dF.1$density*15
dF.1$est.coffee.income.15.usd<-dF.1$est.coffee.income.15/usd
dF.1$est.coffee.income.26.usd<-dF.1$coffee.area.ha*dF.1$Shrub.kg*dF.1$density*26/usd
dF.1$obs.coffee.income.usd<-dF.hhold[match(dF.1$Plot,dF.hhold$plotcode),"Coffee.income"]/usd

#compare two measures of income
g1<-ggplot(dF.1[dF.1$year==2014,],aes(obs.coffee.income.usd,est.coffee.income.15.usd)) + geom_point(aes(color=wereda)) + geom_abline(slope=1,intercept=0,linetype="dashed") + 
  stat_smooth(method="lm")+ xlab("Reported Income from Coffee [US$]") + ylab("Estimated Income from Coffee [US$]")+ggtitle("Assuming Price of 15 birr/kg") +
  #ylim(0,17000)+xlim(0,10000)+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
g2<-ggplot(dF.1[dF.1$year==2014,],aes(obs.coffee.income.usd,est.coffee.income.26.usd)) + geom_point(aes(color=wereda)) + geom_abline(slope=1,intercept=0,linetype="dashed") + 
  stat_smooth(method="lm")+ xlab("Reported Income from Coffee [US$]") + ylab("Estimated Income from Coffee [US$]")+ggtitle("Assuming Price of 26 birr/kg") +
  #ylim(0,17000)+xlim(0,10000)+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
g3<-grid.arrange(g1,g2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ES/Survey.estincome.v.reportedincome.pdf"),g3,height=5,width=10)

#identify which farm is likely selling at 15 birr vs one selling at 26 birr
test<-dF.1 %>% filter(Plot!=""&year==2014) %>% mutate(comp.15=abs(est.coffee.income.15.usd-obs.coffee.income.usd),comp.26=abs(est.coffee.income.26.usd-obs.coffee.income.usd),birr.15=1)
test<- test %>% mutate(birr.15=replace(birr.15,comp.26<comp.15&!is.na(obs.coffee.income.usd),0))
dF.1<-left_join(dF.1,test %>% select(Plot,birr.15),by="Plot")

#if coffee area ha is 0 assume 1
dF.1 <- dF.1 %>% filter(-est.coffee.income.15,-est.coffee.income.15.usd,-est.coffee.income.26.usd) %>% mutate(coffee.area.ha2=coffee.area.ha) %>% mutate(coffee.area.ha2=replace(coffee.area.ha2,coffee.area.ha2==0,1)) %>% 
  mutate(coffee.price=15) %>% mutate(coffee.price=replace(coffee.price,birr.15==0,26)) %>%
  mutate(est.coffee.income.usd = coffee.area.ha*Shrub.kg*density*coffee.price/usd )

#take mean of wereda and year
output<-dF.1 %>% group_by(wereda,year) %>% summarise(est.yield=mean(est.yield,na.rm=T),income=mean(est.coffee.income.usd,na.rm=T),income.sd=sd(est.coffee.income.usd,na.rm=T),propCBD=mean(propCBD,na.rm=T),propCBB=mean(propCBB,na.rm=T),propCLR=mean(propCLR,na.rm=T))
write_csv(output,paste0(getwd(),"/Analysis/ES/MeanIncome_Increases.csv"))

#plot boxplots of income and yield
g1<-ggplot(dF.1,aes(wereda,est.yield,color=factor(year))) + geom_boxplot(width=0.5)+
  xlab("Wereda") + ylab("Mean Yield [kg/ha]")+scale_fill_discrete(name="Year")+
  theme_classic() + ylim(0,1000) + theme(text = element_text(size = 18)
                          ,legend.key = element_blank()
                          ,legend.title = element_blank()
                          ,legend.position="top"
                          ,plot.background = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Variability.yield.by.wereda.pdf"),g1,width=8, height=4)

g2<-ggplot(dF.1,aes(wereda,est.coffee.income.usd,color=factor(year))) + geom_boxplot()+
  xlab("Wereda") + ylab("Mean Income [US$]")+scale_fill_discrete(name="Year")+ylim(0,2000)+
  theme_classic() + theme(text = element_text(size = 18)
                          ,legend.key = element_blank()
                          ,legend.position="none")
g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ES/Variability.yield.and.income.by.wereda.pdf"),g3,width=7,height=8)

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


#compare two measures of income
ggplot(dF.1[dF.1$year==2014,],aes(obs.coffee.income.usd,est.coffee.income.usd)) + geom_point(aes(color=wereda)) + geom_abline(slope=1,intercept=0,linetype="dashed") + 
  stat_smooth(method="lm")+ xlab("Reported Income from Coffee [US$]") + ylab("Estimated Income from Coffee [US$]") +
  #ylim(0,17000)+xlim(0,10000)+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Survey.estincome.v.reportedincome.final.pdf"))

#calculate proportional change in income from 2014
dF.2 <- dF.1 %>% select(Plot,year,est.coffee.income.usd) %>% spread(key="year",value="est.coffee.income.usd")
colnames(dF.2)<-c("plotcode",paste0("y",colnames(dF.2[2:ncol(dF.2)])))
dF.2 <- dF.2 %>% mutate(y.14.15=(y2015-y2014)/y2014,y.14.16=(y2016-y2014)/y2014)

dF.2 <- left_join(dF.2,dF.hhold %>% select(plotcode,Coffee.income), by="plotcode") %>% mutate(o.2014=Coffee.income/usd,o.2015=Coffee.income/usd*(1+y.14.15),o.2016=Coffee.income/usd*(1+y.14.16))
dF.2$wereda <- dF.1[match(dF.2$plotcode,dF.1$Plot),"wereda"]
#save income variability estimates
write.csv(dF.2,paste0(getwd(),"/Analysis/ES/Estimated.variability.income.csv"))

#plot 2014 vs 2015 and 2014 vs 2016
g1<-ggplot(dF.2,aes(o.2014,o.2015)) + geom_point(aes(color=wereda)) + stat_smooth(method="lm") + geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlim(0,7500) + ylim(0,7500) + xlab("Reported Coffee Income in 2014")+ylab("Estimated Coffee Income in 2015")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")

g2<-ggplot(dF.2,aes(o.2014,o.2016)) + geom_point(aes(color=wereda)) + stat_smooth(method="lm") + geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlim(0,7500) + ylim(0,7500) + xlab("Reported Coffee Income in 2014")+ylab("Estimated Coffee Income in 2016")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")

g3<-grid.arrange(g1,g2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ES/Survey.income2014.v.lateryears.pdf"),g3,height=5,width=10)

dF.1$Shrub.kg.1<- dF.1$Shrub.kg + .00001
dF.low<-dF.1[dF.1$kebele!="Badessa"&dF.1$kebele!="Weyra",]
dF.hi<-dF.1[dF.1$kebele=="Badessa"|dF.1$kebele=="Weyra",]

#dM.hi<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.lnorm_delta2.confint.csv"))
#dM.low<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.lnorm_delta2.confint.csv"))
inc.var<-read.csv(paste0(getwd(),"/Analysis/ES/Estimated.variability.income.csv"))

#yayu 2015
yayu.2015<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_yayu_2015.med.csv"))
#doraani 2016
doraani.2016<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_doraani_2016.med.csv"))
#differcnce of 2016 from 2014
diff.16.all<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.diffyield_norm.all.2014.16.csv"))

#calculate additional 2015 income, yayu
yayu.2015<-left_join(yayu.2015,dF.1 %>% filter(year==2015) %>% select(Plot,Shrub.kg,coffee.area.ha,density,coffee.price,obs.coffee.income.usd,est.coffee.income.usd),by="Plot")
yayu.2015 <- yayu.2015 %>% mutate(new.income=value*density*coffee.area.ha*coffee.price/usd)

#save income calculations
write.csv(yayu.2015,paste0(getwd(),"/Analysis/ES/Yield.increase.Yayu.2015.csv"))

#plot yayu 2015 increase yield
ggplot(yayu.2015,aes(Plot,value,group=variable)) + geom_bar(stat="identity",aes(fill=variable)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Yayu 2015")+
  scale_fill_discrete(labels=c("BA Legume","Fruitset"),name="Factor")+theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,axis.text.x = element_blank()
  ,text = element_text(size = 14)
  ,legend.key = element_blank()
  ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Yield.increase.Yayu.2015.pdf"))

#calculate additional 2016 income, doraani
doraani.2016<-left_join(doraani.2016,dF.1 %>% filter(year==2016) %>% select(Plot,Shrub.kg,coffee.area.ha,density,coffee.price,obs.coffee.income.usd,est.coffee.income.usd),by="Plot")
doraani.2016 <- doraani.2016 %>% mutate(new.income=value*density*coffee.area.ha*coffee.price/usd)
#save income calculations
write.csv(doraani.2016,paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.csv"))

#plot doraani 2016 increase yield
ggplot(doraani.2016,aes(Plot,value,group=variable)) + geom_bar(stat="identity",aes(fill=variable)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Doraani 2016")+
  scale_fill_discrete(labels=c("Soil C:N","Planting Density","Fruitset","GapDry"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.pdf"))

ggplot(doraani.2016[doraani.2016$variable=="fruitset1"|doraani.2016$variable=="CN.ratio1",],aes(Plot,value,group=variable)) + geom_bar(stat="identity",aes(fill=variable)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Doraani 2016")+
  scale_fill_discrete(labels=c("Soil C:N","Fruitset"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.less.pdf"))

#calculate additional income in 2016, for yayu
diff.16.all<-left_join(diff.16.all,df.14 %>% select(Plot,Shrub.kg),by="Plot")
diff.16.all<-diff.16.all %>% mutate(new.kg.clr=Shrub.kg+prop.CLR,new.kg.patch=Shrub.kg+patcharea1)
#add 2016 yield, Shrub.kg.x = 2014 and Shrub.kg.y = 2016
#diff.16.all<-left_join(diff.16.all,df.16 %>% select(Plot,Shrub.kg),by="Plot")
diff.16.all<-left_join(diff.16.all,dF.1 %>% filter(year==2016) %>% select(Plot,Shrub.kg,coffee.area.ha,density,coffee.price,obs.coffee.income.usd,est.coffee.income.usd,wereda),by="Plot")
diff.16.all <- diff.16.all %>% mutate(clr.income=new.kg.clr*density*coffee.area.ha*coffee.price/usd,patch.income=new.kg.patch*density*coffee.area.ha*coffee.price/usd)

#plot reducing difference in 2016
g1<-ggplot(diff.16.all[!is.na(diff.16.all$wereda),],aes(Plot,new.kg.clr)) + geom_bar(stat="identity",aes(fill=wereda)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Yield Difference by Managing CLR (2016)")+
  #scale_fill_discrete(labels=c("Soil C:N","Planting Density","Fruitset","GapDry"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="none")
g2<-ggplot(diff.16.all[!is.na(diff.16.all$wereda),],aes(Plot,new.kg.patch)) + geom_bar(stat="identity",aes(fill=wereda)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Yield Difference by Location in Landscape (2016)")+
  #scale_fill_discrete(labels=c("Soil C:N","Planting Density","Fruitset","GapDry"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ES/Yield.difference.2016.pdf"),g3)

