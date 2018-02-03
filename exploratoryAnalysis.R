#Figures of contributing factors to yield model

library(car)
library(MASS)
library(MuMIn)
library(arm)
library(nlme)
library(MCMCglmm)
require(lattice)
library(tidyverse)
library(broom)
library(corrplot)
library(lubridate)
library(gridExtra)

source("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/overdispersion.R")

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.csv"))


#explore micro-climate variables
metdata<-read.csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
metdata$month<-as.Date(as.character(metdata$month))
metdata<-data_frame(Plot=as.character(metdata$Plot),name=metdata$name,month=metdata$month,ah=metdata$ah,elevation=metdata$elevation,psize=metdata$psize,
                    tmax=metdata$tmax,tmin=metdata$tmin)
fc2<-metdata %>% filter(Plot=="FC2")
clean <-metdata %>% filter(Plot=="FC1"&month<"2015-08-01")
clean1 <-metdata %>% filter(Plot=="B15"&month>"2014-08-01")
clean<-bind_rows(clean,clean1)
metdata<- bind_rows(metdata %>% filter(Plot!="FC1"&Plot!="B15"),clean)


#plot out micro-climate (absolute humidity) data to see coverage, FC2 is good, or W3 or B15 or WA3 or Y2
mdata<-metdata %>% gather(key="measure",value="value",-Plot,-name,-month,-elevation,-psize)
ggplot(mdata[grep("ECO_",mdata$name),],aes(month,value,group=Plot))+geom_line(aes(color=Plot))+facet_wrap(~measure,ncol=2)+theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,text = element_text(size = 14)
  ,axis.text.x=element_text(angle = 45,hjust=1))
ggsave(paste0(getwd(),"/Analysis/ES/Microclimate.continuityofmeasures.pdf"),height=8,width=10)

metdata<- metdata %>% filter(Plot!="FC1"&Plot!="FC2")

mdata<-metdata %>% gather(key="measure",value="value",-Plot,-name,-month,-elevation,-psize)

#plot out max temp vs elevation
ggplot(mdata[mdata$measure=="tmax"&mdata$month>"2014-11-01"&mdata$month<"2015-12-01",],aes(elevation,value))+geom_point()+facet_wrap(~month,ncol=3)+stat_smooth(method="lm")+theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,text = element_text(size = 14)
  ,axis.text.x=element_text(angle = 45,hjust=1))
ggsave(paste0(getwd(),"/Analysis/ES/Max.Temp.vs.Elevation.pdf"),height=10,width=8)


metdata<-left_join(metdata,fc2 %>% select(month,ah,tmax),by="month")
#generate a difference from FC2 measures
metdata<-metdata %>% mutate(ah.diff=ah.x-ah.y,tmax.diff=tmax.x-tmax.y)
met.d<-metdata %>% filter(month>"2014-11-01"&month<"2015-12-01")

#plot ah relative to elevation
ggplot(met.d,aes(elevation,ah.x))+geom_point(aes(color=factor(month)))+stat_smooth(method="lm")+
  ggtitle("Absolute Humidity vs Elevation")+xlab("Elevation [m]")+ylab("Absolute Humidity [kg/m3]")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
ggsave(paste0(getwd(),"/Analysis/ES/Absolute.Humidity.Comparison.all.pdf"))


#generate relationship between elevation and ah.diff using all months, cannot correct for temporal autocorrelation
qqp(met.d$ah.diff,"norm") 
cmod<-lmer(ah.diff~ elevation + (1|month)+(1|Plot),data=met.d)
cmods<-standardize(cmod)

emod<-tidy(cmods)
emod$p.value<-NA

p.value<-Anova(cmods)$Pr

if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
emod$p.value[2]<-p.value
emod$r.squared.marg<-r.squaredGLMM(cmods)[1]
emod$r.squared.cond<-r.squaredGLMM(cmods)[2]

eq<-substitute(italic(y) == signif(emod[1,2],3) + signif(emod[2,2],3)%.% italic(x)*","~~italic(r)^2~"="~signif(emod[1,"r.squared.marg"],3))

#plot difference in ah relative to elevation
ggplot(met.d,aes(elevation,ah.diff))+geom_point(aes(color=factor(month)))+stat_smooth(method="lm")+
  ggtitle("Difference in Absolute Humidity from Forest")+xlab("Elevation [m]")+ylab("Diff in Absolute Humidity [kg/m3]")+
  geom_text(x=1600,y=-3,label=paste0("y =",signif(emod[1,2],3)," - ",abs(signif(emod[2,2],3)),"*x ; R2 =",signif(emod[1,"r.squared.marg"],3)))+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
ggsave(paste0(getwd(),"/Analysis/ES/Absolute.Humidity.diffComparison.all.pdf"))

#generate relationship between elevation and tmax.diff using dry/wet transition season months (March to June), cannot correct for temporal autocorrelation
met.d<-met.d %>% filter(month>"2015-02-01"&month<"2015-07-01")
qqp(met.d$tmax.diff,"norm") 
cmod1<-lmer(tmax.diff~ elevation + (1|month)+(1|Plot),data=met.d)
cmod1s<-standardize(cmod1)

emod<-tidy(cmod1s)
emod$p.value<-NA

p.value<-Anova(cmod1s)$Pr

if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
emod$p.value[2]<-p.value
emod$r.squared.marg<-r.squaredGLMM(cmod1s)[1]
emod$r.squared.cond<-r.squaredGLMM(cmod1s)[2]

eq<-substitute(italic(y) == signif(emod[1,2],3) + signif(emod[2,2],3)%.% italic(x)*","~~italic(r)^2~"="~signif(emod[1,"r.squared.marg"],3))

#plot difference in tmax relative to elevation
ggplot(met.d,aes(elevation,tmax.diff))+geom_point(aes(color=factor(month)))+stat_smooth(method="lm")+
  ggtitle("Difference in Maximum Temp from Forest")+xlab("Elevation [m]")+ylab("Diff in Max Temp [C]")+
  geom_text(x=1600,y=-3,label=paste0("y =",signif(emod[1,2],3)," - ",abs(signif(emod[2,2],3)),"*x ; R2 =",signif(emod[1,"r.squared.marg"],3)))+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
ggsave(paste0(getwd(),"/Analysis/ES/Max.Temp.diffComparison.all.pdf"))


#add BA of shade trees and canopy gap to this 
shade<-data_frame(Plot=as.character(d.F$Plot),BA.all=d.F$BA.all, GapDry=d.F$GapDry)
shade <- shade %>% group_by(Plot) %>% summarise(BA.all=mean(BA.all,na.rm=T),GapDry=mean(GapDry,na.rm=T))

metdata <- left_join(metdata,shade,by="Plot")

#look at drivers of differences in micro-climate, look at each month separately (cannot correct for auto-correlation if covariates aren't different)
dates<-unique(metdata$month)
results<-list()
for(i in 1:length(dates)){
  tmp<- metdata %>% filter(!is.na(ah.diff)&month==dates[i])
  if(nrow(tmp)<20) next
  qqp(tmp$ah.diff,"norm",main=dates[i])
  cm01<-lm(ah.diff ~ elevation*psize + GapDry,data=tmp)
  cm01s<-standardize(cm01)
  y<-summary(cm01s)
  y1<-data_frame(Month=dates[i],Variables=row.names(y$coefficients),Estimate=y$coefficients[,1],Std.error=y$coefficients[,2],t.value=y$coefficients[,3],pr=y$coefficients[,4],adj.r.squared=y$adj.r.squared,p.value=anova(cm01)$'Pr(>F)'[1])
  results[[i]]<-y1
  rm(y1,y,cm01,cm01s)

}

final<-do.call(rbind.data.frame,results)
#identify variables that are significant
final<- final %>% mutate(sig="ns") %>% mutate(sig=replace(sig,pr<0.1,"."))
final<- final %>% mutate(sig=replace(sig,pr<0.05,"*"))
final<- final %>% mutate(sig=replace(sig,pr<0.01,"**"))
final<- final %>% mutate(sig=replace(sig,pr<0.001,"***"))

#write results to csv
write.csv(final,paste0(getwd(),"/Analysis/ES/AbsoluteHumidity.elevation.GapDry.csv"))

#look at drivers of differences in micro-climate, look at each month separately (cannot correct for auto-correlation if covariates aren't different)
dates<-unique(metdata$month)
results<-list()
for(i in 1:length(dates)){
  tmp<- metdata %>% filter(!is.na(ah.diff)&month==dates[i])
  if(nrow(tmp)<20) next
  qqp(tmp$ah.diff,"norm",main=dates[i])
  cm01<-lm(ah.diff ~ elevation*psize + BA.all,data=tmp)
  cm01s<-standardize(cm01)
  y<-summary(cm01s)
  y1<-data_frame(Month=dates[i],Variables=row.names(y$coefficients),Estimate=y$coefficients[,1],Std.error=y$coefficients[,2],t.value=y$coefficients[,3],pr=y$coefficients[,4],adj.r.squared=y$adj.r.squared,p.value=anova(cm01)$'Pr(>F)'[1])
  results[[i]]<-y1
  rm(y1,y,cm01,cm01s)
  
}

final<-do.call(rbind.data.frame,results)
#identify variables that are significant
final<- final %>% mutate(sig="ns") %>% mutate(sig=replace(sig,pr<0.1,"."))
final<- final %>% mutate(sig=replace(sig,pr<0.05,"*"))
final<- final %>% mutate(sig=replace(sig,pr<0.01,"**"))
final<- final %>% mutate(sig=replace(sig,pr<0.001,"***"))

#write results to csv
write.csv(final,paste0(getwd(),"/Analysis/ES/AbsoluteHumidity.elevation.BAall.csv"))

#do again for Tmax
dates<-unique(metdata$month)
results<-list()
for(i in 1:length(dates)){
  tmp<- metdata %>% filter(!is.na(ah.diff)&month==dates[i])
  if(nrow(tmp)<20) next
  qqp(tmp$tmax.diff,"norm",main=dates[i])
  cm01<-lm(tmax.diff ~ elevation*psize + GapDry,data=tmp)
  cm01s<-standardize(cm01)
  y<-summary(cm01s)
  y1<-data_frame(Month=dates[i],Variables=row.names(y$coefficients),Estimate=y$coefficients[,1],Std.error=y$coefficients[,2],t.value=y$coefficients[,3],pr=y$coefficients[,4],adj.r.squared=y$adj.r.squared,p.value=anova(cm01)$'Pr(>F)'[1])
  results[[i]]<-y1
  rm(y1,y,cm01,cm01s)
  
}

final<-do.call(rbind.data.frame,results)
#identify variables that are significant
final<- final %>% mutate(sig="ns") %>% mutate(sig=replace(sig,pr<0.1,"."))
final<- final %>% mutate(sig=replace(sig,pr<0.05,"*"))
final<- final %>% mutate(sig=replace(sig,pr<0.01,"**"))
final<- final %>% mutate(sig=replace(sig,pr<0.001,"***"))

#write results to csv
write.csv(final,paste0(getwd(),"/Analysis/ES/Max.Temp.elevation.GapDry.csv"))

results<-list()
for(i in 1:length(dates)){
  tmp<- metdata %>% filter(!is.na(ah.diff)&month==dates[i])
  if(nrow(tmp)<20) next
  qqp(tmp$tmax.diff,"norm",main=dates[i])
  cm01<-lm(tmax.diff ~ elevation*psize + BA.all,data=tmp)
  cm01s<-standardize(cm01)
  y<-summary(cm01s)
  y1<-data_frame(Month=dates[i],Variables=row.names(y$coefficients),Estimate=y$coefficients[,1],Std.error=y$coefficients[,2],t.value=y$coefficients[,3],pr=y$coefficients[,4],adj.r.squared=y$adj.r.squared,p.value=anova(cm01)$'Pr(>F)'[1])
  results[[i]]<-y1
  rm(y1,y,cm01,cm01s)
  
}

final<-do.call(rbind.data.frame,results)
#identify variables that are significant
final<- final %>% mutate(sig="ns") %>% mutate(sig=replace(sig,pr<0.1,"."))
final<- final %>% mutate(sig=replace(sig,pr<0.05,"*"))
final<- final %>% mutate(sig=replace(sig,pr<0.01,"**"))
final<- final %>% mutate(sig=replace(sig,pr<0.001,"***"))

#write results to csv
write.csv(final,paste0(getwd(),"/Analysis/ES/Max.Temp.BAall.csv"))

#look at relationship between disease and micro-climate (AH)
df<-d.F[!is.na(d.F$ah.flower)&!is.na(d.F$ah.fruit),]
df <-data_frame(Plot=df$Plot,Shrub.id=df$Shrub.id,year=df$year, ah.fruit=df$ah.fruit,ah.flower=df$ah.flower,tmax.flower=df$tmax.flower,tmax.fruit=df$tmax.fruit,elevation=df$elevation,patcharea=df$patcharea,BA.all=df$BA.all,BA.shade=df$BA.shade,GapDry=df$GapDry,
                propCBB=df$propCBB,propCBD=df$propCBD,fruit.drop=df$fruit.drop,leaf.drop=df$leaf.drop,fruitset=df$fruitset,prop.ldrop=df$prop.ldrop,propLM=df$propLM,propCLR=df$propCLR,
                iCLR=df$iCLR,propHerb=df$propHerb,pH=df$pH,CN.ratio=df$CN.ratio,Tot.P.ppm=df$Tot.P.ppm,K.meq=df$K.meq,Fe.ppm=df$Fe.ppm)

#variables normally distributed, propCLR, iCLR, prop.ldrop, propHerb (remove 0 and 1.0 values for fruitset)
fset<-df %>% filter(fruitset!=0&fruitset!=1)
pdf(paste0(getwd(),"/Analysis/ES/fruitset.qnorm.pdf"))
qqp(fset$fruitset,"norm")
dev.off()

frm01<-lmer(fruitset~ah.flower*patcharea+BA.shade*GapDry+(1|Plot)+(1|year),data=fset)
frm01s<-standardize(frm01)
summary(frm01s)
r.squaredGLMM(frm01s)

tmp<-tidy(frm01s)
tmp1<-tidy(confint(frm01s))
tmp$lwr<-tmp1[match(tmp$term,tmp1$.rownames),"X2.5.."]
tmp$upr<-tmp1[match(tmp$term,tmp1$.rownames),"X97.5.."]

tmp<-tmp[!is.na(tmp$lwr)&tmp$term!="(Intercept)",]

g1<-ggplot(tmp, aes(x = term, y = estimate, ymin = lwr, ymax = upr)) + geom_errorbar(aes(width=0.2)) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of Micro-climate and Shade on Fruitset")+
  xlab("Variable")+ylab("Effect Size")+geom_hline(yintercept=0,linetype="dashed")+geom_text(y=1.05,x=1,label=paste0("R2 = ",signif(r.squaredGLMM(frm01s)[1],3)))+
  scale_x_discrete(labels=c("z.ah.flower" = "Absolute Humidity\nDuring Flowering", "z.patcharea" = "Area of Patch","z.BA.shade"="Basal Area\nShade Tolerant Trees","z.GapDry" = "Canopy Gap","z.ah.flower:z.patcharea" = "Int AH:PatchArea", "z.BA.shade:z.GapDry" = "Int Basal Area:Canopy Gap"))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/fruitset.model.microclimate.shade.pdf"))

pdf(paste0(getwd(),"/Analysis/ES/CLR.qnorm.pdf"))
qqp(df$propCLR,"norm")
dev.off()

dm01<-lmer(propCLR~ah.flower+K.meq+pH+(1|Plot)+(1|year),data=df)
dm01s<-standardize(dm01)
summary(dm01s)
r.squaredGLMM(dm01s)

tmp<-tidy(dm01s)
tmp1<-tidy(confint(dm01s))
tmp$lwr<-tmp1[match(tmp$term,tmp1$.rownames),"X2.5.."]
tmp$upr<-tmp1[match(tmp$term,tmp1$.rownames),"X97.5.."]

tmp<-tmp[!is.na(tmp$lwr)&tmp$term!="(Intercept)",]

g1<-ggplot(tmp, aes(x = term, y = estimate, ymin = lwr, ymax = upr)) + geom_errorbar(aes(width=0.2)) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of Micro-climate and Soil on Coffee Leaf Rust Incidence")+
  xlab("Variable")+ylab("Effect Size")+geom_hline(yintercept=0,linetype="dashed")+geom_text(y=-0.05,x=1,label=paste0("R2 = ",signif(r.squaredGLMM(dm01s)[1],3)))+
  scale_x_discrete(labels=c("z.ah.flower" = "Absolute Humidity\nDuring Flowering", "z.K.meq" = "Soil Potassium","z.pH" = "Soil pH"))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/CLR.model.microclimate.soil.pdf"))

qqp(df$prop.ldrop,"norm")

dm02<-lmer(prop.ldrop~propHerb+propCLR+(1|Plot)+(1|year),data=df)
dm02s<-standardize(dm02)
summary(dm02s)
r.squaredGLMM(dm02s)

qqp(df$propHerb,"norm")

dm03<-lmer(propHerb~patcharea+ah.fruit+K.meq+(1|Plot)+(1|year),data=df)
dm03s<-standardize(dm03)
summary(dm03s)
r.squaredGLMM(dm03s)


#separate regions into two elevation groups by wereda (Yayu and Doraani)
df.low<-d.F[d.F$kebele!="Badessa"&d.F$kebele!="Weyra",]
df.hi<-d.F[d.F$kebele=="Badessa"|d.F$kebele=="Weyra",]

#do correlation plots
#do correlation matrices to remove correlated variables
d.C<- df.low[,c(5:17,19:ncol(df.low))]
s<-cor(d.C,use="complete.obs")
s[is.na(s)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_low.shrub_2014_2016.pdf"))
corrplot(s, method = "circle",tl.cex = .7)
dev.off()
#save correlation matrix
write.csv(s,paste0(getwd(),"/Analysis/ES/ES.low.shrub_correlation_analysis.csv"))

#do correlation matrices to remove correlated variables
d.C<- df.hi[,c(5:17,19:ncol(df.hi))]
s<-cor(d.C,use="complete.obs")
s[is.na(s)]<-0

pdf(paste0(getwd(),"/Analysis/ES/Corrplot_hi.shrub_2014_2016.pdf"))
corrplot(s, method = "circle",tl.cex = .7)
dev.off()
#save correlation matrix
write.csv(s,paste0(getwd(),"/Analysis/ES/ES.hi.shrub_correlation_analysis.csv"))

#see if relationship between seasonal microclimate measures and elevation, use FC2 to fill in blanks?
metdata<-read.csv(paste0(getwd(),"/MetData/Monthly_metdata_withcanopygap.csv"))
metdata$month<-as.Date(as.character(metdata$month))

clean <-metdata %>% filter(Plot=="FC1"&month<"2015-08-01")
clean1 <-metdata %>% filter(Plot=="B15"&month>"2014-08-01")
clean<-bind_rows(clean,clean1)
metdata<- bind_rows(metdata %>% filter(Plot!="FC1"&Plot!="B15"),clean)

metdata$year<-year(metdata$month)
metdata$month<-month(metdata$month)
#replace -Inf
metdata[metdata=="-Inf"&!is.na(metdata)]<-NA
metdata<-data_frame(Plot=as.character(metdata$Plot),name=metdata$name,month=metdata$month,year=metdata$year,ah=metdata$ah,elevation=metdata$elevation,psize=metdata$psize,
                    tmax=metdata$tmax,tavg=metdata$tavg,vpd=metdata$vpd)

met.flower <-metdata %>% filter(month>=1&month<4) %>% group_by(Plot,year) %>% summarise(vpd.flower=mean(vpd,na.rm=T),ah.flower=mean(ah,na.rm=T),tavg.flower=mean(tavg,na.rm=T),tmax.flower=mean(tmax,na.rm=T))
met.fruit <- metdata %>% filter(month>=4&month<10&year<2017) %>% group_by(Plot,year) %>% summarise(vpd.fruit=mean(vpd,na.rm=T),ah.fruit=mean(ah,na.rm=T),tavg.fruit=mean(tavg,na.rm=T),tmax.fruit=mean(tmax,na.rm=T))

metdata<-left_join(met.flower,met.fruit, by=c("Plot","year"))

#plot met measures vs elevation
ns<-read.csv("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/plotnums.csv")
ns<-data_frame(Plot=as.character(ns$name),elevation=ns$elevation,patcharea=ns$PatchArea,GapWet=ns$GapJuly_15,GapDry=ns$Gap_Nov14,
               plotsize=ns$plotsize,buffer=ns$Buffer..1.yes.,kebele=ns$Kebele)
metdata<-left_join(metdata,ns %>% select(Plot,elevation),by="Plot")

ggplot(metdata,aes(elevation,ah.flower))+geom_point()+facet_wrap(~year,ncol=2)+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Absolute Humidity During Coffee Flowering")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0(getwd(),"/Analysis/ES/ah.flower.vs.elevation.pdf"))

ggplot(metdata,aes(elevation,ah.fruit))+geom_point()+facet_wrap(~year,ncol=2)+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Absolute Humidity During Coffee Berry Development")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0(getwd(),"/Analysis/ES/ah.fruit.vs.elevation.pdf"))

ggplot(metdata,aes(elevation,tavg.flower))+geom_point()+facet_wrap(~year,ncol=2)+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Average Temperature During Coffee Flowering [C]")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0(getwd(),"/Analysis/ES/avgtemp.flower.vs.elevation.pdf"))

ggplot(metdata,aes(elevation,tavg.fruit))+geom_point()+facet_wrap(~year,ncol=2)+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Average Temperature During Coffee Berry Development [C]")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0(getwd(),"/Analysis/ES/avgtemp.fruit.vs.elevation.pdf"))

ggplot(metdata,aes(elevation,tmax.fruit))+geom_point()+facet_wrap(~year,ncol=2)+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Maximum Temperature During Coffee Berry Development [C]")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave(paste0(getwd(),"/Analysis/ES/maxtemp.fruit.vs.elevation.pdf"))

#generate relationship relative to fc2 for ah.flower, ah.fruit and tavg.fruit
fc2<-metdata %>% filter(Plot=="FC2")

mdata<-left_join(metdata %>% select(year,elevation,ah.flower,ah.fruit,tavg.fruit,tmax.fruit), fc2 %>% select(year,ah.flower,ah.fruit,tavg.fruit,tmax.fruit),by="year")
mdata<-mdata %>% group_by(Plot.x,year) %>% mutate(diff.ah.flower=ah.flower.x-ah.flower.y,diff.ah.fruit=ah.fruit.x-ah.fruit.y,diff.tavg.fruit=tavg.fruit.x-tavg.fruit.y,diff.tmax.fruit=tmax.fruit.x-tmax.fruit.y)

#generate relationship between elevation and diff.ah.flower and diff.ah.fruit using dry/wet transition season months (March to June), cannot correct for temporal autocorrelation
qqp(mdata$diff.ah.flower[mdata$year!="2017"],"norm") 
cmod1<-lmer(diff.ah.flower~ elevation +(1|Plot.x),data=mdata[mdata$year!="2017",])

emod<-tidy(cmod1)
emod$p.value<-NA

p.value<-Anova(cmod1)$Pr

if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
emod$p.value[2]<-p.value
emod$r.squared.marg<-r.squaredGLMM(cmod1)[1]
emod$r.squared.cond<-r.squaredGLMM(cmod1)[2]

g1<-ggplot(mdata[mdata$year!="2017",],aes(elevation,diff.ah.flower))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Difference in Absolute Humidity [kg/m3]\nDuring Coffee Flowering")+
  geom_text(x=1600,y=-2.5,label=paste0("y =",signif(emod[1,2],3)," - ",abs(signif(emod[2,2],3)),"*x;\nR2 =",signif(emod[1,"r.squared.marg"],3)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.position = "none",
    text = element_text(size=15))
#ggsave(paste0(getwd(),"/Analysis/ES/diff.ah.flower.vs.elevation.pdf"))

qqp(mdata$diff.ah.fruit[mdata$year!="2017"],"norm") 
cmod1<-lmer(diff.ah.fruit~ elevation +(1|Plot.x),data=mdata[mdata$year!="2017",])

emod1<-tidy(cmod1)
emod1$p.value<-NA
p.value<-Anova(cmod1)$Pr

if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
emod1$p.value[2]<-p.value
emod1$r.squared.marg<-r.squaredGLMM(cmod1)[1]
emod1$r.squared.cond<-r.squaredGLMM(cmod1)[2]

g2<-ggplot(mdata[mdata$year!="2017",],aes(elevation,diff.ah.fruit))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Difference in Absolute Humidity [kg/m3]\nDuring Coffee Berry Development")+
  geom_text(x=1600,y=-2,label=paste0("y =",signif(emod1[1,2],3)," - ",abs(signif(emod1[2,2],3)),"*x;\nR2 =",signif(emod1[1,"r.squared.marg"],3)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.position = "none"
    ,text = element_text(size=15))
#ggsave(paste0(getwd(),"/Analysis/ES/diff.ah.fruit.vs.elevation.pdf"))

qqp(mdata$diff.tavg.fruit[mdata$year!="2017"],"norm") 
cmod1<-lmer(diff.tavg.fruit~ elevation +(1|Plot.x),data=mdata[mdata$year!="2017",])

emod2<-tidy(cmod1)
emod2$p.value<-NA
p.value<-Anova(cmod1)$Pr

if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
emod2$p.value[2]<-p.value
emod2$r.squared.marg<-r.squaredGLMM(cmod1)[1]
emod2$r.squared.cond<-r.squaredGLMM(cmod1)[2]


g3<-ggplot(mdata[mdata$year!="2017",],aes(elevation,diff.tavg.fruit))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Difference in Average Temperature\nDuring Coffee Berry Development [C]")+
  geom_text(x=1450,y=4,label=paste0("y =",signif(emod2[1,2],3)," - ",abs(signif(emod2[2,2],3)),"*x;\nR2 =",signif(emod2[1,"r.squared.marg"],3)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.key=element_blank()
    ,legend.justification=c(0,0), legend.position=c(0,0)
    ,legend.background = element_blank()
    ,text = element_text(size=15))+scale_color_discrete(name="Year")
#ggsave(paste0(getwd(),"/Analysis/ES/diff.avgtemp.fruit.vs.elevation.pdf"))


qqp(mdata$diff.tmax.fruit[mdata$year!="2017"],"norm") 
cmod1<-lmer(diff.tmax.fruit~ elevation +(1|Plot.x),data=mdata[mdata$year!="2017",])

emod3<-tidy(cmod1)
emod3$p.value<-NA
p.value<-Anova(cmod1)$Pr

if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
emod3$p.value[2]<-p.value
emod3$r.squared.marg<-r.squaredGLMM(cmod1)[1]
emod3$r.squared.cond<-r.squaredGLMM(cmod1)[2]

g4<-ggplot(mdata[mdata$year!="2017",],aes(elevation,diff.tmax.fruit))+geom_point(aes(color=factor(year)))+stat_smooth(method="lm")+
  xlab("Elevation [m]")+ylab("Difference in Maximum Temperature\nDuring Coffee Berry Development [C]")+
  geom_text(x=1450,y=-2,label=paste0("y =",signif(emod3[1,2],3)," - ",abs(signif(emod3[2,2],3)),"*x;\nR2 =",signif(emod3[1,"r.squared.marg"],3)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.position="none"
    ,text = element_text(size=15)
    )

g5<-grid.arrange(g1,g2,g3,g4,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ES/diff.microclimatemeasures.vs.elevation.pdf"),g5,height=10,width=10)

d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES_analysis_dataset.csv"))

#use diff.ah.flower and diff.ah.fruit to generate ah calculations for missing plots
df.2fill<-d.F %>% filter(is.na(ah.flower))
fc2<-metdata %>% filter(Plot=="FC2")

fc2 <- fc2 %>% ungroup() %>% select(year,ah.flower,ah.fruit)
colnames(fc2)<-c("year","fc2.ah.flower","fc2.ah.fruit")

df.2fill <- left_join(df.2fill,fc2, by="year")

df.2fill<- df.2fill %>% group_by(Plot,year) %>% mutate(new.ah.flower=emod[1,"estimate"]+emod[2,"estimate"]*elevation+fc2.ah.flower,new.ah.fruit=emod1[1,"estimate"]+emod1[2,"estimate"]*elevation+fc2.ah.fruit) %>%
  select(-fc2.ah.flower,-fc2.ah.fruit)

d.F <- left_join(d.F,df.2fill %>% select(Plot,year,new.ah.flower,new.ah.fruit),by=c("Plot","year"))
d.F<-d.F %>% distinct(ID, .keep_all = TRUE) %>% mutate(ah.fruit=replace(ah.fruit,is.na(ah.fruit),new.ah.fruit[is.na(ah.fruit)]),ah.flower=replace(ah.flower,is.na(ah.flower),new.ah.flower[is.na(ah.flower)])) %>%
  select(-new.ah.flower,new.ah.fruit)

write.csv(d.F,paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))
