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
d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))

df<-d.F[!is.na(d.F$ah.flower)&!is.na(d.F$ah.fruit),]
df <-data_frame(Plot=df$Plot,Shrub.id=df$Shrub.id,year=df$year,kebele=df$kebele, ah.fruit=df$ah.fruit,ah.flower=df$ah.flower,tmax.flower=df$tmax.flower,tmax.fruit=df$tmax.fruit,elevation=df$elevation,patcharea=df$patcharea,BA.all=df$BA.all,BA.shade=df$BA.shade,GapDry=df$GapDry,
                propCBB=df$propCBB,propCBD=df$propCBD,fruit.drop=df$fruit.drop,leaf.drop=df$leaf.drop,fruitset=df$fruitset,prop.ldrop=df$prop.ldrop,propLM=df$propLM,propCLR=df$propCLR,
                iCLR=df$iCLR,propHerb=df$propHerb,pH=df$pH,CN.ratio=df$CN.ratio,Tot.P.ppm=df$Tot.P.ppm,K.meq=df$K.meq,Fe.ppm=df$Fe.ppm)
df.low<-df %>% filter(kebele!="Badessa"&kebele!="Weyra")
df.hi<-df %>% filter(kebele=="Badessa"|kebele=="Weyra")

#variables normally distributed fruitset, propCLR, iCLR, prop.ldrop, propHerb (remove 0 and 1.0 values for fruitset)
#do for yayu
fset<-df.low %>% filter(fruitset!=0&fruitset!=1&!is.na(propCLR))
pdf(paste0(getwd(),"/Analysis/ES/fruitset.yayu.qnorm.pdf"))
qqp(fset$fruitset,"norm")
dev.off()

frm01<-lmer(fruitset~propCLR+K.meq+(1|Plot/Shrub.id)+(1|year),data=fset)
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

#do again for plot level
 d.F.plot<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))
 
 df.2fill<-d.F.plot %>% filter(is.na(ah.flower))
 fc2<-metdata %>% filter(Plot=="FC2")
 
 fc2 <- fc2 %>% ungroup() %>% select(year,ah.flower,ah.fruit)
 colnames(fc2)<-c("year","fc2.ah.flower","fc2.ah.fruit")
 
 df.2fill <- left_join(df.2fill,fc2, by="year")
 
 df.2fill<- df.2fill %>% group_by(Plot,year) %>% mutate(new.ah.flower=emod[1,"estimate"]+emod[2,"estimate"]*elevation+fc2.ah.flower,new.ah.fruit=emod1[1,"estimate"]+emod1[2,"estimate"]*elevation+fc2.ah.fruit) %>%
   select(-fc2.ah.flower,-fc2.ah.fruit)
 
 d.F.plot <- left_join(d.F.plot,df.2fill %>% select(Plot,year,new.ah.flower,new.ah.fruit),by=c("Plot","year"))
 
 d.F.plot<-d.F.plot %>% distinct(ID, .keep_all = TRUE) %>% mutate(ah.fruit=replace(ah.fruit,is.na(ah.fruit),new.ah.fruit[is.na(ah.fruit)]),ah.flower=replace(ah.flower,is.na(ah.flower),new.ah.flower[is.na(ah.flower)])) %>%
   select(-new.ah.flower,new.ah.fruit)

 write.csv(d.F.plot,paste0(getwd(),"/Analysis/ES/ES.plot.mod_analysis_dataset.csv"))
 
 #do figures of correlation plots
 d.F <- read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))
 
 #plot average yield vs GapWet (Doraani) and ah.fruit, elevation, total P, BA legume and BA deciduous (Yayu)
 avg.yield<-d.F %>% select(Plot,year,kebele,avg.kg,GapWet,ah.fruit,elevation,patcharea,Tot.P.ppm,BA.legume,BA.deciduous,BA.all)
 #%>% gather(key="factor",value="value",-Plot,-year,-compost) 
 avg.yield.d<-avg.yield %>% filter(kebele=="Badessa"|kebele=="Weyra")
 avg.yield.y<-avg.yield %>% filter(kebele!="Badessa"&kebele!="Weyra") 
 tmp2<-avg.yield.y %>%  group_by(Plot) %>% summarise(ah.fruit.avg=mean(ah.fruit,na.rm=T))
 avg.yield.y<-left_join(avg.yield.y,tmp2,by="Plot")
 
 #for Doraani
 mod<-lm(avg.kg~GapWet,data=avg.yield.d)
 
 emod<-tidy(mod)
 #if(p.value<0.001) {p.value<-0.001} else if (p.value>0.001&p.value<0.01) {p.value<-0.01} else if(p.value>0.01&p.value<0.05) {p.value<-0.05} else p.value<-"NS"
 #emod$p.value[2]<-p.value
 emod$r.squared.marg<-r.squaredGLMM(mod)[1]
 emod$r.squared.cond<-r.squaredGLMM(mod)[2]
 
 ggplot(avg.yield.d,aes(GapWet,avg.kg)) + geom_point() + stat_smooth(method="lm")+
   xlab("Canopy Gap in Wet Season [%]") + ylab("Average Inter-annual\nShrub Yield [kg]") + ggtitle("Doraani Plots") +
   geom_text(x=20,y=0.15,label=paste0("R2 =",signif(emod[1,"r.squared.marg"],3)))+
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
 ggsave(paste0(getwd(),"/Analysis/ES/CorrFigs.doraani.avgyield.vs.GapWet.pdf"))
 

 mod<-lm(avg.kg~ah.fruit,data=avg.yield.y)
 
 g1<-ggplot(avg.yield.y %>% filter(year!=2014),aes(ah.fruit,avg.kg)) + geom_point(aes(color=factor(year))) + stat_smooth(method="lm",aes(group=factor(year),color=factor(year)))+
   scale_color_discrete(name="Year")+ xlab("AH During Berry Development [kg/m3]") + ylab("Average Inter-annual\nShrub Yield [kg]") + ggtitle("Yayu Plots: Absolute Humidity") +
   geom_text(x=16,y=0.5,label=paste0("R2 =",signif(r.squaredGLMM(mod)[1],2)))+
   theme(
     plot.background = element_blank()
     ,panel.background = element_blank()
     ,panel.grid.major = element_blank()
     ,panel.grid.minor = element_blank()
     ,panel.border = element_blank()
     ,axis.line.x = element_line(color = 'black')
     ,axis.line.y = element_line(color = 'black')
     #,legend.key=element_blank()
     ,legend.justification=c(1,1), legend.position=c(1,1)
     ,legend.key = element_blank()
     ,text = element_text(size=15)
   )
 
 mod1<-lm(avg.kg~elevation,data=avg.yield.y)
 
 g2<-ggplot(avg.yield.y,aes(elevation,avg.kg)) + geom_point() + stat_smooth(method="lm")+
   xlab("Elevation [m]") + ylab("Average Inter-annual\nShrub Yield [kg]") + ggtitle("Elevation") +
   geom_text(x=1400,y=0.5,label=paste0("R2 =",signif(r.squaredGLMM(mod1)[1],2)))+
   theme(
     plot.background = element_blank()
     ,panel.background = element_blank()
     ,panel.grid.major = element_blank()
     ,panel.grid.minor = element_blank()
     ,panel.border = element_blank()
     ,axis.line.x = element_line(color = 'black')
     ,axis.line.y = element_line(color = 'black')
     #,legend.key=element_blank()
     ,legend.justification=c(1,1), legend.position=c(1,1)
     ,legend.key = element_blank()
     ,text = element_text(size=15)
   )
 
 mod2<-lm(avg.kg~BA.legume,data=avg.yield.y %>% filter(year==2015))
 
 g3<-ggplot(avg.yield.y %>% filter(year==2015),aes(BA.legume,avg.kg)) + geom_point() + stat_smooth(method="lm")+
   xlab("Basal Area [m2/ha]") + ylab("Average Inter-annual\nShrub Yield [kg]") + ggtitle("Leguminous Shade Trees") +
   geom_text(x=2.5,y=0.5,label=paste0("R2 =",signif(r.squaredGLMM(mod2)[1],2)))+
   theme(
     plot.background = element_blank()
     ,panel.background = element_blank()
     ,panel.grid.major = element_blank()
     ,panel.grid.minor = element_blank()
     ,panel.border = element_blank()
     ,axis.line.x = element_line(color = 'black')
     ,axis.line.y = element_line(color = 'black')
     #,legend.key=element_blank()
     ,legend.justification=c(1,1), legend.position=c(1,1)
     ,legend.key = element_blank()
     ,text = element_text(size=15)
   )
 
 g4<-grid.arrange(g1,g2,g3,ncol=3)
 ggsave(paste0(getwd(),"/Analysis/ES/CorrFigs.yayu.avgyield.plots.pdf"),g4,width=12,height=4)
 
 rm(avg.yield.d,avg.yield.y,g1,g2,g3,g4,mod,mod1,mod2,mod3,emod,emod1,avg.yield,tmp2)
 
 #plot buffer vs density, %N, canopy gap in dry season, tavg.flower  (Yayu)
 buffer<-d.F %>% filter(kebele!="Badessa"&kebele!="Weyra") %>% select(Plot,year,buffer,density,N.pct,GapDry,tavg.flower) 
# %>% group_by(Plot,buffer) %>%
  #summarise(ah.fruit=mean(ah.fruit,na.rm=T),pH=mean(pH,na.rm=T),Ca.meq=mean(Ca.meq,na.rm=T),eCEC.mol.kg=mean(eCEC.mol.kg,na.rm=T))
 
g2<- ggplot(buffer,aes(factor(buffer),N.pct)) + geom_boxplot()+xlab("Land Cover Type (1=buffer,0=transition)")+
   ylab("Soil N [%]") + ggtitle("Yayu Plots: Soil N")+theme(
     plot.background = element_blank()
     ,panel.background = element_blank()
     ,panel.grid.major = element_blank()
     ,panel.grid.minor = element_blank()
     ,panel.border = element_blank()
     ,axis.line.x = element_line(color = 'black')
     ,axis.line.y = element_line(color = 'black')
     #,legend.key=element_blank()
     ,legend.justification=c(1,1), legend.position=c(1,1)
     ,legend.key = element_blank()
     ,text = element_text(size=15)
   )
 
g1<- ggplot(buffer,aes(factor(buffer),density)) + geom_boxplot()+xlab("Land Cover Type (1=buffer,0=transition)")+
  ylab("Coffee Density [ha-1]") + ggtitle("Yayu Plots: Coffee Density")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

g3<- ggplot(buffer,aes(factor(buffer),GapDry)) + geom_boxplot()+xlab("Land Cover Type (1=buffer,0=transition)")+
  ylab("Canopy Gap in Dry Season [%]") + ggtitle("Yayu Plots: Canopy Gap")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

g4<- ggplot(buffer,aes(factor(buffer),tavg.flower)) + geom_boxplot()+xlab("Land Cover Type (1=buffer,0=transition)")+
  ylab("Avg Temp During Flowering") + ggtitle("Yayu Plots: Average Temperature")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

g5<-grid.arrange(g1,g2,g3,g4,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/CorrFigs.yayu.buffer.plots.pdf"),g5,width=16,height=4)
rm(g1,g2,g3,g4,g5,buffer)

#plot total coffee area vs BA.shade (+ nope), N.pct,C.pct, soil Ca, soil Mg and eCEC and vpd.flower,tavg.flower,tmax.flower (Yayu)
coffee.area<-d.F %>% filter(kebele!="Badessa"&kebele!="Weyra") %>% select(Plot,year,coffee.area.ha,BA.shade,C.pct,N.pct,Ca.meq,Mg.meq,eCEC.mol.kg,tavg.flower,tmax.flower,vpd.flower) 

mod<-lm(C.pct~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g1<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,C.pct)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Soil C [%]") + ggtitle("Soil Carbon")+
  geom_text(x=10,y=6,label=paste0("R2 =",signif(r.squaredGLMM(mod)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod1<-lm(N.pct~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g2<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,N.pct)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Soil N [%]") + ggtitle("Soil Nitrogen")+
  geom_text(x=10,y=0.5,label=paste0("R2 =",signif(r.squaredGLMM(mod1)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod2<-lm(Mg.meq~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g3<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,Mg.meq)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Soil Mg [meq]") + ggtitle("Soil Magnesium")+
  geom_text(x=10,y=40,label=paste0("R2 =",signif(r.squaredGLMM(mod2)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod3<-lm(Ca.meq~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g4<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,Ca.meq)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Soil Ca [meq]") + ggtitle("Soil Calcium")+
  geom_text(x=10,y=40,label=paste0("R2 =",signif(r.squaredGLMM(mod3)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod4<-lm(eCEC.mol.kg~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g5<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,eCEC.mol.kg)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Soil eCEC [mol/kg]") + ggtitle("Soil eCEC")+
  geom_text(x=10,y=80,label=paste0("R2 =",signif(r.squaredGLMM(mod4)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod5<-lm(tavg.flower~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g6<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,tavg.flower)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Avg Temp During Flowering") + ggtitle("Average Temperature")+
  geom_text(x=10,y=27,label=paste0("R2 =",signif(r.squaredGLMM(mod5)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod6<-lm(tmax.flower~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g7<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,tmax.flower)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("Max Temp During Flowering") + ggtitle("Maximum Temperature")+
  geom_text(x=10,y=40,label=paste0("R2 =",signif(r.squaredGLMM(mod6)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod7<-lm(vpd.flower~coffee.area.ha,data=coffee.area %>% filter(coffee.area.ha<40))

g8<-ggplot(coffee.area %>% filter(coffee.area.ha<40),aes(coffee.area.ha,vpd.flower)) + geom_point() + stat_smooth(method="lm") + xlab("Coffee Area [ha]") +
  ylab("VPD During Flowering") + ggtitle("Vapour Pressure Deficit")+
  geom_text(x=10,y=80,label=paste0("R2 =",signif(r.squaredGLMM(mod7)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

g9<-grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/CorrFigs.yayu.coffeearea.plots.pdf"),g9,width=12,height=6)
rm(g1,g2,g3,g4,g5,g6,g7,g8,g9,coffee.area,mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7)

#plot labour vs buffer, pH, soil nutrients (Ca, Mg, Fe, eCEC) and shade diversity (Doraani)
labour<-d.F %>% filter(kebele=="Badessa"|kebele=="Weyra") %>% select(Plot,year,labour,buffer,Shannon.i,pH,Ca.meq,Mg.meq,Fe.ppm,eCEC.mol.kg) 


g1<-ggplot(labour,aes(factor(buffer),labour)) + geom_boxplot()+ xlab("Land Cover Type (1=buffer,0=transition)") +
  ylab("Labour [days/ha]") + ggtitle("Doraani Plots: Buffer")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod<-lm(labour~pH,data=labour)

g2<-ggplot(labour,aes(pH,labour)) + geom_point()+ xlab("Soil pH") + stat_smooth(method="lm")+
  ylab("Labour [days/ha]") + ggtitle("Soil pH")+
  geom_text(x=4.25,y=750,label=paste0("R2 =",signif(r.squaredGLMM(mod)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod1<-lm(labour~Ca.meq,data=labour)

g3<-ggplot(labour,aes(Ca.meq,labour)) + geom_point()+ xlab("Soil Ca [meq]") + stat_smooth(method="lm")+
  ylab("Labour [days/ha]") + ggtitle("Soil Calcium")+
  geom_text(x=15,y=750,label=paste0("R2 =",signif(r.squaredGLMM(mod1)[1],2)))+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod2<-lm(labour~Mg.meq,data=labour)

g4<-ggplot(labour,aes(Mg.meq,labour)) + geom_point()+ xlab("Soil Mg [meq]") + stat_smooth(method="lm")+
  ylab("Labour [days/ha]") + ggtitle("Soil Magnesium")+
  geom_text(x=15,y=750,label=paste0("R2 =",signif(r.squaredGLMM(mod2)[1],2)))+
    theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod3<-lm(labour~Fe.ppm,data=labour)

g5<-ggplot(labour,aes(Fe.ppm,labour)) + geom_point()+ xlab("Soil Fe [ppm]") + stat_smooth(method="lm")+
  ylab("Labour [days/ha]") + ggtitle("Soil Iron")+
  geom_text(x=0.15,y=750,label=paste0("R2 =",signif(r.squaredGLMM(mod3)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod4<-lm(labour~eCEC.mol.kg,data=labour)

g6<-ggplot(labour,aes(eCEC.mol.kg,labour)) + geom_point()+ xlab("Soil eCEC [mol/kg]") + stat_smooth(method="lm")+
  ylab("Labour [days/ha]") + ggtitle("Soil eCEC")+
  geom_text(x=30,y=750,label=paste0("R2 =",signif(r.squaredGLMM(mod4)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod5<-lm(labour~Shannon.i,data=labour %>% filter(year==2015))

g7<-ggplot(labour %>% filter(year==2015),aes(Shannon.i,labour)) + geom_point()+ xlab("Shannon Index") + stat_smooth(method="lm")+
  ylab("Labour [days/ha]") + ggtitle("Shade Tree Diversity")+
  geom_text(x=1,y=750,label=paste0("R2 =",signif(r.squaredGLMM(mod5)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

g8<-grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/CorrFigs.doraani.Labour.plots.pdf"),g8,width=12,height=6)

rm(g1,g2,g3,g4,g5,g6,g7,g8,labour,mod,mod1,mod2,mod3,mod4,mod5)

#plot patch area vs elevation, buffer, pH, composting, BA.legume, BA.deciduous and ah.fruit (Doraani)
patch<-d.F %>% filter(kebele=="Badessa"|kebele=="Weyra") %>% select(Plot,year,patcharea,elevation,buffer,pH,compost,BA.legume,BA.deciduous,ah.fruit) 

g1<-ggplot(patch,aes(factor(buffer),patcharea)) + geom_boxplot()+ xlab("Land Cover Type (1=buffer,0=transition)") +
  ylab("Forest Patch Area [ha]") + ggtitle("Doraani Plots: Buffer")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod<-lm(patcharea~elevation,data=patch)

g2<-ggplot(patch,aes(elevation,patcharea)) + geom_point()+ xlab("Elevation [m]") + stat_smooth(method="lm") +
  ylab("Forest Patch Area Area [ha]") + ggtitle("Elevation")+
  geom_text(x=1725,y=800,label=paste0("R2 =",signif(r.squaredGLMM(mod)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod1<-lm(patcharea~pH,data=patch)

g3<-ggplot(patch,aes(pH,patcharea)) + geom_point()+ xlab("Soil pH") + stat_smooth(method="lm") +
  ylab("Forest Patch Area Area [ha]") + ggtitle("Soil pH")+
  geom_text(x=4.5,y=800,label=paste0("R2 =",signif(r.squaredGLMM(mod1)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod2<-lm(compost~patcharea,data=patch)

g4<-ggplot(patch,aes(patcharea,compost)) + geom_point()+ ylab("Composting [kg/ha]") + stat_smooth(method="lm") +
  xlab("Forest Patch Area Area [ha]") + ggtitle("Composting")+
  geom_text(x=250,y=800,label=paste0("R2 =",signif(r.squaredGLMM(mod2)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod3<-lm(BA.legume~patcharea,data=patch)

g5<-ggplot(patch,aes(patcharea,BA.legume)) + geom_point()+ ylab("Basal Area [m2/ha]") + stat_smooth(method="lm") +
  xlab("Forest Patch Area Area [ha]") + ggtitle("Leguminous Shade Trees")+
  geom_text(x=250,y=10,label=paste0("R2 =",signif(r.squaredGLMM(mod3)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod4<-lm(BA.deciduous~patcharea,data=patch)

g6<-ggplot(patch,aes(patcharea,BA.deciduous)) + geom_point()+ ylab("Basal Area [m2/ha]") + stat_smooth(method="lm") +
  xlab("Forest Patch Area Area [ha]") + ggtitle("Deciduous Shade Trees")+
  geom_text(x=250,y=50,label=paste0("R2 =",signif(r.squaredGLMM(mod4)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(1,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
  )

mod5<-lm(ah.fruit~patcharea,data=patch %>% filter(ah.fruit>10))

g7<-ggplot(patch %>% filter(ah.fruit>10),aes(patcharea,ah.fruit)) + geom_point(aes(color=factor(year)))+ ylab("Absolute Humidity [kg/m3]") + stat_smooth(method="lm") +
  xlab("Forest Patch Area Area [ha]") + ggtitle("AH During Berry Development")+scale_color_discrete(name="Year")+
  geom_text(x=250,y=13,label=paste0("R2 =",signif(r.squaredGLMM(mod5)[1],2)))+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,legend.key=element_blank()
    ,legend.justification=c(1,1), legend.position=c(0.5,1)
    ,legend.key = element_blank()
    ,text = element_text(size=15)
    ,legend.text=element_text(size=10)
    ,legend.background = element_blank()
  )

g8<-grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=4)
ggsave(paste0(getwd(),"/Analysis/ES/CorrFigs.doraani.patcharea.plots.pdf"),g8,width=12,height=6)
