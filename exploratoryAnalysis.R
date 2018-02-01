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

#plot ah relative to elevation
ggplot(met.d,aes(elevation,ah.x))+geom_point(aes(color=factor(month)))+stat_smooth(method="lm")+
  ggtitle("Absolute Humidity vs Elevation")+xlab("Elevation [m]")+ylab("Absolute Humidity [%]")+
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

#generate a difference from FC2 measures
metdata<-metdata %>% mutate(ah.diff=ah.x-ah.y,tmax.diff=tmax.x-tmax.y)
met.d<-metdata %>% filter(month>"2014-11-01"&month<"2015-12-01")

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
  ggtitle("Difference in Absolute Humidity from Forest")+xlab("Elevation [m]")+ylab("Diff in Absolute Humidity [%]")+
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

