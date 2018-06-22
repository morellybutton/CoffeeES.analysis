#Analysis of coffee ES contributions to yield by wereda and year

library(car)
library(MuMIn)
library(arm)
library(lattice)

library(tidyverse)
library(AICcmodavg)


setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")
#setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")

d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))
d.F$farm<-d.F$Plot
#remove NA values for yield
d.F <- d.F[!is.na(d.F$Shrub.kg),]

#make sure binary variables are factors
d.F$kebele<-factor(d.F$kebele)
d.F$compost.bin<-0
d.F[d.F$compost>0,"compost.bin"]<-1
d.F$compost.bin<-factor(d.F$compost.bin)
d.F$b.ffer<-factor(d.F$buffer)
d.F$wereda<-"Yayu"
d.F[d.F$kebele=="Badessa"|d.F$kebele=="Weyra","wereda"]<-"Doraani"
d.F$wereda<-factor(d.F$wereda)

#check which linking function I need, see how well values stay within dashed lines
d.F$Shrub.kg.1<- d.F$Shrub.kg + .00001
d.F <- d.F %>% group_by(Plot,Shrub.id,year) %>% mutate(prop.fdrop = fruit.drop/Tot.fruits) %>% mutate(prop.fdrop = replace(prop.fdrop,Tot.fruits==0,0)) %>% ungroup()

#identify consistently low yielding shrubs
quart14 <- quantile(d.F %>% filter(year==2014) %>% pull(Shrub.kg))

low.yield <- d.F %>% filter(year==2014&Shrub.kg<quart14[3]) %>% select(Plot,Shrub.id)
low.yield <- low.yield %>% mutate(low.yield=1)

d.F <- left_join(d.F,low.yield,by=c("Plot","Shrub.id"))
d.F <- d.F %>% mutate(low.yield=replace(low.yield,is.na(low.yield),0))
d.F.plot <- d.F %>% group_by(Plot,year) %>% summarise(Shrub.kg=median(Shrub.kg,na.rm=T),Tot.fruits=median(Tot.fruits,na.rm=T),fruitset=median(fruitset,na.rm=T),
                                                      propCBB=median(propCBB,na.rm=T),propCBD=median(propCBD,na.rm=T),fruit.drop=median(fruit.drop,na.rm=T),prop.fdrop=median(prop.fdrop,na.rm=T),
                                                      Tot.leaves=median(Tot.leaves,na.rm=T),leaf.drop=median(leaf.drop,na.rm=T),prop.ldrop=median(prop.ldrop,na.rm=T),
                                                      propLM=median(propLM, na.rm=T), propCLR=median(propCLR,na.rm=T), propWilt=median(propWilt,na.rm=T),
                                                      propHerb=median(propHerb,na.rm=T),low.yield=mean(low.yield,na.rm=T))
d.F.plot$ID<-1:nrow(d.F.plot)
d.F.plot <- left_join(d.F.plot,d.F %>% select(-X.1,-X,-ID,-Shrub.kg,-Tot.fruits,-fruitset,-propCBB,-propCBD,-fruit.drop,-Tot.leaves,-leaf.drop,-prop.ldrop,-prop.fdrop,-propLM,-propCLR,-propWilt,-propHerb,-Shrub.kg.1,-low.yield),by=c("Plot","year"))
d.F.plot <- distinct(d.F.plot,ID,.keep_all=T)

#model difference between 2014 and 2016
y.ld14<-d.F.plot %>% filter(year=="2014") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.14=Shrub.kg)
y.ld16<-d.F.plot %>% filter(year=="2016") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.16=Shrub.kg)
y.ld15<-d.F.plot %>% filter(year=="2015") %>% filter(!is.na(Tot.fruits)) %>% select(Plot,Shrub.kg) %>% rename(Shrub.kg.15=Shrub.kg)

combo<-left_join(y.ld14,y.ld15,by="Plot")
combo<-left_join(combo,y.ld16,by="Plot")
combo <- combo %>% mutate(diff.yld1415 = Shrub.kg.15-Shrub.kg.14,diff.yld1416 = Shrub.kg.16-Shrub.kg.14) 

d.F.plot <- left_join(d.F.plot,combo %>% select(Plot,diff.yld1415,diff.yld1416),by="Plot")
d.F.plot.14 <- d.F.plot %>% filter(year==2014)
d.F.plot.1516 <- d.F.plot %>% filter(year!=2014)
d.F.plot.16 <- d.F.plot %>% filter(year==2016)
d.F.plot.15 <- d.F.plot %>% filter(year==2015)

d.F.plot.15<-d.F.plot.15 %>% rename(diff.yld=diff.yld1415)
d.F.plot.16<-d.F.plot.16 %>% rename(diff.yld=diff.yld1416)
diff.yld<-bind_rows(d.F.plot.15 %>% select(Plot,diff.yld),d.F.plot.16 %>% select(Plot,diff.yld))
d.F.plot.1516<-left_join(d.F.plot.1516,diff.yld,by="Plot")
d.F.plot.1516 <- distinct(d.F.plot.1516,ID,.keep_all=T)

d.F.plot.14$diff.yld<-NA
d.F.new <- bind_rows(d.F.plot.14 %>% select(-diff.yld1415,-diff.yld1416),d.F.plot.1516 %>% select(-diff.yld1415,-diff.yld1416))
write.csv(d.F.new,paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#do 2014
qqp(d.F.plot.14$Shrub.kg, "norm")

pm.14d <-lm(Shrub.kg~ rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=d.F.plot.14)
summary(pm.14d)
#check heteroskedasticity
diagnos <- data.frame(Resid = resid(pm.14d, type = "pearson"), Fitted = fitted(pm.14d),Variable = d.F.plot.14$Plot[!is.na(d.F.plot.14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.2014.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

pm.14d <-lm(Shrub.kg~  rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBB)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(elevation), 
            data=d.F.plot.14)
summary(pm.14d)

pdf(paste0(getwd(),"/Analysis/ES/Plot.2014.norm_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

options(na.action = "na.fail")
pm.14d.d<-dredge(pm.14d)

dredg.m01<-subset(pm.14d.d,delta<6)
write.csv(dredg.m01,paste0(getwd(),"/Analysis/ES/GLM.norm.2014_dredged01.csv"))


#do 2015 and 2016
options(na.action = "na.omit")

qqp(d.F.plot.1516$Shrub.kg, "norm")

pm.15d <-lm(Shrub.kg~ rescale(fruitset) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
              data=d.F.plot.1516)
summary(pm.15d)
tmp<-cbind(d.F.plot.1516[,3:17],d.F.plot.1516[,20:30],d.F.plot.1516[,32:58])
x<-cor(tmp, use = "complete.obs")

#check heteroskedasticity
diagnos <- data.frame(Resid = resid(pm.15d, type = "pearson"), Fitted = fitted(pm.15d),Variable = d.F.plot.1516$Plot[!is.na(d.F.plot.1516$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.201516.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

pm.16d <-lm(Shrub.kg~ rescale(fruitset) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + 
              rescale(BA.legume) + rescale(Shannon.i) + rescale(prop.ldrop)  + rescale(prop.fdrop)  + rescale(elevation), 
            data=d.F.plot.1516)
summary(pm.16d)

#do 2015 
qqp(d.F.plot.15$Shrub.kg, "norm")

sm.15d <-lm(Shrub.kg~ rescale(fruitset) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=d.F.plot.15)
summary(sm.15d)
#check heteroskedasticity
diagnos <- data.frame(Resid = resid(sm.15d, type = "pearson"), Fitted = fitted(sm.15d),Variable = d.F.plot.15$Plot[!is.na(d.F.plot.15$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

pm.15d <-lm(Shrub.kg~ rescale(fruitset) + rescale(CN.ratio) + rescale(density) + rescale(propCLR)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=d.F.plot.15)
summary(pm.15d)


options(na.action = "na.fail")
pm.15d.d<-dredge(pm.15d)

dredg.m02<-subset(pm.15d.d,delta<6)
write.csv(dredg.m02,paste0(getwd(),"/Analysis/ES/GLM.norm.2015_dredged01.csv"))

#do 2016 
options(na.action = "na.omit")
qqp(d.F.plot.16$Shrub.kg, "norm")

sm.16d <-lm(Shrub.kg~ rescale(fruitset) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=d.F.plot.16)
summary(sm.16d)
#check heteroskedasticity
diagnos <- data.frame(Resid = resid(sm.16d, type = "pearson"), Fitted = fitted(sm.16d),Variable = d.F.plot.15$Plot[!is.na(d.F.plot.16$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

pm.15d <-lm(Shrub.kg~ rescale(fruitset) + rescale(CN.ratio) + rescale(density) + rescale(propCLR)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=d.F.plot.15)
summary(pm.15d)


options(na.action = "na.fail")
pm.15d.d<-dredge(pm.15d)

dredg.m02<-subset(pm.15d.d,delta<6)
write.csv(dredg.m02,paste0(getwd(),"/Analysis/ES/GLM.norm.2015_dredged01.csv"))


#do weredas separately
df.low<-d.F[d.F$kebele!="Badessa"&d.F$kebele!="Weyra",]
df.hi<-d.F[d.F$kebele=="Badessa"|d.F$kebele=="Weyra",]

#analyse difference in yields (2014 and 2016)
options(na.action = "na.omit")
qqp(d.F.plot.16$diff.yld, "norm")

sm.16d <-lm(diff.yld1416~ rescale(fruitset) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=d.F.plot.16)
summary(sm.16d)
#check heteroskedasticity
diagnos <- data.frame(Resid = resid(sm.16d, type = "pearson"), Fitted = fitted(sm.16d),Variable = d.F.plot.16$Plot[!is.na(d.F.plot.16$propCLR)&!is.na(d.F.plot.16$diff.yld1416)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.2016.ylddiff.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

pm.16d <-lm(diff.yld1416~ rescale(fruitset) + rescale(density)  + rescale(propCBD) + rescale(propCLR)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer)  + rescale(prop.fdrop) + rescale(elevation), 
            data=d.F.plot.16)
summary(pm.16d)
pdf(paste0(getwd(),"/Analysis/ES/Plot.2016.ylddiff.norm_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#analyse difference in yields for both 2015 and 2016, include tmax
options(na.action = "na.omit")
#remove plots without vpd measures
#d.F.plot.1516.v <- d.F.plot.1516 %>% filter(!is.na(vpd.flower)&!is.na(diff.yld))
d.F.plot.1516 <- d.F.plot.1516 %>% filter(!is.na(diff.yld)&Plot!="H6") %>% mutate(low.yield=replace(low.yield,low.yield>=0.5,1),low.yield=replace(low.yield,low.yield<0.5,0))
d.F.plot.1516$low.yield<-factor(d.F.plot.1516$low.yield)

qqp(d.F.plot.1516$diff.yld, "norm")

sm.1516d <-lm(diff.yld ~ rescale(fruitset) + rescale(Tot.P.ppm) + rescale(CN.ratio) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(density) + rescale(low.yield) +
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation),
              data=d.F.plot.1516)
summary(sm.1516d)

#check heteroskedasticity
diagnos <- data.frame(Resid = resid(sm.1516d, type = "pearson"), Fitted = fitted(sm.1516d),Variable = d.F.plot.1516$Plot[!is.na(d.F.plot.1516$propCLR)&!is.na(d.F.plot.1516$diff.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.201516.ylddiff.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

pm.1516d <-lm(diff.yld ~ rescale(fruitset) + rescale(Tot.P.ppm) + rescale(CN.ratio) +  rescale(K.meq)  + rescale(low.yield) +
                rescale(BA.legume) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation),
              data=d.F.plot.1516)
summary(pm.1516d)


pdf(paste0(getwd(),"/Analysis/ES/Plot.201516.ylddiff.norm_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

options(na.action = "na.fail")
pm.1516d.d<-dredge(pm.1516d)

dredg.m03<-subset(pm.1516d.d,delta<2)
write.csv(dredg.m03,paste0(getwd(),"/Analysis/ES/GLM.norm.201516.ylddiff_dredged01.csv"))

cand.set<-list()
#delta 2 has 14 models reduced to 4
cand.set[[1]]<-lm(diff.yld ~ rescale(fruitset) + rescale(low.yield) + rescale(Tot.P.ppm) + rescale(K.meq)  + rescale(coffee.area.ha) + rescale(elevation),data=d.F.plot.1516)
cand.set[[2]]<-lm(diff.yld ~ rescale(fruitset) + rescale(Shannon.i) + rescale(low.yield) + rescale(Tot.P.ppm) + rescale(K.meq)  + rescale(coffee.area.ha) + rescale(BA.legume) + rescale(b.ffer),data=d.F.plot.1516)
cand.set[[3]]<-lm(diff.yld ~ rescale(fruitset) + rescale(Shannon.i) + rescale(low.yield) + rescale(Tot.P.ppm) + rescale(K.meq)  + rescale(coffee.area.ha) + rescale(b.ffer),data=d.F.plot.1516)
cand.set[[4]]<-lm(diff.yld ~ rescale(fruitset) + rescale(low.yield) + rescale(Tot.P.ppm) + rescale(K.meq)  + rescale(coffee.area.ha) + rescale(b.ffer),data=d.F.plot.1516)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_ylddiff.201516.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.201516.norm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.201516.norm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.201516.norm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Fruitset","Low Yielding Plot","Soil Phosphorous","Soil Potassium","Coffee Area","Location in\nBuffer","Shade Diversity","Elevation","Basal Area of\nLeguminous Trees","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Yield Difference\n Per Shrub (2015 & 2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_ylddiff_201516.pdf"),height=8,width=6)

#standardize variables
df <- data.frame(cbind(as.character(d.F.plot.1516$Plot),d.F.plot.1516$low.yield,d.F.plot.1516$year,d.F.plot.1516$diff.yld,rescale(d.F.plot.1516$b.ffer),rescale(d.F.plot.1516$K.meq),rescale(d.F.plot.1516$coffee.area.ha),
                       rescale(d.F.plot.1516$fruitset),rescale(d.F.plot.1516$elevation),rescale(d.F.plot.1516$Shannon.i),rescale(d.F.plot.1516$Tot.P.ppm),rescale(d.F.plot.1516$BA.legume),rescale(d.F.plot.1516$low.yield)),stringsAsFactors = F)
colnames(df)<-c("farm","low.yield","year","diff.yld","z.b.ffer","z.K.meq","z.coffee.area.ha","z.fruitset","z.elevation","z.Shannon.i","z.Tot.P.ppm","z.BA.legume","z.low.yield")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_ylddiff.201516.norm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,year) %>% mutate(diff.yld.mod=tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="rescale(coffee.area.ha)","Estimate"]*z.coffee.area.ha+tmp[tmp$Comparison=="rescale(fruitset)","Estimate"]*z.fruitset+tmp[tmp$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i+tmp[tmp$Comparison=="rescale(low.yield)","Estimate"]*z.low.yield+
                                                                        tmp[tmp$Comparison=="rescale(Tot.P.ppm)","Estimate"]*z.Tot.P.ppm+tmp[tmp$Comparison=="rescale(elevation)","Estimate"]*z.elevation + tmp[tmp$Comparison=="rescale(b.ffer)","Estimate"]*z.b.ffer + tmp[tmp$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume + tmp[tmp$Comparison=="rescale(K.meq)","Estimate"]*z.K.meq)


ggplot(df,aes(diff.yld,diff.yld.mod)) + geom_point(aes(color=factor(low.yield),shape=factor(year))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(-0.6,0.5)+xlim(-0.6,0.5)+
  xlab("Observed Difference in Yield [kg]")+ylab("Modelled Difference in Yield [kg]")+
  ggtitle("2015 & 2016")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.ylddiff.201516.norm.pdf"))

#analyse 2015 and 2016 by wereda
#Doraani
df.hi.15<- df.hi %>% filter(year==2015)
df.hi.16<- df.hi %>% filter(year==2016)
qqp(df.hi.16$Shrub.kg.1, "norm")

options(na.action = "na.omit")

fm.15d <-lmer(Shrub.kg~ rescale(fruitset) + rescale(ah.flower) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation) + (1|Plot) , 
              data=df.hi.15,REML=F)
summary(fm.15d)

fm.16d <-lm(Shrub.kg~ rescale(fruitset) + rescale(ah.flower) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
              data=df.hi.16)
summary(fm.16d)

am.15d <-lmer(Shrub.kg~ rescale(fruitset) + rescale(ah.flower) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation) + (1|Plot), 
              data=df.hi.15,REML=F)
summary(am.15d)
r.squaredGLMM(am.15d)

am.16d <-lm(Shrub.kg~ rescale(fruitset) + rescale(ah.flower) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
              rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(coffee.area.ha) + rescale(elevation), 
            data=df.hi.16)
summary(am.16d)

#check correlations
vcov(am.15d)

options(na.action = "na.fail")
am.15d.d<-dredge(am.15d)

dredg.m01<-subset(am.15d.d,delta<6)
write.csv(dredg.m01,paste0(getwd(),"/Analysis/ES/GLM.lnorm.doraani.201516_dredged01.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.201516.doraani.norm_residuals.pdf"),width=8,height=5)
bwplot(am.15d@frame$Plot~resid(am.15d))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.16d, type = "pearson"), Fitted = fitted(am.16d),Variable = df.hi.16$Plot[!is.na(df.hi.16$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.doraani.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.doraani.norm_qqplotResiduals_all.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

cand.set<-list()
#delta 2 has seven models reduced to 4
cand.set[[1]]<-lmer(Shrub.kg.1~ rescale(ah.flower) + rescale(CN.ratio) + rescale(density) + rescale(elevation) + rescale(K.meq) + rescale(Tot.P.ppm) + (1|Shrub.id), data = df.hi.1516 ,REML=F)
cand.set[[2]]<-lmer(Shrub.kg.1~ rescale(ah.flower) + rescale(CN.ratio) + rescale(density) + rescale(elevation) + rescale(K.meq) + rescale(coffee.area.ha) + (1|Shrub.id), data = df.hi.1516 ,REML=F)
cand.set[[3]]<-lmer(Shrub.kg.1~ rescale(ah.flower) + rescale(CN.ratio) + rescale(density) + rescale(elevation) + rescale(K.meq) + (1|Shrub.id), data = df.hi.1516 ,REML=F)
cand.set[[4]]<-lmer(Shrub.kg.1~ rescale(ah.flower) + rescale(CN.ratio) + rescale(density) + rescale(coffee.area.ha) + (1|Shrub.id), data = df.hi.1516 ,REML=F)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_doraani.201516.norm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.2015.lnorm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2015.doraani.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.doraani.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Coffee Planting Density","Total Soil P","Location in Buffer","Absolute Humidity During Flowering","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Per Shrub Yield (2015)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2015.doraani.pdf"))


#standardize variables
df <- data.frame(cbind(as.character(df.hi.15$farm),as.character(df.hi.15$Shrub.id),as.character(df.hi.15$wereda),df.hi.15$Shrub.kg.1,rescale(df.hi.15$K.meq),rescale(df.hi.15$density),rescale(df.hi.15$b.ffer),rescale(df.hi.15$ah.flower)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.K.meq","z.density","c.b.ffer","z.ah.flower")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.doraani.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="rescale(density)","Estimate"]*z.density+tmp[tmp$Comparison=="rescale(b.ffer)","Estimate"]*c.b.ffer+tmp[tmp$Comparison=="rescale(ah.flower)","Estimate"]*z.ah.flower+
                                                                        tmp[tmp$Comparison=="rescale(K.meq)","Estimate"]*z.K.meq))

#take median of each observed farm
df.med<- df %>% group_by(farm) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("Doraani 2015")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_lnorm.doraani.2015.pdf"))

#Yayu plots
df.low.15<-df.low %>% filter(year=="2015") %>% filter(!is.na(Tot.fruits))

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/Yayu_2015_Shrub.lnorm.pdf"))
qqp(df.low$Shrub.kg.1, "lnorm")
dev.off()

options(na.action = "na.omit")

am.15y <-lmer(log(Shrub.kg.1)~ rescale(elevation) + rescale(fruitset) + rescale(ah.flower) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + (1|Plot), 
              data=df.low.15,REML=F)
#am.14.ds<-standardize(am.14.d)
summary(am.15y)
r.squaredGLMM(am.15y)
vcov(am.15y)

options(na.action = "na.fail")
am.15y.d<-dredge(am.15y)

dredg.m04<-subset(am.15y.d,delta<2)
write.csv(dredg.m04,paste0(getwd(),"/Analysis/ES/GLM.lnorm.yayu.2015_dredged01.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.yayu.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.15y@frame$Plot~resid(am.15y))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.15y, type = "pearson"), Fitted = fitted(am.15y),Variable = df.low.15$Plot[!is.na(df.low.15$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.yayu.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.yayu.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.yayu.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

cand.set<-list()
#delta 2 has eight models reduced to 3
cand.set[[1]]<-lmer(log(Shrub.kg.1)~ rescale(BA.legume) + rescale(density)  + rescale(fruitset) + rescale(prop.fdrop)  + rescale(K.meq) + (1|Plot), data=df.low.15,REML=F)
cand.set[[2]]<-lmer(log(Shrub.kg.1)~ rescale(BA.legume) + rescale(density)  + rescale(fruitset) + rescale(prop.fdrop)  + (1|Plot), data=df.low.15,REML=F)
cand.set[[3]]<-lmer(log(Shrub.kg.1)~ rescale(density)  + rescale(fruitset) + rescale(prop.fdrop)  + rescale(K.meq) + rescale(propCBD) + (1|Plot), data=df.low.15,REML=F)
cand.set[[4]]<-lmer(log(Shrub.kg.1)~ rescale(density)  + rescale(fruitset) + rescale(prop.fdrop)  + rescale(K.meq) + (1|Plot), data=df.low.15,REML=F)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_yayu.2015.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.2015.lnorm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#wwrite.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2015.yayu.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.yayu.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Coffee Planting Density","Fruitset","Proportion of Berries Dropped","Soil K","Basal Area of Leguminous Trees","Incidence of CBD","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors\non Per Shrub Yield Yayu (2015)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2015.yayu.pdf"),height=8,width=6)


#do 2014 for each wereda separately
df.hi.14<- df.hi %>% filter(year==2014)
qqp(df.hi.14$Shrub.kg.1, "norm")

options(na.action = "na.omit")

am.14d <-lmer(Shrub.kg.1~ rescale(elevation) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + (1|Plot), 
              data=df.hi.14,REML=F)
#am.14.ds<-standardize(am.14.d)
summary(am.14d)
r.squaredGLMM(am.14d)

options(na.action = "na.fail")
am.14.d<-dredge(am.14d)

dredg.m03<-subset(am.14.d,delta<2)
write.csv(dredg.m03,paste0(getwd(),"/Analysis/ES/GLM.lnorm.doraani.2014_dredged03.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.doraani.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.14d@frame$Plot~resid(am.14d))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.14d, type = "pearson"), Fitted = fitted(am.14d),Variable = df.hi.14$Plot[!is.na(df.hi.14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.doraani.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.doraani.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.doraani.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

cand.set<-list()
#delta 2 has eight models reduced to 3
cand.set[[1]]<-lmer(log(Shrub.kg.1)~ rescale(prop.fdrop) + rescale(prop.ldrop) + rescale(propCBD) + rescale(Shannon.i) + (1|Plot), data = df.hi.14 ,REML=F)
cand.set[[2]]<-lmer(log(Shrub.kg.1)~ rescale(prop.fdrop) + rescale(prop.ldrop) + rescale(propCBD) + (1|Plot), data = df.hi.14  ,REML=F)
cand.set[[3]]<-lmer(log(Shrub.kg.1)~ rescale(prop.fdrop) + rescale(prop.ldrop) + rescale(propCBD) + rescale(CN.ratio) + (1|Plot), data = df.hi.14  ,REML=F)
cand.set[[4]]<-lmer(log(Shrub.kg.1)~ rescale(prop.fdrop) + rescale(prop.ldrop) + rescale(propCBD) + rescale(CN.ratio) + rescale(Shannon.i) + (1|Plot), data = df.hi.14,REML=F)
cand.set[[5]]<-lmer(log(Shrub.kg.1)~ rescale(prop.fdrop) + rescale(prop.ldrop) + rescale(propCBD) + rescale(Shannon.i) + rescale(density) + (1|Plot), data = df.hi.14  ,REML=F)
cand.set[[6]]<-lmer(log(Shrub.kg.1)~ rescale(prop.fdrop) + rescale(prop.ldrop) + rescale(propCBD) + rescale(Shannon.i) + rescale(density) + rescale(propCBB) + (1|Plot), data = df.hi.14  ,REML=F)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_doraani.2014.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.2014.lnorm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2014.doraani.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2014.doraani.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Proportion of Berries Dropped","Proportion of Leaves Dropped","Incidence of CBD","Diversity of Shade Trees","Soil C:N","Planting density","Incidence of CBB","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Per Shrub Yield (2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2014.doraani.pdf"),height=6,width=5)


#standardize variables
df <- data.frame(cbind(as.character(df.hi.14$farm),as.character(df.hi.14$Shrub.id),as.character(df.hi.14$wereda),df.hi.14$Shrub.kg.1,rescale(df.hi.14$propCBD),rescale(df.hi.14$prop.ldrop),rescale(df.hi.14$prop.fdrop),rescale(df.hi.14$CN.ratio),rescale(df.hi.14$Shannon.i),rescale(df.hi.14$density),rescale(df.hi.14$propCBB)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.propCBD","z.prop.ldrop","z.prop.fdrop","z.CN.ratio","z.Shannon.i","z.density","z.propCBB")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2014.doraani.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="rescale(propCBD)","Estimate"]*z.propCBD+tmp[tmp$Comparison=="rescale(prop.ldrop)","Estimate"]*z.prop.ldrop+tmp[tmp$Comparison=="rescale(prop.fdrop)","Estimate"]*z.prop.fdrop+
                                                                        tmp[tmp$Comparison=="rescale(CN.ratio)","Estimate"]*z.CN.ratio+tmp[tmp$Comparison=="rescale(Shannon.i)","Estimate"]*z.Shannon.i + tmp[tmp$Comparison=="rescale(density)","Estimate"]*z.density + tmp[tmp$Comparison=="rescale(propCBB)","Estimate"]*z.propCBB))

#take median of each observed farm
df.med<- df %>% group_by(farm) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("Doraani 2014")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_lnorm.doraani.2014.pdf"),height=5,width=5)

#Yayu plots
df.low.14<-df.low %>% filter(year=="2014") %>% filter(!is.na(Tot.fruits))

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/Yayu_2014_Shrub.lnorm.pdf"))
qqp(df.low$Shrub.kg.1, "lnorm")
dev.off()

options(na.action = "na.omit")

am.14y <-lmer(log(Shrub.kg.1)~ rescale(elevation) + rescale(Tot.P.ppm) + rescale(CN.ratio) + rescale(density) +  rescale(K.meq)  + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + 
                rescale(BA.legume) + rescale(GapDry) + rescale(Shannon.i) + rescale(b.ffer) + rescale(prop.ldrop)  + rescale(prop.fdrop) + (1|Plot), 
              data=df.low.14,REML=F)
#am.14.ds<-standardize(am.14.d)
summary(am.14y)
r.squaredGLMM(am.14y)

options(na.action = "na.fail")
am.14y.d<-dredge(am.14y)

dredg.m01<-subset(am.14y.d,delta<2)
write.csv(dredg.m01,paste0(getwd(),"/Analysis/ES/GLM.lnorm.yayu.2014_dredged01.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.yayu.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.14y@frame$Plot~resid(am.14y))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.14y, type = "pearson"), Fitted = fitted(am.14y),Variable = df.low.14$Plot[!is.na(df.low.14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.yayu.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.yayu.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.yayu.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

cand.set<-list()
#delta 2 has eight models reduced to 3
cand.set[[1]]<-lmer(log(Shrub.kg.1)~ rescale(elevation) + rescale(propCBB)  + rescale(BA.legume) + rescale(prop.ldrop)  + rescale(prop.fdrop) + (1|Plot), data=df.low.14,REML=F)
cand.set[[2]]<-lmer(log(Shrub.kg.1)~ rescale(elevation) + rescale(propCBB) + rescale(prop.ldrop)  + rescale(prop.fdrop) + (1|Plot), data=df.low.14,REML=F)
cand.set[[3]]<-lmer(log(Shrub.kg.1)~ rescale(propCBB)  + rescale(BA.legume) + rescale(prop.ldrop)  + rescale(prop.fdrop) + (1|Plot), data=df.low.14,REML=F)
cand.set[[4]]<-lmer(log(Shrub.kg.1)~ rescale(elevation) + rescale(propCBB)  + rescale(BA.legume)  + rescale(prop.fdrop) + rescale(propCBD) + (1|Plot), data=df.low.14,REML=F)
cand.set[[5]]<-lmer(log(Shrub.kg.1)~ rescale(propCBB)  + rescale(prop.ldrop)  + rescale(prop.fdrop) + rescale(propCBD) + (1|Plot), data=df.low.14,REML=F)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_yayu.2014.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.2014.lnorm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2014.yayu.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2014.yayu.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Incidence of CBB","Proportion of Berries Dropped","Proportion of Leaves Dropped","Elevation","Basal Area of Leguminous Trees","Incidence of CBD","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Per Shrub Yield (2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2014.yayu.pdf"))


#standardize variables
df <- data.frame(cbind(as.character(df.low.14$farm),as.character(df.low.14$Shrub.id),as.character(df.low.14$wereda),df.low.14$Shrub.kg.1,rescale(df.low.14$propCBD),rescale(df.low.14$propCBB),rescale(df.low.14$prop.ldrop),rescale(df.low.14$prop.fdrop),rescale(df.low.14$elevation),rescale(df.low.14$BA.legume)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.propCBD","z.propCBB","z.prop.ldrop","z.prop.fdrop","z.elevation","z.BA.legume")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2014.yayu.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="rescale(propCBD)","Estimate"]*z.propCBD+tmp[tmp$Comparison=="rescale(propCBB)","Estimate"]*z.propCBB+tmp[tmp$Comparison=="rescale(prop.ldrop)","Estimate"]*z.prop.ldrop+tmp[tmp$Comparison=="rescale(prop.fdrop)","Estimate"]*z.prop.fdrop+
                                                                        tmp[tmp$Comparison=="rescale(elevation)","Estimate"]*z.elevation+tmp[tmp$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume ))

#take median of each observed farm
df.med<- df %>% group_by(farm) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("Yayu 2014")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_lnorm.yayu.2014.pdf"))

#do for 2015 Yayu and Doraani


#standardize variables
df <- data.frame(cbind(as.character(df.low.15$farm),as.character(df.low.15$Shrub.id),as.character(df.low.15$wereda),df.low.15$Shrub.kg.1,rescale(df.low.15$propCBD),rescale(df.low.15$fruitset),rescale(df.low.15$density),rescale(df.low.15$prop.fdrop),rescale(df.low.15$K.meq),rescale(df.low.15$BA.legume)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.propCBD","z.fruitset","z.density","z.prop.fdrop","z.K.meq","z.BA.legume")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.yayu.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="rescale(propCBD)","Estimate"]*z.propCBD+tmp[tmp$Comparison=="rescale(fruitset)","Estimate"]*z.fruitset+tmp[tmp$Comparison=="rescale(density)","Estimate"]*z.density+tmp[tmp$Comparison=="rescale(prop.fdrop)","Estimate"]*z.prop.fdrop+
                                                                        tmp[tmp$Comparison=="rescale(K.meq)","Estimate"]*z.K.meq+tmp[tmp$Comparison=="rescale(BA.legume)","Estimate"]*z.BA.legume ))

#take median of each observed farm
df.med<- df %>% group_by(farm) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("Yayu 2015")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_lnorm.yayu.2015.pdf"),height=5,width=5)

#model yield difference between 2014 and 2016
#Yayu
y.ld14<-df.low %>% filter(year=="2014") %>% filter(!is.na(Tot.fruits)) %>% group_by(Plot) %>% summarise(Shrub.kg.14=median(Shrub.kg.1,na.rm=T))
y.ld16<-df.low %>% filter(year=="2016") %>% filter(!is.na(Tot.fruits)) %>% group_by(Plot) %>% 
  summarise(Shrub.kg.16=median(Shrub.kg.1,na.rm=T),coffee.area.ha=mean(coffee.area.ha),elevation=mean(elevation),patcharea=mean(patcharea),GapDry=mean(GapDry),b.ffer=b.ffer[1],density=mean(density),CN.ratio=mean(CN.ratio),Tot.P.ppm=mean(Tot.P.ppm),K.meq=mean(K.meq),
            BA.legume=mean(BA.legume),fruitset=median(fruitset,na.rm=T),prop.fdrop=median(prop.fdrop,na.rm=T),propCLR=median(propCLR,na.rm=T),propCBD=median(propCBD,na.rm = T),propCBB=median(propCBB,na.rm=T))

y.ld16<-left_join(y.ld16,y.ld14,by="Plot")
y.ld16 <- y.ld16 %>% mutate(diff.yld = Shrub.kg.16-Shrub.kg.14) 

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/Yayu_2014.16.diff_Shrub.lnorm.pdf"))
qqp(y.ld16$diff.yld, "norm")
dev.off()

options(na.action = "na.omit")

am.ydiff <- lm(diff.yld ~ rescale(elevation)+rescale(patcharea)+ rescale(CN.ratio)+rescale(BA.legume) + rescale(K.meq)+ rescale(Tot.P.ppm),data=y.ld16)
#am.14.ds<-standardize(am.14.d)
summary(am.ydiff)
vcov(am.ydiff)

#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.ydiff, type = "pearson"), Fitted = fitted(am.ydiff),Variable = y.ld16$Plot[!is.na(y.ld16$diff.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.16diff.yayu.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.16diff.yayu.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid , data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#plot lm model
pdf(paste0(getwd(),"/Analysis/ES/Model_yielddiff.2014.16.yayu.norm.pdf"),width=8,height=8)
plot_model(am.ydiff,title="Factors Influencing Difference in Shrub Yield Between 2014 and 2016") + ylab("Effect Size") + xlab("Variable") 
dev.off()

tmp<-data.frame(coefficients(am.ydiff),stringsAsFactors = F)
colnames(tmp)<-"Estimate"
tmp$Variable<-rownames(tmp)
tmp$Std.error<-summary(am.ydiff)$coefficients[, 2]
tmp$label <- c("Intercept","elevation","patcharea","CN.ratio","BA.legume","K.meq","Tot.P.ppm")
tmp$t.value <-summary(am.ydiff)$coefficients[, 3]
tmp$confint.LL<-confint(am.ydiff)[,1]
tmp$confint.UL<-confint(am.ydiff)[,2]

write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model_yield_diff2014.16.yayu.pdf"))

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

g1<-ggplot(tmp[tmp$label!="Intercept",], aes(x = label, y = Estimate, ymin = confint.LL, ymax = confint.UL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors\non Difference in Per Shrub Yield Yayu (2014&2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_yield_diff2014.16.yayu.pdf"),height=8,width=6)

#standardize variables
df <- data.frame(cbind(as.character(y.ld16$Plot),y.ld16$diff.yld,rescale(y.ld16$Tot.P.ppm),rescale(y.ld16$patcharea),rescale(y.ld16$K.meq),rescale(y.ld16$elevation),rescale(y.ld16$CN.ratio),rescale(y.ld16$BA.legume)),stringsAsFactors = F)
colnames(df)<-c("Plot","Shrub.yld.diff","z.Tot.P.ppm","z.patcharea","z.K.meq","z.elevation","z.CN.ratio","z.BA.legume")
df[,2:ncol(df)]<-sapply(df[,2:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model_yield_diff2014.16.yayu.pdf"))

#test validity of the model
df<-df %>% filter(!is.na(Shrub.yld.diff)) %>% group_by(Plot) %>% mutate(shrub.diff.mod=tmp[tmp$Variable=="(Intercept)","Estimate"]+tmp[tmp$Variable=="rescale(Tot.P.ppm)","Estimate"]*z.Tot.P.ppm+tmp[tmp$Variable=="rescale(patcharea)","Estimate"]*z.patcharea+tmp[tmp$Variable=="rescale(elevation)","Estimate"]*z.elevation+
                                                                        tmp[tmp$Variable=="rescale(K.meq)","Estimate"]*z.K.meq+tmp[tmp$Variable=="rescale(BA.legume)","Estimate"]*z.BA.legume +tmp[tmp$Variable=="rescale(CN.ratio)","Estimate"]*z.CN.ratio )
ggplot(df,aes(Shrub.yld.diff,shrub.diff.mod)) + geom_point() + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(-0.8,1.0)+xlim(-0.8,1.0)+geom_vline(xintercept = 0,linetype="dashed",color="grey") + geom_hline(yintercept=0,linetype="dashed",color="grey")+
  xlab("Observed Difference in Shrub Yield [kg]")+ylab("Modelled Difference in Shrub Yield [kg]")+
  ggtitle("Yayu 2014/16")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.diffyield_lnorm.yayu.2014.16.pdf"),height=5,width=5)

#both (relative)
y.ld14<-d.F %>% filter(year=="2014") %>% filter(!is.na(Tot.fruits)) %>% group_by(Plot) %>% summarise(Shrub.kg.14=median(Shrub.kg.1,na.rm=T))
y.ld16<-d.F %>% filter(year=="2016") %>% filter(!is.na(Tot.fruits)) %>% group_by(Plot) %>% 
  summarise(Shrub.kg.16=median(Shrub.kg.1,na.rm=T),coffee.area.ha=mean(coffee.area.ha),elevation=mean(elevation),patcharea=mean(patcharea),GapDry=mean(GapDry),b.ffer=b.ffer[1],density=mean(density),CN.ratio=mean(CN.ratio),Tot.P.ppm=mean(Tot.P.ppm),K.meq=mean(K.meq),
            BA.legume=mean(BA.legume),BA.all=mean(BA.all,na.rm=T), Shannon.i=mean(Shannon.i,na.rm=T),fruitset=median(fruitset,na.rm=T),prop.fdrop=median(prop.fdrop,na.rm=T),propCLR=median(propCLR,na.rm=T),propCBD=median(propCBD,na.rm = T),propCBB=median(propCBB,na.rm=T),wereda=wereda[1])

y.ld16<-left_join(y.ld16,y.ld14,by="Plot")
y.ld16 <- y.ld16 %>% mutate(diff.yld = Shrub.kg.16/Shrub.kg.14) 

#remove WA9, weird plot
y.ld16<-y.ld16 %>% filter(Plot!="WA9")

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/AllPlots_2014.16.diff_Shrub.norm.pdf"))
qqp(y.ld16$diff.yld, "norm")
dev.off()

options(na.action = "na.omit")

#Yayu
yayu.diff16<-y.ld16 %>% filter(wereda=="Yayu"&!is.na(diff.yld))

am.diff <- lm(diff.yld ~ rescale(elevation)+rescale(patcharea) + rescale(GapDry) + rescale(propCLR) +rescale(BA.legume) + rescale(CN.ratio),data=yayu.diff16)
#am.14.ds<-standardize(am.14.d)
summary(am.diff)
vcov(am.diff)

options(na.action = "na.fail")
am.diff.d<-dredge(am.diff)
dredg.m05<-subset(am.diff.d,delta<2)

write.csv(dredg.m05,paste0(getwd(),"/Analysis/ES/GLM.lnorm.yayu.reldiff_dredged01.csv"))

#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.diff, type = "pearson"), Fitted = fitted(am.diff),Variable = yayu.diff16$Plot )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.16diff.yayu.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.16diff.yayu.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid , data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

cand.set<-list()
#delta 2 has eight models reduced to 3
cand.set[[1]]<-lm(diff.yld~ rescale(patcharea) + rescale(elevation), data=yayu.diff16)
cand.set[[2]]<-lm(diff.yld~ rescale(patcharea) + rescale(elevation) + rescale(BA.legume), data=yayu.diff16)
cand.set[[3]]<-lm(diff.yld~ rescale(elevation), data=yayu.diff16)
cand.set[[4]]<-lm(diff.yld~ rescale(patcharea) + rescale(elevation) + rescale(GapDry), data=yayu.diff16)
cand.set[[5]]<-lm(diff.yld~ rescale(elevation) + rescale(GapDry), data=yayu.diff16)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_yayu.reldiff.norm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.reldiff.norm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_reldiff.yayu.norm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_reldiff.yayu.norm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Elevation","Patch Area [ha]","GapDry","Basal Area of Leguminous Trees","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Relative Difference\nin Per Shrub Yield Yayu (2016/2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_reldiff.yayu.pdf"),height=8,width=6)

#Doraani
doraani.diff16<-y.ld16 %>% filter(wereda=="Doraani"&!is.na(diff.yld))

am.diff2 <- lm(diff.yld ~ rescale(elevation)+rescale(patcharea) + rescale(density) + rescale(GapDry) + rescale(propCLR) +rescale(BA.legume)*rescale(CN.ratio) +rescale(K.meq),data=doraani.diff16)
#am.14.ds<-standardize(am.14.d)
summary(am.diff2)
vcov(am.diff2)

options(na.action = "na.fail")
am.diff2.d<-dredge(am.diff2)
dredg.m06<-subset(am.diff2.d,delta<2)

write.csv(dredg.m06,paste0(getwd(),"/Analysis/ES/GLM.norm.doraani.reldiff_dredged01.csv"))

#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.diff2, type = "pearson"), Fitted = fitted(am.diff2),Variable = doraani.diff16$Plot )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.16diff.doraani.norm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.16diff.doraani.norm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid , data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#cand.set<-list()
#delta 2 has eight models reduced to 3
#cand.set[[1]]<-lm(diff.yld~ rescale(BA.legume) + rescale(K.meq) + rescale(propCLR), data=doraani.diff16)
#cand.set[[2]]<-lm(diff.yld~ rescale(BA.legume) + rescale(propCLR), data=doraani.diff16)

##create a vector of names to trace back models in set
#Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
#res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
#write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_doraani.reldiff.norm.delta2.csv"))
am.diff2<-lm(diff.yld ~ rescale(propCLR) +rescale(BA.legume)*rescale(CN.ratio),data=doraani.diff16)

#topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.reldiff.norm.delta2.txt"))
summary(am.diff2)
sink() 

tmp<-as.data.frame(coefficients(am.diff2))
tmp$Comparison<-rownames(tmp)
colnames(tmp)<-c("Estimate","Comparison")

#calculate model average and confidence intervals
#vars<-list()
#for(i in 1:nrow(x1)){
#  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
#}
#vars.1<-do.call(rbind.data.frame,vars)
#colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
#vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

#vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

tmp$std.err<-summary(am.diff2)$coefficients[,2]
tmp$tvalue<-summary(am.diff2)$coefficients[,3]
tmp$pvalue<-summary(am.diff2)$coefficients[,4]

#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_reldiff.doraani.norm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_reldiff.doraani.norm_delta2.confint.csv"))
#tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(abs(tmp$tvalue),decreasing=T),"Comparison"],labels=c("(Intercept)","Basal Area of Leguminous Trees","Incidence of Coffee Leaf Rust","BA Legume: Soil C/N","Soil C/N"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$tvalue,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[tmp$Comparison!="(Intercept)",]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Estimate-std.err, ymax = Estimate+std.err)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Relative Difference\nin Per Shrub Yield Doraani (2016/2014)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_reldiff.doraani.pdf"),height=8,width=6)

#Doraani Plots
df.hi<-df.hi %>% filter(year!="2014") %>% filter(!is.na(Tot.fruits))

#df.low.z <- data.frame(scale(df.low %>% select(-X.1,-X,-ID,-year,-Plot,-Shrub.id,-kebele,-tmax.flower,-tmax.fruit,-tavg.flower,-tavg.fruit,-vpd.flower,-vpd.fruit,-new.ah.fruit,-compost.bin,-buffer) ))

#df.low.z$Plot<-df.low$Plot
#df.low.z$Shrub.id<-df.low$Shrub.id
#df.low.z$year<-df.low$year
#df.low.z$Shrub.kg.1 <-df.low$Shrub.kg.1

qqp(df.hi$Shrub.kg.1, "norm")

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/Doraani_total_harvest.lnorm.pdf"))
qqp(df.hi$Shrub.kg.1, "lnorm")
dev.off()

# qqp requires estimates of the parameters of the negative binomial, Poisson # and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I # have shown below.
#nbinom <- fitdistr(d.F.16$Shrub.kg.1, "Negative Binomial") #harvest not binomial
#qqp(d.F$Shrub.kg.1, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

#poisson<-fitdistr(d.F$harvest.1, "Poisson") , poisson can only deal with whole integers
#qqp(d.F$harvest.1, "pois", poisson$estimate)
pdf(paste0(getwd(),"/Analysis/ES/Doraani_total_harvest.gamma.pdf"))
gamma<-fitdistr(df.hi$Shrub.kg.1,"gamma")
qqp(df.hi$Shrub.kg.1, "gamma",shape=gamma$estimate[[1]],rate=gamma$estimate[[2]])
dev.off()

options(na.action = "na.omit")

(fm001<-lmer(Shrub.kg.1~1+(1|Plot/Shrub.id)  + (1|year),data=df.hi,REML=F))
(fm001b<-lmer(Shrub.kg.1~1+(1|Plot/Shrub.id) ,data=df.hi,REML=F))
(fm001c<-lmer(Shrub.kg.1~1 + (1|Plot),data=df.hi,REML=F))
anova(fm001,fm001b,fm001c) #should use mixed models, for plot, Shrub.id and year

#try for temporal autocorrelation, never reaches a result...
#am01_lme<-lme(Shrub.kg.1~ elevation+patcharea + buffer,data=df.low,method="REML",
#random = ~1 + year|Plot/Shrub.id,correlation=corCAR1(form=~year|Plot/Shrub.id),control=list(maxIter=10000, niterEM=10000),na.action="na.omit")

#2015 & 2016
#run it log
am03 <- lmer(log(Shrub.kg.1)~ ah.flower + fruitset  + labour + CN.ratio + K.meq + propCLR + propCBD + propCBB + coffee.area.ha +
               density + BA.legume + GapDry + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F)
am03s<-standardize(am03)
summary(am03)
r.squaredGLMM(am03s)

options(na.action = "na.fail")
am03d<-dredge(am03)

dredg.m03<-subset(am03d,delta<2)
write.csv(dredg.m03,paste0(getwd(),"/Analysis/ES/GLM.lnorm.doraani_dredged03.csv"))

#do again with gamma
am04<-lmer(Shrub.kg.1~ ah.flower + fruitset  + labour + CN.ratio + propCBD + propCLR + propCBB + BA.legume + GapDry +
             patcharea  + (1|Plot/Shrub.id) + (1|year),data=df.low,family=Gamma)
am04s<-standardize(am04)
summary(am04s)
#r.squaredGLMM(am04s)

am02d<-dredge(am02)
dredg.m02<-subset(am02d,delta<6)
write.csv(dredg.m02,paste0(getwd(),"/Analysis/ES/GLM.lnorm.yayu_dredged02.csv"))

#Assumption 1: Within-group errors (or BLUPS) are independent and identically normally distributed, with mean zero and variance sigma squareda dn independent of random effects
#graphic tests include within-group fitted values, observed values and any covariates of interest
#boxplot of residuals by group, to check that residuals are centered around 0, but variability changes with group
pdf(paste0(getwd(),"/Analysis/ES/Shrub.doraani.2015_16.log_residuals.pdf"),width=8,height=5)
bwplot(am03@frame$Plot~resid(am03))
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015_16.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am02@frame$Plot~resid(am02))
dev.off()

#bwplot(fm02s@frame$PLOT~resid(fm02s))
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
plot(am03,id=0.05,adj=-0.3)

diagnos <- data.frame(Resid = resid(am03, type = "pearson"), Fitted = fitted(am03),Variable = df.hi$Plot)

pdf(paste0(getwd(),"/Analysis/ES/Shrub.doraani.2015.16.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.doraani.2015.16.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()

diagnos <- data.frame(Resid = resid(am02, type = "pearson"), Fitted = fitted(am02),Variable = df.low$Plot)

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.16.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.16.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## overal QQ normal plot
qqmath(~Resid, data = diagnos, distribution = qnorm)
## separate by variable
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm)
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.doraani.2015.16.lnorm._qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.16.lnorm._qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

diagnos <- data.frame(Resid = resid(fm02s, type = "pearson"), Fitted = fitted(fm02s),Variable = d.F.15$Plot[!is.na(d.F.15$propCLR)])

#for yayu
cand.set<-list()
#delta 2 has sixmodels
cand.set[[1]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + fruitset + BA.legume + GapDry + patcharea  + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F))
cand.set[[2]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + fruitset + GapDry + patcharea  + propCBD   + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F))
cand.set[[3]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + CN.ratio + fruitset + GapDry + patcharea  + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F))
cand.set[[4]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + fruitset + patcharea  + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F))
cand.set[[5]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + fruitset + GapDry + labour + patcharea + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F))
cand.set[[6]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + fruitset  + labour + patcharea  + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F))

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_yayu.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.lnorm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_yayu.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Absolute Humidity\nDuring Flowering","Fruitset","Basal Area of\nLeguminous Shade Trees","Patch Area","Canopy Gap\nin Dry Season","Labour","Proportion of\nBerries w/ CBD","Soil C:N" ,"(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Yayu Per Shrub Yield (2015 & 2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_yayu.2015.16.pdf"))

#for Doraani
cand.set<-list()
#delta 2 has 10 models after nesting
cand.set[[1]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio + density  + fruitset  + GapDry + propCBB +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[2]]<-standardize(lmer(log(Shrub.kg.1)~ BA.legume + CN.ratio + density + fruitset  + GapDry  +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[3]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio +  density + fruitset  + GapDry +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[4]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio  + fruitset  + GapDry + propCBB +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[5]]<-standardize(lmer(log(Shrub.kg.1)~ BA.legume + CN.ratio + density  + fruitset + K.meq + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[6]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio + fruitset  + GapDry  + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[7]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio + density + fruitset  + GapDry + K.meq +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_doraani.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.lnorm.delta2.txt"))
summary(topmodels.avg)
sink() 

x1<-as.data.frame(summary(topmodels.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_doraani.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"],labels=c("Soil K","Basal Area of\nLeguminous Shade Trees","Proportion of\nBerries w/ CBB","Coffee Density","Canopy Gap\nin Dry Season","Soil C:N" ,"Fruitset","(Intercept)"))
#tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on Doraani Per Shrub Yield (2015 & 2016)")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,axis.text.x=element_text(angle = 45,hjust=1))
g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_doraani.2015.16.pdf"))

#model potential yield increase
tmp.y<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.lnorm_delta2.confint.csv"))
tmp.d<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.lnorm_delta2.confint.csv"))

d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))
#remove NA values for yield
d.F <- d.F[!is.na(d.F$Shrub.kg),]
d.F<-d.F %>% filter(year!=2014)
d.F$Shrub.kg.1<- d.F$Shrub.kg + .00001

#rescale all variables (fruitset, C:N, Canopy gap dry season, coffee density, proportion CBB and CBD, BA legume, Soil k, patch area, ah flower,labour)

#standardize variables by year
d.F$Plot<-as.character(d.F$Plot)
df <- d.F  %>% mutate(z.fruitset=rescale(fruitset),z.CN.ratio=rescale(CN.ratio),z.GapDry=rescale(GapDry),z.density=rescale(density),
                                                      z.propCBB=rescale(propCBB),z.propCBD=rescale(propCBD),z.BA.legume=rescale(BA.legume),z.K.meq=rescale(K.meq),
                                                      z.patcharea=rescale(patcharea),z.ah.flower=rescale(ah.flower),z.labour=rescale(labour))
 
df.low<-df %>% filter(kebele!="Badessa"&kebele!="Weyra") %>% filter(year!="2014")
df.hi<-df %>%filter(kebele=="Badessa"|kebele=="Weyra")  %>% filter(year!="2014")

#test validity of the model
df.low<-df.low %>% group_by(Plot,Shrub.id,year) %>% mutate(shrub.kg.mod=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*z.fruitset+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                                                                              tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                              tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour))
ggplot(df.low,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point(aes(color=factor(year))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  #ylim(0,0.5)+xlim(0,0.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Year")+
  ggtitle("Yayu")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.shrub.yayu.2015.16.pdf"))

#take median of predictions per plot
df.lo <- df.low %>% group_by(year,Plot) %>% summarise(shrub.kg = median(Shrub.kg.1,na.rm=T),shrub.kg.mod=median(shrub.kg.mod,na.rm=T))

ggplot(df.lo,aes(shrub.kg,shrub.kg.mod)) + geom_point(aes(color=factor(year))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.6)+xlim(0,1.6)+
  xlab("Observed Median per Shrub Yield [kg]")+ylab("Modelled Median per Shrub Yield [kg]")+scale_colour_discrete(name="Year")+
  ggtitle("Yayu")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.yayu.2015.16.pdf"))

#remove outlier and NAs, better for modelling 2015
df.lo <- df.lo %>% filter(shrub.kg.mod<1)

#test validity of the model
df.hi<-df.hi %>% group_by(Plot,Shrub.id,year) %>% mutate(shrub.kg.mod=exp(tmp.d[tmp.d$Comparison=="(Intercept)","Estimate"] + tmp.d[tmp.d$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + tmp.d[tmp.d$Comparison=="z.density","Estimate"]*z.density + tmp.d[tmp.d$Comparison=="z.fruitset","Estimate"]*z.fruitset+
                                                                            tmp.d[tmp.d$Comparison=="z.GapDry","Estimate"]*z.GapDry + tmp.d[tmp.d$Comparison=="z.propCBB","Estimate"]*z.propCBB + tmp.d[tmp.d$Comparison=="z.BA.legume","Estimate"]*z.BA.legume +
                                                                            tmp.d[tmp.d$Comparison=="z.K.meq","Estimate"]*z.K.meq))
ggplot(df.hi,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point(aes(color=factor(year))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  #ylim(0,0.5)+xlim(0,0.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Year")+
  ggtitle("Doraani")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.shrub.doraani.2015.16.pdf"))

#take median of predictions per plot - better for modelling 2016
df.high <- df.hi %>% group_by(year,Plot) %>% summarise(shrub.kg = median(Shrub.kg.1,na.rm=T),shrub.kg.mod=median(shrub.kg.mod,na.rm=T))

ggplot(df.high,aes(shrub.kg,shrub.kg.mod)) + geom_point(aes(color=factor(year))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,0.8)+xlim(0,0.8)+
  xlab("Observed Median per Shrub Yield [kg]")+ylab("Modelled Median per Shrub Yield [kg]")+scale_colour_discrete(name="Year")+
  ggtitle("Doraani")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    #,axis.text.x=element_text(angle = 45,hjust=1)
  )
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.doraani.2015.16.pdf"))

#calculate potential increase of yield by component
#yayu, BA.legume (only to median non-0 BA) and fruitset

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(fruitset1=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*max(df.low$z.fruitset,na.rm=T)+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                                                                             tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                             tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(fruitset2=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*min(df.low$z.fruitset,na.rm=T)+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                                                                             tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                             tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)

#find median BA.legume for those with BA forests
med.BAlegume<-df.low %>% filter(BA.legume!=0) %>% group_by(year) %>% summarise(med.BA=median(BA.legume,na.rm=T))
z.med.BA<-df.low %>% filter(BA.legume==med.BAlegume[1,2]) %>% group_by(year) %>% summarise(z.BA.legume=mean(z.BA.legume,na.rm=T))

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(BA.legume1=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*z.fruitset+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*as.numeric(z.med.BA[1,2])+
                                                                              tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                              tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(BA.legume2=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*z.fruitset+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*min(df.low$z.BA.legume,na.rm=T)+
                                                                              tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                              tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)


df.low<- df.low %>%  mutate(BA.legume1=replace(BA.legume1,BA.legume1<0,NA),fruitset1=replace(fruitset1,fruitset1<0,NA)) 
df.2<- df.low %>% group_by(Plot,year) %>% summarise(BA.legume1=median(BA.legume1,na.rm=T),fruitset1=median(fruitset1,na.rm=T)) %>%
  gather(key="variable",value="value",c(-Plot,-year))

ggplot(df.2,aes(Plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg shrub-1]")+facet_wrap(~year,ncol=1)+
  xlab("Farm")+scale_fill_discrete(labels=c("Basal Area Legume","Fruitset"))+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_yayu_2015.2016.med.pdf"))

#plot just 2015
ggplot(df.2[df.2$year==2015,],aes(Plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg shrub-1]")+
  xlab("Farm")+scale_fill_discrete(labels=c("Basal Area Legume","Fruitset"))+ggtitle("Yayu 2015")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="bottom"
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_yayu_2015.med.pdf"))

#save augmented yield values
write.csv(df.2[df.2$year==2015,],paste0(getwd(),"/Analysis/ES/Yield_potential_increase_yayu_2015.med.csv"))

#doraani, fruitset, soil C:N, GapDry, coffee density
df.hi<- df.hi %>% group_by(Plot, Shrub.id,year) %>% mutate(fruitset1=exp(tmp.d[tmp.d$Comparison=="(Intercept)","Estimate"] + tmp.d[tmp.d$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + tmp.d[tmp.d$Comparison=="z.density","Estimate"]*z.density + tmp.d[tmp.d$Comparison=="z.fruitset","Estimate"]*max(df.hi$z.fruitset,na.rm=T)+
                                                                           tmp.d[tmp.d$Comparison=="z.GapDry","Estimate"]*z.GapDry + tmp.d[tmp.d$Comparison=="z.propCBB","Estimate"]*z.propCBB + tmp.d[tmp.d$Comparison=="z.BA.legume","Estimate"]*z.BA.legume +
                                                                           tmp.d[tmp.d$Comparison=="z.K.meq","Estimate"]*z.K.meq)-Shrub.kg.1)

df.hi<- df.hi %>% group_by(Plot, Shrub.id,year) %>% mutate(CN.ratio1=exp(tmp.d[tmp.d$Comparison=="(Intercept)","Estimate"] + tmp.d[tmp.d$Comparison=="z.CN.ratio","Estimate"]*min(df.hi$z.CN.ratio,na.rm=T) + tmp.d[tmp.d$Comparison=="z.density","Estimate"]*z.density + tmp.d[tmp.d$Comparison=="z.fruitset","Estimate"]*z.fruitset+
                                                                           tmp.d[tmp.d$Comparison=="z.GapDry","Estimate"]*z.GapDry + tmp.d[tmp.d$Comparison=="z.propCBB","Estimate"]*z.propCBB + tmp.d[tmp.d$Comparison=="z.BA.legume","Estimate"]*z.BA.legume +
                                                                           tmp.d[tmp.d$Comparison=="z.K.meq","Estimate"]*z.K.meq)-Shrub.kg.1)

df.hi<- df.hi %>% group_by(Plot, Shrub.id,year) %>% mutate(GapDry1=exp(tmp.d[tmp.d$Comparison=="(Intercept)","Estimate"] + tmp.d[tmp.d$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + tmp.d[tmp.d$Comparison=="z.density","Estimate"]*z.density + tmp.d[tmp.d$Comparison=="z.fruitset","Estimate"]*z.fruitset+
                                                                         tmp.d[tmp.d$Comparison=="z.GapDry","Estimate"]*max(df.hi$z.GapDry,na.rm=T) + tmp.d[tmp.d$Comparison=="z.propCBB","Estimate"]*z.propCBB + tmp.d[tmp.d$Comparison=="z.BA.legume","Estimate"]*z.BA.legume +
                                                                         tmp.d[tmp.d$Comparison=="z.K.meq","Estimate"]*z.K.meq)-Shrub.kg.1)

df.hi<- df.hi %>% group_by(Plot, Shrub.id,year) %>% mutate(density1=exp(tmp.d[tmp.d$Comparison=="(Intercept)","Estimate"] + tmp.d[tmp.d$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + tmp.d[tmp.d$Comparison=="z.density","Estimate"]*max(df.hi$z.density,na.rm=T) + tmp.d[tmp.d$Comparison=="z.fruitset","Estimate"]*z.fruitset+
                                                                          tmp.d[tmp.d$Comparison=="z.GapDry","Estimate"]*z.GapDry + tmp.d[tmp.d$Comparison=="z.propCBB","Estimate"]*z.propCBB + tmp.d[tmp.d$Comparison=="z.BA.legume","Estimate"]*z.BA.legume +
                                                                          tmp.d[tmp.d$Comparison=="z.K.meq","Estimate"]*z.K.meq)-Shrub.kg.1)

df.hi<- df.hi %>% mutate(CN.ratio1=replace(CN.ratio1,CN.ratio1<0,NA),fruitset1=replace(fruitset1,fruitset1<0,NA),GapDry1=replace(GapDry1,GapDry1<0,NA),density1=replace(density1,density1<0,NA))
 
  
df.3<- df.hi %>% group_by(Plot,year) %>%  summarise(CN.ratio1=median(CN.ratio1,na.rm=T),fruitset1=median(fruitset1,na.rm=T),GapDry1=median(GapDry1,na.rm=T),density1=median(density1,na.rm=T)) %>%
  gather(key="variable",value="value",c(-Plot,-year))

ggplot(df.3,aes(Plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg shrub-1]")+facet_wrap(~year,ncol=1)+
  xlab("Farm")+scale_fill_discrete(labels=c("Soil C:N","Fruitset","Canopy Gap in Dry Season","Coffee Density"))+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_doraani_2015.2016.med.pdf"))

#do just for 2016
ggplot(df.3[df.3$year==2016,],aes(Plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg shrub-1]")+facet_wrap(~year,ncol=1)+
  xlab("Farm")+scale_fill_discrete(labels=c("Soil C:N","Fruitset","Canopy Gap in Dry Season","Coffee Density"))+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_doraani_2016.med.pdf"))

#save augmented yield values
write.csv(df.3[df.3$year==2016,],paste0(getwd(),"/Analysis/ES/Yield_potential_increase_doraani_2016.med.csv"))


