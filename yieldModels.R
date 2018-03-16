#Analysis of cocoa ES benefits/dis-benefits
library(car)
library(MASS)
library(MuMIn)
library(arm)
library(nlme)
library(MCMCglmm)
require(lattice)
library(glmmLasso)
library(tidyverse)
library(AICcmodavg)

#code to check for overdispersion
#source("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/overdispersion.R")

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")
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
d.F[d.F$kebele!="Badessa"&d.F$kebele!="Weyra","wereda"]<-"Doraani"
d.F$wereda<-factor(d.F$wereda)

#check which linking function I need, see how well values stay within dashed lines
d.F$Shrub.kg.1<- d.F$Shrub.kg + .00001

#run log, 2014
options(na.action = "na.omit")
df.14<-d.F[d.F$year==2014,]

am.14 <- lmer(log(Shrub.kg.1)~ elevation*patcharea + b.ffer + coffee.area.ha  + compost.bin + CN.ratio  + Tot.P.ppm + K.meq + propCBD + 
                propCBB + propCLR + BA.legume + Shannon.i + GapDry + prop.ldrop + wereda + (1|farm), data = df.14 ,REML=F)
am.14s<-standardize(am.14)
summary(am.14s)
r.squaredGLMM(am.14s)

#try it by removing lowest correlation value per step
am.14b <- lmer(log(Shrub.kg.1)~ elevation+patcharea + b.ffer  + compost.bin  + Tot.P.ppm + K.meq + propCBD + 
                 propCBB + BA.legume + GapDry + prop.ldrop + wereda + (1|farm), data = df.14 ,REML=F)
am.14bs<-standardize(am.14b)
summary(am.14bs)
r.squaredGLMM(am.14bs)

options(na.action = "na.fail")
am.14d<-dredge(am.14bs)

dredg.m14<-subset(am.14d,delta<2)
write.csv(dredg.m14,paste0(getwd(),"/Analysis/ES/GLM.all.2014_dredged03.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.14b@frame$farm~resid(am.14b))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.14b, type = "pearson"), Fitted = fitted(am.14b),Variable = df.14$farm[!is.na(df.14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#for 2014
cand.set<-list()
#delta 2 has sixmodels
cand.set[[1]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + BA.legume + Shannon.i + GapDry + (1|farm), data = df.14 ,REML=F))
cand.set[[2]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + Shannon.i + GapDry + (1|farm), data = df.14 ,REML=F))
cand.set[[3]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + BA.legume + Shannon.i + (1|farm), data = df.14 ,REML=F))
cand.set[[4]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + Shannon.i + (1|farm), data = df.14 ,REML=F))
cand.set[[5]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + (1|farm), data = df.14 ,REML=F))


#for 2014
##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
#res.table <-aictab(cand.set = dredg.m14, modnames = Modnames, sort = TRUE)
#write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_2014.d3.lnorm.delta2.csv"))

topmodels.avg<-model.avg(dredg.m14) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_2014.d3.lnorm.delta2.txt"))
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
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2014.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2014.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Elevation","Diversity of Shade Trees","Canopy Gap\nin Dry Season","Basal Area of\nLeguminous Shade Trees","(Intercept)"))
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
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2014.pdf"))

#produce mean and sd for all variables of interest (elevation, BA.legume, Shannon.i, GapDry)
#standardize variables
df <- data.frame(cbind(as.character(df.15$farm),as.character(df.15$Shrub.id),as.character(df.15$wereda),df.15$Shrub.kg.1,rescale(df.15$GapDry),rescale(df.15$BA.legume),rescale(df.15$elevation),rescale(df.15$Shannon.i)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.GapDry","z.BA.legume","z.elevation","z.Shannon.i")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2014.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="z.elevation","Estimate"]*z.elevation+tmp[tmp$Comparison=="z.Shannon.i","Estimate"]*z.Shannon.i+tmp[tmp$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                                                                              tmp[tmp$Comparison=="z.GapDry","Estimate"]*z.GapDry))

#take median of each observed farm
df.med<- df %>% group_by(farm,wereda) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point(aes(color=factor(wereda))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.0)+xlim(0,1.0)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("2014")+theme(
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
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.plot.2014.pdf"))

#run log, 2015
options(na.action = "na.omit")
df.15<-d.F[d.F$year==2015,]

am.15 <- lmer(log(Shrub.kg.1)~ elevation+patcharea + b.ffer + coffee.area.ha + fruitset  + labour + CN.ratio  + K.meq + propCBD + 
                propCBB + propCLR + BA.legume +GapDry + wereda + (1|farm), data = df.15 ,REML=F)
am.15s<-standardize(am.15)
summary(am.15s)
r.squaredGLMM(am.15s)

options(na.action = "na.fail")
am.15d<-dredge(am.15s)

dredg.m15<-subset(am.15d,delta<2)
write.csv(dredg.m15,paste0(getwd(),"/Analysis/ES/GLM.all.2015_dredged03.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.15s@frame$farm~resid(am.15s))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.15s, type = "pearson"), Fitted = fitted(am.15s),Variable = df.15$farm[!is.na(df.15$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#for 2015
cand.set<-list()
#delta 2 has sixmodels
cand.set[[1]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + BA.legume + CN.ratio + patcharea + (1|farm), data = df.15 ,REML=F))
cand.set[[2]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + BA.legume + patcharea + (1|farm), data = df.15  ,REML=F))
cand.set[[3]]<-standardize(lmer(log(Shrub.kg.1)~ wereda + BA.legume + patcharea + (1|farm), data = df.15 ,REML=F))
cand.set[[4]]<-standardize(lmer(log(Shrub.kg.1)~ elevation + BA.legume + b.ffer + (1|farm), data = df.15 ,REML=F))

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_2015.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_2015.lnorm.delta2.txt"))
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
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2015.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Basal Area of\nLeguminous Shade Trees","Patch Area","Elevation","Soil C:N","Wereda: Yayu","Location in Buffer","(Intercept)"))
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
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2015.pdf"))


#standardize variables
df <- data.frame(cbind(as.character(df.15$farm),as.character(df.15$Shrub.id),as.character(df.15$wereda),df.15$Shrub.kg.1,rescale(df.15$patcharea),rescale(df.15$BA.legume),rescale(df.15$elevation),rescale(df.15$CN.ratio),rescale(df.15$b.ffer),rescale(df.15$wereda)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.patcharea","z.BA.legume","z.elevation","z.CN.ratio","c.buffer","c.wereda")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="z.elevation","Estimate"]*z.elevation+tmp[tmp$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio+tmp[tmp$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                                                                        tmp[tmp$Comparison=="z.patcharea","Estimate"]*z.patcharea+tmp[tmp$Comparison=="c.wereda","Estimate"]*c.wereda+tmp[tmp$Comparison=="c.b.ffer","Estimate"]*c.buffer))

#take median of each observed farm
df.med<- df %>% group_by(farm,wereda) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point(aes(color=factor(wereda))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("2015")+theme(
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
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.plot.2015.pdf"))


#run log, 2016
options(na.action = "na.omit")
df.16<-d.F[d.F$year==2016,]

am.16 <- lmer(Shrub.kg.1~ elevation+patcharea + b.ffer + coffee.area.ha + fruitset  + labour + CN.ratio  + K.meq + propCBD + 
                propCBB + propCLR + BA.legume +GapDry + wereda + (1|farm), data = df.16 ,REML=F)
am.16s<-standardize(am.16)
summary(am.16s)
r.squaredGLMM(am.16s)

options(na.action = "na.fail")
am.16d<-dredge(am.16s)

dredg.m16<-subset(am.16d,delta<2)
write.csv(dredg.m16,paste0(getwd(),"/Analysis/ES/GLM.all.2016_dredged01.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2016.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.16s@frame$farm~resid(am.16s))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.16s, type = "pearson"), Fitted = fitted(am.16s),Variable = df.16$farm[!is.na(df.16$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2016.lnorm_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2016.lnorm_ResidualvFittedValues.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted | Variable, data = diagnos)
dev.off()
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2016.lnorm_qqplotResiduals_byplot.pdf"),width=8,height=8)
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#for 2016
cand.set<-list()
#delta 2 has sixmodels
cand.set[[1]]<-standardize(lmer(Shrub.kg.1~ elevation + CN.ratio + GapDry + (1|farm), data = df.16 ,REML=F))
cand.set[[2]]<-standardize(lmer(Shrub.kg.1~ elevation + CN.ratio + b.ffer + (1|farm), data = df.16  ,REML=F))
cand.set[[3]]<-standardize(lmer(Shrub.kg.1~ elevation + CN.ratio + (1|farm), data = df.16 ,REML=F))
cand.set[[4]]<-standardize(lmer(Shrub.kg.1~ elevation + b.ffer + (1|farm), data = df.16 ,REML=F))
cand.set[[5]]<-standardize(lmer(Shrub.kg.1~ elevation + (1|farm), data = df.16  ,REML=F))
cand.set[[6]]<-lmer(Shrub.kg.1~ 1+ (1|farm), data =df.16  ,REML=F)
cand.set[[7]]<-standardize(lmer(Shrub.kg.1~ CN.ratio + GapDry + (1|farm), data = df.16 ,REML=F))
cand.set[[8]]<-standardize(lmer(Shrub.kg.1~ GapDry + (1|farm), data = df.16 ,REML=F))
cand.set[[9]]<-standardize(lmer(Shrub.kg.1~ CN.ratio + wereda + (1|farm), data = df.16 ,REML=F))
cand.set[[10]]<-standardize(lmer(Shrub.kg.1~ CN.ratio  + (1|farm), data = df.16 ,REML=F))

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_2016.lnorm.delta2.csv"))

topmodels.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_2016.lnorm.delta2.txt"))
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
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_2016.lnorm_delta2.confint.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2016.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 2
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Elevation","Soil C:N","Canopy Gap in Dry Season","Location in Buffer","Wereda","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
  theme(text = element_text(size=12),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence of ES factors on\nPer Shrub Yield (2016)")+
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
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2016.pdf"))


#standardize variables
df <- data.frame(cbind(as.character(df.15$farm),as.character(df.15$Shrub.id),as.character(df.15$wereda),df.15$Shrub.kg.1,rescale(df.15$patcharea),rescale(df.15$BA.legume),rescale(df.15$elevation),rescale(df.15$CN.ratio),rescale(df.15$b.ffer)),stringsAsFactors = F)
colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.patcharea","z.BA.legume","z.elevation","z.CN.ratio","c.buffer")
df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.lnorm_delta2.confint.csv"))
tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="z.elevation","Estimate"]*z.elevation+tmp[tmp$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio+tmp[tmp$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                                                                        tmp[tmp$Comparison=="z.patcharea","Estimate"]*z.patcharea+tmp[tmp$Comparison=="c.b.ffer","Estimate"]*c.buffer))

#take median of each observed farm
df.med<- df %>% group_by(farm,wereda) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point(aes(color=factor(wereda))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("2015")+theme(
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
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.plot.2015.pdf"))



#do weredas separately
df.low<-d.F[d.F$kebele!="Badessa"&d.F$kebele!="Weyra",]
df.hi<-d.F[d.F$kebele=="Badessa"|d.F$kebele=="Weyra",]

#do 2014 for each wereda separately
df.hi.14<- df.hi %>% filter(year==2014)
qqp(df.hi.14$Shrub.kg.1, "lnorm")

options(na.action = "na.omit")

am.14.d <-lmer(log(Shrub.kg.1)~ pH + Tot.P.ppm + density +  K.meq  + propCBD + propCLR + propCBB + BA.pioneer + GapDry +
               patcharea + (1|Plot), data=df.hi.14,REML=F)
am.14.ds<-standardize(am.14.d)
summary(am.14.ds)
r.squaredGLMM(am.14.ds)

options(na.action = "na.fail")
am.14.d<-dredge(am.14.d)
dredg.m02<-subset(am.14.d,delta<2)
write.csv(dredg.m02,paste0(getwd(),"/Analysis/ES/GLM.lnorm.doraani.2014_dredged02.csv"))

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2014.doraani.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am.14.ds@frame$Plot~resid(am.14.ds))
dev.off()
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
diagnos <- data.frame(Resid = resid(am.14.ds, type = "pearson"), Fitted = fitted(am.14.ds),Variable = df.hi.14$Plot[!is.na(df.hi.14$propCLR)] )
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
#delta 2 has sixmodels
cand.set[[1]]<-standardize(lmer(log(Shrub.kg.1)~ pH + BA.pioneer + propCBD + Tot.P.ppm + (1|Plot), data = df.hi.14 ,REML=F))
cand.set[[2]]<-standardize(lmer(log(Shrub.kg.1)~ BA.pioneer + propCBD + Tot.P.ppm + (1|Plot), data = df.hi.14  ,REML=F))

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
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Basal Area of\nPioneer Shade Trees","Incidence of CBD","Total Soil P","Soil pH","(Intercept)"))
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
ggsave(paste0(getwd(),"/Analysis/ES/Model_averaged_results_2014.doraani.pdf"))


#standardize variables
#df <- data.frame(cbind(as.character(df.15$farm),as.character(df.15$Shrub.id),as.character(df.15$wereda),df.15$Shrub.kg.1,rescale(df.15$patcharea),rescale(df.15$BA.legume),rescale(df.15$elevation),rescale(df.15$CN.ratio),rescale(df.15$b.ffer),rescale(df.15$wereda)),stringsAsFactors = F)
#colnames(df)<-c("farm","Shrub.id","wereda","Shrub.kg.1","z.patcharea","z.BA.legume","z.elevation","z.CN.ratio","c.buffer","c.wereda")
#df[,4:ncol(df)]<-sapply(df[,4:ncol(df)],as.numeric)

#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_2015.lnorm_delta2.confint.csv"))
#tmp<-tmp[!is.na(tmp$full),]

#test validity of the model
#df<-df %>% group_by(farm,wereda,Shrub.id) %>% mutate(shrub.kg.mod=exp(tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="z.elevation","Estimate"]*z.elevation+tmp[tmp$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio+tmp[tmp$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
#tmp[tmp$Comparison=="z.patcharea","Estimate"]*z.patcharea+tmp[tmp$Comparison=="c.wereda","Estimate"]*c.wereda+tmp[tmp$Comparison=="c.b.ffer","Estimate"]*c.buffer))

#take median of each observed farm
#df.med<- df %>% group_by(farm,wereda) %>% summarise(Shrub.kg.1=median(Shrub.kg.1,na.rm=T),shrub.kg.mod=mean(shrub.kg.mod,na.rm=T))

ggplot(df.med,aes(Shrub.kg.1,shrub.kg.mod)) + geom_point(aes(color=factor(wereda))) + geom_abline(slope=1,intercept=0,linetype="dashed")+
  ylim(0,1.5)+xlim(0,1.5)+
  xlab("Observed per Shrub Yield [kg]")+ylab("Modelled per Shrub Yield [kg]")+scale_colour_discrete(name="Wereda")+
  ggtitle("2015")+theme(
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
ggsave(paste0(getwd(),"/Analysis/ES/Modelled.yield_vs.obsyield.plot.2015.pdf"))

#Yayu plots
df.low<-df.low %>% filter(year!="2014") %>% filter(!is.na(No.fruits))
df.low.z <- data.frame(scale(df.low %>% select(-X.1,-X,-ID,-year,-Plot,-kebele,-tmax.flower,-tmax.fruit,-tavg.flower,-tavg.fruit,-vpd.flower,-vpd.fruit,-new.ah.fruit,-compost.bin,-buffer) ))
df.low.z$Plot<-df.low$Plot
df.low.z$Shrub.id<-df.low$Shrub.id
df.low.z$year<-df.low$year
df.low.z$Shrub.kg.1 <-df.low$Shrub.kg.1

qqp(df.low$Shrub.kg.1, "norm")

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/Yayu_total_harvest.lnorm.pdf"))
qqp(df.low$Shrub.kg.1, "lnorm")
dev.off()

# qqp requires estimates of the parameters of the negative binomial, Poisson # and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I # have shown below.
#nbinom <- fitdistr(d.F.16$Shrub.kg.1, "Negative Binomial") #harvest not binomial
#qqp(d.F$Shrub.kg.1, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

#poisson<-fitdistr(d.F$harvest.1, "Poisson") , poisson can only deal with whole integers
#qqp(d.F$harvest.1, "pois", poisson$estimate)
pdf(paste0(getwd(),"/Analysis/ES/Yayu_total_harvest.gamma.pdf"))
gamma<-fitdistr(df.low$Shrub.kg.1,"gamma")
qqp(df.low$Shrub.kg.1, "gamma",shape=gamma$estimate[[1]],rate=gamma$estimate[[2]])
dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/HC1415_pertransdistharvest.pdf"),width=8,height=5)
#interaction.plot(d.F2$distance.cat,d.F2$transect,d.F2$harvest)
#dev.off()

#run glmms, generalized linear mixed-effects models
#develop "global model" for yield prediction, though analyze each year separately at present

options(na.action = "na.omit")

(fm001<-glmer(Shrub.kg.1~1+(1|Plot)  + (1|year),data=df.low,family=Gamma))
(fm001b<-glmer(Shrub.kg.1~1+(1|Plot) ,data=df.low,family=Gamma))
#(fm001c<-glmer(Shrub.kg.1~1 ,data=df.low,family=Gamma))
anova(fm001,fm001b) #should use mixed models, for plot, Shrub.id and year

#try for temporal autocorrelation, never reaches a result...
#am01_lme<-lme(Shrub.kg.1~ elevation+patcharea + buffer,data=df.low,method="REML",
#random = ~1 + year|Plot/Shrub.id,correlation=corCAR1(form=~year|Plot/Shrub.id),control=list(maxIter=10000, niterEM=10000),na.action="na.omit")

#2015 & 2016
#run it gamma and log
am01 <- glmer(Shrub.kg.1~ ah.flower + fruitset  + labour + CN.ratio + propCBD + propCBB + BA.legume + GapDry +
              patcharea  + (1|Plot) + (1|year),data=df.low.z,family=Gamma)
#am01s<-standardize(am01)
summary(am01)
#r.squaredGLMM(am01)

options(na.action = "na.fail")
am01d<-dredge(am01)

dredg.m01<-subset(am01d,delta<6)
write.csv(dredg.m01,paste0(getwd(),"/Analysis/ES/GLM.gamma.yayu_dredged01.csv"))

#do again with log
am02 <-lmer(log(Shrub.kg.1)~ ah.flower + fruitset  + labour + CN.ratio + propCBD + propCLR + propCBB + BA.legume + GapDry +
               patcharea  + (1|Plot/Shrub.id) + (1|year),data=df.low,REML=F)
am02s<-standardize(am02)
summary(am02s)
r.squaredGLMM(am02s)

am02d<-dredge(am02)
dredg.m02<-subset(am02d,delta<2)
write.csv(dredg.m02,paste0(getwd(),"/Analysis/ES/GLM.lnorm.yayu_dredged02.csv"))


#Assumption 1: Within-group errors (or BLUPS) are independent and identically normally distributed, with mean zero and variance sigma squareda dn independent of random effects
#graphic tests include within-group fitted values, observed values and any covariates of interest
#boxplot of residuals by group, to check that residuals are centered around 0, but variability changes with group
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015_16.gamma_residuals.pdf"),width=8,height=5)
bwplot(am01@frame$Plot~resid(am01))
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015_16.lnorm_residuals.pdf"),width=8,height=5)
bwplot(am02@frame$Plot~resid(am02))
dev.off()

#bwplot(fm02s@frame$PLOT~resid(fm02s))
#plot standardized residuals versus fitted values by plot,
#id identifies critical value (standarized residuals greater than 1-id/2 standard normal quantile in absolute value identified in plot)
plot(am01,id=0.05,adj=-0.3)

diagnos <- data.frame(Resid = resid(am01, type = "pearson"), Fitted = fitted(am01),Variable = df.low.z$Plot)

pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.16.gamma_ResidualvFittedValues_all.pdf"),width=8,height=8)
xyplot(Resid ~ Fitted, data = diagnos)
dev.off()

## separate by variable
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.16.gamma_ResidualvFittedValues.pdf"),width=8,height=8)
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

#issues with W1, BD4, and GC3
#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## overal QQ normal plot
qqmath(~Resid, data = diagnos, distribution = qnorm)
## separate by variable
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm)
## we could add a line to indicate 'normal' with a bit more work
pdf(paste0(getwd(),"/Analysis/ES/Shrub.2015.16.gamma._qqplotResiduals_byplot.pdf"),width=8,height=8)
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

#pdf(paste0(getwd(),"/Analysis/ES/HC1415_pertransdistharvest.pdf"),width=8,height=5)
#interaction.plot(d.F2$distance.cat,d.F2$transect,d.F2$harvest)
#dev.off()

#run glmms, generalized linear mixed-effects models
#develop "global model" for yield prediction, though analyze each year separately at present

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

#issues with W1, BD4, and GC3
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

#produce mean and sd for all variables of interest in the models (fruitset, C:N, Canopy gap dry season, coffee density, proportion CBB and CBD, BA legume, Soil k, patch area, ah flower,labour)
df.m <- d.F %>% summarise(fruitset.m=mean(fruitset,na.rm=T),fruitset.sd=sd(fruitset,na.rm = T),CN.ratio.m=mean(CN.ratio, na.rm=T),CN.ratio.sd=sd(CN.ratio,na.rm=T),GapDry.m=mean(GapDry,na.rm=T),GapDry.sd=sd(GapDry,na.rm=T),
                         density.m=mean(density,na.rm=T),density.sd=sd(density,na.rm=T),propCBB.m=mean(propCBB,na.rm=T),propCBB.sd=sd(propCBB,na.rm=T),propCBD.m=mean(propCBD,na.rm=T),propCBD.sd=sd(propCBD,na.rm=T),
                         BA.legume.m=mean(BA.legume,na.rm=T),BA.legume.sd=sd(BA.legume,na.rm=T),K.meq.m=mean(K.meq,na.rm=T),K.meq.sd=sd(K.meq,na.rm=T),patcharea.m=mean(patcharea,na.rm=T),patcharea.sd=sd(patcharea,na.rm=T),
                         ah.flower.m=mean(ah.flower,na.rm=T),ah.flower.sd=sd(ah.flower,na.rm=T),labour.m=mean(labour,na.rm=T),labour.sd=sd(labour,na.rm=T))
df.m$year<-2015
df.m[2,]<-df.m
df.m[2,"year"]<-2016

#standardize variables by year
d.F$Plot<-as.character(d.F$Plot)
df <- left_join(d.F %>% select(Plot,year,Shrub.id,kebele,Shrub.kg.1,labour,patcharea,GapDry,density,CN.ratio,K.meq,BA.legume,fruitset,propCBB,propCBD,ah.flower), df.m,by="year") %>% group_by(Plot,Shrub.id,year) %>% 
  mutate(z.fruitset=(fruitset-fruitset.m)/fruitset.sd/2,z.CN.ratio=(CN.ratio-CN.ratio.m)/CN.ratio.sd/2,z.GapDry=(GapDry-GapDry.m)/GapDry.sd/2,z.density=(density-density.m)/density.sd/2,
         z.propCBB=(propCBB-propCBB.m)/propCBB.sd/2,z.propCBD=(propCBD-propCBD.m)/propCBD.sd/2,z.BA.legume=(BA.legume-BA.legume.m)/BA.legume.sd/2,z.K.meq=(K.meq-K.meq.m)/K.meq.sd/2,z.patcharea=(patcharea-patcharea.m)/patcharea.sd/2,
         z.ah.flower=(ah.flower-ah.flower.m)/ah.flower.sd/2,z.labour=(labour-labour.m)/labour.sd/2)

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

#take median of predictions per plot
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
#yayu, BA.legume and fruitset

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(fruitset1=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*max(df.low$z.fruitset,na.rm=T)+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                        tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                        tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(fruitset2=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*min(df.low$z.fruitset,na.rm=T)+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*z.BA.legume+
                        tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                        tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(BA.legume1=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*z.fruitset+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*max(df.low$z.BA.legume,na.rm=T)+
                                                                             tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                             tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)

df.low<- df.low %>% group_by(Plot, Shrub.id,year) %>% mutate(BA.legume2=exp(tmp.y[tmp.y$Comparison=="(Intercept)","Estimate"]+tmp.y[tmp.y$Comparison=="z.ah.flower","Estimate"]*z.ah.flower+tmp.y[tmp.y$Comparison=="z.fruitset","Estimate"]*z.fruitset+tmp.y[tmp.y$Comparison=="z.BA.legume","Estimate"]*min(df.low$z.BA.legume,na.rm=T)+
                                                                             tmp.y[tmp.y$Comparison=="z.GapDry","Estimate"]*z.GapDry+ tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.patcharea","Estimate"]*z.patcharea + tmp.y[tmp.y$Comparison=="z.propCBD","Estimate"]*z.propCBD + tmp.y[tmp.y$Comparison=="z.CN.ratio","Estimate"]*z.CN.ratio + 
                                                                             tmp.y[tmp.y$Comparison=="z.labour","Estimate"]*z.labour)-Shrub.kg.1)


df.2<- df.low %>% ungroup() %>% select(Plot,year,BA.legume1,fruitset1,Shrub.kg.1) %>% mutate(BA.legume1=replace(BA.legume1,BA.legume1<0,NA),fruitset1=replace(fruitset1,fruitset1<0,NA)) %>% group_by(Plot,year) %>% summarise(BA.legume1=median(BA.legume1,na.rm=T),fruitset1=median(fruitset1,na.rm=T)) %>%
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

df.3<- df.hi %>% ungroup() %>% select(Plot,year,fruitset1,CN.ratio1,GapDry1,density1,Shrub.kg.1) %>% mutate(CN.ratio1=replace(CN.ratio1,CN.ratio1<0,NA),fruitset1=replace(fruitset1,fruitset1<0,NA),GapDry1=replace(GapDry1,GapDry1<0,NA),density1=replace(density1,density1<0,NA)) %>% group_by(Plot,year) %>% 
  summarise(CN.ratio1=median(CN.ratio1,na.rm=T),fruitset1=median(fruitset1,na.rm=T),GapDry1=median(GapDry1,na.rm=T),density1=median(density1,na.rm=T)) %>%
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


