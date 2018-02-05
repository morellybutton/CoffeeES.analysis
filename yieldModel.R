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
source("/users/alex/Documents/Research/Africa/ECOLIMITS/Codes/overdispersion.R")

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")
d.F<-read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub.mod_analysis_dataset.csv"))

#remove NA values for yield
d.F <- d.F[!is.na(d.F$Shrub.kg),]

#make sure binary variables are factors
d.F$kebele<-factor(d.F$kebele)
d.F$compost.bin<-0
d.F[d.F$compost>0,"compost.bin"]<-1
d.F$compost.bin<-factor(d.F$compost.bin)
d.F$buffer<-factor(d.F$buffer)

#check which linking function I need, see how well values stay within dashed lines
d.F$Shrub.kg.1<- d.F$Shrub.kg + .00001

df.low<-d.F[d.F$kebele!="Badessa"&d.F$kebele!="Weyra",]
df.hi<-d.F[d.F$kebele=="Badessa"|d.F$kebele=="Weyra",]

#Yayu plots
df.low<-df.low %>% filter(year!="2014") %>% filter(!is.na(Tot.fruits))

df.low.z <- data.frame(scale(df.low %>% select(-X.1,-X,-ID,-year,-Plot,-Shrub.id,-kebele,-tmax.flower,-tmax.fruit,-tavg.flower,-tavg.fruit,-vpd.flower,-vpd.fruit,-new.ah.fruit,-compost.bin,-buffer) ))

df.low.z$Plot<-df.low$Plot
df.low.z$Shrub.id<-df.low$Shrub.id
df.low.z$year<-df.low$year
df.low.z$Shrub.kg.1 <-df.low$Shrub.kg.1

qqp(df.low.z$Shrub.kg.1, "norm")

# lnorm means lognormal
pdf(paste0(getwd(),"/Analysis/ES/Yayu_total_harvest.lnorm.pdf"))
qqp(df.low.z$Shrub.kg.1, "lnorm")
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

(fm001<-glmer(Shrub.kg.1~1+(1|Plot/Shrub.id)  + (1|year),data=df.low.z,family=Gamma))
(fm001b<-glmer(Shrub.kg.1~1+(1|Plot/Shrub.id) ,data=df.low.z,family=Gamma))
(fm001c<-glmer(Shrub.kg.1~1 + (1|Plot),data=df.low.z,family=Gamma))
anova(fm001,fm001b,fm001c) #should use mixed models, for plot, Shrub.id and year

#try for temporal autocorrelation, never reaches a result...
#am01_lme<-lme(Shrub.kg.1~ elevation+patcharea + buffer,data=df.low,method="REML",
#random = ~1 + year|Plot/Shrub.id,correlation=corCAR1(form=~year|Plot/Shrub.id),control=list(maxIter=10000, niterEM=10000),na.action="na.omit")

#2015 & 2016
#run it gamma and log
am01 <- glmer(Shrub.kg.1~ ah.flower + fruitset  + labour + CN.ratio  + propCLR + propCBD + propCBB + BA.legume + GapDry +
              patcharea  + (1|Plot) + (1|year),data=df.low.z,family=Gamma)
#am01s<-standardize(am01)
summary(am01)
#r.squaredGLMM(am01s)

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
               density + BA.legume + GapDry + elevation  + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F)
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
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Absolute Humidity\nDuring Flowering","Fruitset","Basal Area of\nLeguminous Shade Trees","Patch Area","Canopy Gap\nin Dry Season","Labour","Proportion of\nBerries w/ CBB","Soil C:N" ,"(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

#match by labels harder to identify
#tmp[grep("z.PropCPB",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="z.PropCPB","Importance"]
#tmp[grep("Cocoa.dens.cat",tmp$Comparison),"Importance"]<-x1[x1$Comparison=="Cocoa.dens.cat","Importance"]

#order by importance
tmp<-tmp[!is.na(tmp$Importance),]

g1<-ggplot(tmp, aes(x = Comparison, y = full, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width=0.2) + geom_point()+
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
cand.set[[4]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio  + fruitset  + GapDry  + propCBB +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[5]]<-standardize(lmer(log(Shrub.kg.1)~ BA.legume + CN.ratio + density  + fruitset + K.meq + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[6]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + density  +  elevation  + fruitset  + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[7]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio + fruitset  + GapDry +  (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[8]]<-standardize(lmer(log(Shrub.kg.1)~ BA.legume + density  +  elevation  + fruitset + labour + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[9]]<-standardize(lmer(log(Shrub.kg.1)~ CN.ratio + density + fruitset + K.meq + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))
cand.set[[10]]<-standardize(lmer(log(Shrub.kg.1)~ ah.flower + BA.legume + density  +  elevation  + fruitset + K.meq + (1|Plot/Shrub.id) + (1|year),data=df.hi,REML=F))

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
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"],labels=c("Labour","Absolute Humidity\nDuring Flowering","Elevation","Soil K","Proportion of\nBerries w/ CBB","Basal Area of\nLeguminous Shade Trees","Canopy Gap\nin Dry Season","Coffee Density","Soil C:N" ,"Fruitset","(Intercept)"))
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

