#Revisiting statistical analysis for El Nino coffee landscapes paper

library(tidyverse)

#setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")
folder_names<-"G:/My Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Yayu"
#pubs folder
ptemp<-"Publications/2022/CoffeeLandscapes/"
setwd(paste0(folder_names,dtemp))

d.F.plot <- read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))
#d.F.shrub <- read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub_analysis_dataset.csv"))


library(arm)
library(car)
#library(MuMIn)
library(lattice)


tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))
tmp14<- tmp1 %>% filter(year==2014)
tmp2<- tmp1 %>% filter(year!=2014)
tmp15<- tmp1 %>% filter(year==2015)
tmp16<- tmp1 %>% filter(year==2016)

#pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.allyrs.pdf"))
#hist(tmp1$Shrub.kg)
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.2014.pdf"))
#hist(tmp14$Shrub.kg, main="Histogram of 2014 Shrub Yields",xlab="Shrub Yield",cex.lab=2,cex.main=2)
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.shockyrs.pdf"))
#hist(tmp2$Shrub.kg, main="Histogram of Shock Year Shrub Yields",xlab="Shrub Yield",cex.lab=2,cex.main=2)
#dev.off()


####Normal Year Analysis
#linear model
rm1<-lm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB),data=tmp14)
summary(rm1) # not significant


#check multicollinearity
vif(rm1)

#linear model: 2014, no intercept
rm1b<-lm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) - 1,data=tmp14)

summary(rm1b) # not significant
vif(rm1b)
confint(rm1b, level = .95, method = c("boot"))

#gamma model: 2014 w intercept
rm1c<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           #poly(rescale(GapDry),2) + 
            rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) ,family  = Gamma(link = "log"),data=tmp14)
summary(rm1c)
vif(rm1c)
confint(rm1c, level = .90, method = c("boot"))

#gamma model: 2014 w intercept
#order of removal, 1)  + rescale(tmax.anom.fruit), 2) + rescale(buffer) , 3)  poly(rescale(GapDry),2),
#4) + rescale(C.pct), 5)  + rescale(propCLR), 6)  + rescale(CN.ratio), 7) rescale(coffee.area.ha) + ,
#8)  + rescale(dbh.mn) , 9) rescale(elevation)*rescale(patcharea)
rm1d<-glm(Shrub.kg~rescale(elevation)+rescale(patcharea) + rescale(K.meq) +
            rescale(BA.legume)*rescale(Shannon.i) +
            rescale(GapDry) + rescale(propCBD) + rescale(propCBB) ,family  = Gamma(link = "log"),data=tmp14)
summary(rm1d)
vif(rm1d)

ci1<-confint(rm1d, level = .90, method = c("boot"))
mod1<-rm1d
MuMIn::r.squaredGLMM(rm1d) #trigamma distribution preferred whenever available

write.csv(ci1,paste0(getwd(),"/Analysis/ES/Normyr_90CIs_v1.csv"))


#run dredge to check models, run with summary statistics
#calculating confidence intervals using Dormann et al (2018)
options(na.action = "na.fail")
norm_dd<-MuMIn::dredge(rm1c, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
)
#subset by AIC < 4
norm_dd4<- subset(norm_dd, delta < 4)

#write.csv(norm_dd4,paste0(getwd(),"/Analysis/ES/Normyr_dredge_aic4.csv"))
#remove models that are more complex versions of what is already there
dd1<-glm(Shrub.kg~rescale(elevation) + rescale(GapDry)  + rescale(propCBB) +
           rescale(Shannon.i), family  = Gamma(link = "log"),data=tmp14)
dd2<-glm(Shrub.kg~rescale(elevation) + rescale(GapDry)  + rescale(patcharea) +
         rescale(Shannon.i), family  = Gamma(link = "log"),data=tmp14)
dd3<-glm(Shrub.kg~rescale(elevation) + rescale(GapDry) +
         rescale(Shannon.i) + rescale(tmax.anom.fruit), family  = Gamma(link = "log"),data=tmp14)
dd4<-glm(Shrub.kg~rescale(elevation) + rescale(GapDry) +
           rescale(Shannon.i), family  = Gamma(link = "log"),data=tmp14)
dd5<-glm(Shrub.kg~rescale(elevation) + rescale(Shannon.i),
         family  = Gamma(link = "log"),data=tmp14)

dd_avg<-MuMIn::model.avg(dd1,dd2,dd3,dd4,dd5)
#"full model" from above
dd_full <- glm(Shrub.kg~rescale(elevation) + rescale(GapDry)  + rescale(propCBB) +
                 rescale(Shannon.i) + rescale(patcharea) +  rescale(tmax.anom.fruit), family  = Gamma(link = "log"),data=tmp14)
ci1.1<-confint(dd_full, level = .90, method = c("boot"))
MuMIn::r.squaredGLMM(dd_full) #trigamma distribution preferred whenever available


#combine into one data.frame
tmp<-data.frame(dd_full$coefficients)
rm_norm<-data.frame(rownames(tmp))
colnames(rm_norm)<-"variable"

rm_norm$full<-dd_avg$coefficients[1,]
rm_norm$subset<-dd_avg$coefficients[2,]
rm_norm$perc5<-ci1.1[,1]
rm_norm$perc95<-ci1.1[,2]

write.csv(rm_norm,paste0(getwd(),"/Analysis/ES/Normyr_gamma.model_ci.csv"))

mod1<-dd_full
#check heteroskedasticity
diagnos <- data.frame(Resid = resid(mod1, type = "pearson"), Fitted = fitted(mod1),Variable = mod1$data$Plot, yield=mod1$data$Shrub.kg)
#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_ResidualvFittedValues_2014.pdf"),width=8,height=8)
d1<-ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20)) 
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_qqplotResiduals_2014.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos, distribution = qnorm, main = list("QQ-Plot", cex = 2), 
       xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_fittedvsobserved_2014.pdf"),width=8,height=8)
d3<-ggplot(diagnos, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
#dev.off()
ggpubr::ggarrange(d1,d3,d2,ncol=3)
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_gammalogmodel.avg_normalyr.tiff"),height=5,width=15)
##diagnostic plot for larger model looks more defensible##

#gamma model for all years with intercept
#rm4a<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
#           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           #poly(rescale(GapDry),2) + 
#            rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) ,family  = Gamma(link = "log"),data=tmp1)
rm4a<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) + factor(year) +
            rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
            poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) ,family  = Gamma(link = "log"),data=tmp1)

summary(rm4a)
vif(rm4a)
#confint(rm4b)

MuMIn::r.squaredGLMM(rm4a)

#gamma model for all years with intercept
#order of removal 1)  + rescale(buffer) , 2) + rescale(K.meq), 3) + rescale(C.pct),
#4)  + rescale(tmax.anom.fruit), 5) rescale(elevation)*rescale(patcharea)
rm4b<-glmer(Shrub.kg~rescale(elevation) + rescale(patcharea) + rescale(CN.ratio)  +
              rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(dbh.mn) +
              poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + (1|Plot),family  = Gamma(link = "log"), data=tmp1)
summary(rm4b)
vif(rm4b)
#confint(rm4b)

MuMIn::r.squaredGLMM(rm4b)

system.time(ci2.1<-confint(rm4b, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))

write.csv(ci2.1,paste0(getwd(),"/Analysis/ES/Allyrs_90CIs_v1.csv"))

##diagnostic plots & higher R2 for rm4c###
#gamma model for all years with intercept and year as factor
#order of removal 1) + rescale(tmax.anom.fruit), 2) + rescale(propCBD), 3)  + rescale(buffer),
#4) rescale(elevation)*rescale(patcharea), 5)  + rescale(K.meq), 6)  + rescale(dbh.mn),
#8) rescale(coffee.area.ha) + , 9) rescale(C.pct) + , 10) + rescale(propCLR),
#10) rescale(BA.legume)*rescale(Shannon.i), 11) poly(rescale(GapDry),2)
rm4c<-glmer(Shrub.kg~rescale(elevation)+rescale(patcharea) + factor(year) + rescale(CN.ratio) +
              rescale(BA.legume) + rescale(Shannon.i)  +
              poly(rescale(GapDry),2) + rescale(propCBB) + (1|Plot),family  = Gamma(link = "log"), data=tmp1)
summary(rm4c)
vif(rm4c)
#confint(rm4b)

MuMIn::r.squaredGLMM(rm4c)

system.time(ci2.2<-confint(rm4c, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))
write.csv(ci2.2,paste0(getwd(),"/Analysis/ES/Allyrs_90CIs_v2.csv"))

#run dredge to check models, run with summary statistics
#calculating confidence intervals using Dormann et al (2018)
options(na.action = "na.fail")
all_dd<-MuMIn::dredge(rm4a, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  })
)
#subset by AIC < 4
all_dd2<- subset(all_dd, delta < 2)

write.csv(all_dd2,paste0(getwd(),"/Analysis/ES/Allyr_dredge_aic2.csv"))

#remove models that are more complex versions of what is already there
ad1<-glmer(Shrub.kg~rescale(elevation) + rescale(BA.legume) + rescale(Shannon.i) + factor(year) + 
           rescale(propCBB) + (1|Plot) ,family  = Gamma(link = "log"),data=tmp1)
ad2<-glmer(Shrub.kg~rescale(elevation) + rescale(Shannon.i) + factor(year) + 
           rescale(propCBB) + rescale(C.pct) + (1|Plot) ,family  = Gamma(link = "log"),data=tmp1)
ad3<-glmer(Shrub.kg~rescale(elevation) + rescale(Shannon.i) + factor(year) + 
           rescale(propCBB) + (1|Plot), family  = Gamma(link = "log"),data=tmp1)
ad4<-glmer(Shrub.kg~rescale(elevation) + rescale(BA.legume) + rescale(Shannon.i) + factor(year) + (1|Plot),
         family  = Gamma(link = "log"),data=tmp1)
ad5<-glmer(Shrub.kg~rescale(elevation) + factor(year) + rescale(propCBB) +
        rescale(C.pct) + rescale(dbh.mn) + (1|Plot), family  = Gamma(link = "log"),data=tmp1)
ad6<-glmer(Shrub.kg~rescale(elevation) + factor(year) + rescale(propCBB) +
           rescale(C.pct) + (1|Plot), family  = Gamma(link = "log"),data=tmp1)
ad7<-glmer(Shrub.kg~rescale(elevation) + rescale(BA.legume) + rescale(Shannon.i) + factor(year) + 
           rescale(CN.ratio) + (1|Plot) ,family  = Gamma(link = "log"),data=tmp1)
ad8<-glmer(Shrub.kg~rescale(elevation) + rescale(Shannon.i) + factor(year) + (1|Plot),
         family  = Gamma(link = "log"),data=tmp1)

ad_avg<-MuMIn::model.avg(ad1,ad2,ad3,ad4,ad5,ad6,ad7,ad8)

#"full model" from above
ad_full <- glmer(Shrub.kg~rescale(elevation) + rescale(BA.legume)  + rescale(propCBB) + factor(year) +
                 rescale(Shannon.i) + rescale(dbh.mn) +  rescale(C.pct) + rescale(CN.ratio) + (1|Plot), family  = Gamma(link = "log"),data=tmp1)
system.time(ci2.3<-confint(ad_full, level = .90, method = c("boot"),
                           .progress="txt", PBargs=list(style=3)))
write.csv(ci2.3,paste0(getwd(),"/Analysis/ES/Allyrs_90CIs_v3.csv"))
MuMIn::r.squaredGLMM(ad_full)

#combine into one data.frame
tmp<-data.frame(summary(ad_full)$coefficients)
rm_all<-data.frame(rownames(tmp))
colnames(rm_all)<-"variable"

rm_all$full<-ad_avg$coefficients[1,]
rm_all$subset<-ad_avg$coefficients[2,]
rm_all$perc5<-ci2.3[3:nrow(ci2.3),1]
rm_all$perc95<-ci2.3[3:nrow(ci2.3),2]

write.csv(rm_norm,paste0(getwd(),"/Analysis/ES/Allyr_gamma.model_ci.csv"))

mod2<-rm4c

#check heteroskedasticity
diagnos <- data.frame(yield = mod2@frame$Shrub.kg, Resid = resid(mod2, type = "pearson"), Fitted = fitted(mod2),Variable = mod2@frame$Plot )
#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_ResidualvFittedValues_2014.pdf"),width=8,height=8)
d1<-ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20)) 
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_qqplotResiduals_2014.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos, distribution = qnorm, main = list("QQ-Plot", cex = 2), 
           xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
           panel = function(x, ...) {
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
           })
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_fittedvsobserved_2014.pdf"),width=8,height=8)
d3<-ggplot(diagnos, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
#dev.off()
ggpubr::ggarrange(d1,d3,d2,ncol=3)
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_gammalogmodel_allyrs_v2.tiff"),height=5,width=15)


###do same model but for shock years
#2015 & 2016
rm5a<- glmer(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
        rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
        poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)  + (1|Plot),family  = Gamma(link = "log"), data=tmp2)

summary(rm5a)
vif(rm5a)

MuMIn::r.squaredGLMM(rm5a)

#confint(rm5a, level = .90, method = c("boot"))
#write.csv(ci4c,paste0(getwd(),"/Analysis/ES/Normalyr_90CIs.csv"))
#ci4c<-read.csv(paste0(getwd(),"/Analysis/ES/Normalyr_90CIs.csv"))

#MuMIn::r.squaredGLMM(rm5a)


#2015 & 2016
#order of removal 1) + rescale(buffer) , 2) rescale(BA.legume)*rescale(Shannon.i), 3) + rescale(CN.ratio)
#4) + rescale(Shannon.i), 5) + rescale(propCLR), 6) + rescale(propCBD), 7)+ rescale(dbh.mn), 8) rescale(coffee.area.ha) + 
#9)  + rescale(K.meq), 10) + rescale(C.pct)
rm5b<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)  +
             rescale(BA.legume) + rescale(tmax.anom.fruit) +
              poly(rescale(GapDry),2) + rescale(propCBB)  + (1|Plot),family  = Gamma(link = "log"), data=tmp2)
summary(rm5b)
vif(rm5b)

MuMIn::r.squaredGLMM(rm5b)

system.time(ci3.1<-confint(rm5b, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))
write.csv(ci3.1,paste0(getwd(),"/Analysis/ES/shockyrs_90CIs_v1.csv"))

#2015 & 2016
#take away tmax first 1) + rescale(tmax.anom.fruit), 2) + rescale(buffer), 3) + rescale(K.meq)
#4)+ rescale(propCLR), 5) + rescale(dbh.mn), 6)  + rescale(CN.ratio) , 7) rescale(coffee.area.ha) + ,
#8)rescale(BA.legume)*rescale(Shannon.i) , 9) + rescale(Shannon.i)
rm5c<- glmer(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(C.pct) +
               rescale(BA.legume)  +
               poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCBB)  + (1|Plot),family  = Gamma(link = "log"), data=tmp2)

summary(rm5c)
vif(rm5c)

MuMIn::r.squaredGLMM(rm5c)

system.time(ci3.2<-confint(rm5c, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))
write.csv(ci3.2,paste0(getwd(),"/Analysis/ES/shockyrs_90CIs_v2.csv"))

mod3<-rm5b
#check heteroskedasticity
diagnos <- data.frame(yield = mod3@frame$Shrub.kg, Resid = resid(mod3, type = "pearson"), Fitted = fitted(mod3),Variable = as.character(mod3@frame$Plot) )
#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_ResidualvFittedValues_2014.pdf"),width=8,height=8)
d1<-ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20)) 
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_qqplotResiduals_2014.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos, distribution = qnorm, main = list("QQ-Plot", cex = 2), 
           xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
           panel = function(x, ...) {
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
           })
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldgamma_fittedvsobserved_2014.pdf"),width=8,height=8)
d3<-ggplot(diagnos, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
#dev.off()
ggpubr::ggarrange(d1,d3,d2,ncol=3)
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_gammalogmodel_shockyrs_v1.tiff"),height=5,width=15)

###############################################################
#####create figures of significant models####
#Normal Year
mod1<-glm(Shrub.kg~rescale(elevation)+rescale(patcharea) + rescale(K.meq) +
            rescale(BA.legume)*rescale(Shannon.i) +
            rescale(GapDry) + rescale(propCBD) + rescale(propCBB) ,family  = Gamma(link = "log"),data=tmp14)

ci1<-read.csv(paste0(getwd(),"/Analysis/ES/Normyr_90CIs_v1.csv"))

mod1.glm<-as.data.frame(summary(mod1)$coefficients)
colnames(mod1.glm)<-c("Coefficients","Std_error","t_value","p_value")

#add confidenct intervals
mod1.glm$CI.5 <- ci1[,2]
mod1.glm$CI.95 <- ci1[,3]
mod1.glm$Comparison<-rownames(mod1.glm)
rownames(mod1.glm)<-1:nrow(mod1.glm)

#mod1.glm <- mod1.glm %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
#  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
mod1.glm$Labels <- c("Intercept","Elevation","Patch Area","Soil\nPotassium Content","Basal Area of\nLeguminous Trees",
                    "Shade Diversity","Canopy Gap","Coffee Berry\nDisease","Coffee Berry\nBorer",
                    "Shade Diversity:\nLeguminous Trees")
#add in labels for colors & shapes
mod1.glm$shades<-c("Disease/Other","Landscape","Landscape","Soil","Shade Management","Shade Management",
                "Shade Management","Disease/Other","Disease/Other","Shade Management")

mod1.glm$shapes<-c("Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Year","Year","Fixed")

#order by size of effect and Importance factor
mod1.glm <- mod1.glm %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(mod1.glm))

#write.csv(mod1.glm,paste0(getwd(),"/Analysis/ES/Finalmodel_yldgamma_normyr.csv"))
mod1.glm<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldgamma_normyr.csv"))

mod1.glm$Labels<-factor(mod1.glm$Labels,levels=mod1.glm[order(mod1.glm$Importance,decreasing=T),"Labels"])
mod1.glm$shades<-factor(mod1.glm$shades,levels=c("Landscape","Shade Management","Soil","Disease/Other"),ordered=T)
#x.lm$sig<-factor(x.lm$sig, levels=c(0,1),labels=c("Significant","NS"))

mod1_r2<-MuMIn::r.squaredGLMM(mod1) #trigamma distribution preferred whenever available

g1<-ggplot(mod1.glm, aes(x = Labels, y = Coefficients, ymin = CI.5, ymax = CI.95)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(color=factor(shades),shape=factor(shapes))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(taxis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Normal Year")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed") + theme_classic() + 
  annotate("text",x=0.75,y=-0.5,label=paste("R^2 == ",signif(mod1_r2[3,2], 3)),angle=0,size=5,parse=T) +
  theme(axis.text.x=element_text(angle = 45,hjust=1),legend.position="right",legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 20))

g2<-g1+coord_flip()
g2
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldgammlog_normyr.tiff"),height=10,width=9)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_results_yld14.v4.pdf",height=6,width=6)


###################################################
###Yield shock years
mod2<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)  +
              rescale(BA.legume) + rescale(tmax.anom.fruit) +
              poly(rescale(GapDry),2) + rescale(propCBB)  + (1|Plot),family  = Gamma(link = "log"), data=tmp2)

ci2<-read.csv(paste0(getwd(),"/Analysis/ES/shockyrs_90CIs_v1.csv"))

mod2.glm<-as.data.frame(summary(mod2)$coefficients)
colnames(mod2.glm)<-c("Coefficients","Std_error","t_value","p_value")
mod2.glm$Comparison<-rownames(mod2.glm)
rownames(mod2.glm)<-1:nrow(mod2.glm)


mod2.glm$CI.95 <- ci2[3:nrow(ci2),3]
mod2.glm$CI.5 <- ci2[3:nrow(ci2),2]

#x.gamma <- x.gamma %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
 # mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
mod2.glm$Labels <- c("Intercept","Elevation","Patch Area","Basal Area of\nLeguminous Trees","Max Temperature\nFruiting","Canopy Gap","Canopy Gap^2",
                   "Coffee\nBerry Borer","Elevation:Patch Area")
#add in labels for colors & shapes
mod2.glm$shades<-c("Disease/Other","Landscape","Landscape","Shade Management","Disease/Other","Shade Management","Shade Management","Disease/Other","Landscape")

mod2.glm$shapes<-c("Fixed","Fixed","Fixed","Fixed","Year","Fixed","Fixed","Year","Fixed")

#order by size of effect and Importance factor
mod2.glm <- mod2.glm %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(mod2.glm))

#write.csv(mod2.glm,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink_shockyrs.csv"))
mod2.glm<-read.csv(paste0(getwd(),"/Analysis/ES/inalmodel_yldglmgammaloglink_shockyrs.csv"))

mod2.glm$Labels<-factor(mod2.glm$Labels,levels=mod2.glm[order(mod2.glm$Importance,decreasing=T),"Labels"])
mod2.glm$shades<-factor(mod2.glm$shades,levels=c("Landscape","Shade Management","Soil","Disease/Other"),ordered=T)

mod2_r2<-MuMIn::r.squaredGLMM(mod2)

g3<-ggplot(mod2.glm, aes(x = Labels, y = Coefficients, ymin = CI.5, ymax = CI.95)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shock Years")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(axis.text.x=element_text(angle = 45,hjust=1),plot.title = element_text(hjust = 0.5),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=-2.0,label=paste("R^2 == ",signif(mod2_r2[3,2], 3)),angle=0,size=5,parse=T) + theme(text = element_text(size = 20))

g4<-g3+coord_flip()
g4
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglm_shockyrs.tiff"),height=10,width=9)

###################################################
###All years
mod3<-glmer(Shrub.kg~rescale(elevation)+rescale(patcharea) + factor(year) + rescale(CN.ratio) +
              rescale(BA.legume) + rescale(Shannon.i)  +
              poly(rescale(GapDry),2) + rescale(propCBB) + (1|Plot),family  = Gamma(link = "log"), data=tmp1)

ci3<-read.csv(paste0(getwd(),"/Analysis/ES/Allyrs_90CIs_v2.csv"))

mod3.glm<-as.data.frame(summary(mod3)$coefficients)
colnames(mod3.glm)<-c("Coefficients","Std_error","t_value","p_value")
mod3.glm$Comparison<-rownames(mod3.glm)
rownames(mod3.glm)<-1:nrow(mod3.glm)


mod3.glm$CI.95 <- ci3[3:nrow(ci3),3]
mod3.glm$CI.5 <- ci3[3:nrow(ci3),2]

#x.gamma <- x.gamma %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
# mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
mod3.glm$Labels <- c("Intercept","Elevation","Patch Area","Year(2015)","Year(2016)","Soil C:N","Basal Area of\nLeguminous Trees","Shade Diversity","Canopy Gap","Canopy Gap^2",
                     "Coffee\nBerry Borer")
#add in labels for colors & shapes
mod3.glm$shades<-c("Disease/Other","Landscape","Landscape","Disease/Other","Disease/Other","Soil","Shade Management","Shade Management","Shade Management","Shade Management","Disease/Other")

mod3.glm$shapes<-c("Fixed","Fixed","Fixed","Year","Year","Fixed","Fixed","Fixed","Fixed","Fixed","Year")

#order by size of effect and Importance factor
mod3.glm <- mod3.glm %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(mod3.glm))

#write.csv(mod3.glm,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink_allyrs.csv"))
mod3.glm<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink_allyrs.csv"))

mod3.glm$Labels<-factor(mod3.glm$Labels,levels=mod3.glm[order(mod3.glm$Importance,decreasing=T),"Labels"])
mod3.glm$shades<-factor(mod3.glm$shades,levels=c("Landscape","Shade Management","Soil","Disease/Other"),ordered=T)

mod3_r2<-MuMIn::r.squaredGLMM(mod3)

g5<-ggplot(mod3.glm, aes(x = Labels, y = Coefficients, ymin = CI.5, ymax = CI.95)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("All Years")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(axis.text.x=element_text(angle = 45,hjust=1),plot.title = element_text(hjust = 0.5),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=-1.0,label=paste("R^2 == ",signif(mod3_r2[3,2], 3)),angle=0,size=5,parse=T) + theme(text = element_text(size = 20))

g6<-g5+coord_flip()
g6
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglm_allyrs.tiff"),height=10,width=9)

library(ggpubr)
#create overall title
#text <- "Landscape and Management Influences on Yield"

# Create a text grob
#tgrob <- text_grob(text,size = 24)
# Draw the text
#plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,2,0,2, "cm"))

#ggarrange(plot_0,NULL,g2, g4,
#          ncol =2,nrow = 2,heights = c(1,5),common.legend=T,legend=c("bottom"),labels="auto")

ggarrange(g6, g2, g4, ncol =3,common.legend=T,legend=c("bottom"),labels="auto",
          font.label = list(size = 18, color = "black", face = "bold", family = NULL),
          label.x=0.2)

ggsave(paste0(folder_names,ptemp,"Finalmodel_results_comboplots.tiff"),height=7,width=18)


#####################################################################
######Create 2D figures of landscape and shade management interactions
library(paletteer)
library(tidyverse)
library(lme4)
library(arm)

d.F.plot <- read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))

ymp1<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldlm.csv"))
ymp2<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglm.csv"))

#variables of interest
elev<-tibble(elev=seq(as.integer(min(d.F.plot$elevation)),as.integer(max(d.F.plot$elevation)),by=1))
patch<-tibble(patch=seq(as.integer(min(d.F.plot$patcharea)),as.integer(max(d.F.plot$patcharea)),by=5))
z.elev<-attributes(scale(d.F.plot$elevation))
z.patch<-attributes(scale(d.F.plot$patcharea))

z.year<-attributes(scale(d.F.plot$year))
#z.buffer<-attributes(scale(d.F.plot$buffer))

year<- tibble(y2=2015,y3=2016) %>% mutate(y2=(2015-z.year[[2]])/(2*z.year[[3]]), y3=(2016-z.year[[2]])/(2*z.year[[3]]))


#where does buffer exist (range of elevation and patch area?)
#buff<- d.F.plot %>% filter(buffer==1) %>% summarise(patch_min=min(patcharea,na.rm=T), patch_max=max(patcharea,na.rm=T),
#                                                 elev_min=min(elevation,na.rm=T), elev_max=max(elevation,na.rm=T)) %>% 
#  mutate(patch_min = (patch_min-z.patch[[2]])/(2*z.patch[[3]]), patch_max =(patch_max-z.patch[[2]])/(2*z.patch[[3]]),
#         elev_min = (elev_min-z.elev[[2]])/(2*z.elev[[3]]), elev_max = (elev_max-z.elev[[2]])/(2*z.elev[[3]]) )


#newdata
elev <- elev %>% mutate(elevation=(elev-z.elev[[2]])/(2*z.elev[[3]]))
patch <- patch %>% mutate(patcharea=(patch-z.patch[[2]])/(2*z.patch[[3]]))

tmp.land<-merge(elev,patch)
#tmp14<- tmp14 %>% mutate(year=factor(2014),BA.legume=0,Shannon.i=0,propCBB=0,Plot=NA)

tmp.land <- tmp.land %>% mutate(yld.2014=ymp1[ymp1$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  ymp1[ymp1$Comparison=="rescale(patcharea)","Coefficients"]*patcharea,
                                #yld.2014b=ymp1[ymp1$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                #  ymp1[ymp1$Comparison=="rescale(patcharea)","Coefficients"]*patcharea,
                                yld.2015 = ymp2[ymp2$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  ymp2[ymp2$Comparison=="rescale(patcharea)","Coefficients"]*patcharea,
                                yld.2016 = ymp2[ymp2$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  ymp2[ymp2$Comparison=="rescale(elevation):factor(year)2016","Coefficients"]*elevation) 


g1<-ggplot(tmp.land, aes(patch, elev, z = yld.2014)) +geom_raster(aes(fill=yld.2014)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.75,1.1)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2014)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) +
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g2<-ggplot(tmp.land, aes(patch, elev, z = yld.2015)) +geom_raster(aes(fill=yld.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.75,1.1)) +
  theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2015)") + 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16))+ guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g3<-ggplot(tmp.land, aes(patch, elev, z = yld.2016)) +geom_raster(aes(fill=yld.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.75,1.1)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2016)") + 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) + guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

#ggpubr::ggarrange(g1,g2,g3,ncol=3,nrow=1, common.legend=T)

legume<-tibble(legume=seq(as.integer(min(d.F.plot$BA.legume)),as.integer(max(d.F.plot$BA.legume)),by=0.30))
diversity<-tibble(diversity=seq(as.integer(min(d.F.plot$Shannon.i)),as.integer(max(d.F.plot$Shannon.i)),by=0.04))
z.legume<-attributes(scale(d.F.plot$BA.legume))
z.diversity<-attributes(scale(d.F.plot$Shannon.i))

#newdata
legume <- legume %>% mutate(BA.legume=(legume-z.legume[[2]])/(2*z.legume[[3]]))
diversity <- diversity %>% mutate(Shannon.i=(diversity-z.diversity[[2]])/(2*z.diversity[[3]]))

tmp.shade<-merge(legume,diversity)
#tmp14<- tmp14 %>% mutate(year=factor(2014),BA.legume=0,Shannon.i=0,propCBB=0,Plot=NA)
tmp.shade <- tmp.shade %>% mutate(yld.2014 = ymp1[ymp1$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    ymp1[ymp1$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    ymp1[ymp1$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Coefficients"]*Shannon.i*BA.legume,
                                  yld.2015 = ymp2[ymp2$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    ymp2[ymp2$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    ymp2[ymp2$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Coefficients"]*Shannon.i*BA.legume,
                                  yld.2016 = ymp2[ymp2$Comparison=="factor(year)2016:rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    ymp2[ymp2$Comparison=="factor(year)2016:rescale(BA.legume)","Coefficients"]*BA.legume +
                                    ymp2[ymp2$Comparison=="factor(year)2016:rescale(BA.legume):rescale(Shannon.i)","Coefficients"]*BA.legume*Shannon.i)
max(tmp.land$yld.2014, tmp.land$yld.2014b,tmp.land$yld.2015,tmp.land$yld.2016,tmp.shade$yld.2014,
    tmp.shade$yld.2014b,tmp.shade$yld.2015,tmp.shade$yld.2016)
min(tmp.land$yld.2014, tmp.land$yld.2014b,tmp.land$yld.2015,tmp.land$yld.2016,tmp.shade$yld.2014,
    tmp.shade$yld.2014b,tmp.shade$yld.2015,tmp.shade$yld.2016)



g4<-ggplot(tmp.shade, aes(diversity, legume,  z = yld.2014)) +geom_raster(aes(fill=yld.2014)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.75,1.1)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2014)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) +
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g5<-ggplot(tmp.shade, aes(diversity, legume, z = yld.2015)) +geom_raster(aes(fill=yld.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.75,1.1)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2015)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) + 
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g6<-ggplot(tmp.shade, aes(diversity, legume, z = yld.2016)) +geom_raster(aes(fill=yld.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.75,1.1)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2016)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) + 
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

ggpubr::ggarrange(g1,g2,g3,g4,g5,g6,ncol=3,nrow=2, common.legend=T,labels="auto",font.label = list(size = 18, color = "black", face = "bold", family = NULL),
                  label.x=0.1)
ggsave(paste0(folder_names,ptemp,"/Yield.Influence.Landscape.ShadeManagement3.tiff"),height=9,width=12)

##############################################
#Figure of shock year yield

rst<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgaussloglink.csv"))

tmp.land <- tmp.land %>% mutate(resist.2015 = rst[rst$Comparison=="rescale(elevation)","Coefficients"]*elevation +
                                  rst[rst$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  rst[rst$Comparison=="rescale(elevation):rescale(tmax.anom.fruit)","Coefficients"]*elevation*as.numeric(tmax[tmax$year==2015,2]),
                                resist.2016 = rst[rst$Comparison=="rescale(elevation)","Coefficients"]*elevation +
                                  rst[rst$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  rst[rst$Comparison=="rescale(elevation):rescale(tmax.anom.fruit)","Coefficients"]*elevation*as.numeric(tmax[tmax$year==2016,2]))

tmp.shade <- tmp.shade %>% mutate(resist.2015 = rst[rst$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    rst[rst$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    rst[rst$Comparison=="rescale(tmax.anom.fruit):rescale(BA.legume)","Coefficients"]*BA.legume*as.numeric(tmax[tmax$year==2015,2]) +
                                    rst[rst$Comparison=="rescale(tmax.anom.fruit):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(tmax[tmax$year==2015,2]),
                                  resist.2016 = rst[rst$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    rst[rst$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    rst[rst$Comparison=="rescale(tmax.anom.fruit):rescale(BA.legume)","Coefficients"]*BA.legume*as.numeric(tmax[tmax$year==2016,2]) +
                                    rst[rst$Comparison=="rescale(tmax.anom.fruit):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(tmax[tmax$year==2016,2]))

r1<-ggplot(tmp.land, aes(patch, elev, z = resist.2015)) +geom_raster(aes(fill=resist.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.55,3),breaks = c(-1.5, 0, 3.0)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2015)") + theme(text=element_text(size=16))

r2<-ggplot(tmp.land, aes(patch, elev, z = resist.2016)) +geom_raster(aes(fill=resist.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.55,3),breaks = c(-1.5, 0, 3.0)) +
  theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2016)") + theme(text=element_text(size=16))

r3<-ggplot(tmp.shade, aes(diversity, legume, z = resist.2015)) +geom_raster(aes(fill=resist.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.55,3),breaks = c(-1.5, 0, 3.0)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2015)") + theme(text=element_text(size=16))

r4<-ggplot(tmp.shade, aes(diversity, legume, z = resist.2016)) +geom_raster(aes(fill=resist.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.55,3),breaks = c(-1.5, 0, 3.0)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2016)") + theme(text=element_text(size=16))

ggpubr::ggarrange(r1,r2,r3,r4,ncol=2,nrow=2, common.legend=T)
ggsave(paste0(folder_names,ptemp,"/Yield.Influence.Landscape.ShadeManagement.tiff"),height=11,width=10)

##################################################################
#Climate Figures
#graph of temperature anomaly and maximum water deficit
era5<-read.csv(paste0(getwd(),"/Analysis/ElNino/era5_anomalies.csv"))
oni<-read_csv(paste0(folder_names,ptemp,"/ONI.csv"))

era5<-era5 %>% mutate(Date=as.Date(as.character(Date)))

#add "month" value
oni <- oni %>% mutate(Date=as.Date(paste0(PeriodNum,"-01"),format="%Y-%m-%d")) %>% rename(oni=NOAA)

era5<-left_join(era5,oni %>% select(Date,oni),by="Date")

g1<-ggplot(era5 %>% filter(Plot=="B13"&year>=1981),aes(Date,wd_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [sigma]") + ggtitle("Quarterly Averaged Water Deficit") +
  #ylim(-60,90)+
  theme(text=element_text(size=12),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex",
                        limits=c(-2,2.6)) +
  geom_vline(aes(xintercept=as.Date("2014-01-01")),linetype="dashed") +
  annotate("text",x=as.Date("2015-07-01"),y=1.5,label=paste0("Study\nPeriod"),size=4) +
  geom_vline(aes(xintercept=as.Date("2016-12-01")),linetype="dashed") 
#annotate("text",x=as.Date("2017-01-01"),y=90,label=paste0("Year 2"),angle=90)

g1a<-ggplot(era5 %>% filter(Plot=="B13"&year>=2014&year<=2016),aes(Date,wd_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity",colour="black") + theme_classic() +
  xlab("Year") + ylab("") + 
  ggtitle("Quarterly Averaged Water Deficit") +
  #ylim(-60,90)+
  theme(text=element_text(size=12),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab", limits=c(-2,2.6),
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-01-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2015-11-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2016-11-01")),linetype="dashed") +
  annotate("text",x=as.Date("2014-06-01"),y=1,label="Normal Year",size=4) +
  annotate("text",x=as.Date("2015-06-01"),y=0.9,label="Shock Year\n1",size=4) +
  annotate("text",x=as.Date("2016-06-01"),y=0.9,label="Shock Year\n2",size=4)

g2<-ggplot(era5 %>% filter(Plot=="B13"&year>=1981),aes(Date,tmax_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [sigma]") + ggtitle("Quarterly Averaged Maximum Temperature") +
  #ylim(-5,5)+
  theme(text=element_text(size=12),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab", limits=c(-2,2.6),
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  #annotate("text",x=as.Date("2016-06-01"),y=-1.25,label=paste0("Study\nPeriod"),size=3) +
  geom_vline(aes(xintercept=as.Date("2017-12-01")),linetype="dashed") 
# geom_segment(aes(x = as.Date("2015-01-01"), y = 2.15, xend = as.Date("2015-10-01"), yend = 2.15), size=0.5,arrow = arrow(length = unit(0.5, "cm")))

g2a<-ggplot(era5 %>% filter(Plot=="B13"&year>=2014&year<=2016),aes(Date,tmax_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity",colour="black") + theme_classic() +
  xlab("Year") + ylab("") + 
  ggtitle("Quarterly Averaged Maximum Temperature") +
  #ylim(-5,5)+
  theme(text=element_text(size=12),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab", limits=c(-2,2.6),
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-01-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2015-11-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2016-11-01")),linetype="dashed")
  

ggpubr::ggarrange(g1,g1a,g2,g2a,ncol=2,nrow=2,align="hv",widths=c(1.25,1),labels="auto", common.legend=T)
ggsave(paste0(folder_names,ptemp,"/Era5.Anom.Comparison.tiff"),height=9,width=15)

g3<-ggplot(era5 %>% filter(Plot=="B13"&year>=1981),aes(Date,precip_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [sigma]") + ggtitle("Quarterly Averaged Precipitation") +
  #ylim(-60,90)+
  theme(text=element_text(size=12),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex",
                        limits=c(-2,2.6)) +
  geom_vline(aes(xintercept=as.Date("2014-01-01")),linetype="dashed") +
  annotate("text",x=as.Date("2015-07-01"),y=1.5,label=paste0("Study\nPeriod"),size=4) +
  geom_vline(aes(xintercept=as.Date("2016-12-01")),linetype="dashed") 
#annotate("text",x=as.Date("2017-01-01"),y=90,label=paste0("Year 2"),angle=90)

g3a<-ggplot(era5 %>% filter(Plot=="B13"&year>=2014&year<=2016),aes(Date,precip_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity",colour="black") + theme_classic() +
  xlab("Year") + ylab("") + 
  ggtitle("Quarterly Averaged Precipitation") +
  #ylim(-60,90)+
  theme(text=element_text(size=12),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab", limits=c(-2,2.6),
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-01-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2015-11-01")),linetype="dashed") +
  geom_vline(aes(xintercept=as.Date("2016-11-01")),linetype="dashed") +
  annotate("text",x=as.Date("2014-06-01"),y=1,label="Normal Year",size=4) +
  annotate("text",x=as.Date("2015-06-01"),y=0.9,label="Shock Year\n1",size=4) +
  annotate("text",x=as.Date("2016-06-01"),y=0.9,label="Shock Year\n2",size=4)

