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

pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.allyrs.pdf"))
hist(tmp1$Shrub.kg)
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.2014.pdf"))
hist(tmp14$Shrub.kg, main="Histogram of 2014 Shrub Yields",xlab="Shrub Yield",cex.lab=2,cex.main=2)
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.shockyrs.pdf"))
hist(tmp2$Shrub.kg, main="Histogram of Shock Year Shrub Yields",xlab="Shrub Yield",cex.lab=2,cex.main=2)
dev.off()


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


mod<-rm1b
#check heteroskedasticity
diagnos <- data.frame(Resid = resid(mod, type = "pearson"), Fitted = fitted(mod),Variable = tmp14$Plot[!is.na(tmp14$propCLR)], yield=tmp14$Shrub.kg[!is.na(tmp14$propCLR)] )
#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_2014.pdf"),width=8,height=8)
d1<-ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20)) 
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_2014.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos, distribution = qnorm, main = list("QQ-Plot", cex = 2), 
           xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
           panel = function(x, ...) {
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
           })
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_fittedvsobserved_2014.pdf"),width=8,height=8)
d3<-ggplot(diagnos, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
#dev.off()
ggpubr::ggarrange(d1,d3,d2,ncol=3)


#gamma model: 2014 w intercept
rm1c<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) ,family  = Gamma(link = "log"),data=tmp14)
summary(rm1c)
vif(rm1c)
confint(rm1c, level = .95, method = c("boot"))

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
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_gammalogmodel_normalyr.tiff"),height=5,width=15)


#gamma model for all years with intercept
rm4a<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
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

#ci2<-confint(rm4b, level = .95, method = c("boot"), nsim = 500, boot.type = c("basic"))
confint(rm4b, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3))
#ci4<-confint(ym8, level = .95, method = c("boot"), nsim = 500, boot.type = c("basic"))
#write.csv(ci2,paste0(getwd(),"/Analysis/ES/Allyrs_90CIs.csv"))

#gamma model all years without intercept** only year relevant for predicting yields
#order of removal 1) + rescale(tmax.anom.fruit) , 2) + rescale(C.pct), 3) rescale(dbh.mn) +
#4) + rescale(buffer), 5) rescale(coffee.area.ha) +, 6)  + rescale(propCBD) [VIF too high with year]
#rm4b<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(K.meq) + rescale(buffer) +
#              rescale(coffee.area.ha) + rescale(C.pct) + rescale(BA.legume)*rescale(Shannon.i) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
#              poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + (1|Plot),family  = Gamma(link = "log"), data=tmp1)
#summary(rm4b)
#vif(rm4b)
#confint(rm4b)

#MuMIn::r.squaredGLMM(rm4b)

#ci2<-confint(rm4b, level = .95, method = c("boot"), nsim = 500, boot.type = c("basic"))
#system.time(ci2<-confint(rm4b, level = .90, method = c("boot"),
                         #.progress="txt", PBargs=list(style=3)))
#ci4<-confint(ym8, level = .95, method = c("boot"), nsim = 500, boot.type = c("basic"))
#write.csv(ci2,paste0(getwd(),"/Analysis/ES/Allyrs_90CIs.csv"))


#check heteroskedasticity
diagnos <- data.frame(yield = rm4b@frame$Shrub.kg, Resid = resid(rm4b, type = "pearson"), Fitted = fitted(rm4b),Variable = rm4b@frame$Plot )
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


#gamma model for all years with intercept and year as factor
#order of removal 1) + rescale(tmax.anom.fruit), 2) + rescale(propCBD), 3)  + rescale(buffer),
#4) rescale(elevation)*rescale(patcharea)[added back in], 5)  + rescale(K.meq), 6)  + rescale(dbh.mn),
#8) rescale(coffee.area.ha) + , 9) rescale(C.pct) + 
rm4c<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea) + factor(year) + rescale(CN.ratio) +
              rescale(BA.legume)*rescale(Shannon.i)  +
              poly(rescale(GapDry),2) + rescale(propCLR) + rescale(propCBB) + (1|Plot),family  = Gamma(link = "log"), data=tmp1)
summary(rm4c)
vif(rm4c)
#confint(rm4b)

MuMIn::r.squaredGLMM(rm4c)

system.time(ci2<-confint(rm4c, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))
#write.csv(ci2,paste0(getwd(),"/Analysis/ES/Allyrs_90CIs_v2.csv"))
          
#check heteroskedasticity
diagnos <- data.frame(yield = rm4c@frame$Shrub.kg, Resid = resid(rm4c, type = "pearson"), Fitted = fitted(rm4c),Variable = rm4c@frame$Plot )
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
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_gammalogmodel_allyrs.tiff"),height=5,width=15)


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

MuMIn::r.squaredGLMM(rm5a)


#2015 & 2016
#order of removal 1) + rescale(buffer) , 2) rescale(BA.legume)*rescale(Shannon.i), 3) + rescale(CN.ratio)
#4) + rescale(Shannon.i), 5) + rescale(propCLR), 6) + rescale(propCBD), 7)+ rescale(dbh.mn), 8) rescale(coffee.area.ha) + 
#9)  + rescale(K.meq)
rm5b<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(C.pct) +
             rescale(BA.legume) + rescale(tmax.anom.fruit) +
              poly(rescale(GapDry),2) + rescale(propCBB)  + (1|Plot),family  = Gamma(link = "log"), data=tmp2)
summary(rm5b)
vif(rm5b)

MuMIn::r.squaredGLMM(rm5b)

system.time(ci3<-confint(rm5b, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))
write.csv(ci3,paste0(getwd(),"/Analysis/ES/shockyrs_90CIs_v2.csv"))

#check heteroskedasticity
diagnos <- data.frame(yield = rm5b@frame$Shrub.kg, Resid = resid(rm5b, type = "pearson"), Fitted = fitted(rm5b),Variable = as.character(rm5b@frame$Plot) )
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
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_gammalogmodel_shockyrs.tiff"),height=5,width=15)

#2015
rm4d<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio)  + rescale(K.meq) +
            rescale(BA.legume)*rescale(Shannon.i) + 
            poly(rescale(GapDry),2) + rescale(propCBB) + rescale(propCLR) - 1,family  = Gamma(link = "log"), data=tmp15)
summary(rm4d)
vif(rm4d)
confint(rm4d, method="boot")

MuMIn::r.squaredGLMM(rm4c)

#2016
rm4e<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio)  + rescale(K.meq) +
            rescale(BA.legume)*rescale(Shannon.i) + 
            poly(rescale(GapDry),2) + rescale(propCBB) + rescale(propCLR) - 1,family  = Gamma(link = "log"), data=tmp16)
summary(rm4e)
vif(rm4e)
confint(rm4e, method="boot")

MuMIn::r.squaredGLMM(rm4c)

#both shock years
rm4f<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio)  + rescale(K.meq) +
            rescale(BA.legume)*rescale(Shannon.i) + 
            poly(rescale(GapDry),2) + rescale(propCBB) + rescale(propCLR) - 1 + (1|Plot),family  = Gamma(link = "log"), data=tmp2)
summary(rm4f)
vif(rm4f)
system.time(ci4f<-confint(rm4f, level = .90, method = c("boot"),
                         .progress="txt", PBargs=list(style=3)))
write.csv(ci4f,paste0(getwd(),"/Analysis/ES/Shockyrs_90CIs.csv"))

MuMIn::r.squaredGLMM(rm4f)

#guassian loglink model
rm3<-glm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB),family  = gaussian(link = "log"), data=tmp14)
summary(rm3)

MuMIn::r.squaredGLMM(rm3)
#check multicollinearity
vif(rm3)


#create figure of diagnostic figures
ggpubr::ggarrange(d1,d3,d2,ncol=3)
ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_lmmodel_normalyr.tiff"),height=5,width=15)

#create figures of significant models
x.glm<-as.data.frame(summary(rm2)$coefficients)
colnames(x.glm)<-c("Coefficients","Std_error","t_value","p_value")

ci1<-confint(rm2, level = .95, method = c("boot"), nsim = 500, boot.type = c("basic"))

x.glm$CI.2.5 <- ci1[,1]
x.glm$CI.97.5 <- ci1[,2]
x.glm$Comparison<-rownames(x.glm)
rownames(x.glm)<-1:nrow(x.glm)

x.glm <- x.glm %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.glm$Labels <- c("Intercept","Elevation","Patch Area","Soil\nPotassium Content","Basal Area of\nLeguminous Trees",
                    "Shade Diversity","Soil C:N","Soil\nCarbon Content","Canopy Gap","Coffee Berry\nDisease",
                    "Shade Diversity:\nLeguminous Trees")
#add in labels for colors & shapes
x.glm$shades<-c("Disease/Other","Landscape","Landscape","Soil","Shade Management","Shade Management","Soil","Soil",
                "Shade Management","Disease/Other","Shade Management")

x.glm$shapes<-c("Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Year","Fixed")

#order by size of effect and Importance factor
x.glm <- x.glm %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.glm))

#write.csv(x.glm,paste0(getwd(),"/Analysis/ES/Finalmodel_yldgamma.csv"))
#write.csv(x.glm,paste0(getwd(),"/Analysis/ES/Finalmodel_yldlm.csv"))
x.glm<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldlm.csv"))

x.glm$Labels<-factor(x.glm$Labels,levels=x.glm[order(x.glm$Importance,decreasing=T),"Labels"])
x.glm$shades<-factor(x.glm$shades,levels=c("Landscape","Shade Management","Soil","Disease/Other"),ordered=T)
#x.lm$sig<-factor(x.lm$sig, levels=c(0,1),labels=c("Significant","NS"))

#remove intercept
x.glm<-x.glm %>% filter(Labels!="Intercept")
r.glm<-MuMIn::r.squaredGLMM(rm2)

g1<-ggplot(x.glm, aes(x = Labels, y = Coefficients, ymin = CI.2.5, ymax = CI.97.5)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(color=factor(shades),shape=factor(shapes))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(taxis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Normal Year")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed") + theme_classic() + 
  annotate("text",x=0.75,y=-0.5,label=paste("R^2 == ",signif(r.glm[1,1], 3)),angle=0,size=5,parse=T) +
  theme(axis.text.x=element_text(angle = 45,hjust=1),legend.position="right",legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 20))

g2<-g1+coord_flip()
g2
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldlm.tiff"),height=10,width=9)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_results_yld14.v4.pdf",height=6,width=6)


###################################################################
###################################################
###Yield shock years


pdf(paste0(getwd(),"/Analysis/ES/Plot.yld201516_lnorm.pdf"),width=8,height=8)
qqp(tmp2$Shrub.kg, "lnorm")
dev.off()


#add 2014 yield to datasets
tmp14 <- tmp14 %>% mutate(norm.yld=Shrub.kg)

tmp2<- left_join(tmp2,tmp14 %>% dplyr::select(Plot,norm.yld), by="Plot")

tmp2$year<-factor(tmp2$year)
#linear model
#tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))
ym1<-lmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(elevation)*rescale(patcharea) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
            rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) +  rescale(BA.legume)*factor(year) + rescale(Shannon.i)*factor(year)+ rescale(buffer) + rescale(dbh.mn) +
            poly(rescale(GapDry),2)*factor(year) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),data=tmp2)
summary(ym1)
MuMIn::r.squaredGLMM(ym1)

#check multicollinearity
vif(ym1)

#check heteroskedasticity

diagnos <- data.frame(Resid = resid(ym1), Fitted = fitted(ym1),Variable = tmp2$Plot[!is.na(tmp2$norm.yld)])
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20))
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_shock.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
       xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gaussian loglink
ym2<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(elevation)*rescale(patcharea) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) +  rescale(BA.legume)*factor(year) + rescale(Shannon.i)*factor(year)+ rescale(buffer) + rescale(dbh.mn) +
             poly(rescale(GapDry),2)*factor(year) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + (1|Plot),family="gaussian"(link='log'),data=tmp2)
summary(ym2)
MuMIn::r.squaredGLMM(ym2)
#check multicollinearity
vif(ym2)


#check heteroskedasticity
diagnos2 <- data.frame(Resid = resid(ym2, type = "pearson"), Fitted = fitted(ym2),Variable = tmp2$Plot[!is.na(tmp2$norm.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgaussloglink_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
ggplot(diagnos2, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw() + theme(text = element_text(size = 20))

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgaussloglink_qqplotResiduals_shock.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos2, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
       xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gaussian loglink
#order of removal: 1) + rescale(CN.ratio), 2) rescale(coffee.area.ha) +, 3) + rescale(patcharea)*factor(year), 4)  poly(rescale(GapDry),2)*factor(year),
#5) + rescale(BA.legume)*factor(year), 6)  + rescale(propCBB), 7) + rescale(buffer), 8) + rescale(Shannon.i)*factor(year),
#9) + rescale(norm.yld), 10) + rescale(dbh.mn), 11) rescale(BA.legume)*rescale(Shannon.i), 12) + rescale(K.meq)  
ym2b<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(elevation)*rescale(patcharea) + rescale(C.pct) +
              rescale(BA.legume)*rescale(Shannon.i) +
              rescale(GapDry)*factor(year) + rescale(propCBD) + rescale(propCLR) + (1|Plot),family="gaussian"(link='log'),data=tmp2)
summary(ym2b)
MuMIn::r.squaredGLMM(ym2b)
#check multicollinearity
vif(ym2b)


#glm gamma loglink
ym3<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(elevation)*rescale(patcharea) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) +  rescale(BA.legume)*factor(year) + rescale(Shannon.i)*factor(year)+ rescale(buffer) + rescale(dbh.mn) +
             poly(rescale(GapDry),2)*factor(year) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) +(1|Plot),family="Gamma"(link='log'),data=tmp2)
summary(ym3)
MuMIn::r.squaredGLMM(ym3)

#check multicollinearity
vif(ym3)

#check heteroskedasticity
diagnos3 <- data.frame(Resid = resid(ym3, type = "pearson"), Fitted = fitted(ym3),Variable = tmp2$Plot[!is.na(tmp2$norm.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
d1<-ggplot(diagnos3, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20))

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_qqplotResiduals_all.v1.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos3, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
       xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gamma loglink
#order removed 1) rescale(elevation)*rescale(patcharea), 2) + rescale(buffer), 3) rescale(coffee.area.ha) + 4) + rescale(propCBD) 
#5) + rescale(CN.ratio) , 6)  + rescale(dbh.mn) , 7) rescale(GapDry) + , 8) rescale(propCLR) + 9)  rescale(BA.legume)*rescale(year)*rescale(Shannon.i),
#10) + rescale(K.meq) 
ym4<-glmer(Shrub.kg~rescale(elevation)*rescale(year) + rescale(patcharea)*rescale(year) + rescale(norm.yld) + rescale(C.pct) +
            rescale(BA.legume)*rescale(year)+rescale(Shannon.i)*rescale(year)  + 
            rescale(propCBB)+(1|Plot),family="Gamma"(link='log'),data=tmp2)
summary(ym4)
vif(ym4)
r.gamma<-MuMIn::r.squaredGLMM(ym4)

#model assessment
diagnos4 <- data.frame(yield = ym4@frame$Shrub.kg, Fitted = fitted(ym4),Variable = ym4@frame$Plot )
tiff(paste0(getwd(),"/Analysis/ES/Plot.yldlm_fittedvsobserved_shockyrs.tiff"))
d3<-ggplot(diagnos4, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
dev.off()


#simplified full model
ym5<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(CN.ratio) + rescale(norm.yld) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*factor(year)+rescale(Shannon.i)*factor(year) + rescale(buffer) + rescale(dbh.mn) +
             rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+factor(year) + (1|Plot),family="Gamma"(link='log'),data=tmp2)
summary(ym5)
MuMIn::r.squaredGLMM(ym5)

#check heteroskedasticity
diagnos3 <- data.frame(Resid = resid(ym5, type = "pearson"), Fitted = fitted(ym5),Variable = ym5@frame$Plot  )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
d1<-ggplot(diagnos3, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20))

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_qqplotResiduals_all.v1.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos3, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
           xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
           panel = function(x, ...) {
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
           })
dev.off()

#model assessment
diagnos5 <- data.frame(yield = ym5@frame$Shrub.kg, Fitted = fitted(ym5),Variable = ym5@frame$Plot )
tiff(paste0(getwd(),"/Analysis/ES/Plot.yldglm_fittedvsobserved_shockyrs.tiff"))
d3<-ggplot(diagnos5, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
dev.off()

#simplified full model
#glm gamma loglink
#order removed 1) + rescale(buffer), 2) rescale(coffee.area.ha) + 3) rescale(GapDry) + 4) + rescale(dbh.mn) , 5) + rescale(CN.ratio)
#6) rescale(propCBD) + , 7)  + rescale(K.meq)
ym6<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year)  + rescale(norm.yld)  +
             rescale(BA.legume)*factor(year)+rescale(Shannon.i)*factor(year) + rescale(C.pct) +
             rescale(propCLR) + rescale(propCBB)+factor(year) + (1|Plot),family="Gamma"(link='log'),data=tmp2)
summary(ym6)
MuMIn::r.squaredGLMM(ym6)


#simplified full model
#glm gamma loglink
#order removed 1) poly(rescale(GapDry),2)*factor(year), 2) + rescale(buffer), 3) + rescale(dbh.mn), 4) poly(rescale(GapDry),
#5) + rescale(elevation)*rescale(patcharea), 6) rescale(GapDry) +, 7)  + rescale(Shannon.i)*factor(year), 8)  rescale(coffee.area.ha) +,
#9) + rescale(CN.ratio), 10) + rescale(GapDry), 11)  + rescale(norm.yld), 12) + rescale(propCLR)
ym6b<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(C.pct) + rescale(K.meq) +
             rescale(BA.legume)*factor(year) +  + rescale(BA.legume)*rescale(Shannon.i) +
              rescale(propCBD) + rescale(propCBB) + (1|Plot),family="Gamma"(link='log'),data=tmp2)
summary(ym6b)
MuMIn::r.squaredGLMM(ym6b)
vif(ym6b)

#Explore linear model
#order removed 1) poly(rescale(GapDry),2)*factor(year), 2) + rescale(CN.ratio), 3) + rescale(BA.legume)*rescale(Shannon.i),
#4) rescale(Shannon.i)*factor(year), 5) + rescale(buffer), 6) rescale(elevation)*rescale(patcharea) +, 7) rescale(coffee.area.ha) +,
#8) + rescale(Shannon.i)
ym7<-lmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(norm.yld) + rescale(C.pct) + rescale(K.meq) +
            rescale(BA.legume)*factor(year) + rescale(dbh.mn) +
            poly(rescale(GapDry),2) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),data=tmp2)
summary(ym7)
MuMIn::r.squaredGLMM(ym7)

#check multicollinearity
vif(ym7)

#Explore gamma loglink without intercept
#order of removal: 1) poly(rescale(GapDry),2)*factor(year), 2) + rescale(buffer), 3) poly(rescale(GapDry),2)
#4) + rescale(dbh.mn), 5) + rescale(elevation)*rescale(patcharea), 6) + rescale(BA.legume)*rescale(Shannon.i)
#7) rescale(GapDry) +, 8) rescale(coffee.area.ha) +, 9) rescale(propCBD) +, 10) + rescale(CN.ratio)
#11) + rescale(norm.yld)  
ym8<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) + rescale(C.pct) + rescale(K.meq) +
             rescale(BA.legume)*factor(year) + rescale(Shannon.i)*factor(year) +
             rescale(propCLR) + rescale(propCBB) +(1|Plot) - 1,family="Gamma"(link='log'),data=tmp2)
summary(ym8)
MuMIn::r.squaredGLMM(ym8)

#check multicollinearity
vif(ym8)

#check heteroskedasticity

diagnos8 <- data.frame(Resid = resid(ym8), Fitted = fitted(ym8), Variable = ym8@frame$Plot)
#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_shock.v2.pdf"),width=8,height=8)
d1<-ggplot(diagnos8, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20))
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_shock.v2.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos8, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
       xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
#dev.off()

diagnos9 <- data.frame(yield = ym8@frame$Shrub.kg, Fitted = fitted(ym8),Variable = ym8@frame$Plot )
#tiff(paste0(getwd(),"/Analysis/ES/Plot.yldglm_fittedvsobserved_shockyrs.tiff"))
d3<-ggplot(diagnos9, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
#dev.off()

#create figure of diagnostic figures
ggpubr::ggarrange(d1,d3,d2,ncol=3)

#Explore gamma loglink without intercept and without year
#order of removal: 1) + rescale(buffer), 2)  + rescale(C.pct), 3) + rescale(dbh.mn),
#4) + rescale(propCBD), 5)  rescale(coffee.area.ha) +, 6) + rescale(propCLR), 7) + rescale(K.meq)
#8) poly(rescale(GapDry),2)
ym9<-glmer(Shrub.kg~rescale(elevation) + factor(year) + rescale(patcharea) + rescale(norm.yld) + rescale(CN.ratio) +
             rescale(BA.legume) + rescale(Shannon.i) +
             rescale(GapDry) + rescale(propCBB) + (1|Plot) - 1,family="Gamma"(link='log'),data=tmp2)
summary(ym9)
MuMIn::r.squaredGLMM(ym9)

#check multicollinearity
vif(ym9)

#check heteroskedasticity

diagnos9 <- data.frame(Resid = resid(ym9), Fitted = fitted(ym9), Variable = ym9@frame$Plot)
#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_shock.v2.pdf"),width=8,height=8)
d1<-ggplot(diagnos9, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20))
#dev.off()

#pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_shock.v2.pdf"),width=8,height=8)
d2<-qqmath(~Resid, data = diagnos9, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
           xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
           panel = function(x, ...) {
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
           })
#dev.off()

diagnos9 <- data.frame(yield = ym9@frame$Shrub.kg, Fitted = fitted(ym9),Variable = ym9@frame$Plot )
#tiff(paste0(getwd(),"/Analysis/ES/Plot.yldglm_fittedvsobserved_shockyrs.tiff"))
d3<-ggplot(diagnos9, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
#dev.off()

#create figure of diagnostic figures
ggpubr::ggarrange(d1,d3,d2,ncol=3)

ggsave(paste0(folder_names,ptemp,"/Diagnosticfigures_model_shockyrs.tiff"),height=5,width=15)

#create figures of significant models
x.gamma<-as.data.frame(summary(ym9)$coefficients)
colnames(x.gamma)<-c("Coefficients","Std_error","t_value","p_value")
x.gamma$Comparison<-rownames(x.gamma)
rownames(x.gamma)<-1:nrow(x.gamma)

ci0 <- confint(ym9,method="Wald")
#ci1 <- confint(ym8)
system.time(ci4<-confint(ym9, level = .95, method = c("boot"),
                            .progress="txt", PBargs=list(style=3)))
#ci4<-confint(ym8, level = .95, method = c("boot"), nsim = 500, boot.type = c("basic"))

x.gamma$CI.97.5 <- ci4[3:12,2]
x.gamma$CI.2.5 <- ci4[3:12,1]

#x.gamma <- x.gamma %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
 # mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.gamma$Labels <- c("Elevation","Year(2015)","Year(2016)","Patch Area","Yield in\nNormal Year","Soil C:N",
                    "Basal Area of\nLeguminous Trees","Shade Diversity","Canopy Gap","Coffee\nBerry Borer")
#add in labels for colors & shapes
x.gamma$shades<-c("Landscape","Disease/Other","Disease/Other","Landscape","Disease/Other","Soil","Shade Management","Shade Management","Shade Management",
                  "Disease/Other")

x.gamma$shapes<-c("Fixed","Year","Year","Fixed","Fixed","Fixed","Fixed","Fixed","Fixed","Year")

#order by size of effect and Importance factor
x.gamma <- x.gamma %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.gamma))

#write.csv(x.gamma,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink.csv"))
#write.csv(x.gamma,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglm.csv"))
#x.gamma<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglm.csv"))

x.gamma$Labels<-factor(x.gamma$Labels,levels=x.gamma[order(x.gamma$Importance,decreasing=T),"Labels"])
x.gamma$shades<-factor(x.gamma$shades,levels=c("Landscape","Shade Management","Soil","Disease/Other"),ordered=T)

#remove intercept
x.gamma<-x.gamma %>% filter(Labels!="Intercept")
r.gamma <- MuMIn::r.squaredGLMM(ym9)

g3<-ggplot(x.gamma, aes(x = Labels, y = Coefficients, ymin = CI.2.5, ymax = CI.97.5)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Shock Years")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(axis.text.x=element_text(angle = 45,hjust=1),plot.title = element_text(hjust = 0.5),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=-2.0,label=paste("R^2 == ",signif(r.gamma[1,1], 3)),angle=0,size=5,parse=T) + theme(text = element_text(size = 20))

g4<-g3+coord_flip()
g4
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglm.tiff"),height=10,width=9)

library(ggpubr)
#create overall title
#text <- "Landscape and Management Influences on Yield"

# Create a text grob
##tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,2,0,2, "cm"))

#ggarrange(plot_0,NULL,g2, g4,
#          ncol =2,nrow = 2,heights = c(1,5),common.legend=T,legend=c("bottom"),labels="auto")

ggarrange(g2, g4, ncol =2,common.legend=T,legend=c("bottom"),labels="auto",
          font.label = list(size = 18, color = "black", face = "bold", family = NULL),
          label.x=0.2)

ggsave(paste0(folder_names,ptemp,"Finalmodel_results_comboplots.tiff"),height=10.5,width=16)

###################################################################
###################################################
####All Year analysis
#gamma model
am1<-glmer(Shrub.kg~rescale(elevation)+
             rescale(patcharea)+
             factor(year) + 
             rescale(CN.ratio) + 
             rescale(C.pct) + 
             rescale(K.meq) + 
             rescale(coffee.area.ha) + 
             rescale(BA.legume)*rescale(Shannon.i) + # this interaction makes sense, but hard to say if there's enough data to evaluate it
             rescale(buffer) + 
             rescale(dbh.mn) + 
             rescale(tmax.anom.fruit) + 
             rescale(mwd.flower) + rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB) + (1|Plot),family="Gamma"(link='log'),data=tmp1)
summary(am1)
#check multicollinearity
vif(am1)

#gamma model: all years
#order of removal [due to VIF] 1) rescale(tmax.anom.fruit) , 2)rescale(mwd.flower) + 3) rescale(propCBD) + 4) rescale(buffer) + 
# [due to low significance] 5)  rescale(coffee.area.ha) + , 6) rescale(dbh.mn) + , 7) rescale(patcharea)+ 8) rescale(K.meq) + 
#9)  rescale(GapDry) + 10) rescale(CN.ratio) + 11) rescale(propCLR) + 
am2<-glmer(Shrub.kg~rescale(elevation)+
             factor(year) + 
             rescale(C.pct) + 
             rescale(BA.legume)*rescale(Shannon.i) + # this interaction makes sense, but hard to say if there's enough data to evaluate it
             rescale(propCBB) + (1|Plot),family="Gamma"(link='log'),data=tmp1)
summary(am2)
vif(am2)


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

