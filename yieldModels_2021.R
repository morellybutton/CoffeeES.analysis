#Revisiting statistical analysis for El Nino coffee landscapes paper

library(tidyverse)

#setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")
folder_names<-"/Users/AMOREL001/Google Drive/Research/"
#data folder
dtemp<-"Africa/ECOLIMITS1/ECOLIMITS2019/Yayu"
#pubs folder
ptemp<-"Publications/2021/CoffeeLandscapes/"
setwd(paste0(folder_names,dtemp))

d.F.plot <- read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))
#d.F.shrub <- read.csv(paste0(getwd(),"/Analysis/ES/ES.shrub_analysis_dataset.csv"))

#calculate farm yield and standard error of yield for shrubs and plots
d.F.plot <- d.F.plot %>% mutate(Shrub.kg.max=Shrub.kg+se.kg,Shrub.kg.min=Shrub.kg-se.kg) %>% 
  mutate(kg.ha=Shrub.kg*density,kg.ha.max=Shrub.kg.max*(density+sedens),kg.ha.min=Shrub.kg.min*(density+sedens)) %>% 
  mutate(kg.ha.max=replace(kg.ha.max,is.na(sedens),Shrub.kg.max[is.na(sedens)]*(density[is.na(sedens)])),
         kg.ha.min=replace(kg.ha.min,is.na(sedens),Shrub.kg.min[is.na(sedens)]*(density[is.na(sedens)])))


#calculate resistance and resilience values using Isbell et al (2015) Nature
#resistance = Yn/abs(Ye-Yn)
#where Yn = mean productivity during normal year and Ye is yield during climate shock
#resilience = abs(Ye-Yn)/abs(Y[e+1]-Yn)
#where Y[e+1] = productivity during the year after a climate event.
#add in inverse of resistance
#iresistance = abs(Ye-Yn)/Yn

#calculate 2015 resistance
tmp15<-d.F.plot %>% filter(year==2015) %>% select(Plot,Shrub.kg,Shrub.kg.max,Shrub.kg.min,kg.ha,kg.ha.max,kg.ha.min) %>% 
  rename(Shrub.kg.15=Shrub.kg,Shrub.kg.15.max=Shrub.kg.max,Shrub.kg.15.min=Shrub.kg.min,
         kg.ha.15=kg.ha,kg.ha.15.max=kg.ha.max,kg.ha.15.min=kg.ha.min)
tmp14<-d.F.plot %>% filter(year==2014) %>% select(Plot,Shrub.kg,Shrub.kg.max,Shrub.kg.min,kg.ha,kg.ha.max,kg.ha.min) %>% 
  rename(Shrub.kg.14=Shrub.kg,Shrub.kg.14.max=Shrub.kg.max,Shrub.kg.14.min=Shrub.kg.min,
         kg.ha.14=kg.ha,kg.ha.14.max=kg.ha.max,kg.ha.14.min=kg.ha.min) 
  
tmp15<-left_join(tmp15,tmp14,by="Plot")

tmp15 <- tmp15 %>% mutate(resist=Shrub.kg.14/abs(Shrub.kg.15-Shrub.kg.14),
                          resist.max=Shrub.kg.14.max/abs(Shrub.kg.15.max-Shrub.kg.14.max),
                          resist.min=Shrub.kg.14.min/abs(Shrub.kg.15.min-Shrub.kg.14.min),
                          iresist=abs(Shrub.kg.15-Shrub.kg.14)/Shrub.kg.14,
                          iresist.max=abs(Shrub.kg.15.max-Shrub.kg.14.max)/Shrub.kg.14.max,
                          iresist.min=abs(Shrub.kg.15.min-Shrub.kg.14.min)/Shrub.kg.14.min)

tmp16<-d.F.plot %>% filter(year==2016) %>% select(Plot,Shrub.kg,Shrub.kg.max,Shrub.kg.min,kg.ha,kg.ha.max,kg.ha.min) %>% 
  rename(Shrub.kg.16=Shrub.kg,Shrub.kg.16.max=Shrub.kg.max,Shrub.kg.16.min=Shrub.kg.min,
         kg.ha.16=kg.ha,kg.ha.16.max=kg.ha.max,kg.ha.16.min=kg.ha.min)
tmp16<-left_join(tmp16,tmp14,by="Plot")

tmp16 <- tmp16 %>% mutate(resist=Shrub.kg.14/abs(Shrub.kg.16-Shrub.kg.14),
                          resist.max=Shrub.kg.14.max/abs(Shrub.kg.16.max-Shrub.kg.14.max),
                          resist.min=Shrub.kg.14.min/abs(Shrub.kg.16.min-Shrub.kg.14.min),
                          iresist=abs(Shrub.kg.16-Shrub.kg.14)/Shrub.kg.14,
                          iresist.max=abs(Shrub.kg.16.max-Shrub.kg.14.max)/Shrub.kg.14.max,
                          iresist.min=abs(Shrub.kg.16.min-Shrub.kg.14.min)/Shrub.kg.14.min)
tmp15$year<-2015
tmp16$year<-2016

tmpall<-rbind(tmp15 %>% select(Plot,year,resist,resist.max,resist.min,iresist,iresist.max,iresist.min),
              tmp16 %>% select(Plot,year,resist,resist.max,resist.min,iresist,iresist.max,iresist.min))

d.F.plot<-left_join(d.F.plot,tmpall,by=c("Plot","year"))

rm(tmp15,tmp14,tmp16,tmpall)
#write.csv(d.F.plot,paste0(getwd(),"/Analysis/ES/ES.plot_analysis.wresist_dataset.csv"))
#coffee.density<-read.csv(paste0(getwd(),"/Yield/Coffee.density_2015_dbhmeans.csv"))

#plot resistence or inverse resistance
ggplot() + geom_point(data=d.F.plot %>% filter(year!=2014), aes(Shrub.kg,resist,colour=factor(year))) +
 geom_smooth(data=d.F.plot %>% filter(year!=2014), method="lm",aes(Shrub.kg,resist,group=year,colour=factor(year))) +
  theme_classic() + xlab("Shrub Yield")+ylab("Yield Resistance") +
  ggtitle("Yield Resistance vs Shrub Yield") + theme(legend.title = element_blank(),legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/ResistanceYield.vs.Yield.pdf"),height=5,width=5)

ggplot() + geom_point(data=d.F.plot %>% filter(year!=2014), aes(Shrub.kg,iresist,colour=factor(year))) +
  geom_smooth(data=d.F.plot %>% filter(year!=2014), method="lm",aes(Shrub.kg,iresist,group=year,colour=factor(year)))  +
  theme_classic() + xlab("Shrub Yield")+ylab("Inverse Yield Resistance") +
  ggtitle("Yield Resistance vs Shrub Yield") + theme(legend.title = element_blank(),legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/InvResistanceYield.vs.Yield.pdf"),height=5,width=5)

library(arm)
library(car)
#library(MuMIn)
library(lattice)

#do a corrplot
tmp<-d.F.plot %>% dplyr::select(-Plot,-ID,-X,-kebele)
corrplot(tmp,color=TRUE,details=TRUE)

tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))

pdf(paste0(getwd(),"/Analysis/ES/Hist.kg.ha.allyrs.pdf"))
hist(tmp1$kg.ha)
dev.off()
pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.allyrs.pdf"))
hist(tmp1$Shrub.kg)
dev.off()



#yield for all years (shrub.kg)
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldallyrs_lnorm.pdf"),width=8,height=8)
qqp(tmp1$Shrub.kg, "lnorm")
dev.off()

#linear model
#tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))
rm1<-lmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),data=tmp1)
summary(rm1)
MuMIn::r.squaredGLMM(rm1)

#check multicollinearity
vif1<-vif(rm1)

#check heteroskedasticity
diagnos <- data.frame(Resid = resid(rm1, type = "pearson"), Fitted = fitted(rm1),Variable = tmp1$Plot[!is.na(tmp1$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_all.v2.pdf"),width=8,height=8)
ggplot(rm1, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_all.v2.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gaussian loglink
rm2<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit)  + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
            rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) + rescale(buffer) + rescale(dbh.mn)  +
            rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),family="gaussian"(link='log'),data=tmp1)
summary(rm2)
MuMIn::r.squaredGLMM(rm2)
#check multicollinearity
vif2<-vif(rm2)

#check heteroskedasticity
diagnos2 <- data.frame(Resid = resid(rm2, type = "pearson"), Fitted = fitted(rm2),Variable = tmp1$Plot[!is.na(tmp1$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgaussloglink_ResidualvFittedValues_all.v2.pdf"),width=8,height=8)
ggplot(rm2, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgaussloglink_qqplotResiduals_all.v2.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos2, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gamma loglink
rm3<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
            rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) + rescale(buffer) + rescale(dbh.mn) +
            rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),family="Gamma"(link='log'),data=tmp1)
summary(rm3)
MuMIn::r.squaredGLMM(rm3)

#check multicollinearity
vif3<-vif(rm3)

#check heteroskedasticity
diagnos3 <- data.frame(Resid = resid(rm3, type = "pearson"), Fitted = fitted(rm3),Variable = tmp1$Plot[!is.na(tmp1$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_ResidualvFittedValues_all.v1.pdf"),width=8,height=8)
ggplot(rm3, aes(.fitted, .resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_qqplotResiduals_all.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos3, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gamma loglink
#order removed 1) rescale(dbh.mn), 2) rescale(coffee.area.ha), 3) rescale(K.meq), 4) rescale(CN.ratio), 5) rescale(GapDry)
# 6) rescale(buffer) , 7) rescale(propCBB), 8)  rescale(propCLR), 9) rescale(C.pct) 

rm4<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit) + 
             rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) +
             rescale(propCBD) + (1|Plot),family="Gamma"(link='log'),data=tmp1)
summary(rm4)
vif(rm4)
r.gamma<-MuMIn::r.squaredGLMM(rm4)

#remove three way interactions
rm6<-glmer(Shrub.kg~rescale(elevation) + rescale(patcharea)*rescale(tmax.anom.fruit) + 
             rescale(BA.legume)+ rescale(Shannon.i)*rescale(tmax.anom.fruit) +
             rescale(propCBD) + (1|Plot),family="Gamma"(link='log'),data=tmp1)
summary(rm6)
vif(rm6)
r.gamma<-MuMIn::r.squaredGLMM(rm6)

#gaussian loglink ****(haven't redone this one - still have year rather than tmax anomaly)****
#order removed 1) rescale(tmax.anom.fruit), 2) rescale(mwd.flower), 3) rescale(propCBD),
#4) rescale(elevation)*rescale(patcharea), 5) rescale(CN.ratio), 6) rescale(coffee.area.ha),
#7) rescale(dbh.mn), 8) rescale(K.meq), 9) rescale(buffer), 10) rescale(propCLR) ,
#11) rescale(C.pct)
rm5<-glmer(Shrub.kg~rescale(elevation)*factor(year) + rescale(patcharea)*factor(year) +  
             rescale(BA.legume)*rescale(Shannon.i)*factor(year) + 
             rescale(GapDry) + rescale(propCBB)+(1|Plot),family="gaussian"(link='log'),data=tmp1)
summary(rm5)
vif(rm5)
r.gauss<-MuMIn::r.squaredGLMM(rm5)

#create figures of significant models
x.gamma<-as.data.frame(summary(rm6)$coefficients)
colnames(x.gamma)<-c("Coefficients","Std_error","t_value","p_value")
x.gamma$Comparison<-rownames(x.gamma)
rownames(x.gamma)<-1:nrow(x.gamma)

x.gamma <- x.gamma %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.gamma$Labels <- c("Intercept","Elevation","Patch Area","Maximum Temperature\nAnomaly","Basal Area of\nLeguminous Trees",
                    "Shade Diversity","Coffee\nBerry Disease","Patch Area:Max Temp Anomaly",
                    "Shade Diversity:Max Temp Anomaly")
#add in labels for colors & shapes
x.gamma$shades<-c("Other","Elevation","Patch Area","Other","Leguminous Trees","Shade Diversity",
                  "Other","Patch Area","Shade Diversity")

x.gamma$shapes<-c("Fixed","Fixed","Fixed","Year","Fixed","Fixed","Year","Year","Year")

#order by size of effect and Importance factor
x.gamma <- x.gamma %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.gamma))

#write.csv(x.gamma,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink2.csv"))
x.gamma<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink2.csv"))

x.gamma$Labels<-factor(x.gamma$Labels,levels=x.gamma[order(x.gamma$Importance,decreasing=T),"Labels"])
x.gamma$shades<-factor(x.gamma$shades,levels=c("Elevation","Patch Area","Leguminous Trees","Shade Diversity",
                                              "Other"),ordered=T)

#remove intercept
x.gamma<-x.gamma %>% filter(Labels!="Intercept")

g1<-ggplot(x.gamma, aes(x = Labels, y = Coefficients, ymin = Coefficients-Std_error, ymax = Coefficients+Std_error)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Shrub Yield (kg)\n[All Years]")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=-1.25,label=paste("R2 = ",signif(r.gamma[1,1], 3)),angle=0,size=4)

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglmgammaloglink2.tiff"),height=8,width=9)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_results_yld14.v4.pdf",height=6,width=6)

#create figures of significant models, gaussian
x.gauss<-as.data.frame(summary(rm5)$coefficients)
colnames(x.gauss)<-c("Coefficients","Std_error","t_value","p_value")
x.gauss$Comparison<-rownames(x.gauss)
rownames(x.gauss)<-1:nrow(x.gauss)

x.gauss <- x.gauss %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.gauss$Labels <- c("Intercept","Elevation","2015","2016","Patch Area","Basal Area of\nLeguminous Trees",
                    "Shade Diversity","Canopy Gap","Coffee\nBerry Borer","Elevation:2015","Elevation:2016","Patch Area:2015",
                    "Patch Area:2016","BA Legume:\nShade Diversity","BA Legume:2015","BA Legume:2016","Shade Diversity:2015",
                    "Shade Diversity:2016","BA Legume:\nShade Diversity:2015","BA Legume:\nShade Diversity:2016")
#add in labels for colors & shapes
x.gauss$shades<-c("Other","Elevation","Other","Other","Patch Area","Leguminous Trees","Shade Diversity",
                  "Other","Other","Elevation","Elevation","Patch Area","Patch Area","Shade Tree\nInteraction",
                  "Leguminous Trees","Leguminous Trees","Shade Diversity","Shade Diversity","Shade Tree\nInteraction","Shade Tree\nInteraction")

x.gauss$shapes<-c("Fixed","Fixed","Year","Year","Fixed","Fixed","Fixed",
                  "Fixed","Fixed","Year","Year","Year","Year","Fixed",
                  "Year","Year","Year","Year","Year","Year")

#order by size of effect and Importance factor
x.gauss <- x.gauss %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.gauss))

#write.csv(x.gauss,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgaussloglink.csv"))
x.gauss<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgaussloglink.csv"))

x.gauss$Labels<-factor(x.gauss$Labels,levels=x.gauss[order(x.gauss$Importance,decreasing=T),"Labels"])
x.gauss$shades<-factor(x.gauss$shades,levels=c("Elevation","Patch Area","Leguminous Trees","Shade Diversity",
                                               "Shade Tree\nInteraction","Other"),ordered=T)

#remove intercept
x.gauss<-x.gauss %>% filter(Labels!="Intercept")

g1<-ggplot(x.gauss, aes(x = Labels, y = Coefficients, ymin = Coefficients-Std_error, ymax = Coefficients+Std_error)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Shrub Yield (kg)")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=1.25,label=paste("R2 = ",signif(r.gauss[1,1], 3)),angle=0,size=4)

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglmgaussloglink.pdf"),height=8,width=7)
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/AnalysisFigures/Model_results_yld14.v4.pdf",height=6,width=6)


###################################################################
###################################################
###Yield individual years

tmp2<- tmp1 %>% filter(year==2014)

#yield for 2014
pdf(paste0(getwd(),"/Analysis/ES/Plot.yld2014_norm.pdf"),width=8,height=8)
qqp(tmp2$Shrub.kg, "norm")
dev.off()


tmp3<- tmp1 %>% filter(year==2015)

#yield for 2015
pdf(paste0(getwd(),"/Analysis/ES/Plot.yld2015_lnorm.pdf"),width=8,height=8)
qqp(tmp3$Shrub.kg, "lnorm")
dev.off()


tmp4<- tmp1 %>% filter(year==2016)

#yield for 2016
pdf(paste0(getwd(),"/Analysis/ES/Plot.yld2016_lnorm.pdf"),width=8,height=8)
qqp(tmp4$Shrub.kg, "lnorm")
dev.off()

#yield for 2015 & 2016
tmp5<- tmp1 %>% filter(year!=2014)

pdf(paste0(getwd(),"/Analysis/ES/Plot.yld201516_lnorm.pdf"),width=8,height=8)
qqp(tmp5$Shrub.kg, "lnorm")
dev.off()


#add 2014 yield to datasets
tmp2 <- tmp2 %>% rename(norm.yld=Shrub.kg)

tmp5<- left_join(tmp5,tmp2 %>% select(Plot,norm.yld), by="Plot")

library(arm)
library(car)
library(lattice)

#linear model
#tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))
ym1<-lmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
            rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
            rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),data=tmp5)
summary(ym1)
MuMIn::r.squaredGLMM(ym1)

#check multicollinearity
vif1<-vif(ym1)

#check heteroskedasticity

tmp5.1 <- tmp5 %>% filter(!is.na(tmp5$propCLR)&!is.na(tmp5$norm.yld))
                 
diagnos <- data.frame(Resid = resid(ym1), Fitted = fitted(ym1),Variable = tmp5$Plot[!is.na(tmp5$norm.yld)])
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_shock.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gaussian loglink
ym2<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) + rescale(buffer) + rescale(dbh.mn)  +
             rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),family="gaussian"(link='log'),data=tmp5)
summary(ym2)
MuMIn::r.squaredGLMM(ym2)
#check multicollinearity
vif2<-vif(ym2)

#check heteroskedasticity
diagnos2 <- data.frame(Resid = resid(ym2, type = "pearson"), Fitted = fitted(ym2),Variable = tmp5$Plot[!is.na(tmp5$propCLR)&!is.na(tmp5$norm.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgaussloglink_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
ggplot(diagnos2, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgaussloglink_qqplotResiduals_shock.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos2, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gamma loglink
ym3<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(tmax.anom.fruit) + rescale(CN.ratio) + rescale(norm.yld) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(tmax.anom.fruit) + rescale(buffer) + rescale(dbh.mn) +
             rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),family="Gamma"(link='log'),data=tmp5)
summary(ym3)
MuMIn::r.squaredGLMM(ym3)

#check multicollinearity
vif3<-vif(ym3)

#check heteroskedasticity
diagnos3 <- data.frame(Resid = resid(ym3, type = "pearson"), Fitted = fitted(ym3),Variable = tmp5$Plot[!is.na(tmp5$propCLR)&!is.na(tmp5$norm.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
ggplot(diagnos3, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_qqplotResiduals_all.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos3, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

#glm gaussian loglink
#order removed 1)  + rescale(CN.ratio) , 2)  + rescale(propCBD), 4)  + rescale(propCBB),
#5) rescale(coffee.area.ha) + 6)  + rescale(buffer), 7) rescale(GapDry) + ,8)  + rescale(dbh.mn) 
#9) + rescale(K.meq)
ym4<-glmer(Shrub.kg~rescale(elevation)*rescale(tmax.anom.fruit) + rescale(patcharea) + rescale(norm.yld) + rescale(C.pct)  +
             rescale(BA.legume)*rescale(tmax.anom.fruit)  + rescale(Shannon.i)*rescale(tmax.anom.fruit) +
             rescale(propCLR) +(1|Plot),family="gaussian"(link='log'),data=tmp5)
summary(ym4)
vif(ym4)
r.gauss<-MuMIn::r.squaredGLMM(ym4)


#create figures of significant models
x.gauss<-as.data.frame(summary(ym4)$coefficients)
colnames(x.gauss)<-c("Coefficients","Std_error","t_value","p_value")
x.gauss$Comparison<-rownames(x.gauss)
rownames(x.gauss)<-1:nrow(x.gauss)

x.gauss <- x.gauss %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.gauss$Labels <- c("Intercept","Elevation","Maximum Temperature\nAnomaly","Patch Area","Yield in\nNormal Year","Soil Carbon Content",
                    "Basal Area of\nLeguminous Trees","Shade Diversity","Coffee\nLeaf Rust","Elevation:Max Temp Anomaly",
                    "Leguminous Trees:Max Temp Anomaly","Shade Diversity:Max Temp Anomaly")
#add in labels for colors & shapes
x.gauss$shades<-c("Other","Elevation","Other","Patch Area","Other","Soil","Leguminous Trees","Shade Diversity","Disease",
                  "Elevation","Leguminous Trees","Shade Diversity")

x.gauss$shapes<-c("Fixed","Fixed","Year","Fixed","Fixed","Fixed","Fixed","Fixed","Year","Year","Year","Year")

#order by size of effect and Importance factor
x.gauss <- x.gauss %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.gauss))

#write.csv(x.gauss,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgaussloglink.csv"))
x.gauss<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgaussloglink.csv"))

x.gauss$Labels<-factor(x.gauss$Labels,levels=x.gauss[order(x.gauss$Importance,decreasing=T),"Labels"])
x.gauss$shades<-factor(x.gauss$shades,levels=c("Elevation","Patch Area","Leguminous Trees","Shade Diversity","Soil","Disease",
                                               "Other"),ordered=T)

#remove intercept
x.gauss<-x.gauss %>% filter(Labels!="Intercept")

g1<-ggplot(x.gauss, aes(x = Labels, y = Coefficients, ymin = Coefficients-Std_error, ymax = Coefficients+Std_error)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Shrub Yield (kg)\n[Shock Years]")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=1.25,label=paste("R2 = ",signif(r.gauss[1,1], 3)),angle=0,size=4)

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglmgaussloglink.tiff"),height=8,width=9)


#####################################################################
######Create 2D figures of landscape and shade management interactions
library(paletteer)
library(tidyverse)
library(lme4)
library(arm)

d.F.plot<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis.wresist_dataset.csv"))
tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))

yld_model<-glmer(Shrub.kg~rescale(elevation) + rescale(patcharea)*rescale(tmax.anom.fruit) + 
                   rescale(BA.legume)+ rescale(Shannon.i)*rescale(tmax.anom.fruit) +
                   rescale(propCBD) + (1|Plot),family="Gamma"(link='log'),data=tmp1)

summary(yld_model)
car::vif(yld_model)
MuMIn::r.squaredGLMM(yld_model)

tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink2.csv"))

#use mean max temp anomaly for each year for these figures)
z.tmax<-attributes(scale(d.F.plot$tmax.anom.fruit))
tmax <- tmp1 %>% mutate(z.tmax.anom=(tmax.anom.fruit-z.tmax[[2]])/(2*z.tmax[[3]])) %>% 
  dplyr::select(year,z.tmax.anom) %>% group_by(year) %>% 
  summarise(tmax.anom=max(z.tmax.anom,na.rm=T))

#variables of interest
elev<-tibble(elev=seq(as.integer(min(d.F.plot$elevation)),as.integer(max(d.F.plot$elevation)),by=1))
patch<-tibble(patch=seq(as.integer(min(d.F.plot$patcharea)),as.integer(max(d.F.plot$patcharea)),by=5))
z.elev<-attributes(scale(d.F.plot$elevation))
z.patch<-attributes(scale(d.F.plot$patcharea))

#tmp <- d.F.plot %>% dplyr::select(Plot,elevation) %>% distinct(Plot, elevation, .keep_all = TRUE) %>% 
 # mutate(elevation=signif(elevation,4)) %>% arrange(elevation)

#newdata
elev <- elev %>% mutate(elevation=(elev-z.elev[[2]])/(2*z.elev[[3]]))
patch <- patch %>% mutate(patcharea=(patch-z.patch[[2]])/(2*z.patch[[3]]))

tmp.land<-merge(elev,patch)
#tmp14<- tmp14 %>% mutate(year=factor(2014),BA.legume=0,Shannon.i=0,propCBB=0,Plot=NA)

tmp.land <- tmp.land %>% mutate(yld.2014=tmp[tmp$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp[tmp$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  tmp[tmp$Comparison=="rescale(patcharea):rescale(tmax.anom.fruit)","Coefficients"]*patcharea*as.numeric(tmax[tmax$year==2014,2]),
                                yld.2015 = tmp[tmp$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp[tmp$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  tmp[tmp$Comparison=="rescale(patcharea):rescale(tmax.anom.fruit)","Coefficients"]*patcharea*as.numeric(tmax[tmax$year==2015,2]),
                                yld.2016 = tmp[tmp$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp[tmp$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  tmp[tmp$Comparison=="rescale(patcharea):rescale(tmax.anom.fruit)","Coefficients"]*patcharea*as.numeric(tmax[tmax$year==2016,2]))

g1<-ggplot(tmp.land, aes(patch, elev, z = yld.2014)) +geom_raster(aes(fill=yld.2014)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.6,1.4)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2014) [Anom = 0.246 C]") + theme(text=element_text(size=16)) +
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g2<-ggplot(tmp.land, aes(patch, elev, z = yld.2015)) +geom_raster(aes(fill=yld.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.6,1.4)) +
  theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2015) [Anom = 1.52 C]") + 
  theme(text=element_text(size=16))+ guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g3<-ggplot(tmp.land, aes(patch, elev, z = yld.2016)) +geom_raster(aes(fill=yld.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.6,1.4)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2016) [Anom = 0.675 C]") + 
  theme(text=element_text(size=16)) + guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

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
tmp.shade <- tmp.shade %>% mutate(yld.2014 = tmp[tmp$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    tmp[tmp$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    tmp[tmp$Comparison=="rescale(tmax.anom.fruit):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(tmax[tmax$year==2014,2]),
                                  yld.2015 =  tmp[tmp$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    tmp[tmp$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    tmp[tmp$Comparison=="rescale(tmax.anom.fruit):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(tmax[tmax$year==2015,2]),
                                  yld.2016 =  tmp[tmp$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    tmp[tmp$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    tmp[tmp$Comparison=="rescale(tmax.anom.fruit):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(tmax[tmax$year==2016,2]))

g4<-ggplot(tmp.shade, aes(diversity, legume,  z = yld.2014)) +geom_raster(aes(fill=yld.2014)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.6,1.4)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2014)") + theme(text=element_text(size=16)) +
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g5<-ggplot(tmp.shade, aes(diversity, legume, z = yld.2015)) +geom_raster(aes(fill=yld.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.6,1.4)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2015)") + theme(text=element_text(size=16)) + 
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g6<-ggplot(tmp.shade, aes(diversity, legume, z = yld.2016)) +geom_raster(aes(fill=yld.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-1.6,1.4)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2016)") + theme(text=element_text(size=16)) + 
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

ggpubr::ggarrange(g1,g2,g3,g4,g5,g6,ncol=3,nrow=2, common.legend=T)
ggsave(paste0(folder_names,ptemp,"/Yield.Influence.Landscape.ShadeManagement2.tiff"),height=9,width=12)

##############################################
#Figure of shock year yield
rst<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgaussloglink.csv"))

z.tmax<-attributes(scale(d.F.plot$tmax.anom.fruit))

tmax <- tmp1 %>% mutate(z.tmax.anom=(tmax.anom.fruit-z.tmax[[2]])/(2*z.tmax[[3]])) %>% 
  filter(year!=2014) %>% dplyr::select(year,z.tmax.anom) %>% group_by(year) %>% 
  summarise(tmax.anom=max(z.tmax.anom,na.rm=T))

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

g1<-ggplot(era5 %>% filter(Plot=="B13"&year>=1981),aes(Date,wd_anom,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [mm]") + ggtitle("Monthly Water Deficit") +
  ylim(-60,90)+
  theme(text=element_text(size=16),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  annotate("text",x=as.Date("2016-06-01"),y=85,label=paste0("Study\nPeriod"),size=5) +
  geom_vline(aes(xintercept=as.Date("2017-12-01")),linetype="dashed") 
#annotate("text",x=as.Date("2017-01-01"),y=90,label=paste0("Year 2"),angle=90)

g2<-ggplot(era5 %>% filter(Plot=="B13"&year>=1981),aes(Date,tmax_anom,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Anomalies [C]") + ggtitle("Monthly Maximum Temperature") +
  ylim(-5,5)+
  theme(text=element_text(size=16),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") +
  geom_vline(aes(xintercept=as.Date("2014-11-01")),linetype="dashed") +
  annotate("text",x=as.Date("2016-06-01"),y=4.5,label=paste0("Study\nPeriod"),size=5) +
  geom_vline(aes(xintercept=as.Date("2017-12-01")),linetype="dashed") 
# geom_segment(aes(x = as.Date("2015-01-01"), y = 2.15, xend = as.Date("2015-10-01"), yend = 2.15), size=0.5,arrow = arrow(length = unit(0.5, "cm")))

ggpubr::ggarrange(g1,g2,ncol=1,nrow=2,align="hv",heights=c(1.25,1),labels="auto", common.legend=T)
ggsave(paste0(folder_names,ptemp,"/Era5.Anom.Comparison.pdf"),height=10,width=12)


