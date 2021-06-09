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


library(arm)
library(car)
#library(MuMIn)
library(lattice)

#do a corrplot
tmp<-d.F.plot %>% dplyr::select(-Plot,-ID,-X,-kebele)
corrplot(tmp,color=TRUE,details=TRUE)

tmp1<-d.F.plot %>% filter(!is.na(Shrub.kg)&Shrub.kg>0&!is.na(dbh.mn)&!is.na(propCLR))

pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.allyrs.pdf"))
hist(tmp1$Shrub.kg)
dev.off()

tmp14<- tmp1 %>% filter(year==2014)

pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.2014.pdf"))
hist(tmp14$Shrub.kg, main="Histogram of 2014 Shrub Yields",xlab="Shrub Yield",cex.lab=2,cex.main=2)
dev.off()

tmp2<- tmp1 %>% filter(year!=2014)

pdf(paste0(getwd(),"/Analysis/ES/Hist.Shrub.kg.shockyrs.pdf"))
hist(tmp2$Shrub.kg, main="Histogram of Shock Year Shrub Yields",xlab="Shrub Yield",cex.lab=2,cex.main=2)
dev.off()


####Normal Year Analysis
#linear model
rm1<-lm(Shrub.kg~rescale(elevation)*rescale(patcharea) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
           rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i) + rescale(buffer) + rescale(dbh.mn) + rescale(tmax.anom.fruit) +
           rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB),data=tmp14)
summary(rm1)


#check multicollinearity
vif(rm1)

#linear model: 2014
#order of removal 1)  + rescale(propCBB), 2)  + rescale(C.pct), 3) rescale(elevation)*rescale(patcharea),
#4) + rescale(tmax.anom.fruit), 5) rescale(coffee.area.ha) + , 6) + rescale(dbh.mn) , 7)  + rescale(CN.ratio),
#8) + rescale(propCLR)
rm2<-lm(Shrub.kg~rescale(elevation) + rescale(patcharea) + rescale(buffer) + rescale(K.meq) +
          rescale(BA.legume)*rescale(Shannon.i)   +
          rescale(GapDry) + rescale(propCBD) ,data=tmp14)
summary(rm2)
vif(rm2)

#where does buffer exist (range of elevation and patch area?)
buff<- tmp14 %>% filter(buffer==1) %>% summarise(patch_min=min(patcharea,na.rm=T), patch_max=max(patcharea,na.rm=T),
                                          elev_min=min(elevation,na.rm=T), elev_max=max(elevation,na.rm=T))

#check heteroskedasticity
diagnos <- data.frame(Resid = resid(rm2, type = "pearson"), Fitted = fitted(rm2),Variable = tmp14$Plot[!is.na(tmp14$propCLR)],yield=tmp14$Shrub.kg[!is.na(tmp14$propCLR)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_ResidualvFittedValues_2014.pdf"),width=8,height=8)
ggplot(diagnos, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20)) 
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_qqplotResiduals_2014.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos, distribution = qnorm, main = list("QQ-Plot", cex = 2), 
       xlab = list(cex = 2), ylab = list(cex = 2), prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldlm_fittedvsobserved_2014.pdf"),width=8,height=8)
ggplot(diagnos, aes(yield,Fitted))+geom_point() +
  geom_abline(yintercept=0, slope=1, col="red", linetype="dashed") +xlab("Observed values")+ylab("Predicted Values") +
  ggtitle("Model Assessment")+theme_bw() + xlim(0,1.5) + ylim(0,1.5) + theme(text = element_text(size = 20)) 
dev.off()


#create figures of significant models
x.lm<-as.data.frame(summary(rm2)$coefficients)
colnames(x.lm)<-c("Coefficients","Std_error","t_value","p_value")
x.lm$Comparison<-rownames(x.lm)
rownames(x.lm)<-1:nrow(x.lm)

x.lm <- x.lm %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.lm$Labels <- c("Intercept","Elevation","Patch Area","Located in\nbuffer","Soil K","Basal Area of\nLeguminous Trees",
                    "Shade Diversity","Canopy Gap","Coffee Berry\nDisease",
                    "Shade Diversity:\nLeguminous Trees")
#add in labels for colors & shapes
x.lm$shades<-c("Other","Landscape","Landscape","Landscape","Soil","Shade Management","Shade Management",
                  "Shade Management","Disease","Shade Management")


#order by size of effect and Importance factor
x.lm <- x.lm %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.lm))

#write.csv(x.lm,paste0(getwd(),"/Analysis/ES/Finalmodel_yldlm.csv"))
x.lm<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldlm.csv"))

x.lm$Labels<-factor(x.lm$Labels,levels=x.lm[order(x.lm$Importance,decreasing=T),"Labels"])
x.lm$shades<-factor(x.lm$shades,levels=c("Landscape","Shade Management","Soil","Disease", "Other"),ordered=T)
x.lm$sig<-factor(x.lm$sig, levels=c(0,1),labels=c("Significant","NS"))

#remove intercept
x.lm<-x.lm %>% filter(Labels!="Intercept")
r.lm<-summary(rm2)

g1<-ggplot(x.lm, aes(x = Labels, y = Coefficients, ymin = Coefficients-Std_error, ymax = Coefficients+Std_error)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(color=factor(shades),shape=factor(sig))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Shrub Yield (kg)\n[2014]")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 20),axis.text.x=element_text(angle = 45,hjust=1),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=0.75,y=0.1,label=paste("R2 = ",signif(r.lm$r.squared, 3)),angle=0,size=4)

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldlm.tiff"),height=8,width=9)
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
ym1<-lmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(year) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
            rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(year) + rescale(buffer) + rescale(dbh.mn) +
            rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),data=tmp2)
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
ym2<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(year) + rescale(norm.yld) + rescale(CN.ratio) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(year) + rescale(buffer) + rescale(dbh.mn)  +
             rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),family="gaussian"(link='log'),data=tmp2)
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

#glm gamma loglink
ym3<-glmer(Shrub.kg~rescale(elevation)*rescale(patcharea)*rescale(year) + rescale(CN.ratio) + rescale(norm.yld) + rescale(C.pct) + rescale(K.meq) +
             rescale(coffee.area.ha) + rescale(BA.legume)*rescale(Shannon.i)*rescale(year) + rescale(buffer) + rescale(dbh.mn) +
             rescale(GapDry) + rescale(propCBD) + rescale(propCLR) + rescale(propCBB)+(1|Plot),family="Gamma"(link='log'),data=tmp2)
summary(ym3)
MuMIn::r.squaredGLMM(ym3)

#check multicollinearity
vif(ym3)

#check heteroskedasticity
diagnos3 <- data.frame(Resid = resid(ym3, type = "pearson"), Fitted = fitted(ym3),Variable = tmp2$Plot[!is.na(tmp2$norm.yld)] )
pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_ResidualvFittedValues_shock.v1.pdf"),width=8,height=8)
ggplot(diagnos3, aes(Fitted, Resid))+geom_point() +stat_smooth(method="loess")+
  geom_hline(yintercept=0, col="red", linetype="dashed") +xlab("Fitted values")+ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")+theme_bw()  + theme(text = element_text(size = 20))

dev.off()

pdf(paste0(getwd(),"/Analysis/ES/Plot.yldglmgammaloglink_qqplotResiduals_all.v1.pdf"),width=8,height=8)
qqmath(~Resid, data = diagnos3, distribution = qnorm,  main = list("QQ-Plot", cex = 2), 
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


#create figures of significant models
x.gamma<-as.data.frame(summary(ym4)$coefficients)
colnames(x.gamma)<-c("Coefficients","Std_error","t_value","p_value")
x.gamma$Comparison<-rownames(x.gamma)
rownames(x.gamma)<-1:nrow(x.gamma)

x.gamma <- x.gamma %>% mutate(p_value=replace(p_value,p_value>0.05,"NS")) %>% 
  mutate(sig=0) %>% mutate(sig=replace(sig,p_value=="NS",1))
x.gamma$Labels <- c("Intercept","Elevation","Year","Patch Area","Yield in\nNormal Year","Soil\nCarbon Content",
                    "Basal Area of\nLeguminous Trees","Shade Diversity","Coffee\nBerry Borer","Elevation:Year",
                    "Patch\nArea:Year","Leguminous\nTrees:Year","Shade\nDiversity:Year")
#add in labels for colors & shapes
x.gamma$shades<-c("Other","Landscape","Other","Landscape","Other","Soil","Shade Management","Shade Management","Disease",
                  "Landscape","Landscape","Shade Management","Shade Management")

x.gamma$shapes<-c("Fixed","Fixed","Year","Fixed","Fixed","Fixed","Fixed","Fixed","Year","Year","Year","Year","Year")

#order by size of effect and Importance factor
x.gamma <- x.gamma %>% arrange(desc(abs(t_value))) %>% 
  mutate(Importance=1:nrow(x.gamma))

#write.csv(x.gamma,paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink.csv"))
x.gamma<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink.csv"))

x.gamma$Labels<-factor(x.gamma$Labels,levels=x.gamma[order(x.gamma$Importance,decreasing=T),"Labels"])
x.gamma$shades<-factor(x.gamma$shades,levels=c("Landscape","Shade Management","Soil","Disease",
                                               "Other"),ordered=T)

#remove intercept
x.gamma<-x.gamma %>% filter(Labels!="Intercept")

g1<-ggplot(x.gamma, aes(x = Labels, y = Coefficients, ymin = Coefficients-Std_error, ymax = Coefficients+Std_error)) + 
  geom_errorbar(width=0.2,aes(color=factor(shades))) + 
  geom_point(size=5,aes(shape=factor(shapes),color=factor(shades))) + scale_color_viridis_d() +
  scale_shape_manual(values = c(15, 16)) +
  theme(text = element_text(size=16),axis.text.x = element_text(angle=90, vjust=1)) +ggtitle("Influence on Shrub Yield (kg)\n[Shock Years]")+
  xlab("Variable [ranked by significance]")+ylab("Effect Size") + geom_hline(yintercept = 0, linetype="dashed")+theme_classic() +
  theme(text = element_text(size = 16),axis.text.x=element_text(angle = 45,hjust=1),plot.title = element_text(hjust = 0.5),legend.position="right",legend.title=element_blank()) +
  annotate("text",x=1,y=-0.75,label=paste("R2 = ",signif(r.gamma[1,1], 3)),angle=0,size=5) + theme(text = element_text(size = 20))

g1+coord_flip()
ggsave(paste0(getwd(),"/Analysis/ES/Finalmodel_results_yldglmgammaloglink.tiff"),height=8,width=9)


#####################################################################
######Create 2D figures of landscape and shade management interactions
library(paletteer)
library(tidyverse)
library(lme4)
library(arm)

d.F.plot <- read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset.csv"))

tmp1<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldlm.csv"))
tmp2<-read.csv(paste0(getwd(),"/Analysis/ES/Finalmodel_yldglmgammaloglink.csv"))

#variables of interest
elev<-tibble(elev=seq(as.integer(min(d.F.plot$elevation)),as.integer(max(d.F.plot$elevation)),by=1))
patch<-tibble(patch=seq(as.integer(min(d.F.plot$patcharea)),as.integer(max(d.F.plot$patcharea)),by=5))
z.elev<-attributes(scale(d.F.plot$elevation))
z.patch<-attributes(scale(d.F.plot$patcharea))

z.year<-attributes(scale(d.F.plot$year))
z.buffer<-attributes(scale(d.F.plot$buffer))

year<- tibble(y2=2015,y3=2016) %>% mutate(y2=(2015-z.year[[2]])/(2*z.year[[3]]), y3=(2016-z.year[[2]])/(2*z.year[[3]]))


#where does buffer exist (range of elevation and patch area?)
buff<- d.F.plot %>% filter(buffer==1) %>% summarise(patch_min=min(patcharea,na.rm=T), patch_max=max(patcharea,na.rm=T),
                                                 elev_min=min(elevation,na.rm=T), elev_max=max(elevation,na.rm=T)) %>% 
  mutate(patch_min = (patch_min-z.patch[[2]])/(2*z.patch[[3]]), patch_max =(patch_max-z.patch[[2]])/(2*z.patch[[3]]),
         elev_min = (elev_min-z.elev[[2]])/(2*z.elev[[3]]), elev_max = (elev_max-z.elev[[2]])/(2*z.elev[[3]]) )


#newdata
elev <- elev %>% mutate(elevation=(elev-z.elev[[2]])/(2*z.elev[[3]]))
patch <- patch %>% mutate(patcharea=(patch-z.patch[[2]])/(2*z.patch[[3]]))

tmp.land<-merge(elev,patch)
#tmp14<- tmp14 %>% mutate(year=factor(2014),BA.legume=0,Shannon.i=0,propCBB=0,Plot=NA)

tmp.land <- tmp.land %>% mutate(yld.2014=tmp1[tmp1$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp1[tmp1$Comparison=="rescale(patcharea)","Coefficients"]*patcharea,
                                yld.2014b=tmp1[tmp1$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp1[tmp1$Comparison=="rescale(patcharea)","Coefficients"]*patcharea,
                                yld.2015 = tmp2[tmp2$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp2[tmp2$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  tmp2[tmp2$Comparison=="rescale(year):rescale(patcharea)","Coefficients"]*patcharea*as.numeric(year$y2)+
                                  tmp2[tmp2$Comparison=="rescale(elevation):rescale(year)","Coefficients"]*elevation*as.numeric(year$y2),
                                yld.2016 = tmp2[tmp2$Comparison=="rescale(elevation)","Coefficients"]*elevation + 
                                  tmp2[tmp2$Comparison=="rescale(patcharea)","Coefficients"]*patcharea +
                                  tmp2[tmp2$Comparison=="rescale(year):rescale(patcharea)","Coefficients"]*patcharea*as.numeric(year$y3)+
                                  tmp2[tmp2$Comparison=="rescale(elevation):rescale(year)","Coefficients"]*elevation*as.numeric(year$y3)) %>% 
  mutate(yld.2014b=replace(yld.2014,elevation>=buff$elev_min&elevation<=buff$elev_max&patcharea>=buff$patch_min&patcharea<=buff$patch_max,
                           yld.2014[elevation>=buff$elev_min&elevation<=buff$elev_max&patcharea>=buff$patch_min&patcharea<=buff$patch_max]+tmp1[tmp1$Comparison=="rescale(buffer)","Coefficients"]*(1-z.buffer[[2]])/(2*z.buffer[[3]])))


g1<-ggplot(tmp.land, aes(patch, elev, z = yld.2014)) +geom_raster(aes(fill=yld.2014)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.9,1.05)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2014)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) +
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g2<-ggplot(tmp.land, aes(patch, elev, z = yld.2015)) +geom_raster(aes(fill=yld.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.9,1.05)) +
  theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Yield") + ggtitle("Influence of Landscape\nFeatures on Shrub Yield\n(2015)") + 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16))+ guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g3<-ggplot(tmp.land, aes(patch, elev, z = yld.2016)) +geom_raster(aes(fill=yld.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.9,1.05)) + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
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
tmp.shade <- tmp.shade %>% mutate(yld.2014 = tmp1[tmp1$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    tmp1[tmp1$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    tmp1[tmp1$Comparison=="rescale(BA.legume):rescale(Shannon.i)","Coefficients"]*Shannon.i*BA.legume,
                                  yld.2015 = tmp2[tmp2$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    tmp2[tmp2$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    tmp2[tmp2$Comparison=="rescale(year):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(year$y2)+
                                    tmp2[tmp2$Comparison=="rescale(year):rescale(BA.legume)","Coefficients"]*BA.legume*as.numeric(year$y2),
                                  yld.2016 =  tmp2[tmp2$Comparison=="rescale(BA.legume)","Coefficients"]*BA.legume +
                                    tmp2[tmp2$Comparison=="rescale(Shannon.i)","Coefficients"]*Shannon.i +
                                    tmp2[tmp2$Comparison=="rescale(year):rescale(Shannon.i)","Coefficients"]*Shannon.i*as.numeric(year$y3)+
                                    tmp2[tmp2$Comparison=="rescale(year):rescale(BA.legume)","Coefficients"]*BA.legume*as.numeric(year$y3))
max(tmp.land$yld.2014, tmp.land$yld.2014b,tmp.land$yld.2015,tmp.land$yld.2016,tmp.shade$yld.2014,
    tmp.shade$yld.2014b,tmp.shade$yld.2015,tmp.shade$yld.2016)
min(tmp.land$yld.2014, tmp.land$yld.2014b,tmp.land$yld.2015,tmp.land$yld.2016,tmp.shade$yld.2014,
    tmp.shade$yld.2014b,tmp.shade$yld.2015,tmp.shade$yld.2016)



g4<-ggplot(tmp.shade, aes(diversity, legume,  z = yld.2014)) +geom_raster(aes(fill=yld.2014)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.9,1.05)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2014)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) +
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g5<-ggplot(tmp.shade, aes(diversity, legume, z = yld.2015)) +geom_raster(aes(fill=yld.2015)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.9,1.05)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2015)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) + 
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

g6<-ggplot(tmp.shade, aes(diversity, legume, z = yld.2016)) +geom_raster(aes(fill=yld.2016)) +
  scale_fill_paletteer_c("scico::roma",direction = 1, limits=c(-0.9,1.05)) + theme_classic() + xlab("Shannon Index") + ylab("Basal Area\nLeguminous Trees [m2]")+
  labs(fill="Yield") + ggtitle("Influence of Shade\nTrees on Shrub Yield\n(2016)") + theme(plot.title = element_text(hjust = 0.5),text=element_text(size=16)) + 
  guides(fill = guide_colourbar(label.theme = element_text(angle = 45)))

ggpubr::ggarrange(g1,g2,g3,g4,g5,g6,ncol=3,nrow=2, common.legend=T)
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


