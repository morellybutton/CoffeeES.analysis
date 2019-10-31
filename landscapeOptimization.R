#landscape optimisation of 10x10 pixel grid
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

betas <- d.F.new %>% filter(year==2014) %>% mutate(shannon.i=arm::rescale(Shannon.i),ba.legume=arm::rescale(BA.legume),
                                                   patch=arm::rescale(patcharea)) %>% 
  select(shannon.i,ba.legume,patch) %>% summarise_all(c("max","min"))
div.vals<-d.F.new %>% filter(year==2014) %>% summarise(mean=mean(Shannon.i,na.rm=T),sd=sd(Shannon.i,na.rm=T)) %>% 
  select(mean,sd)
patch.vals<-d.F.new %>% filter(year==2014) %>% summarise(mean=mean(patcharea,na.rm=T),sd=sd(patcharea,na.rm=T)) %>% 
  select(mean,sd)
legume.vals<-d.F.new %>% filter(year==2014) %>% summarise(mean=mean(BA.legume,na.rm=T),sd=sd(BA.legume,na.rm=T)) %>% 
  select(mean,sd)

#include max ba and median coffee density
#max.ba <- betas %>% pull(ba.legume_max) 
m_density<-d.F.new %>% filter(year==2014)  %>% summarise(density=median(density,na.rm=T)) %>% pull(density)

#for 2014 yield
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model_yld14.delta2.v4.confint.csv"))

#equation for normal year
int14 <-tmp.14 %>% filter(Comparison=="(Intercept)") %>% select(Coefficients) %>% as.numeric()
shan.i14 <- tmp.14 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Coefficients) %>% as.numeric()
ba.l14 <- tmp.14 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Coefficients) %>% as.numeric()
int.sb14 <- tmp.14 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Coefficients) %>% as.numeric()
p.tch14 <- tmp.14 %>% filter(Comparison=="rescale(patcharea)") %>% select(Coefficients) %>% as.numeric()
elev14 <- tmp.14 %>% filter(Comparison=="rescale(elevation)") %>% select(Coefficients) %>% as.numeric()

yld_norm <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  x4 <- x[4] #basal area legume
  int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3
}  

#int14.l <-tmp.14 %>% filter(X=="(Intercept)") %>% select(Lower.CL) %>% as.numeric()
#shan.i14.l <- tmp.14 %>% filter(X=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
#ba.l14.l <- tmp.14 %>% filter(X=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
#int.sb14.l <- tmp.14 %>% filter(X=="rescale(BA.legume):rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
#p.tch14.l <- tmp.14 %>% filter(X=="rescale(patcharea)") %>% select(Lower.CL) %>% as.numeric()
#elev14.l <- tmp.14 %>% filter(X=="rescale(elevation)") %>% select(Lower.CL) %>% as.numeric()

#yld_norm_lower <- function(x) {
#  x1 <- x[1] #elevation
#  x2 <- x[2] #shade diversity
#  x3 <- x[3] #patcharea
#  int14.l + elev14.l*x1 + shan.i14.l*x2 + ba.l14.l*max.ba + int.sb14.l*max.ba*x2 + p.tch14.l*x3
#}  

#int14.u <-tmp.14 %>% filter(X=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()
#shan.i14.u <- tmp.14 %>% filter(X=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
#ba.l14.u <- tmp.14 %>% filter(X=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
#int.sb14.u <- tmp.14 %>% filter(X=="rescale(BA.legume):rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
#p.tch14.u <- tmp.14 %>% filter(X=="rescale(patcharea)") %>% select(Upper.CL) %>% as.numeric()
#elev14.u <- tmp.14 %>% filter(X=="rescale(elevation)") %>% select(Upper.CL) %>% as.numeric()

#yld_norm_upper <- function(x) {
#  x1 <- x[1] #elevation
#  x2 <- x[2] #shade diversity
#  x3 <- x[3] #patcharea
#  int14.u + elev14.u*x1 + shan.i14.u*x2 + ba.l14.u*max.ba + int.sb14.u*max.ba*x2 + p.tch14.u*x3
#}  

#for farm resilience
tmp.rm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resilience.delta2.confint.csv"))

#equation for resilience
int.rm <-tmp.rm %>% filter(Comparison=="(Intercept)") %>% select(full) %>% as.numeric()
shan.rm <- tmp.rm %>% filter(Comparison=="rescale(Shannon.i)") %>% select(full) %>% as.numeric()
ba.rm <- tmp.rm %>% filter(Comparison=="rescale(BA.legume)") %>% select(full) %>% as.numeric()
int.sb.rm <- tmp.rm %>% filter(Comparison=="rescale(BA.legume):rescale(Shannon.i)") %>% select(full) %>% as.numeric()
p.tch.rm <- tmp.rm %>% filter(Comparison=="rescale(patcharea)") %>% select(full) %>% as.numeric()
elev.rm <- tmp.rm %>% filter(Comparison=="rescale(elevation)") %>% select(full) %>% as.numeric()

yld_resilience <- function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  x4 <- x[4] #basal area legume
  exp(int.rm + elev.rm*x1 + shan.rm*x2 + ba.rm*x4 + int.sb.rm*x4*x2 + p.tch.rm*x3)
}

#for 2015 yield difference
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v4.confint.csv"))


#for farm resistance
tmp.sm<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_resistance15.delta2.confint.csv"))

#equation for resilience
int.sm <-tmp.sm %>% filter(Comparison=="(Intercept)") %>% select(full) %>% as.numeric()
shan.sm <- tmp.sm %>% filter(Comparison=="rescale(Shannon.i)") %>% select(full) %>% as.numeric()
ba.sm <- tmp.sm %>% filter(Comparison=="rescale(BA.legume)") %>% select(full) %>% as.numeric()
#int.sb.sm <- tmp.sm %>% filter(Comparison=="rescale(BA.legume):rescale(Shannon.i)") %>% select(full) %>% as.numeric()
p.tch.sm <- tmp.sm %>% filter(Comparison=="rescale(patcharea)") %>% select(full) %>% as.numeric()
elev.sm <- tmp.sm %>% filter(Comparison=="rescale(elevation)") %>% select(full) %>% as.numeric()

yld_resistance <- function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  x4 <- x[4] #basal area legume
  exp(int.sm + elev.sm*x1 + shan.sm*x2 + ba.sm*x4 + p.tch.sm*x3)
}

#for 2015 yield difference
#tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v4.confint.csv"))
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld15.delta2.v2.confint.csv"))

#equation for hot year
int15 <-tmp.15 %>% filter(X=="(Intercept)") %>% select(full) %>% as.numeric()
shan.i15 <- tmp.15 %>% filter(X=="rescale(Shannon.i)") %>% select(full) %>% as.numeric()
ba.l15 <- tmp.15 %>% filter(X=="rescale(BA.legume)") %>% select(full) %>% as.numeric()
#p.tch15 <- tmp.15 %>% filter(X=="rescale(patcharea)") %>% select(full) %>% as.numeric()
elev15 <- tmp.15 %>% filter(X=="rescale(elevation)") %>% select(full) %>% as.numeric()
int.sb15 <- tmp.15%>% filter(Comparison=="rescale(BA.legume):rescale(Shannon.i)") %>% select(full) %>% as.numeric()

yld_hot <- function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  x4 <- x[4] #basal area legume
  (int15 + elev15*x1 + shan.i15*x2 + ba.l15*x4 + int.sb15*x4*x2 + 0*x3)
}

#equation for hot year
#int15.l <-tmp.15 %>% filter(X=="(Intercept)") %>% select(Lower.CL) %>% as.numeric()
#shan.i15.l <- tmp.15 %>% filter(X=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
#ba.l15.l <- tmp.15 %>% filter(X=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
#p.tch15.l <- tmp.15 %>% filter(X=="rescale(patcharea)") %>% select(Lower.CL) %>% as.numeric()

#yld_hot_lower <- function(x){
#  x1 <- x[1] #elevation
#  x2 <- x[2] #shade diversity
#  x3 <- x[3] #patcharea
#  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int15.l + shan.i15.l*x2 + ba.l15.l*max.ba +  p.tch15.l*x3)
#}

#int15.u <-tmp.15 %>% filter(X=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()
#shan.i15.u <- tmp.15 %>% filter(X=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
#ba.l15.u <- tmp.15 %>% filter(X=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
#p.tch15.u <- tmp.15 %>% filter(X=="rescale(patcharea)") %>% select(Upper.CL) %>% as.numeric()

#yld_hot_upper <- function(x){
#  x1 <- x[1] #elevation
#  x2 <- x[2] #shade diversity
#  x3 <- x[3] #patcharea
#  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int15.u + shan.i15.u*x2 + ba.l15.u*max.ba +  p.tch15.u*x3)
#}

#for 2016 yield difference
#tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model_logylddiff16.delta2.v4.confint.csv"))
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld16.delta2.v1.confint.csv"))

#equation for dry year
int16 <- tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(full) %>% as.numeric()
shan.i16 <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(full) %>% as.numeric()
#ba.l16 <- tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(full) %>% as.numeric()
#int.sb16 <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(full) %>% as.numeric()
elev16 <- tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(full) %>% as.numeric()
int.ep16 <- tmp.16 %>% filter(Comparison=="rescale(elevation):rescale(patcharea)") %>% select(full) %>% as.numeric()
p.tch16 <- tmp.16 %>% filter(X=="rescale(patcharea)") %>% select(full) %>% as.numeric()

yld_dry <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  x4 <- x[4] #basal area legume
  exp(int16 + elev16*x1 + shan.i16*x2 + 0*x4 + p.tch16*x3 + int.ep16*x1*x3) 
}

#int16.l <- tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Lower.CL) %>% as.numeric()
#shan.i16.l <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
#ba.l16.l <- tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
#int.sb16.l <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
#elev16.l <- tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Lower.CL) %>% as.numeric()

#yld_dry_lower <- function(x) {
#  x1 <- x[1] #elevation
#  x2 <- x[2] #shade diversity
#  x3 <- x[3] #patcharea
#  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int16.l + elev16.l*x1 + shan.i16.l*x2 + ba.l16.l*max.ba + int.sb16.l*max.ba*x2) 
#}

#int16.u <- tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()
#shan.i16.u <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
#ba.l16.u <- tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
#int.sb16.u <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
#elev16.u <- tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Upper.CL) %>% as.numeric()

#yld_dry_upper <- function(x) {
#  x1 <- x[1] #elevation
#  x2 <- x[2] #shade diversity
#  x3 <- x[3] #patcharea
#  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int16.u + elev16.u*x1 + shan.i16.u*x2 + ba.l16.u*max.ba + int.sb16.u*max.ba*x2) 
#}

#equation to minimize variability between all years (take mean of difference between normal and shock years)
#var_min <-function(x){
  #x1 <- x[1] #elevation
  #x2 <- x[2] #shade diversity
  #x3 <- x[3] #patcharea
  #x4 <- x[4] #basal area legume
  #0.5*sum(int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3 - ((int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3)*exp(int16 + elev16*x1 + shan.i16*x2 + ba.l16*x4 + int.sb16*x4*x2)),
  #        int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3 - ((int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3)*exp(int15 + shan.i15*x2 + ba.l15*x4 +  p.tch15*x3)))
#}

#equation to maximise inter-annual yields for all years (take sum of all years)
#var_max <-function(x){
  #x1 <- x[1] #elevation
  #x3 <- x[3] #patcharea
  #x2 <- x[2] #shade diversity
  #x4 <- x[4] #basal area legume
  #sum(int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3,((int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3)*exp(int16 + elev16*x1 + shan.i16*x2 + ba.l16*x4 + int.sb16*x4*x2)),
  #        ((int14 + elev14*x1 + shan.i14*x2 + ba.l14*x4 + int.sb14*x4*x2 + p.tch14*x3)*exp(int15 + shan.i15*x2 + ba.l15*x4 +  p.tch15*x3)))
#}

#create grid of set elevations
elev.min<-d.F.new %>% select(elevation) %>% min()
elev.max<-d.F.new %>% select(elevation) %>% max()
set.seed(50)

#create elevation windows up the mountainside
steps<-abs(rlogis(10,location=0,scale=1)) %>% sort()
#plot(steps)
steps1<-steps/max(steps)
elev.diff<-elev.max-elev.min
steps2<-steps1*elev.diff
#elev.steps<-seq(from=elev.min,to=elev.max,by=(elev.max-elev.min)/10)
elev.steps<-steps2+elev.min

row1<-runif(10, min = elev.min, max = elev.steps[1])
row2<-runif(10, min = elev.steps[1], max = elev.steps[2])
row3<-runif(10, min = elev.steps[2], max = elev.steps[3])
row4<-runif(10, min = elev.steps[3], max = elev.steps[4])
row5<-runif(10, min = elev.steps[4], max = elev.steps[5])
row6<-runif(10, min = elev.steps[5], max = elev.steps[6])
row7<-runif(10, min = elev.steps[6], max = elev.steps[7])
row8<-runif(10, min = elev.steps[7], max = elev.steps[8])
row9<-runif(10, min = elev.steps[8], max = elev.steps[9])
row10<-runif(10, min = elev.steps[9], max = elev.max)

combo<-rbind(row10,row9)
rm(row10,row9)
combo<-rbind(combo,row8)
rm(row8)
combo<-rbind(combo,row7)
rm(row7)
combo<-rbind(combo,row6)
rm(row6)
combo<-rbind(combo,row5)
rm(row5)
combo<-rbind(combo,row4)
rm(row4)
combo<-rbind(combo,row3)
rm(row3)
combo<-rbind(combo,row2)
rm(row2)
combo<-rbind(combo,row1)
rm(row1)
combo<-data.frame(combo)

combo$y<-c(10,9,8,7,6,5,4,3,2,1)
z_combo<-gather(combo,key="index",value="elevation",-y)
z_combo$x<-as.numeric(str_split_fixed(z_combo$index,"X",2)[,2])

g1<-ggplot(z_combo, aes( x,y, z = elevation)) +geom_raster(aes(fill=elevation)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c()+ theme_classic() +
  labs(fill="Elevation (m)") + ggtitle("Theoretical Landscape\n(10 x 10 grid)") + 
  theme(text=element_text(size=16),legend.position="top")

#show elevational climb
y_combo <- z_combo %>% group_by(y) %>% summarise(m_elev=mean(elevation))
g2<-ggplot(y_combo,aes(m_elev,y)) + geom_line() + theme_classic() + xlab("Elevation [m]") +
  ylab("") + theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
                   text=element_text(size=16)) + ggtitle("Profile of Landscape")

ggpubr::ggarrange(g1,g2,ncol=2,nrow=1,align="hv",common.legend=T,legend="right")
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/TheoreticalLandscape.pdf",height=5,width=11)

#calculate rescaled values
elev.rescale<-d.F.new %>% filter(year==2014)  %>% mutate(elev.rescale=arm::rescale(elevation),mean=mean(elevation,na.rm=T),sd=sd(elevation,na.rm=T)) %>% 
  select(elevation,elev.rescale,mean,sd) %>% 
  mutate(test=(elevation-mean)/2/sd)
elev.vals<-elev.rescale %>% select(mean,sd) %>% summarise_all("mean")

z_combo <- z_combo %>% mutate(elev.rescale=(elevation-elev.vals$mean)/2/elev.vals$sd)



#though how does density vary by elevation?
#summary(lm(density~elevation,data=d.F.new %>% filter(year==2014)))
#ggplot(d.F.new %>% filter(year==2014) ,aes(elevation,density) ) + geom_point() + stat_smooth(method="lm") + theme_classic() +
#  geom_hline(yintercept=m_density,linetype="dashed",color="grey") + theme(text=element_text(size=16)) +
#  xlab("Elevation [m]") + ylab("Coffee Density [shrubs/ha]") + ggtitle("Variation in Coffee Shrub Density")
#ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/DensityvsElevation.pdf",height=4,width=5)

#take mean density per elevation tranch (<y_combo[4] = 1, >y_combo[3]&<y_combo[8] = 2, >y_combo[8] = 3)
#c_density <- d.F.new %>% filter(year==2014) %>% select(elevation,density) %>% 
#  mutate(elev.group=2) %>% mutate(elev.group=replace(elev.group,elevation<=as.numeric(y_combo[4,2]),1),
#                                  elev.group=replace(elev.group,elevation>as.numeric(y_combo[8,2]),3)) %>% 
#  group_by(elev.group) %>% summarise(m_density=median(density,na.rm=T))

betas.sh <- seq(to=betas %>% select(shannon.i_max) %>% as.numeric(),
                from=betas %>% select(shannon.i_min) %>% as.numeric(),
                by=(betas %>% select(shannon.i_max) %>% as.numeric()-betas %>% select(shannon.i_min) %>% as.numeric())/100)
betas.pt <- seq(to=betas %>% select(patch_max) %>% as.numeric(),
                from=betas %>% select(patch_min) %>% as.numeric(),
                by=(betas %>% select(patch_max) %>% as.numeric()-betas %>% select(patch_min) %>% as.numeric())/100)
betas.ba <- seq(to=betas %>% select(ba.legume_max) %>% as.numeric(),
                from=betas %>% select(ba.legume_min) %>% as.numeric(),
                by=(betas %>% select(ba.legume_max) %>% as.numeric()-betas %>% select(ba.legume_min) %>% as.numeric())/100)

betas.sh1 <- rep(betas.sh, each = 101*101)
betas.pt1 <- rep(betas.pt, times = 101*101)
betas.ba1 <- rep(betas.ba, each = 101,times=101)

#practice on pixel [1,1] in the landscape
betas.elev<-z_combo %>% filter(y==1&x==1) %>% pull(elev.rescale)

yld<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_norm)
yld.hot<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_hot)
yld.dry<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_dry)
yld.rm<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_resilience)
yld.sm<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_resistance)

#yld <- matrix(yld, 11) 
#contour(betas.pt, betas.sh,  yld)
f1_1<-tibble(yld,yld.hot,yld.dry,yld.rm,yld.sm,betas.elev,betas.sh1,betas.pt1,betas.ba1)
#extract conditions for maximum value and identify block being represented
max.yld <- f1_1 %>% filter(yld==max(yld)) %>% rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1,z.ba.legume=betas.ba1) %>% 
  mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,
         ba.legume=z.ba.legume*2*legume.vals$sd+legume.vals$mean,x=1,y=1)

#extract conditions for maximum resilience
max.rm <- f1_1 %>% filter(yld>0&yld.hot>0&yld.dry>0) %>% filter(yld.rm==max(yld.rm)) %>% 
  rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1,z.ba.legume=betas.ba1) %>% 
  mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,
         ba.legume=z.ba.legume*2*legume.vals$sd+legume.vals$mean,x=1,y=1)

#extract conditions for maximum resistance
max.sm <- f1_1 %>% filter(yld>0&yld.hot>0&yld.dry>0) %>% filter(yld.sm==max(yld.sm)) %>% 
  rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1,z.ba.legume=betas.ba1) %>% 
  mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,
         ba.legume=z.ba.legume*2*legume.vals$sd+legume.vals$mean,x=1,y=1)

#f1_1 <- f1_1 %>% group_by(betas.elev,betas.sh1,betas.pt1,betas.ba1) %>% 
#  mutate(var.min=0.5*sum(yld-yld.hot,yld-yld.dry),var.max=sum(yld,yld.hot,yld.dry)) %>% 
#  mutate(var.diff=var.min/var.max) %>% ungroup()
#var.min<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, var_min)
#var.max<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, var_max)
#yld.hot<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_hot)
#yld.dry<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_dry)
#min.var<-min.var %>% arrange(var.min,var.max)
#min.var$id <- 1:nrow(min.var)
#ggplot(min.var,aes(var.min,var.max)) + geom_line() + geom_point(aes(var.min,var.diff2),color="red")
#output1<-tibble(var.min,var.max,yld,yld.hot,yld.dry,betas.elev,betas.sh1,betas.pt1,betas.ba1)
#min.var <- f1_1 %>% filter(var.min>0,var.max>0,var.diff>0,yld>0,yld.hot>0,yld.dry>0) %>% 
#  filter(var.min==min(var.min)) %>% 
 # rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
  # mutate(yld=yld_norm(cbind(z.elevation,z.diversity,z.patcharea)),yld.dry=yld_dry(cbind(z.elevation,z.diversity,z.patcharea)),yld.hot=yld_hot(cbind(z.elevation,z.diversity,z.patcharea))) %>% 
 # mutate(yld.ha=yld*m_density,
#         yld.hot.ha=yld.hot*m_density,yld.dry.ha=yld.dry*m_density,
#         elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
#         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
 #        patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=1,y=1) %>% 
#  filter(diversity==max(diversity)) %>% filter(patcharea==max(patcharea)) 

#mid.var <- f1_1 %>% filter(var.min>0,var.max>0,var.diff>0,yld>0,yld.hot>0,yld.dry>0) %>% 
#  filter(var.diff<var.min) %>% filter(var.max==max(var.max))  %>% 
#  rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
  # mutate(yld=yld_norm(cbind(z.elevation,z.diversity,z.patcharea)),yld.dry=yld_dry(cbind(z.elevation,z.diversity,z.patcharea)),yld.hot=yld_hot(cbind(z.elevation,z.diversity,z.patcharea))) %>% 
 # mutate(yld.ha=yld*m_density,
#         yld.hot.ha=yld.hot*m_density,yld.dry.ha=yld.dry*m_density,
#         elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
#         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
#         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=1,y=1)
 

results_max<-tibble()
#results_var<-tibble()
#results_mid<-tibble()
results_rm<-tibble()
results_sm<-tibble()
#c.dens<-m_density
for(i in 1:10){
  for(j in 1:10) {
    #identify elevation group/density
    elevation<-z_combo %>% filter(y==j&x==i) %>% pull(elevation)
    if(elevation>y_combo[8,2]) {
      elev.g=3 } else if(elevation<=y_combo[4,2]) {
        elev.g=1} else {
          elev.g=2}
    #c.dens<-c_density %>% filter(elev.group==elev.g) %>% pull(m_density)
    
    betas.elev<-z_combo %>% filter(y==j&x==i) %>% pull(elev.rescale)
       
    yld<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_norm)
    yld.hot<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_hot)
    yld.dry<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_dry)
    yld.rm<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_resilience)
    yld.sm<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, yld_resistance)
    
    output<-tibble(yld,yld.hot,yld.dry,yld.rm,yld.sm,betas.elev,betas.sh1,betas.pt1,betas.ba1)
    #extract conditions for maximum value 
    max.yld <- output %>% filter(yld==max(yld)) %>% rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1,z.ba.legume=betas.ba1) %>% 
      mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
             diversity=z.diversity*2*div.vals$sd+div.vals$mean,
             patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,
             ba.legume=z.ba.legume*2*legume.vals$sd+legume.vals$mean,x=i,y=j)
    #max.yld$yld.hot<-yld_hot(cbind(max.yld$z.elevation,max.yld$z.diversity,max.yld$z.patcharea,max.yld$z.ba.legume))
    #max.yld$yld.dry<-yld_dry(cbind(max.yld$z.elevation,max.yld$z.diversity,max.yld$z.patcharea,max.yld$z.ba.legume))
    max.yld<-max.yld %>% mutate(yld.hot.ha=yld.hot*m_density,yld.dry.ha=yld.dry*m_density)
    results_max<-bind_rows(results_max,max.yld)
  rm(max.yld)
  #extract conditions for minimium variability value (minimize difference and not negative)
  #for hot year
  #var.hot<-apply(cbind(betas.elev,betas.sh1,betas.pt1), 1, var_hot)
  #output<-tibble(var.hot,betas.elev,betas.sh1,betas.pt1)
  #min.hot <- output %>% filter(var.hot>0) %>% filter(var.hot==min(var.hot)) %>% rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
  #  mutate(yld=yld_norm(cbind(z.elevation,z.diversity,z.patcharea)),yld.dry=yld_dry(cbind(z.elevation,z.diversity,z.patcharea)),yld.hot=yld_hot(cbind(z.elevation,z.diversity,z.patcharea))) %>% 
  #  mutate(yld.ha=yld*c.dens,
  #    yld.hot.ha=yld.hot*c.dens,yld.dry.ha=yld.dry*c.dens,
  #         elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
  #         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
  #         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=i,y=j)
  #var.min<-apply(cbind(betas.elev,betas.sh1,betas.pt1,betas.ba1), 1, var_min)
   
  max.rm <- output %>% filter(yld>0&yld.hot>0&yld.dry>0) %>% filter(yld.rm==max(yld.rm)) %>% 
    rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1,z.ba.legume=betas.ba1) %>% 
    mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
           diversity=z.diversity*2*div.vals$sd+div.vals$mean,
           patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,
           ba.legume=z.ba.legume*2*legume.vals$sd+legume.vals$mean,x=i,y=j)
  results_rm<-bind_rows(results_rm,max.rm)
  rm(max.rm)
  
  #output <- output %>% group_by(betas.elev,betas.sh1,betas.pt1) %>% 
  #  mutate(var.min=0.5*sum(yld-yld.hot,yld-yld.dry),var.max=sum(yld,yld.hot,yld.dry)) %>% 
  #  mutate(var.diff=var.min/var.max) %>% ungroup()
  #min.var <- output %>% filter(var.min>0,yld>0,yld.hot>0.5,yld.dry>0) %>% 
    #filter(var.diff<var.min) %>% 
  #  filter(var.min==min(var.min)) %>% 
  #  rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
   # mutate(yld=yld_norm(cbind(z.elevation,z.diversity,z.patcharea)),yld.dry=yld_dry(cbind(z.elevation,z.diversity,z.patcharea)),yld.hot=yld_hot(cbind(z.elevation,z.diversity,z.patcharea))) %>% 
  #  mutate(yld.ha=yld*m_density,
  #         yld.hot.ha=yld.hot*m_density,yld.dry.ha=yld.dry*m_density,
   #        elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
   #        diversity=z.diversity*2*div.vals$sd+div.vals$mean,
  #         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=i,y=j)
  #results_var<-bind_rows(results_var,min.var)
  #rm(min.var)
  
  max.sm <- output %>% filter(yld>0&yld.hot>0&yld.dry>0) %>% filter(yld.sm==max(yld.sm)) %>% 
    rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1,z.ba.legume=betas.ba1) %>% 
    mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
           diversity=z.diversity*2*div.vals$sd+div.vals$mean,
           patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,
           ba.legume=z.ba.legume*2*legume.vals$sd+legume.vals$mean,x=i,y=j)
  
  results_sm<-bind_rows(results_sm,max.sm)
  
  #mid.var <- output %>% filter(var.min>0,yld>0,yld.hot>0.5,yld.dry>0) %>% 
  #  filter(var.diff<var.min) %>% filter(var.max==max(var.max)) %>% 
  #  rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
    # mutate(yld=yld_norm(cbind(z.elevation,z.diversity,z.patcharea)),yld.dry=yld_dry(cbind(z.elevation,z.diversity,z.patcharea)),yld.hot=yld_hot(cbind(z.elevation,z.diversity,z.patcharea))) %>% 
  #  mutate(yld.ha=yld*m_density,
  #         yld.hot.ha=yld.hot*m_density,yld.dry.ha=yld.dry*m_density,
  #         elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
  #         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
  #         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=i,y=j)
  
  #results_mid<-bind_rows(results_mid,mid.var)
  
  rm(output,max.sm,max.rm,yld.hot,yld.dry,yld)
  print(c(i,j))
  }
}

#save output!
write.csv(results_max,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.maxylds.csv")
#write.csv(results_mid,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.var.diff.lt.var.min.maxyld.csv")
#write.csv(results_var,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.min.var.min.csv")
write.csv(results_rm,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.resilience.csv")
write.csv(results_sm,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.resistance.csv")

results_max<-read.csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.maxylds.csv")
#results_var<-read.csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.min.var.min.csv")
#results_mid<-read.csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscapes.var.diff.lt.var.min.maxyld.csv")

#calculate yields per ha
results_max<- results_max %>% group_by(x,y) %>% mutate(yld.ha=yld*m_density) %>% ungroup()
results_rm<- results_rm %>% group_by(x,y) %>% mutate(yld.ha=yld*m_density,yld.dry.ha=yld.dry*m_density,
                                                     yld.hot.ha=yld.hot*m_density) %>% ungroup()
results_sm<- results_sm %>% group_by(x,y) %>% mutate(yld.ha=yld*m_density,yld.dry.ha=yld.dry*m_density,
                                                     yld.hot.ha=yld.hot*m_density) %>% ungroup()

#to calculate errors around each estimate, generate 10,000 values for each parameter based on normal distribution
#Estimate as mean and standard deviation from standard error
#normal year
int14 <-rnorm(10000,mean=tmp.14 %>% filter(Comparison=="(Intercept)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.14 %>% filter(Comparison=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()-tmp.14 %>% filter(Comparison=="(Intercept)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(51))
shan.i14 <- rnorm(10000,mean=tmp.14 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.14 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()-tmp.14 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(51))
ba.l14 <- rnorm(10000,mean=tmp.14 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.14 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()-tmp.14 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(51))
int.sb14 <-  rnorm(10000,mean=tmp.14 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.14 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()-tmp.14 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(51))
p.tch14 <- rnorm(10000,mean=tmp.14 %>% filter(Comparison=="rescale(patcharea)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.14 %>% filter(Comparison=="rescale(patcharea)") %>% select(Upper.CL) %>% as.numeric()-tmp.14 %>% filter(Comparison=="rescale(patcharea)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(51))
elev14 <- rnorm(10000,mean=tmp.14 %>% filter(Comparison=="rescale(elevation)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.14 %>% filter(Comparison=="rescale(elevation)") %>% select(Upper.CL) %>% as.numeric()-tmp.14 %>% filter(Comparison=="rescale(elevation)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(51))

#hot year
int15 <- rnorm(10000,mean=tmp.15 %>% filter(Comparison=="(Intercept)") %>% select(full) %>% as.numeric(),sd=(tmp.15 %>% filter(Comparison=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()-tmp.15 %>% filter(Comparison=="(Intercept)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(42))
shan.i15 <- rnorm(10000,mean=tmp.15 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(full) %>% as.numeric(),sd=(tmp.15 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()-tmp.15 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(42))
ba.l15 <- rnorm(10000,mean=tmp.15 %>% filter(Comparison=="rescale(BA.legume)") %>% select(full) %>% as.numeric(),sd=(tmp.15 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()-tmp.15 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(42))
p.tch15 <- rnorm(10000,mean=tmp.15 %>% filter(Comparison=="rescale(patcharea)") %>% select(full) %>% as.numeric(),sd=(tmp.15 %>% filter(Comparison=="rescale(patcharea)") %>% select(Upper.CL) %>% as.numeric()-tmp.15 %>% filter(Comparison=="rescale(patcharea)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(42))

#dry year
int16 <- rnorm(10000,mean=tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()-tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(45))
shan.i16 <- rnorm(10000,mean=tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()-tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(45))
ba.l16 <- rnorm(10000,mean=tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()-tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(45))
int.sb16 <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Coefficients) %>% as.numeric()
elev16 <-  rnorm(10000,mean=tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Coefficients) %>% as.numeric(),sd=(tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Upper.CL) %>% as.numeric()-tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Lower.CL) %>% as.numeric())/2/1.96*sqrt(45))

for(i in 1:nrow(results_max)){
  #calculate normal yield variables
  max_norm <- yld_norm(cbind(results_max$z.elevation[i],results_max$z.diversity[i],results_max$z.patcharea[i]))
  #remove negative values
  max_norm<-max_norm[max_norm>0]
  #sort and find .25 and 97.5 points
  max_norm<-sort(max_norm)
  results_max$norm.low.ci[i]<-max_norm[250*length(max_norm)/10000]*m_density/1000
  results_max$norm.hi.ci[i]<-max_norm[9750*length(max_norm)/10000]*m_density/1000
  
  max_hot <- yld_hot(cbind(results_max$z.elevation[i],results_max$z.diversity[i],results_max$z.patcharea[i]))
  #remove negative values and unreasonably hight values 
  max_hot<-max_hot[max_hot>0&max_hot<1000]
  #sort and find .25 and 97.5 points
  max_hot<-sort(max_hot)
  results_max$hot.low.ci[i]<-max_hot[250*length(max_hot)/10000]*m_density/1000
  results_max$hot.hi.ci[i]<-max_hot[9750*length(max_hot)/10000]*m_density/1000
  
  #calculate dry year yield variables
  max_dry <- yld_dry(cbind(results_max$z.elevation[i],results_max$z.diversity[i],results_max$z.patcharea[i]))
  #remove negative values
  max_dry<-max_dry[max_dry>0&max_dry<1000]
  #sort and find .25 and 97.5 points
  max_dry<-sort(max_dry)
  results_max$dry.low.ci[i]<-max_dry[250*length(max_dry)/10000]*m_density/1000
  results_max$dry.hi.ci[i]<-max_dry[9750*length(max_dry)/10000]*m_density/1000
}

for(i in 1:nrow(results_mid)){
  #calculate normal yield variables
  mid_norm <- yld_norm(cbind(results_mid$z.elevation[i],results_mid$z.diversity[i],results_mid$z.patcharea[i]))
  #remove negative values
  mid_norm<-mid_norm[mid_norm>0]
  #sort and find .25 and 97.5 points and convert to tons per ha with coffee density and /1000
  mid_norm<-sort(mid_norm)
  results_mid$norm.low.ci[i]<-mid_norm[250*length(mid_norm)/10000]*m_density/1000
  results_mid$norm.hi.ci[i]<-mid_norm[9750*length(mid_norm)/10000]*m_density/1000
  
  mid_hot <- yld_hot(cbind(results_mid$z.elevation[i],results_mid$z.diversity[i],results_mid$z.patcharea[i]))
  #remove negative values and unreasonably high values
  mid_hot<-mid_hot[mid_hot>0&mid_hot<1000]
  #sort and find .25 and 97.5 points
  mid_hot<-sort(mid_hot)
  results_mid$hot.low.ci[i]<-mid_hot[250*length(mid_hot)/10000]*m_density/1000
  results_mid$hot.hi.ci[i]<-mid_hot[9750*length(mid_hot)/10000]*m_density/1000
  
  #calculate dry year yield variables
  mid_dry <- yld_dry(cbind(results_mid$z.elevation[i],results_mid$z.diversity[i],results_mid$z.patcharea[i],results_mid$z.ba.legume[i]))
  #remove negative values and unreasonably high values
  mid_dry<-mid_dry[mid_dry>0&mid_dry<1000]
  #sort and find .25 and 97.5 points
  mid_dry<-sort(mid_dry)
  results_mid$dry.low.ci[i]<-mid_dry[250*length(mid_dry)/10000]*m_density/1000
  results_mid$dry.hi.ci[i]<-mid_dry[9750*length(mid_dry)/10000]*m_density/1000
  
}

for(i in 1:nrow(results_var)){
  #calculate normal yield variables
  var_norm <- yld_norm(cbind(results_var$z.elevation[i],results_var$z.diversity[i],results_var$z.patcharea[i]))
  #remove negative values
  var_norm<-var_norm[var_norm>0]
  #sort and find .25 and 97.5 points
  var_norm<-sort(var_norm)
  results_var$norm.low.ci[i]<-var_norm[250*length(var_norm)/10000]*m_density/1000
  results_var$norm.hi.ci[i]<-var_norm[9750*length(var_norm)/10000]*m_density/1000
  
  var_hot <- yld_hot(cbind(results_var$z.elevation[i],results_var$z.diversity[i],results_var$z.patcharea[i]))
  #remove negative values and unreasonably high values
  var_hot<-var_hot[var_hot>0&var_hot<1000]
  #sort and find .25 and 97.5 points
  var_hot<-sort(var_hot)
  results_var$hot.low.ci[i]<-var_hot[250*length(var_hot)/10000]*m_density/1000
  results_var$hot.hi.ci[i]<-var_hot[9750*length(var_hot)/10000]*m_density/1000
  
  #calculate dry year yield variables
  var_dry <- yld_dry(cbind(results_var$z.elevation[i],results_var$z.diversity[i],results_var$z.patcharea[i]))
  #remove negative values and unreasonably high values
  var_dry<-var_dry[var_dry>0&var_dry<10]
  #sort and find .25 and 97.5 points
  var_dry<-sort(var_dry)
  results_var$dry.low.ci[i]<-var_dry[250*length(var_dry)/10000]*m_density/1000
  results_var$dry.hi.ci[i]<-var_dry[9750*length(var_dry)/10000]*m_density/1000
  
}


norm_max=results_max %>% pull(yld.ha) %>% sum()
#norm_max_up=results_max %>% pull(norm.hi.ci) %>% sum()
#norm_max_low=results_max %>% pull(norm.low.ci) %>% sum()

norm_rm=results_rm %>% pull(yld.ha) %>% sum()
norm_sm=results_sm %>% pull(yld.ha) %>% sum()

#norm_var=results_var %>% pull(yld.ha) %>% sum()
#norm_var_up=results_var %>% pull(norm.hi.ci) %>% sum()
#norm_var_low=results_var %>% pull(norm.low.ci) %>% sum()

#norm_mid=results_mid %>% pull(yld.ha) %>% sum()
#norm_mid_up=results_mid %>% pull(norm.hi.ci) %>% sum()
#norm_mid_low=results_mid %>% pull(norm.low.ci) %>% sum()

g1<-ggplot(results_max, aes( x,y, z = yld.ha)) +geom_raster(aes(fill=yld.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1050),breaks=seq(0,1050, by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Maximising Yields:\nNormal Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
g1a<-ggplot(results_rm, aes( x,y, z = yld.ha)) +geom_raster(aes(fill=yld.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1050),breaks=seq(0,1050, by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Maximising\nResilience") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
g1b<-ggplot(results_sm, aes( x,y, z = yld.ha)) +geom_raster(aes(fill=yld.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1050),breaks=seq(0,1050, by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Maximising\nResistance") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
g1c<-ggpubr::ggarrange(g1,g1a,g1b,ncol=3,nrow=1,align="hv",common.legend=T,legend="right")

hot_max=results_max %>% pull(yld.hot.ha) %>% sum()
#hot_max_low=results_max %>% pull(hot.low.ci) %>% sum()
#hot_max_up=results_max %>% pull(hot.hi.ci ) %>% sum()

hot_rm=results_rm %>% pull(yld.hot.ha) %>% sum()
hot_sm=results_sm %>% pull(yld.hot.ha) %>% sum()

#hot_var=results_var %>% pull(yld.hot.ha) %>% sum()
#hot_var_low=results_var %>% pull(hot.low.ci) %>% sum()
#hot_var_up=results_var %>% pull(hot.hi.ci) %>% sum()

#hot_mid=results_mid %>% pull(yld.hot.ha) %>% sum()
#hot_mid_low=results_mid %>% pull(hot.low.ci) %>% sum()
#hot_mid_up=results_mid %>% pull(hot.hi.ci) %>% sum()

g2<-ggplot(results_max, aes( x,y, z = yld.hot.ha)) +geom_raster(aes(fill=yld.hot.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1000),breaks=seq(0,1050,by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Hot Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(hot_max,4)," kg"),size=4,color="white")
g2a<-ggplot(results_rm, aes( x,y, z = yld.hot.ha)) +geom_raster(aes(fill=yld.hot.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1050),breaks=seq(0,1050, by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Hot Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
g2b<-ggplot(results_sm, aes( x,y, z = yld.hot.ha)) +geom_raster(aes(fill=yld.hot.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1000),breaks=seq(0,1050,by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Hot Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(hot_var,4)," kg"),size=4,color="white")
g2c<-ggpubr::ggarrange(g2,g2a,g2b,ncol=3,nrow=1,align="hv",common.legend=T,legend="right")

dry_max=results_max %>% pull(yld.dry.ha) %>% sum()
#dry_max_low=results_max %>% pull(dry.low.ci) %>% sum()
#dry_max_up=results_max %>% pull(dry.hi.ci) %>% sum()

dry_rm=results_rm %>% pull(yld.dry.ha) %>% sum()
dry_sm=results_sm %>% pull(yld.dry.ha) %>% sum()

#dry_var=results_var %>% pull(yld.dry.ha) %>% sum()
#dry_var_low=results_var %>% pull(dry.low.ci) %>% sum()
#dry_var_up=results_var %>% pull(dry.hi.ci) %>% sum()

#dry_mid=results_mid %>% pull(yld.dry.ha) %>% sum()
#dry_mid_up=results_mid %>% pull(dry.hi.ci) %>% sum()
#dry_mid_low=results_mid %>% pull(dry.low.ci) %>% sum()

g3<-ggplot(results_max, aes( x,y, z = yld.dry.ha)) +geom_raster(aes(fill=yld.dry.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,500),breaks=seq(0,500,by=75))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Dry Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(dry_max,4)," kg"),size=4,color="white")
g3a<-ggplot(results_rm, aes( x,y, z = yld.dry.ha)) +geom_raster(aes(fill=yld.dry.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,500),breaks=seq(0,500, by=75))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Dry Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
g3b<-ggplot(results_sm, aes( x,y, z = yld.dry.ha)) +geom_raster(aes(fill=yld.dry.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,500),breaks=seq(0,500,by=75))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Dry Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(dry_var,4)," kg"),size=4,color="white")
g3c<-ggpubr::ggarrange(g3,g3a,g3b,ncol=3,nrow=1,align="hv",common.legend=T,legend="right")

ggpubr::ggarrange(g1c,g2c,g3c,ncol=1,nrow=3,align="hv",heights=c(1.1,1,1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/MaximisationvsResilience.rasters.pdf",height=11,width=12)

#results_max_low<-results_max %>% ungroup() %>% select(yld.ha.low,yld.dry.low,yld.hot.low) %>% rename(yld.ha=yld.ha.low,yld.dry=yld.dry.low,yld.hot=yld.hot.low)
#results_max_up<-results_max %>% ungroup() %>% select(yld.ha.up,yld.dry.up,yld.hot.up) %>% rename(yld.ha=yld.ha.up,yld.dry=yld.dry.up,yld.hot=yld.hot.up)
#results_max_ci<-bind_rows(results_max_low,results_max_up)

#or do again as histograms
b1 <- ggplot(results_max,aes(yld.ha)) + geom_freqpoly(binwidth=50, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=results_max,aes(yld.hot.ha,color="Hot Year"),binwidth=25,size=1) + 
  geom_freqpoly(data=results_max,aes(yld.dry.ha,color="Dry Year"),binwidth=25,size=1) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.ha,fill="Normal Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.hot,fill="Hot Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=25,aes(yld.dry,fill="Dry Year"),alpha=1/2) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-0.5,4000) +
  scale_color_viridis_d(begin=0.8,end=0) + #scale_fill_viridis_d(begin=0.8,end=0) + #annotate("text",x=450,y=100,label=paste0("Normal Year Total = ",signif(norm_max,4)," kg"),size=5) + 
  ggtitle("Maximising Yields")
  #annotate("text",x=450,y=93,label=paste0("Hot Year Total = ",signif(hot_max,4)," kg"),size=5) + 
  #annotate("text",x=450,y=86,label=paste0("Dry Year Total = ",signif(dry_max,4)," kg"),size=5)

#results_var_low<-results_var %>% ungroup() %>% select(yld.ha.low,yld.dry.low,yld.hot.low) %>% rename(yld.ha=yld.ha.low,yld.dry=yld.dry.low,yld.hot=yld.hot.low)
#results_var_up<-results_var %>% ungroup() %>% select(yld.ha.up,yld.dry.up,yld.hot.up) %>% rename(yld.ha=yld.ha.up,yld.dry=yld.dry.up,yld.hot=yld.hot.up)
#results_var_ci<-bind_rows(results_var_low,results_var_up)

b2 <- ggplot(results_rm,aes(yld.ha)) + geom_freqpoly(binwidth=50, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=results_rm,aes(yld.hot.ha,color="Hot Year"),binwidth=50,size=1) + 
  geom_freqpoly(data=results_rm,aes(yld.dry.ha,color="Dry Year"),binwidth=50,size=1) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.ha,fill="Normal Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.hot,fill="Hot Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=25,aes(yld.dry,fill="Dry Year"),alpha=1/2) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-0.5,4000) +
  scale_color_viridis_d(begin=0.8,end=0) + #annotate("text",x=500,y=40,label=paste0("Normal Year Total = ",signif(norm_var,4)," kg"),size=5) + 
  ggtitle("Maximising Resilience")
  #annotate("text",x=500,y=37,label=paste0("Hot Year Total = ",signif(hot_var,4)," kg"),size=5) + 
  #annotate("text",x=500,y=34,label=paste0("Dry Year Total = ",signif(dry_var,4)," kg"),size=5)

b3 <- ggplot(results_sm,aes(yld.ha)) + geom_freqpoly(binwidth=50, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=results_sm,aes(yld.hot.ha,color="Hot Year"),binwidth=50,size=1) + 
  geom_freqpoly(data=results_sm,aes(yld.dry.ha,color="Dry Year"),binwidth=50,size=1) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.ha,fill="Normal Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.hot,fill="Hot Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=25,aes(yld.dry,fill="Dry Year"),alpha=1/2) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-0.5,4000) +
  scale_color_viridis_d(begin=0.8,end=0) + #annotate("text",x=500,y=40,label=paste0("Normal Year Total = ",signif(norm_var,4)," kg"),size=5) + 
  ggtitle("Maximising Resistance")
#annotate("text",x=500,y=37,label=paste0("Hot Year Total = ",signif(hot_var,4)," kg"),size=5) + 
#annotate("text",x=500,y=34,label=paste0("Dry Year Total = ",signif(dry_var,4)," kg"),size=5)

ggpubr::ggarrange(b1,b2,b3,ncol=3,nrow=1,align="hv",common.legend=T,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/MaximisationvsResilience.histos.pdf",height=5,width=15)

#save .csv for table
#tmp<-tibble(Maximising.Yields=norm_max,Maximising.Yields.ci.low=norm_max_low,Maximising.Yields.ci.high=norm_max_up,
#            Minimising.Variability=norm_var,Minimising.Variability.ci.low=norm_var_low,Minimising.Variability.ci.high=norm_var_up,
#            MinVar.MaxYlds=norm_mid,MinVar.MaxYlds.ci.low=norm_mid_low,MinVar.MaxYlds.ci.high=norm_mid_up)
#tmp$Climate.Conditions="Normal"
#tmp2=tibble(Maximising.Yields=hot_max,Maximising.Yields.ci.low=hot_max_low,Maximising.Yields.ci.high=hot_max_up,
#            Minimising.Variability=hot_var,Minimising.Variability.ci.low=hot_var_low,Minimising.Variability.ci.high=hot_var_up,
#            MinVar.MaxYlds=hot_mid,MinVar.MaxYlds.ci.low=hot_mid_low,MinVar.MaxYlds.ci.high=hot_mid_up)
#tmp2$Climate.Conditions="Hot"
#tmp<-bind_rows(tmp,tmp2)
#tmp2=tibble(Maximising.Yields=dry_max,Maximising.Yields.ci.low=dry_max_low,Maximising.Yields.ci.high=dry_max_up,
#            Minimising.Variability=dry_var,Minimising.Variability.ci.low=dry_var_low,Minimising.Variability.ci.high=dry_var_up,
#            MinVar.MaxYlds=dry_mid,MinVar.MaxYlds.ci.low=dry_mid_low,MinVar.MaxYlds.ci.high=dry_mid_up)
#tmp2$Climate.Conditions="Dry"
#tmp<-bind_rows(tmp,tmp2)

#calculate percent difference
#tmp<- tmp %>% group_by(Climate.Conditions) %>% mutate(Pct.Diff=Minimising.Variability/Maximising.Yields*100,Pct.Diff2=MinVar.MaxYlds/Maximising.Yields*100) %>% 
#  mutate(Diff.Max=Maximising.Yields/(tmp %>% filter(Climate.Conditions=="Normal") %>% pull(Maximising.Yields))*100,
 #        Diff.Mid=MinVar.MaxYlds/(tmp %>% filter(Climate.Conditions=="Normal") %>% pull(MinVar.MaxYlds))*100) 
##         Diff.Var=Minimising.Variability/(tmp %>% filter(Climate.Conditions=="Normal") %>% pull(Minimising.Variability))*100,
#write.csv(tmp,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/LandscapeYieldComparisons.csv")

#do for landscape conditions
#patcharea
y1a<-ggplot(results_max, aes( x,y, z = patcharea)) +geom_raster(aes(fill=patcharea)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + theme_classic() +
  labs(fill="Patch Area\n[ha]") + #ggtitle("Maximising Yields") + 
  ylab("Maximising\nYields")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
y1b<-ggplot(results_rm, aes( x,y, z = patcharea)) +geom_raster(aes(fill=patcharea)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000))+ theme_classic() +
  labs(fill="Patch Area\n[ha]") + #ggtitle("Minimising Variability") + 
  ylab("Maximising\nResilience")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
y1c<-ggplot(results_sm, aes( x,y, z = patcharea)) +geom_raster(aes(fill=patcharea)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000))+ theme_classic() +
  labs(fill="Patch Area\n[ha]") + #ggtitle("Minimising Variability") + 
  ylab("Maximising\nResistance")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
y1<-ggpubr::ggarrange(y1a,y1b,y1c,ncol=1,nrow=3,align="hv",common.legend=T,legend="bottom")

#Shade Diversity
y2a<-ggplot(results_max, aes( x,y, z = diversity)) +geom_raster(aes(fill=diversity)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2.2),breaks=seq(0,2.2, by=1)) + theme_classic() +
  labs(fill="Shade Diversity\n[H]") + #ggtitle("Maximising Yields") + 
  theme(text=element_text(size=16))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
y2b<-ggplot(results_rm, aes( x,y, z = diversity)) +geom_raster(aes(fill=diversity)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2.2),breaks=seq(0,2.2, by=1))+ theme_classic() +
  labs(fill="Shade Diversity\n[H]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
y2c<-ggplot(results_sm, aes( x,y, z = diversity)) +geom_raster(aes(fill=diversity)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2.2),breaks=seq(0,2.2, by=1))+ theme_classic() +
  labs(fill="Shade Diversity\n[H]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
y2<-ggpubr::ggarrange(y2a,y2b,y2c,ncol=1,nrow=3,align="hv",common.legend=T,legend="bottom")

#y4<-ggpubr::ggarrange(y1c,y2c,ncol=2,nrow=1,align="hv",widths=c(1.25,1))


#Elevation and BA.legume
y3a<-ggplot(results_max, aes( x,y, z = elevation)) +geom_raster(aes(fill=elevation)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(1300,2000),breaks=seq(1300,2000, by=500)) + theme_classic() +
  labs(fill="Elevation\n[m]") + 
  theme(text=element_text(size=16),legend.position="bottom")
  #ggtitle("Maximising Yields") + 
  #ylab("Landscape\nConfiguration")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0),legend.position="bottom")# + 
  #annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
y3<-ggpubr::ggarrange(y3a,y3a,y3a,ncol=1,nrow=3,align="hv",common.legend=T,legend="bottom")

y4a<-ggplot(results_max, aes( x,y, z = d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max())) +geom_raster(aes(fill=d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max())) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,21),breaks=seq(0,21, by=10))+ theme_classic() +
  labs(fill="Basal Area\nLeguminous Trees\n[m2/ha]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16),legend.position="bottom") #+ annotate("text",x=5,y=1,label=paste("Leguminous Trees = ",signif(d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max(),4)," m2/ha"),size=4)
y4b<-ggplot(results_rm, aes( x,y, z = ba.legume)) +geom_raster(aes(fill=ba.legume)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,21),breaks=seq(0,21, by=10))+ theme_classic() +
  labs(fill="Basal Area\nLeguminous Trees\n[m2/ha]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16),legend.position="bottom") #+ annotate("text",x=5,y=1,label=paste("Leguminous Trees = ",signif(d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max(),4)," m2/ha"),size=4)
y4c<-ggplot(results_sm, aes( x,y, z = ba.legume)) +geom_raster(aes(fill=ba.legume)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,21),breaks=seq(0,21, by=10))+ theme_classic() +
  labs(fill="Basal Area\nLeguminous Trees\n[m2/ha]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16),legend.position="bottom") #+ annotate("text",x=5,y=1,label=paste("Leguminous Trees = ",signif(d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max(),4)," m2/ha"),size=4)
y4<-ggpubr::ggarrange(y4a,y4b,y4c,ncol=1,nrow=3,align="hv",common.legend=T,legend="bottom")

ggpubr::ggarrange(y1,y2,y3,y4,ncol=4,nrow=1,align="hv",widths=c(1.25,1,1,1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscape.Configuration.pdf",height=10,width=15)



