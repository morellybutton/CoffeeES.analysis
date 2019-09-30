#landscape optimisation of 10x10 pixel grid
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#for 2014 yield
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))

#equation for normal year
int14 <-tmp.14 %>% filter(X=="(Intercept)") %>% select(Estimate) %>% as.numeric()
shan.i14 <- tmp.14 %>% filter(X=="rescale(Shannon.i)") %>% select(Estimate) %>% as.numeric()
ba.l14 <- tmp.14 %>% filter(X=="rescale(BA.legume)") %>% select(Estimate) %>% as.numeric()
int.sb14 <- tmp.14 %>% filter(X=="rescale(BA.legume):rescale(Shannon.i)") %>% select(Estimate) %>% as.numeric()
p.tch14 <- tmp.14 %>% filter(X=="rescale(patcharea)") %>% select(Estimate) %>% as.numeric()
elev14 <- tmp.14 %>% filter(X=="rescale(elevation)") %>% select(Estimate) %>% as.numeric()

yld_norm <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3
}  

int14.l <-tmp.14 %>% filter(X=="(Intercept)") %>% select(Lower.CL) %>% as.numeric()
shan.i14.l <- tmp.14 %>% filter(X=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
ba.l14.l <- tmp.14 %>% filter(X=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
int.sb14.l <- tmp.14 %>% filter(X=="rescale(BA.legume):rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
p.tch14.l <- tmp.14 %>% filter(X=="rescale(patcharea)") %>% select(Lower.CL) %>% as.numeric()
elev14.l <- tmp.14 %>% filter(X=="rescale(elevation)") %>% select(Lower.CL) %>% as.numeric()

yld_norm_lower <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  int14.l + elev14.l*x1 + shan.i14.l*x2 + ba.l14.l*max.ba + int.sb14.l*max.ba*x2 + p.tch14.l*x3
}  

int14.u <-tmp.14 %>% filter(X=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()
shan.i14.u <- tmp.14 %>% filter(X=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
ba.l14.u <- tmp.14 %>% filter(X=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
int.sb14.u <- tmp.14 %>% filter(X=="rescale(BA.legume):rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
p.tch14.u <- tmp.14 %>% filter(X=="rescale(patcharea)") %>% select(Upper.CL) %>% as.numeric()
elev14.u <- tmp.14 %>% filter(X=="rescale(elevation)") %>% select(Upper.CL) %>% as.numeric()

yld_norm_upper <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  int14.u + elev14.u*x1 + shan.i14.u*x2 + ba.l14.u*max.ba + int.sb14.u*max.ba*x2 + p.tch14.u*x3
}  

#for 2015 yield difference
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))

#equation for hot year
int15 <-tmp.15 %>% filter(X=="(Intercept)") %>% select(Estimate) %>% as.numeric()
shan.i15 <- tmp.15 %>% filter(X=="rescale(Shannon.i)") %>% select(Estimate) %>% as.numeric()
ba.l15 <- tmp.15 %>% filter(X=="rescale(BA.legume)") %>% select(Estimate) %>% as.numeric()
p.tch15 <- tmp.15 %>% filter(X=="rescale(patcharea)") %>% select(Estimate) %>% as.numeric()

yld_hot <- function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int15 + shan.i15*x2 + ba.l15*max.ba +  p.tch15*x3)
}

#equation for hot year
int15.l <-tmp.15 %>% filter(X=="(Intercept)") %>% select(Lower.CL) %>% as.numeric()
shan.i15.l <- tmp.15 %>% filter(X=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
ba.l15.l <- tmp.15 %>% filter(X=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
p.tch15.l <- tmp.15 %>% filter(X=="rescale(patcharea)") %>% select(Lower.CL) %>% as.numeric()

yld_hot_lower <- function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int15.l + shan.i15.l*x2 + ba.l15.l*max.ba +  p.tch15.l*x3)
}

int15.u <-tmp.15 %>% filter(X=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()
shan.i15.u <- tmp.15 %>% filter(X=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
ba.l15.u <- tmp.15 %>% filter(X=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
p.tch15.u <- tmp.15 %>% filter(X=="rescale(patcharea)") %>% select(Upper.CL) %>% as.numeric()

yld_hot_upper <- function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int15.u + shan.i15.u*x2 + ba.l15.u*max.ba +  p.tch15.u*x3)
}

#for 2016 yield difference
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

#equation for dry year
int16 <- tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Estimate) %>% as.numeric()
shan.i16 <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Estimate) %>% as.numeric()
ba.l16 <- tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Estimate) %>% as.numeric()
int.sb16 <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Estimate) %>% as.numeric()
elev16 <- tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Estimate) %>% as.numeric()

yld_dry <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int16 + elev16*x1 + shan.i16*x2 + ba.l16*max.ba + int.sb16*max.ba*x2) 
}

int16.l <- tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Lower.CL) %>% as.numeric()
shan.i16.l <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Lower.CL) %>% as.numeric()
ba.l16.l <- tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
int.sb16.l <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Lower.CL) %>% as.numeric()
elev16.l <- tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Lower.CL) %>% as.numeric()

yld_dry_lower <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int16.l + elev16.l*x1 + shan.i16.l*x2 + ba.l16.l*max.ba + int.sb16.l*max.ba*x2) 
}

int16.u <- tmp.16 %>% filter(Comparison=="(Intercept)") %>% select(Upper.CL) %>% as.numeric()
shan.i16.u <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i)") %>% select(Upper.CL) %>% as.numeric()
ba.l16.u <- tmp.16 %>% filter(Comparison=="rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
int.sb16.u <- tmp.16 %>% filter(Comparison=="rescale(Shannon.i):rescale(BA.legume)") %>% select(Upper.CL) %>% as.numeric()
elev16.u <- tmp.16 %>% filter(Comparison=="rescale(elevation)") %>% select(Upper.CL) %>% as.numeric()

yld_dry_upper <- function(x) {
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  (int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int16.u + elev16.u*x1 + shan.i16.u*x2 + ba.l16.u*max.ba + int.sb16.u*max.ba*x2) 
}

#equation to minimize variability between all years (take mean of difference between normal and shock years)
var_min <-function(x){
  x1 <- x[1] #elevation
  x2 <- x[2] #shade diversity
  x3 <- x[3] #patcharea
  0.5*sum(int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3 - ((int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int16 + elev16*x1 + shan.i16*x2 + ba.l16*max.ba + int.sb16*max.ba*x2)),
          int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3 - ((int14 + elev14*x1 + shan.i14*x2 + ba.l14*max.ba + int.sb14*max.ba*x2 + p.tch14*x3)*exp(int15 + shan.i15*x2 + ba.l15*max.ba +  p.tch15*x3)) )
}


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

betas <- d.F.new %>% filter(year==2014) %>% mutate(shannon.i=arm::rescale(Shannon.i),ba.legume=arm::rescale(BA.legume),
                                                   patch=arm::rescale(patcharea)) %>% 
  select(shannon.i,ba.legume,patch) %>% summarise_all(c("max","min"))
div.vals<-d.F.new %>% filter(year==2014) %>% summarise(mean=mean(Shannon.i,na.rm=T),sd=sd(Shannon.i,na.rm=T)) %>% 
  select(mean,sd)
patch.vals<-d.F.new %>% filter(year==2014) %>% summarise(mean=mean(patcharea,na.rm=T),sd=sd(patcharea,na.rm=T)) %>% 
  select(mean,sd)

#include max ba and median coffee density
max.ba <- betas %>% pull(ba.legume_max) 
m_density<-d.F.new %>% filter(year==2014)  %>% summarise(density=median(density,na.rm=T)) %>% pull(density)

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
betas.sh1 <- rep(betas.sh, each = 101)
betas.pt1 <- rep(betas.pt, times = 101)

#practice on pixel [1,1] in the landscape
betas.elev<-z_combo %>% filter(y==1&x==1) %>% pull(elev.rescale)

yld<-apply(cbind(betas.elev,betas.sh1,betas.pt1), 1, yld_norm)
#yld <- matrix(yld, 11) 
#contour(betas.pt, betas.sh,  yld)
f1_1<-tibble(yld,betas.elev,betas.sh1,betas.pt1)
#extract conditions for maximum value and identify block being represented
max.yld <- f1_1 %>% filter(yld==max(yld)) %>% rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
  mutate(elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
         diversity=z.diversity*2*div.vals$sd+div.vals$mean,
         patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=1,y=1)

results_max<-tibble()
results_var<-tibble()
c.dens<-m_density
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
       
    yld<-apply(cbind(betas.elev,betas.sh1,betas.pt1), 1, yld_norm)
    
    output<-tibble(yld,betas.elev,betas.sh1,betas.pt1)
    #extract conditions for maximum value 
    max.yld <- output %>% filter(yld==max(yld)) %>% rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
      mutate(yld.ha=yld*c.dens,
             elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
             diversity=z.diversity*2*div.vals$sd+div.vals$mean,
             patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=i,y=j)
    max.yld$yld.hot<-yld_hot(cbind(max.yld$z.elevation,max.yld$z.diversity,max.yld$z.patcharea))
    max.yld$yld.dry<-yld_dry(cbind(max.yld$z.elevation,max.yld$z.diversity,max.yld$z.patcharea))
    max.yld<-max.yld %>% mutate(yld.hot.ha=yld.hot*c.dens,yld.dry.ha=yld.dry*c.dens)
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
  var.min<-apply(cbind(betas.elev,betas.sh1,betas.pt1), 1, var_min)
  yld.hot<-apply(cbind(betas.elev,betas.sh1,betas.pt1), 1, yld_hot)
  yld.dry<-apply(cbind(betas.elev,betas.sh1,betas.pt1), 1, yld_dry)
  
  output1<-tibble(var.min,yld,yld.hot,yld.dry,betas.elev,betas.sh1,betas.pt1)
  min.var <- output1 %>% filter(var.min>0&yld>0,yld.hot>0,yld.dry>0) %>% filter(var.min==min(var.min)) %>% rename(z.elevation=betas.elev,z.diversity=betas.sh1,z.patcharea=betas.pt1) %>% 
   # mutate(yld=yld_norm(cbind(z.elevation,z.diversity,z.patcharea)),yld.dry=yld_dry(cbind(z.elevation,z.diversity,z.patcharea)),yld.hot=yld_hot(cbind(z.elevation,z.diversity,z.patcharea))) %>% 
    mutate(yld.ha=yld*c.dens,
           yld.hot.ha=yld.hot*c.dens,yld.dry.ha=yld.dry*c.dens,
           elevation=z.elevation*2*elev.vals$sd+elev.vals$mean,
           diversity=z.diversity*2*div.vals$sd+div.vals$mean,
           patcharea=z.patcharea*2*patch.vals$sd+patch.vals$mean,x=i,y=j)
  results_var<-bind_rows(results_var,min.var)
  rm(output1,min.var,var.min,yld.hot,yld.dry,yld)
  }
}

#plot yield maximisation
results_max <- results_max %>% group_by(x,y) %>% 
  mutate(m.diff=mean(yld.hot.ha-yld.ha,yld.dry.ha-yld.ha),diff.hot=yld.hot.ha-yld.ha,diff.dry=yld.dry.ha-yld.ha) %>% 
  mutate(yld.ha.low= yld_norm_lower(cbind(z.elevation,z.diversity,z.patcharea))*m_density,
         yld.ha.up=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_norm_upper)*m_density,
         yld.hot.low=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_hot_lower)*m_density,
         yld.hot.up=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_hot_upper)*m_density,
         yld.dry.low=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_dry_lower)*m_density,
         yld.dry.up=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_dry_upper)*m_density)
results_var <- results_var %>% group_by(x,y) %>% 
  mutate(m.diff=mean(yld.hot.ha-yld.ha,yld.dry.ha-yld.ha),diff.hot=yld.hot.ha-yld.ha,diff.dry=yld.dry.ha-yld.ha) %>% 
  mutate(yld.ha.low=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_norm_lower)*m_density,
         yld.ha.up=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_norm_upper)*m_density,
         yld.hot.low=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_hot_lower)*m_density,
         yld.hot.up=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_hot_upper)*m_density,
         yld.dry.low=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_dry_lower)*m_density,
         yld.dry.up=apply(cbind(z.elevation,z.diversity,z.patcharea), 1, yld_dry_upper)*m_density)

norm_max=results_max %>% pull(yld.ha) %>% sum()
norm_max_up=results_max %>% pull(yld.ha.up) %>% sum()
norm_max_low=results_max %>% pull(yld.ha.low) %>% sum()

norm_var=results_var %>% pull(yld.ha) %>% sum()
norm_var_up=results_var %>% pull(yld.ha.up) %>% sum()
norm_var_low=results_var %>% pull(yld.ha.low) %>% sum()

g1<-ggplot(results_max, aes( x,y, z = yld.ha)) +geom_raster(aes(fill=yld.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1000),breaks=seq(0,1000, by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Maximising Yields:\nNormal Year") + 
  theme(text=element_text(size=16)) #+ annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
g1b<-ggplot(results_var, aes( x,y, z = yld.ha)) +geom_raster(aes(fill=yld.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1000),breaks=seq(0,1000, by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Minimising Variability:\nNormal Year") + 
  theme(text=element_text(size=16)) + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
g1c<-ggpubr::ggarrange(g1,g1b,ncol=2,nrow=1,align="hv",common.legend=T,legend="right")

hot_max=results_max %>% pull(yld.hot.ha) %>% sum()
hot_max_low=results_max %>% pull(yld.hot.low) %>% sum()
hot_max_up=results_max %>% pull(yld.hot.up) %>% sum()

hot_var=results_var %>% pull(yld.hot.ha) %>% sum()
hot_var_low=results_var %>% pull(yld.hot.low) %>% sum()
hot_var_up=results_var %>% pull(yld.hot.up) %>% sum()

g2<-ggplot(results_max, aes( x,y, z = yld.hot.ha)) +geom_raster(aes(fill=yld.hot.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1000),breaks=seq(0,1000,by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Hot Year") + 
  theme(text=element_text(size=16)) + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(hot_max,4)," kg"),size=4,color="white")
g2b<-ggplot(results_var, aes( x,y, z = yld.hot.ha)) +geom_raster(aes(fill=yld.hot.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,1000),breaks=seq(0,1000,by=150))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Hot Year") + 
  theme(text=element_text(size=16)) + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(hot_var,4)," kg"),size=4,color="white")
g2c<-ggpubr::ggarrange(g2,g2b,ncol=2,nrow=1,align="hv",common.legend=T,legend="right")

dry_max=results_max %>% pull(yld.dry.ha) %>% sum()
dry_max_low=results_max %>% pull(yld.dry.low) %>% sum()
dry_max_up=results_max %>% pull(yld.dry.up) %>% sum()

dry_var=results_var %>% pull(yld.dry.ha) %>% sum()
dry_var_low=results_var %>% pull(yld.dry.low) %>% sum()
dry_var_up=results_var %>% pull(yld.dry.up) %>% sum()

g3<-ggplot(results_max, aes( x,y, z = yld.dry.ha)) +geom_raster(aes(fill=yld.dry.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,350),breaks=seq(0,350,by=75))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Dry Year") + 
  theme(text=element_text(size=16)) + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(dry_max,4)," kg"),size=4,color="white")
g3b<-ggplot(results_var, aes( x,y, z = yld.dry.ha)) +geom_raster(aes(fill=yld.dry.ha)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,350),breaks=seq(0,350,by=75))+ theme_classic() +
  labs(fill="Yield [kg/ha]") + ggtitle("Dry Year") + 
  theme(text=element_text(size=16)) + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(dry_var,4)," kg"),size=4,color="white")
g3c<-ggpubr::ggarrange(g3,g3b,ncol=2,nrow=1,align="hv",common.legend=T,legend="right")

ggpubr::ggarrange(g1c,g2c,g3c,ncol=1,nrow=3,align="hv")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/MaximisationvsVariability.rasters.pdf",height=11,width=9)

results_max_low<-results_max %>% ungroup() %>% select(yld.ha.low,yld.dry.low,yld.hot.low) %>% rename(yld.ha=yld.ha.low,yld.dry=yld.dry.low,yld.hot=yld.hot.low)
results_max_up<-results_max %>% ungroup() %>% select(yld.ha.up,yld.dry.up,yld.hot.up) %>% rename(yld.ha=yld.ha.up,yld.dry=yld.dry.up,yld.hot=yld.hot.up)
results_max_ci<-bind_rows(results_max_low,results_max_up)

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

results_var_low<-results_var %>% ungroup() %>% select(yld.ha.low,yld.dry.low,yld.hot.low) %>% rename(yld.ha=yld.ha.low,yld.dry=yld.dry.low,yld.hot=yld.hot.low)
results_var_up<-results_var %>% ungroup() %>% select(yld.ha.up,yld.dry.up,yld.hot.up) %>% rename(yld.ha=yld.ha.up,yld.dry=yld.dry.up,yld.hot=yld.hot.up)
results_var_ci<-bind_rows(results_var_low,results_var_up)

b2 <- ggplot(results_var,aes(yld.ha)) + geom_freqpoly(binwidth=50, aes(color="Normal Year"),size=1) + theme_classic() + 
  xlab("Farm Yield [kg/ha]") +
  geom_freqpoly(data=results_var,aes(yld.hot.ha,color="Hot Year"),binwidth=50,size=1) + 
  geom_freqpoly(data=results_var,aes(yld.dry.ha,color="Dry Year"),binwidth=50,size=1) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.ha,fill="Normal Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=50,aes(yld.hot,fill="Hot Year"),alpha=1/2) +
  #geom_area(data=results_max_ci,stat="bin",binwidth=25,aes(yld.dry,fill="Dry Year"),alpha=1/2) +
  #geom_vline(xintercept=0,linetype="dashed") +
  theme(text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=14)) + #xlim(-0.5,4000) +
  scale_color_viridis_d(begin=0.8,end=0) + #annotate("text",x=500,y=40,label=paste0("Normal Year Total = ",signif(norm_var,4)," kg"),size=5) + 
  ggtitle("Minimising Yield Variability") 
  #annotate("text",x=500,y=37,label=paste0("Hot Year Total = ",signif(hot_var,4)," kg"),size=5) + 
  #annotate("text",x=500,y=34,label=paste0("Dry Year Total = ",signif(dry_var,4)," kg"),size=5)

ggpubr::ggarrange(b1,b2,ncol=2,nrow=1,align="hv",common.legend=T,legend="right")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/MaximisationvsVariability.histos.pdf",height=5,width=11)

#save .csv for table
tmp<-tibble(Maximising.Yields=norm_max,Maximising.Yields.ci=0.5*sum(norm_max_low-norm_max,norm_max-norm_max_up),
            Minimising.Variability=norm_var,Minimising.Variability.ci=0.5*sum(norm_var-norm_var_low,norm_var_up-norm_var))
tmp$Climate.Conditions="Normal"
tmp2=tibble(Maximising.Yields=hot_max,Maximising.Yields.ci=0.5*sum(hot_max-hot_max_low,hot_max_up-hot_max),
            Minimising.Variability=hot_var,Minimising.Variability.ci=0.5*sum(hot_var-hot_var_low,hot_var_up-hot_var))
tmp2$Climate.Conditions="Hot"
tmp<-bind_rows(tmp,tmp2)
tmp2=tibble(Maximising.Yields=dry_max,Maximising.Yields.ci=0.5*sum(dry_max_low-dry_max,dry_max-dry_max_up),
            Minimising.Variability=dry_var,Minimising.Variability.ci=0.5*sum(dry_var-dry_var_low,dry_var_up-dry_var))
tmp2$Climate.Conditions="Dry"
tmp<-bind_rows(tmp,tmp2)

#calculate percent difference
tmp<- tmp %>% group_by(Climate.Conditions) %>% mutate(Pct.Diff=Minimising.Variability/Maximising.Yields*100) %>% 
  mutate(Diff.Max=Maximising.Yields/(tmp %>% filter(Climate.Conditions=="Normal") %>% pull(Maximising.Yields))*100,
         Diff.Var=Minimising.Variability/(tmp %>% filter(Climate.Conditions=="Normal") %>% pull(Minimising.Variability))*100) 
write.csv(tmp,"/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/LandscapeYieldComparisons.csv")

#do for landscape conditions
#patcharea
y1<-ggplot(results_max, aes( x,y, z = patcharea)) +geom_raster(aes(fill=patcharea)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000)) + theme_classic() +
  labs(fill="Patch Area\n[ha]") + #ggtitle("Maximising Yields") + 
  ylab("Maximising\nYields")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
y1b<-ggplot(results_var, aes( x,y, z = patcharea)) +geom_raster(aes(fill=patcharea)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2300),breaks=seq(0,2300, by=1000))+ theme_classic() +
  labs(fill="Patch Area\n[ha]") + #ggtitle("Minimising Variability") + 
  ylab("Minimising\nVariability")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
y1c<-ggpubr::ggarrange(y1,y1b,ncol=1,nrow=2,align="hv",common.legend=T,legend="bottom")

#Shade Diversity
y2<-ggplot(results_max, aes( x,y, z = diversity)) +geom_raster(aes(fill=diversity)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2),breaks=seq(0,2, by=1)) + theme_classic() +
  labs(fill="Shade Diversity\n[H]") + #ggtitle("Maximising Yields") + 
  theme(text=element_text(size=16))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)
y2b<-ggplot(results_var, aes( x,y, z = diversity)) +geom_raster(aes(fill=diversity)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,2),breaks=seq(0,2, by=1))+ theme_classic() +
  labs(fill="Shade Diversity\n[H]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16))# + annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_var,4)," kg"),size=4,color="white")
y2c<-ggpubr::ggarrange(y2,y2b,ncol=1,nrow=2,align="hv",common.legend=T,legend="bottom")

y4<-ggpubr::ggarrange(y1c,y2c,ncol=2,nrow=1,align="hv",widths=c(1.25,1))


#Elevation and BA.legume
y3<-ggplot(results_max, aes( x,y, z = elevation)) +geom_raster(aes(fill=elevation)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(1300,2000),breaks=seq(1300,2000, by=500)) + theme_classic() +
  labs(fill="Elevation\n[m]") + 
  theme(text=element_text(size=16),legend.position="bottom")
  #ggtitle("Maximising Yields") + 
  #ylab("Landscape\nConfiguration")+theme(text=element_text(size=16),axis.title.y = element_text(angle = 0),legend.position="bottom")# + 
  #annotate("text",x=4,y=1,label=paste("Total Harvest = ",signif(norm_max,4)," kg"),size=4)

results_var$BA.legume<-d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max()
y3b<-ggplot(results_var, aes( x,y, z = BA.legume)) +geom_raster(aes(fill=BA.legume)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c(limits=c(0,21),breaks=seq(0,21, by=10))+ theme_classic() +
  labs(fill="Basal Area\nLeguminous Trees\n[m2/ha]") + #ggtitle("Minimising Variability") + 
  theme(text=element_text(size=16),legend.position="bottom") #+ annotate("text",x=5,y=1,label=paste("Leguminous Trees = ",signif(d.F.new %>% filter(year==2014) %>% pull(BA.legume) %>% max(),4)," m2/ha"),size=4)
y3c<-ggpubr::ggarrange(y3,y3,ncol=1,nrow=2,align="hv",common.legend=T,legend="bottom")

ggpubr::ggarrange(y4,y3c,ncol=2,nrow=1,align="hv",widths=c(2.1,1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/Landscape.Configuration.pdf",height=8,width=13)



