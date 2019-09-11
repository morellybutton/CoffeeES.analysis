#landscape optimisation of 10x10 pixel grid
library(tidyverse); library(nlme)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

d.F.new<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))

#for 2014 yield
tmp.14<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yld14.delta6.v3.confint.csv"))
#for 2015 yield difference
tmp.15<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff15.delta2.v3.confint.csv"))
#for 2016 yield difference
tmp.16<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_logylddiff16.delta2.v3.confint.csv"))

#create grid of set elevations
elev.min<-d.F.new %>% select(elevation) %>% min()
elev.max<-d.F.new %>% select(elevation) %>% max()

#create elevation windows up the mountainside
elev.steps<-seq(from=elev.min,to=elev.max,by=(elev.max-elev.min)/10)

row1<-runif(10, min = elev.steps[1], max = elev.steps[2])
row2<-runif(10, min = elev.steps[2], max = elev.steps[3])
row3<-runif(10, min = elev.steps[3], max = elev.steps[4])
row4<-runif(10, min = elev.steps[4], max = elev.steps[5])
row5<-runif(10, min = elev.steps[5], max = elev.steps[6])
row6<-runif(10, min = elev.steps[6], max = elev.steps[7])
row7<-runif(10, min = elev.steps[7], max = elev.steps[8])
row8<-runif(10, min = elev.steps[8], max = elev.steps[9])
row9<-runif(10, min = elev.steps[9], max = elev.steps[10])
row10<-runif(10, min = elev.steps[10], max = elev.steps[11])

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

ggplot(z_combo, aes( x,y, z = elevation)) +geom_raster(aes(fill=elevation)) +
  #scale_fill_gradientn(colours = rev(terrain.colors(20))) + 
  scale_fill_viridis_c()+ theme_classic() +
  labs(fill="Elevation (m)") + ggtitle("Theoretical Landscape (10 x 10 grid)") + theme(text=element_text(size=16))

#calculate rescaled values
elev.rescale<-d.F.new %>% filter(year==2014)  %>% mutate(elev.rescale=arm::rescale(elevation),mean=mean(elevation,na.rm=T),sd=sd(elevation,na.rm=T)) %>% 
  select(elevation,elev.rescale,mean,sd) %>% 
  mutate(test=(elevation-mean)/2/sd)
elev.vals<-elev.rescale %>% select(mean,sd) %>% summarise_all("mean")

z_combo <- z_combo %>% mutate(elev.rescale=(elevation-elev.vals$mean)/2/elev.vals$sd)
#equation for normal year
int14 <-tmp.14 %>% filter(X=="(Intercept)") %>% select(Estimate) %>% as.numeric()
shan.i <- tmp.14 %>% filter(X=="rescale(Shannon.i)") %>% select(Estimate) %>% as.numeric()
ba.l <- tmp.14 %>% filter(X=="rescale(BA.legume)") %>% select(Estimate) %>% as.numeric()
int.sb <- tmp.14 %>% filter(X=="rescale(BA.legume):rescale(Shannon.i)") %>% select(Estimate) %>% as.numeric()
p.tch <- tmp.14 %>% filter(X=="rescale(patcharea)") %>% select(Estimate) %>% as.numeric()
elev <- tmp.14 %>% filter(X=="rescale(elevation)") %>% select(Estimate) %>% as.numeric()


yld_norm <- function(x) { 
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  int14 + elev*x1 + shan.i*x2 + ba.l*x3 + int.sb*x3*x2 + p.tch*x4 
  }

betas <- d.F.new %>% filter(year==2014) %>% mutate(shannon.i=arm::rescale(Shannon.i),ba.legume=arm::rescale(BA.legume),
                                                   patch=arm::rescale(patcharea)) %>% 
  select(shannon.i,ba.legume,patch) %>% summarise_all(c("max","min"))

betas.sh <- seq(to=betas %>% select(shannon.i_max) %>% as.numeric(),
                from=betas %>% select(shannon.i_min) %>% as.numeric(),
                by=(betas %>% select(shannon.i_max) %>% as.numeric()-betas %>% select(shannon.i_min) %>% as.numeric())/10)
betas.ba <- seq(to=betas %>% select(ba.legume_max) %>% as.numeric(),
                from=betas %>% select(ba.legume_min) %>% as.numeric(),
                by=(betas %>% select(ba.legume_max) %>% as.numeric()-betas %>% select(ba.legume_min) %>% as.numeric())/10)
betas.pt <- seq(to=betas %>% select(patch_max) %>% as.numeric(),
                from=betas %>% select(patch_min) %>% as.numeric(),
                by=(betas %>% select(patch_max) %>% as.numeric()-betas %>% select(patch_min) %>% as.numeric())/10)

#practice on pixel [1,1] in the landscape
f1_1<-


f <- function(x) { stopifnot(is.numeric(x)) 
  stopifnot(is.finite(x)) 
  stopifnot(length(x) == 2) 
  x1 <- x[1]
  x2 <- x[2]
  sin(x1) + sin(x2) + cos(3 * x1 * x2) + x1^2 + x2^2 }
x <- seq(-2, 2, length = 101) 
xx <- rep(x, each = 101) 
head(xx)
yy <- rep(x, times = 101) 
head(yy)

zz <- apply(cbind(xx, yy), 1, f) 
class(zz)
zz <- matrix(zz, 101) 
contour(x, x, zz)