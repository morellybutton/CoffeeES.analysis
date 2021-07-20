library(lubridate); library(viridis); library(mgcv);
library(rstanarm); library(tidyverse); library(loo); 
options(mc.cores = parallel::detectCores()-1)
library(RcppRoll); library(visreg);
library(plantecophys)

setwd("/Users/AMOREL001/Google Drive/Research/Africa/ECOLIMITS1/ECOLIMITS2019/Yayu/")

# Functions ---------------------------------------------------------------
#taken from FAO (http://www.fao.org/3/X0490E/x0490e07.htm) and originally 
#Murray FW (1967) On the computation of saturation vapor pressure. J. Appl. Meteorol. 6: 203-204
#calc_vpd <- function(d2m_C, t2m_C){
#  e_a <- 0.6108*exp(17.27*d2m_C/(d2m_C+237.3))
#  e_s <- 0.6108*exp(17.27*t2m_C/(t2m_C+237.3))
#  vpd_kPa <- e_s - e_a
#  return(vpd_kPa)
#}

# --- Data prep ----------------------------------------------------------------------------------
clim <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_TMAX_YAYU.csv"), guess_max = 1e5) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Plot,Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(tmax=mean-273.15) %>% 
  group_by(Plot) %>% 
  arrange(Date) %>% 
  mutate(mx_tmax = roll_maxr(tmax, n=12, fill=NA)) %>% select(-mean)
tmp <- clim %>% filter(year>=1980 & year <= 2010) %>% 
  group_by(Plot,month) %>% 
  summarize(u_tmax = mean(tmax, na.rm=T), 
            u_mx_tmax = mean(mx_tmax, na.rm=T))
clim <- left_join(clim, tmp, by=c("Plot","month"))
clim <- clim %>% mutate(tmax_anom = tmax-u_tmax, 
                        mx_tmax_anom = mx_tmax - u_mx_tmax) 

dwpt <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_DWPT_YAYU.csv")) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Plot,Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(dwpt=mean-273.15) %>% 
  group_by(Plot) %>% 
  arrange(Date) %>% 
  mutate(mn_dwpt = roll_meanr(dwpt, n=12, fill=NA)) %>% select(-mean)

tmean <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_TMEAN_YAYU.csv")) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Plot,Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(tmean=mean-273.15) %>% 
  group_by(Plot) %>% 
  arrange(Date) %>% 
  mutate(mn_tmean = roll_meanr(tmean, n=12, fill=NA)) %>% select(-mean)
tmp <- tmean %>% filter(year>=1980 & year <= 2010) %>% 
  group_by(Plot,month) %>% 
  summarize(u_tmean = mean(tmean, na.rm=T), 
            u_mx_tmean = mean(mn_tmean, na.rm=T))
tmean <- left_join(tmean, tmp, by=c("Plot","month"))
clim <- left_join(clim,tmean, by=c("Plot","Date","month","year"))

kpa <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_PRES_YAYU.csv")) %>%
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  select(Plot,Date,mean,year,month) %>% 
  #gather(key="Plot",value="mean",-Date,-year,-month) %>% 
  mutate(kpa=mean/1000) %>% 
  group_by(Plot) %>% 
  arrange(Date) %>% 
  mutate(mn_kpa = roll_meanr(kpa, n=12, fill=NA)) %>% select(-mean)

#calculated using plantecophys package
vpd <- tibble(DewtoVPD(Tdew=dwpt$dwpt,TdegC=tmean$tmean,Pa=kpa$kpa))
colnames(vpd)<-"vpd"
vpd<-bind_cols(dwpt %>% select(Plot,Date,year,month),vpd)

tmp <- vpd %>% filter(year>=1980 & year <= 2017) %>% 
  group_by(Plot,month) %>% 
  summarize(u_vpd = mean(vpd, na.rm=T)) #to get kPa
vpd <- left_join(vpd,tmp,by=c("Plot","month"))

clim <- left_join(clim,vpd, by=c("Plot","Date","month","year"))

#pet <- read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_pet.csv")) %>% 
#  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
#  gather(key="site",value="pet",-Date,-year,-month) %>% mutate(pet=pet/10)
#tmp <- pet %>% filter(year>=1986 & year <= 2017) %>% 
#  group_by(site,month) %>% 
#  summarize(u_pet = mean(pet, na.rm=T))
#pet <- left_join(pet,tmp,by=c("site","month"))

#clim <- left_join(clim,pet, by=c("site","Date","month","year"))

ppt <- read_csv(paste0(getwd(),"/Analysis/ElNino/ERA5_PPT_YAYU.csv")) %>% 
  select(Plot,Date,mean) %>% 
  mutate(Date=as.Date(Date,format="%d/%m/%Y")) %>% mutate(year=year(Date),month=month(Date)) %>%
  mutate(ppt=mean*1000) %>% mutate(wd=ppt-55) %>% mutate(wd=replace(wd,wd>0,0)) %>% 
  select(-mean)
  #gather(key="Plot",value="ppt",-Date,-year,-month)
tmp <- ppt %>% filter(year>=1980 & year <= 2017) %>% 
  group_by(Plot,month) %>% 
  summarize(u_precip = mean(ppt, na.rm=T),
            u_wd = mean(wd,na.rm=T))
ppt <- left_join(ppt,tmp,by=c("Plot","month"))

clim <- left_join(clim,ppt, by=c("Plot","Date","month","year"))
clim <- clim %>% mutate(tmean_anom = tmean-u_tmean,vpd_anom = vpd-u_vpd,precip_anom = ppt-u_precip,wd_anom=wd-u_wd)

tmp <- clim %>% filter(year>=1980 & year <= 2016) %>% 
  group_by(Plot, month) %>% 
  summarize(vpd_sigma = sd(vpd, na.rm=T),
            tmean_sigma = sd(tmean, na.rm=T), 
            precip_sigma = sd(ppt, na.rm=T),
            mx_tmax_sigma = sd(mx_tmax, na.rm=T),
            tmax_sigma=sd(tmax,na.rm=T),
            wd_sigma=sd(wd,na.rm=T))
clim <- left_join(clim, tmp, by=c('Plot','month'))

clim <- clim %>% 
  #mutate(u_p_et = u_precip/pet) %>%
  #mutate(p_et = ppt/pet) %>%
  #mutate(u_season = ifelse(u_precip>pet, ("wet"), ("dry")),
  #       season = ifelse(ppt>pet, ("wet"), ("dry")), 
  #       p_et100 = ifelse(ppt>100,T,F), 
   #      p_et_bin = ifelse(ppt>pet, T, F), 
   #      u_p_et_bin = ifelse(u_precip >= pet, T, F)) %>% 
  mutate(tmax_anom_sigma = tmax_anom/tmax_sigma,
         vpd_anom_sigma = vpd_anom/vpd_sigma, 
         precip_anom_sigma = precip_anom/precip_sigma,
         tmean_anom_sigma = tmean_anom/tmean_sigma,
         wd_anom_sigma = wd_anom/wd_sigma)
         #pet_anom_sigma = pet_anom/pet_sigma)
clim <- clim %>% group_by(Plot) %>% arrange(Date) %>% 
  mutate(tmax_anom_sigma_3mo = roll_meanr(tmax_anom_sigma, n=3),
         vpd_anom_sigma_3mo = roll_meanr(vpd_anom_sigma, n=3),
         precip_anom_sigma_3mo = roll_meanr(precip_anom_sigma, n=3),
         tmean_anom_sigma_3mo = roll_meanr(tmean_anom_sigma, n=3),
         wd_anom_sigma_3mo = roll_meanr(wd_anom_sigma, n=3)) %>% 
         #pet_anom_sigma_3mo = roll_meanr(pet_anom_sigma, n=3)) %>% 
  ungroup()

write_csv(clim,paste0(getwd(),"/Analysis/ElNino/era5_anomalies.csv"))

#extract relevant time periods for yield modelling, years 2014-2016
#flowering = Jan-March
#fruiting = July-Oct
#yield.tc <- clim %>% filter(year>=2014,month<3|month>6&month<11) %>% 
#  mutate(flowering=1,flowering=replace(flowering,month>3,0)) %>%
#  mutate(fruiting=1,fruiting=replace(fruiting,month<7|month>10,0)) %>%
#  group_by(year,site,fruiting,flowering) %>%
#  summarise(tmax=mean(tmax,na.rm=T),vpd=mean(vpd,na.rm=T),
#            tmax_anom=mean(tmax_anom,na.rm=T),vpd_anom=mean(vpd_anom,na.rm=T),
#            p_et=mean(p_et,na.rm=T),u_p_et=mean(u_p_et,na.rm=T)) %>% ungroup()

#yield.flower<-yield.tc %>% filter(flowering==1) %>% select(-fruiting,-flowering) %>%
#  rename(tmax.flower=tmax,vpd.flower=vpd,tmax.anom.flower=tmax_anom,
#         vpd_anom.flower=vpd_anom,p_et.flower=p_et,u_p_et.flower=u_p_et)
#
#yield.fruit<-yield.tc %>% filter(fruiting==1) %>% select(-fruiting,-flowering) %>%
#  rename(tmax.fruit=tmax,vpd.fruit=vpd,tmax.anom.fruit=tmax_anom,
 #        vpd_anom.fruit=vpd_anom,p_et.fruit=p_et,u_p_et.fruit=u_p_et)

#yield.all<-left_join(yield.flower,yield.fruit, by=c("site","year"))
#write_csv(yield.all,paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies4yieldmodel.csv"))

