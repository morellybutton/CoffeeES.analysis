#code to calculate monthly average and sd for each plot over TerraClim record

library(tidyverse)
library(lubridate)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")

#open vpd, tmax, ppt, and pet datasets
vpd<-read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_vpd.csv"))
tmax<-read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_tmax.csv"))
pet<-read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_pet.csv"))
ppt<-read_csv(paste0(getwd(),"/Analysis/ElNino/TerraClim_precip.csv"))

#create month column and summarise average and sd over those
vpd$month<-month(vpd$Date)
tmax$month<-month(tmax$Date)
pet$month<-month(pet$Date)
ppt$month<-month(ppt$Date)

#for vpd
m_vpd <- vpd %>% group_by(month) %>% 
  summarise(m_B10=mean(B10,na.rm=T),m_B12=mean(B12,na.rm=T),m_B13=mean(B13,na.rm=T),m_B15=mean(B15,na.rm=T),m_B16=mean(B16,na.rm=T),m_B17=mean(B17,na.rm=T),
            m_B19=mean(B19,na.rm=T),m_B2=mean(B2,na.rm=T),m_B3=mean(B3,na.rm=T),m_B4=mean(B4,na.rm=T),m_B7=mean(B7,na.rm=T),m_B8=mean(B8,na.rm=T),m_BD1=mean(BD1,na.rm=T),
            m_BD2=mean(BD2,na.rm=T),m_BD3=mean(BD3,na.rm=T),m_BD4=mean(BD4,na.rm=T),m_FC1=mean(FC1,na.rm=T),m_FC2=mean(FC2,na.rm=T),m_GC1=mean(GC1,na.rm=T),m_GC2=mean(GC2,na.rm=T),
            m_GC3=mean(GC3,na.rm=T),m_GC4=mean(GC4,na.rm=T),m_GC5=mean(GC5,na.rm=T),m_GC6=mean(GC6,na.rm=T),m_GR1=mean(GR1,na.rm=T),m_GR2=mean(GR2,na.rm=T),m_H1=mean(H1,na.rm=T),
            m_H10=mean(H10,na.rm=T),m_H3=mean(H3,na.rm=T),m_H4=mean(H4,na.rm=T),m_H5=mean(H5,na.rm=T),m_H6=mean(H6,na.rm=T),m_H7=mean(H7,na.rm=T),m_H9=mean(H9,na.rm=T),m_H11=mean(H11,na.rm=T),
            m_W1=mean(W1,na.rm=T),m_W2=mean(W2,na.rm=T),m_W3=mean(W3,na.rm=T),m_W5=mean(W5,na.rm=T),m_W6=mean(W6,na.rm=T),m_WA2=mean(WA2,na.rm=T),m_WA3=mean(WA3,na.rm=T),m_WA4=mean(WA4,na.rm=T),
            m_WA5=mean(WA5,na.rm=T),m_WA6=mean(WA6,na.rm=T),m_WA7=mean(WA7,na.rm=T),m_WA8=mean(WA8,na.rm=T),m_WA9=mean(WA9,na.rm=T),m_WE2=mean(WE2,na.rm=T),m_WE3=mean(WE3,na.rm=T),m_WE4=mean(WE4,na.rm=T),
            m_WE5=mean(WE5,na.rm=T),m_WE6=mean(WE6,na.rm=T),m_Y1=mean(Y1,na.rm=T),m_Y2=mean(Y2,na.rm=T),m_Y3=mean(Y3,na.rm=T))

sd_vpd <- vpd %>% group_by(month) %>% 
  summarise(sd_B10=sd(B10,na.rm=T),sd_B12=sd(B12,na.rm=T),sd_B13=sd(B13,na.rm=T),sd_B15=sd(B15,na.rm=T),sd_B16=sd(B16,na.rm=T),sd_B17=sd(B17,na.rm=T),
            sd_B19=sd(B19,na.rm=T),sd_B2=sd(B2,na.rm=T),sd_B3=sd(B3,na.rm=T),sd_B4=sd(B4,na.rm=T),sd_B7=sd(B7,na.rm=T),sd_B8=sd(B8,na.rm=T),sd_BD1=sd(BD1,na.rm=T),
            sd_BD2=sd(BD2,na.rm=T),sd_BD3=sd(BD3,na.rm=T),sd_BD4=sd(BD4,na.rm=T),sd_FC1=sd(FC1,na.rm=T),sd_FC2=sd(FC2,na.rm=T),sd_GC1=sd(GC1,na.rm=T),sd_GC2=sd(GC2,na.rm=T),
            sd_GC3=sd(GC3,na.rm=T),sd_GC4=sd(GC4,na.rm=T),sd_GC5=sd(GC5,na.rm=T),sd_GC6=sd(GC6,na.rm=T),sd_GR1=sd(GR1,na.rm=T),sd_GR2=sd(GR2,na.rm=T),sd_H1=sd(H1,na.rm=T),
            sd_H10=sd(H10,na.rm=T),sd_H3=sd(H3,na.rm=T),sd_H4=sd(H4,na.rm=T),sd_H5=sd(H5,na.rm=T),sd_H6=sd(H6,na.rm=T),sd_H7=sd(H7,na.rm=T),sd_H9=sd(H9,na.rm=T),sd_H11=sd(H11,na.rm=T),
            sd_W1=sd(W1,na.rm=T),sd_W2=sd(W2,na.rm=T),sd_W3=sd(W3,na.rm=T),sd_W5=sd(W5,na.rm=T),sd_W6=sd(W6,na.rm=T),sd_WA2=sd(WA2,na.rm=T),sd_WA3=sd(WA3,na.rm=T),sd_WA4=sd(WA4,na.rm=T),
            sd_WA5=sd(WA5,na.rm=T),sd_WA6=sd(WA6,na.rm=T),sd_WA7=sd(WA7,na.rm=T),sd_WA8=sd(WA8,na.rm=T),sd_WA9=sd(WA9,na.rm=T),sd_WE2=sd(WE2,na.rm=T),sd_WE3=sd(WE3,na.rm=T),sd_WE4=sd(WE4,na.rm=T),
            sd_WE5=sd(WE5,na.rm=T),sd_WE6=sd(WE6,na.rm=T),sd_Y1=sd(Y1,na.rm=T),sd_Y2=sd(Y2,na.rm=T),sd_Y3=sd(Y3,na.rm=T))

vpd1<-left_join(vpd,m_vpd,by="month")
vpd1<-left_join(vpd1,sd_vpd,by="month")

#for tmax
m_tmax <- tmax %>% group_by(month) %>% 
  summarise(m_B10=mean(B10,na.rm=T),m_B12=mean(B12,na.rm=T),m_B13=mean(B13,na.rm=T),m_B15=mean(B15,na.rm=T),m_B16=mean(B16,na.rm=T),m_B17=mean(B17,na.rm=T),
            m_B19=mean(B19,na.rm=T),m_B2=mean(B2,na.rm=T),m_B3=mean(B3,na.rm=T),m_B4=mean(B4,na.rm=T),m_B7=mean(B7,na.rm=T),m_B8=mean(B8,na.rm=T),m_BD1=mean(BD1,na.rm=T),
            m_BD2=mean(BD2,na.rm=T),m_BD3=mean(BD3,na.rm=T),m_BD4=mean(BD4,na.rm=T),m_FC1=mean(FC1,na.rm=T),m_FC2=mean(FC2,na.rm=T),m_GC1=mean(GC1,na.rm=T),m_GC2=mean(GC2,na.rm=T),
            m_GC3=mean(GC3,na.rm=T),m_GC4=mean(GC4,na.rm=T),m_GC5=mean(GC5,na.rm=T),m_GC6=mean(GC6,na.rm=T),m_GR1=mean(GR1,na.rm=T),m_GR2=mean(GR2,na.rm=T),m_H1=mean(H1,na.rm=T),
            m_H10=mean(H10,na.rm=T),m_H3=mean(H3,na.rm=T),m_H4=mean(H4,na.rm=T),m_H5=mean(H5,na.rm=T),m_H6=mean(H6,na.rm=T),m_H7=mean(H7,na.rm=T),m_H9=mean(H9,na.rm=T),m_H11=mean(H11,na.rm=T),
            m_W1=mean(W1,na.rm=T),m_W2=mean(W2,na.rm=T),m_W3=mean(W3,na.rm=T),m_W5=mean(W5,na.rm=T),m_W6=mean(W6,na.rm=T),m_WA2=mean(WA2,na.rm=T),m_WA3=mean(WA3,na.rm=T),m_WA4=mean(WA4,na.rm=T),
            m_WA5=mean(WA5,na.rm=T),m_WA6=mean(WA6,na.rm=T),m_WA7=mean(WA7,na.rm=T),m_WA8=mean(WA8,na.rm=T),m_WA9=mean(WA9,na.rm=T),m_WE2=mean(WE2,na.rm=T),m_WE3=mean(WE3,na.rm=T),m_WE4=mean(WE4,na.rm=T),
            m_WE5=mean(WE5,na.rm=T),m_WE6=mean(WE6,na.rm=T),m_Y1=mean(Y1,na.rm=T),m_Y2=mean(Y2,na.rm=T),m_Y3=mean(Y3,na.rm=T))

sd_tmax <- tmax %>% group_by(month) %>% 
  summarise(sd_B10=sd(B10,na.rm=T),sd_B12=sd(B12,na.rm=T),sd_B13=sd(B13,na.rm=T),sd_B15=sd(B15,na.rm=T),sd_B16=sd(B16,na.rm=T),sd_B17=sd(B17,na.rm=T),
            sd_B19=sd(B19,na.rm=T),sd_B2=sd(B2,na.rm=T),sd_B3=sd(B3,na.rm=T),sd_B4=sd(B4,na.rm=T),sd_B7=sd(B7,na.rm=T),sd_B8=sd(B8,na.rm=T),sd_BD1=sd(BD1,na.rm=T),
            sd_BD2=sd(BD2,na.rm=T),sd_BD3=sd(BD3,na.rm=T),sd_BD4=sd(BD4,na.rm=T),sd_FC1=sd(FC1,na.rm=T),sd_FC2=sd(FC2,na.rm=T),sd_GC1=sd(GC1,na.rm=T),sd_GC2=sd(GC2,na.rm=T),
            sd_GC3=sd(GC3,na.rm=T),sd_GC4=sd(GC4,na.rm=T),sd_GC5=sd(GC5,na.rm=T),sd_GC6=sd(GC6,na.rm=T),sd_GR1=sd(GR1,na.rm=T),sd_GR2=sd(GR2,na.rm=T),sd_H1=sd(H1,na.rm=T),
            sd_H10=sd(H10,na.rm=T),sd_H3=sd(H3,na.rm=T),sd_H4=sd(H4,na.rm=T),sd_H5=sd(H5,na.rm=T),sd_H6=sd(H6,na.rm=T),sd_H7=sd(H7,na.rm=T),sd_H9=sd(H9,na.rm=T),sd_H11=sd(H11,na.rm=T),
            sd_W1=sd(W1,na.rm=T),sd_W2=sd(W2,na.rm=T),sd_W3=sd(W3,na.rm=T),sd_W5=sd(W5,na.rm=T),sd_W6=sd(W6,na.rm=T),sd_WA2=sd(WA2,na.rm=T),sd_WA3=sd(WA3,na.rm=T),sd_WA4=sd(WA4,na.rm=T),
            sd_WA5=sd(WA5,na.rm=T),sd_WA6=sd(WA6,na.rm=T),sd_WA7=sd(WA7,na.rm=T),sd_WA8=sd(WA8,na.rm=T),sd_WA9=sd(WA9,na.rm=T),sd_WE2=sd(WE2,na.rm=T),sd_WE3=sd(WE3,na.rm=T),sd_WE4=sd(WE4,na.rm=T),
            sd_WE5=sd(WE5,na.rm=T),sd_WE6=sd(WE6,na.rm=T),sd_Y1=sd(Y1,na.rm=T),sd_Y2=sd(Y2,na.rm=T),sd_Y3=sd(Y3,na.rm=T))

tmax1<-left_join(tmax,m_tmax,by="month")
tmax1<-left_join(tmax1,sd_tmax,by="month")

#for pet
m_pet <- pet %>% group_by(month) %>% 
  summarise(m_B10=mean(B10,na.rm=T),m_B12=mean(B12,na.rm=T),m_B13=mean(B13,na.rm=T),m_B15=mean(B15,na.rm=T),m_B16=mean(B16,na.rm=T),m_B17=mean(B17,na.rm=T),
            m_B19=mean(B19,na.rm=T),m_B2=mean(B2,na.rm=T),m_B3=mean(B3,na.rm=T),m_B4=mean(B4,na.rm=T),m_B7=mean(B7,na.rm=T),m_B8=mean(B8,na.rm=T),m_BD1=mean(BD1,na.rm=T),
            m_BD2=mean(BD2,na.rm=T),m_BD3=mean(BD3,na.rm=T),m_BD4=mean(BD4,na.rm=T),m_FC1=mean(FC1,na.rm=T),m_FC2=mean(FC2,na.rm=T),m_GC1=mean(GC1,na.rm=T),m_GC2=mean(GC2,na.rm=T),
            m_GC3=mean(GC3,na.rm=T),m_GC4=mean(GC4,na.rm=T),m_GC5=mean(GC5,na.rm=T),m_GC6=mean(GC6,na.rm=T),m_GR1=mean(GR1,na.rm=T),m_GR2=mean(GR2,na.rm=T),m_H1=mean(H1,na.rm=T),
            m_H10=mean(H10,na.rm=T),m_H3=mean(H3,na.rm=T),m_H4=mean(H4,na.rm=T),m_H5=mean(H5,na.rm=T),m_H6=mean(H6,na.rm=T),m_H7=mean(H7,na.rm=T),m_H9=mean(H9,na.rm=T),m_H11=mean(H11,na.rm=T),
            m_W1=mean(W1,na.rm=T),m_W2=mean(W2,na.rm=T),m_W3=mean(W3,na.rm=T),m_W5=mean(W5,na.rm=T),m_W6=mean(W6,na.rm=T),m_WA2=mean(WA2,na.rm=T),m_WA3=mean(WA3,na.rm=T),m_WA4=mean(WA4,na.rm=T),
            m_WA5=mean(WA5,na.rm=T),m_WA6=mean(WA6,na.rm=T),m_WA7=mean(WA7,na.rm=T),m_WA8=mean(WA8,na.rm=T),m_WA9=mean(WA9,na.rm=T),m_WE2=mean(WE2,na.rm=T),m_WE3=mean(WE3,na.rm=T),m_WE4=mean(WE4,na.rm=T),
            m_WE5=mean(WE5,na.rm=T),m_WE6=mean(WE6,na.rm=T),m_Y1=mean(Y1,na.rm=T),m_Y2=mean(Y2,na.rm=T),m_Y3=mean(Y3,na.rm=T))

sd_pet <- pet %>% group_by(month) %>% 
  summarise(sd_B10=sd(B10,na.rm=T),sd_B12=sd(B12,na.rm=T),sd_B13=sd(B13,na.rm=T),sd_B15=sd(B15,na.rm=T),sd_B16=sd(B16,na.rm=T),sd_B17=sd(B17,na.rm=T),
            sd_B19=sd(B19,na.rm=T),sd_B2=sd(B2,na.rm=T),sd_B3=sd(B3,na.rm=T),sd_B4=sd(B4,na.rm=T),sd_B7=sd(B7,na.rm=T),sd_B8=sd(B8,na.rm=T),sd_BD1=sd(BD1,na.rm=T),
            sd_BD2=sd(BD2,na.rm=T),sd_BD3=sd(BD3,na.rm=T),sd_BD4=sd(BD4,na.rm=T),sd_FC1=sd(FC1,na.rm=T),sd_FC2=sd(FC2,na.rm=T),sd_GC1=sd(GC1,na.rm=T),sd_GC2=sd(GC2,na.rm=T),
            sd_GC3=sd(GC3,na.rm=T),sd_GC4=sd(GC4,na.rm=T),sd_GC5=sd(GC5,na.rm=T),sd_GC6=sd(GC6,na.rm=T),sd_GR1=sd(GR1,na.rm=T),sd_GR2=sd(GR2,na.rm=T),sd_H1=sd(H1,na.rm=T),
            sd_H10=sd(H10,na.rm=T),sd_H3=sd(H3,na.rm=T),sd_H4=sd(H4,na.rm=T),sd_H5=sd(H5,na.rm=T),sd_H6=sd(H6,na.rm=T),sd_H7=sd(H7,na.rm=T),sd_H9=sd(H9,na.rm=T),sd_H11=sd(H11,na.rm=T),
            sd_W1=sd(W1,na.rm=T),sd_W2=sd(W2,na.rm=T),sd_W3=sd(W3,na.rm=T),sd_W5=sd(W5,na.rm=T),sd_W6=sd(W6,na.rm=T),sd_WA2=sd(WA2,na.rm=T),sd_WA3=sd(WA3,na.rm=T),sd_WA4=sd(WA4,na.rm=T),
            sd_WA5=sd(WA5,na.rm=T),sd_WA6=sd(WA6,na.rm=T),sd_WA7=sd(WA7,na.rm=T),sd_WA8=sd(WA8,na.rm=T),sd_WA9=sd(WA9,na.rm=T),sd_WE2=sd(WE2,na.rm=T),sd_WE3=sd(WE3,na.rm=T),sd_WE4=sd(WE4,na.rm=T),
            sd_WE5=sd(WE5,na.rm=T),sd_WE6=sd(WE6,na.rm=T),sd_Y1=sd(Y1,na.rm=T),sd_Y2=sd(Y2,na.rm=T),sd_Y3=sd(Y3,na.rm=T))

pet1<-left_join(pet,m_pet,by="month")
pet1<-left_join(pet1,sd_pet,by="month")

#for ppt
m_ppt <- ppt %>% group_by(month) %>% 
  summarise(m_B10=mean(B10,na.rm=T),m_B12=mean(B12,na.rm=T),m_B13=mean(B13,na.rm=T),m_B15=mean(B15,na.rm=T),m_B16=mean(B16,na.rm=T),m_B17=mean(B17,na.rm=T),
            m_B19=mean(B19,na.rm=T),m_B2=mean(B2,na.rm=T),m_B3=mean(B3,na.rm=T),m_B4=mean(B4,na.rm=T),m_B7=mean(B7,na.rm=T),m_B8=mean(B8,na.rm=T),m_BD1=mean(BD1,na.rm=T),
            m_BD2=mean(BD2,na.rm=T),m_BD3=mean(BD3,na.rm=T),m_BD4=mean(BD4,na.rm=T),m_FC1=mean(FC1,na.rm=T),m_FC2=mean(FC2,na.rm=T),m_GC1=mean(GC1,na.rm=T),m_GC2=mean(GC2,na.rm=T),
            m_GC3=mean(GC3,na.rm=T),m_GC4=mean(GC4,na.rm=T),m_GC5=mean(GC5,na.rm=T),m_GC6=mean(GC6,na.rm=T),m_GR1=mean(GR1,na.rm=T),m_GR2=mean(GR2,na.rm=T),m_H1=mean(H1,na.rm=T),
            m_H10=mean(H10,na.rm=T),m_H3=mean(H3,na.rm=T),m_H4=mean(H4,na.rm=T),m_H5=mean(H5,na.rm=T),m_H6=mean(H6,na.rm=T),m_H7=mean(H7,na.rm=T),m_H9=mean(H9,na.rm=T),m_H11=mean(H11,na.rm=T),
            m_W1=mean(W1,na.rm=T),m_W2=mean(W2,na.rm=T),m_W3=mean(W3,na.rm=T),m_W5=mean(W5,na.rm=T),m_W6=mean(W6,na.rm=T),m_WA2=mean(WA2,na.rm=T),m_WA3=mean(WA3,na.rm=T),m_WA4=mean(WA4,na.rm=T),
            m_WA5=mean(WA5,na.rm=T),m_WA6=mean(WA6,na.rm=T),m_WA7=mean(WA7,na.rm=T),m_WA8=mean(WA8,na.rm=T),m_WA9=mean(WA9,na.rm=T),m_WE2=mean(WE2,na.rm=T),m_WE3=mean(WE3,na.rm=T),m_WE4=mean(WE4,na.rm=T),
            m_WE5=mean(WE5,na.rm=T),m_WE6=mean(WE6,na.rm=T),m_Y1=mean(Y1,na.rm=T),m_Y2=mean(Y2,na.rm=T),m_Y3=mean(Y3,na.rm=T))

sd_ppt <- ppt %>% group_by(month) %>% 
  summarise(sd_B10=sd(B10,na.rm=T),sd_B12=sd(B12,na.rm=T),sd_B13=sd(B13,na.rm=T),sd_B15=sd(B15,na.rm=T),sd_B16=sd(B16,na.rm=T),sd_B17=sd(B17,na.rm=T),
            sd_B19=sd(B19,na.rm=T),sd_B2=sd(B2,na.rm=T),sd_B3=sd(B3,na.rm=T),sd_B4=sd(B4,na.rm=T),sd_B7=sd(B7,na.rm=T),sd_B8=sd(B8,na.rm=T),sd_BD1=sd(BD1,na.rm=T),
            sd_BD2=sd(BD2,na.rm=T),sd_BD3=sd(BD3,na.rm=T),sd_BD4=sd(BD4,na.rm=T),sd_FC1=sd(FC1,na.rm=T),sd_FC2=sd(FC2,na.rm=T),sd_GC1=sd(GC1,na.rm=T),sd_GC2=sd(GC2,na.rm=T),
            sd_GC3=sd(GC3,na.rm=T),sd_GC4=sd(GC4,na.rm=T),sd_GC5=sd(GC5,na.rm=T),sd_GC6=sd(GC6,na.rm=T),sd_GR1=sd(GR1,na.rm=T),sd_GR2=sd(GR2,na.rm=T),sd_H1=sd(H1,na.rm=T),
            sd_H10=sd(H10,na.rm=T),sd_H3=sd(H3,na.rm=T),sd_H4=sd(H4,na.rm=T),sd_H5=sd(H5,na.rm=T),sd_H6=sd(H6,na.rm=T),sd_H7=sd(H7,na.rm=T),sd_H9=sd(H9,na.rm=T),sd_H11=sd(H11,na.rm=T),
            sd_W1=sd(W1,na.rm=T),sd_W2=sd(W2,na.rm=T),sd_W3=sd(W3,na.rm=T),sd_W5=sd(W5,na.rm=T),sd_W6=sd(W6,na.rm=T),sd_WA2=sd(WA2,na.rm=T),sd_WA3=sd(WA3,na.rm=T),sd_WA4=sd(WA4,na.rm=T),
            sd_WA5=sd(WA5,na.rm=T),sd_WA6=sd(WA6,na.rm=T),sd_WA7=sd(WA7,na.rm=T),sd_WA8=sd(WA8,na.rm=T),sd_WA9=sd(WA9,na.rm=T),sd_WE2=sd(WE2,na.rm=T),sd_WE3=sd(WE3,na.rm=T),sd_WE4=sd(WE4,na.rm=T),
            sd_WE5=sd(WE5,na.rm=T),sd_WE6=sd(WE6,na.rm=T),sd_Y1=sd(Y1,na.rm=T),sd_Y2=sd(Y2,na.rm=T),sd_Y3=sd(Y3,na.rm=T))

ppt1<-left_join(ppt,m_ppt,by="month")
ppt1<-left_join(ppt1,sd_ppt,by="month")


