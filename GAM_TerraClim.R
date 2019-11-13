#analysis to analyze trend using
setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu")

library(tidyverse)
library(mgcv)

terra_clim<-read_csv(paste0(getwd(),"/Analysis/ElNino/terraclim_anomalies.csv"))
#open ONI values
oni<-read_csv("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/ONI.csv")
#add "month" value
oni <- oni %>% mutate(Date=as.Date(paste0(PeriodNum,"-01"),format="%Y-%m-%d")) %>% rename(oni=NOAA)

terra_clim<-left_join(terra_clim,oni %>% select(Date,oni),by="Date")

#add in Time variable
terra_clim<-transform(terra_clim, Time = as.numeric(Date))
#ylab <- expression(Temperature ~ (degree*C))

#Start with Temperature
dF<-terra_clim  %>% filter(site=="B13"&!is.na(tmax_anom_sigma_3mo))

p1 <- ggplot(dF, aes(x = Date, y = tmax_anom_sigma_3mo)) +
  geom_point()
p1 + geom_line()


## AR(1)
m1 <- gamm(tmax_anom_sigma_3mo ~ s(Time),
                           data = dF, correlation = corARMA(form = ~ 1|year, p = 1))

## AR(2)
m2 <- gamm(tmax_anom_sigma_3mo ~s(Time),
                           data = dF, correlation = corARMA(form = ~ 1|year, p = 2))


anova(m1$lme, m2$lme)

summary(m2$gam)

ACF <- acf(resid(m2$lme, type = "normalized"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))

#Before drawing the fitted trend, I want to put a simultaneous confidence interval around the estimate. 
#mgcv makes this very easy to do via posterior simulation
tmpf <- tempfile()
curl::curl_download("https://gist.githubusercontent.com/gavinsimpson/d23ae67e653d5bfff652/raw/25fd719c3ab699e48927e286934045622d33b3bf/simulate.gamm.R", tmpf)
source(tmpf)

set.seed(10)
newd <- with(dF, data.frame(Time = seq(min(Time), max(Time), length.out = 200)))
sims <- simulate(m2, nsim = 10000, newdata = newd)

ci <- apply(sims, 1L, quantile, probs = c(0.025, 0.975))
newd <- transform(newd,
                  fitted = predict(m2$gam, newdata = newd),
                  lower  = ci[1, ],
                  upper  = ci[2, ])
#Having arranged the fitted values and upper and lower simultaneous confidence intervals tidily they can be 
#added easily to the existing plot of the data
newd$Date<-as.Date(newd$Time,origin="1970-01-01")
p1 + geom_ribbon(data = newd, aes(ymin = lower, ymax = upper, x = Date, y = fitted),
                 alpha = 0.2, fill = "grey") +
  geom_line(data = newd, aes(y = fitted, x = Date))

r_squared1<-signif(summary(m2$gam)$r.sq,3)
mylabel = bquote(italic(R)^2 == .(format(r_squared1, digits = 3)))

z1<-ggplot() + geom_bar(data=dF,aes(Date,tmax_anom_sigma_3mo,fill=oni),stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Maximum Temperature Anomalies") +
  geom_ribbon(data = newd, aes(ymin = lower, ymax = upper, x = Date, y = fitted),
              alpha = 0.2, fill = "grey") +
  geom_line(data = newd, aes(y = fitted, x = Date),size=1)  +
  annotate("text",x=as.Date("2013-12-01"),y=-2,label=mylabel,  size=6) +
  theme(text=element_text(size=18),legend.title.align=0.5 ) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex")

#To get a sense of the uncertainty in the shapes of the simulated trends we can plot 
#some of the draws from the posterior distribution of the model
set.seed(42)
S <- 50
sims2 <- setNames(data.frame(sims[, sample(10000, S)]), paste0("sim", seq_len(S)))
sims2 <- setNames(stack(sims2), c("tmax_anom_sigma_3mo", "Simulation"))
sims2 <- transform(sims2, Time = rep(newd$Time, S))

sims2$Date<-as.Date(sims2$Time,origin="1970-01-01")
ggplot(sims2, aes(x = Date, y = tmax_anom_sigma_3mo, group = Simulation)) +
  geom_line(alpha = 0.3)

#An instructive visualisation for the period of the purported pause or 
#hiatus in global warming is to look at the shapes of the posterior simulations and the slopes of the trends for each year.
#I first look at the posterior simulations:

ggplot(sims2, aes(x = Date, y = tmax_anom_sigma_3mo, group = Simulation)) +
  geom_line(alpha = 0.5) #+ xlim(c(1995, 2015)) + ylim(c(0.2, 0.75))


tmpf <- tempfile()
curl::curl_download("https://gist.githubusercontent.com/gavinsimpson/ca18c9c789ef5237dbc6/raw/295fc5cf7366c831ab166efaee42093a80622fa8/derivSimulCI.R", tmpf)
source(tmpf)

fd <- derivSimulCI(m2, samples = 10000, n = 200)

#Loading required package: MASS

CI <- apply(fd[[1]]$simulations, 1, quantile, probs = c(0.025, 0.975))
sigD <- signifD(fd[["Time"]]$deriv, fd[["Time"]]$deriv, CI[2, ], CI[1, ],
                eval = 0)
newd <- transform(newd,
                  derivative = fd[["Time"]]$deriv[, 1], # computed first derivative
                  fdUpper = CI[2, ],                    # upper CI on first deriv
                  fdLower = CI[1, ],                    # lower CI on first deriv
                  increasing = sigD$incr,               # where is curve increasing?
                  decreasing = sigD$decr)               # ... or decreasing?

#A ggplot2 version of the derivatives is produced using the code below. 
#The two additional geom_line() calls add thick lines over sections of the derivative plot 
#to illustrate those points where zero is not contained within the confidence interval of the first derivative.

ggplot(newd, aes(x = Date, y = derivative)) +
  geom_ribbon(aes(ymax = fdUpper, ymin = fdLower), alpha = 0.3, fill = "grey") +
  geom_line() +
  geom_line(aes(y = increasing), size = 1.5) +
  geom_line(aes(y = decreasing), size = 1.5) +
  ylab(expression(italic(hat(f) * "'") * (Year))) +
  xlab("Year")


#do for precipitation

p2 <- ggplot(dF, aes(x = Date, y = precip_anom_sigma_3mo )) +
  geom_point()
p2 + geom_line()


## AR(1)
m3 <- gamm(precip_anom_sigma_3mo  ~ s(Time),
           data = dF, correlation = corARMA(form = ~ 1|year, p = 1))

## AR(2)
m4 <- gamm(precip_anom_sigma_3mo  ~s(Time),
           data = dF, correlation = corARMA(form = ~ 1|year, p = 2))


anova(m3$lme, m4$lme)

summary(m4$gam)

ACF <- acf(resid(m4$lme, type = "normalized"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))

#Before drawing the fitted trend, I want to put a simultaneous confidence interval around the estimate. 
#mgcv makes this very easy to do via posterior simulation
tmpf <- tempfile()
curl::curl_download("https://gist.githubusercontent.com/gavinsimpson/d23ae67e653d5bfff652/raw/25fd719c3ab699e48927e286934045622d33b3bf/simulate.gamm.R", tmpf)
source(tmpf)

set.seed(10)
newd1 <- with(dF, data.frame(Time = seq(min(Time), max(Time), length.out = 200)))
sims <- simulate(m4, nsim = 10000, newdata = newd1)

ci <- apply(sims, 1L, quantile, probs = c(0.025, 0.975))
newd1 <- transform(newd1,
                  fitted = predict(m4$gam, newdata = newd1),
                  lower  = ci[1, ],
                  upper  = ci[2, ])
#Having arranged the fitted values and upper and lower simultaneous confidence intervals tidily they can be 
#added easily to the existing plot of the data
newd1$Date<-as.Date(newd1$Time,origin="1970-01-01")
p2 + geom_ribbon(data = newd1, aes(ymin = lower, ymax = upper, x = Date, y = fitted),
                 alpha = 0.2, fill = "grey") +
  geom_line(data = newd1, aes(y = fitted, x = Date))

r_squared2<-signif(summary(m4$gam)$r.sq,3)
mylabel2 = bquote(italic(R)^2 == .(format(r_squared2, digits = 3)))

z3<-ggplot() + geom_bar(data=dF,aes(Date,precip_anom_sigma_3mo,fill=oni),stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Precipitation Anomalies") +
  geom_ribbon(data = newd1, aes(ymin = lower, ymax = upper, x = Date, y = fitted),
              alpha = 0.2, fill = "grey") +
  geom_line(data = newd1, aes(y = fitted, x = Date),size=1)  +
  annotate("text",x=as.Date("2013-12-01"),y=2,label=mylabel2,  size=6) +
  theme(text=element_text(size=16),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") 

ggpubr::ggarrange(z3,z1,ncol=1,nrow=2,align="hv",labels="auto",common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/TerraClim.StdAnom.GAM.Comparison.pdf",height=9,width=12)

#To get a sense of the uncertainty in the shapes of the simulated trends we can plot 
#some of the draws from the posterior distribution of the model
set.seed(42)
S <- 50
sims2 <- setNames(data.frame(sims[, sample(10000, S)]), paste0("sim", seq_len(S)))
sims2 <- setNames(stack(sims2), c("precip_anom_sigma_3mo", "Simulation"))
sims2 <- transform(sims2, Time = rep(newd$Time, S))

sims2$Date<-as.Date(sims2$Time,origin="1970-01-01")
ggplot(sims2, aes(x = Date, y = precip_anom_sigma_3mo, group = Simulation)) +
  geom_line(alpha = 0.3)

#An instructive visualisation for the period of the purported pause or 
#hiatus in global warming is to look at the shapes of the posterior simulations and the slopes of the trends for each year.
#I first look at the posterior simulations:

ggplot(sims2, aes(x = Date, y = precip_anom_sigma_3mo, group = Simulation)) +
  geom_line(alpha = 0.5)# + xlim(c(1995, 2015)) + ylim(c(0.2, 0.75))


tmpf <- tempfile()
curl::curl_download("https://gist.githubusercontent.com/gavinsimpson/ca18c9c789ef5237dbc6/raw/295fc5cf7366c831ab166efaee42093a80622fa8/derivSimulCI.R", tmpf)
source(tmpf)

fd <- derivSimulCI(m4, samples = 10000, n = 200)

#Loading required package: MASS

CI <- apply(fd[[1]]$simulations, 1, quantile, probs = c(0.025, 0.975))
sigD <- signifD(fd[["Time"]]$deriv, fd[["Time"]]$deriv, CI[2, ], CI[1, ],
                eval = 0)
newd1 <- transform(newd1,
                  derivative = fd[["Time"]]$deriv[, 1], # computed first derivative
                  fdUpper = CI[2, ],                    # upper CI on first deriv
                  fdLower = CI[1, ],                    # lower CI on first deriv
                  increasing = sigD$incr,               # where is curve increasing?
                  decreasing = sigD$decr)               # ... or decreasing?

#A ggplot2 version of the derivatives is produced using the code below. 
#The two additional geom_line() calls add thick lines over sections of the derivative plot 
#to illustrate those points where zero is not contained within the confidence interval of the first derivative.

ggplot(newd1, aes(x = Date, y = derivative)) +
  geom_ribbon(aes(ymax = fdUpper, ymin = fdLower), alpha = 0.3, fill = "grey") +
  geom_line() +
  geom_line(aes(y = increasing), size = 1.5) +
  geom_line(aes(y = decreasing), size = 1.5) +
  ylab(expression(italic(hat(f) * "'") * (Year))) +
  xlab("Year") + theme_classic()

set.seed(123)
nsim <- 10000
pauseD <- derivSimulCI(m4, samples = nsim,
                       newdata = data.frame(Time = seq(9496, 17501, by = 500)))

annSlopes <- setNames(stack(setNames(data.frame(pauseD$Time$simulations),
                                     paste0("sim", seq_len(nsim)))),
                      c("Derivative", "Simulations"))
annSlopes <- transform(annSlopes, Time = rep(seq(9496, 17501, by = 500), each = nsim))

annSlopes$Date<-as.Date(annSlopes$Time,origin="1970-01-01")
ggplot(annSlopes, aes(x = Derivative, group = Date)) +
  geom_line(stat = "density", trim = TRUE) + facet_wrap(~ Date)

#Kernel density estimates of the first derivative of posterior simulations from the fitted trend model for selected years
#Kernel density estimates of the first derivative of posterior simulations from the fitted trend model for selected years

#We can also look at the smallest derivative for each year over all of the 10,000 posterior simulations

minD <- aggregate(Derivative ~ Time, data = annSlopes, FUN = min)
minD$Date<-as.Date(minD$Time,origin="1970-01-01")
ggplot(minD, aes(x = Date, y = Derivative)) +
  geom_point()

#Dotplot showing the minimum first derivative over 10,000 posterior simulations from the fitted additive model
#Dotplot showing the minimum first derivative over 10,000 posterior simulations from the fitted additive model

#Only 4 of the 18 years have a single simulation with a derivative less than 0. 
#We can also plot all the kernel density estimates on the same plot to see if there is much variation between years 
#(there doesn’t appear to be much going on from the previous figures).

library("viridis")
annSlopes$Date1<-as.numeric(lubridate::year(annSlopes$Date))
ggplot(annSlopes, aes(x = Derivative, group = Date1, colour = Date1)) +
  geom_line(stat = "density", trim = TRUE) + scale_color_viridis(option = "magma") +
  theme(legend.position = "top", legend.key.width = unit(3, "cm")) + theme_classic()


#old code
#assess the frequency of hot or dry years (plot standardised anomalies)
z1<-ggplot(terra_clim %>% filter(site=="B13"&year>=1986),aes(Date,tmax_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Maximum Temperature Anomalies") +
  stat_smooth(color="black") + theme(text=element_text(size=16),legend.title.align=0.5 ) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") 





z3<-ggplot(terra_clim %>% filter(site=="B13"&year>=1986),aes(Date,precip_anom_sigma_3mo,fill=oni)) + geom_bar(stat="identity") + theme_classic() +
  xlab("Year") + ylab("Quarterly Standardized\nAnomalies") + ggtitle("Precipitation Anomalies") +
  stat_smooth(color="black") + theme(text=element_text(size=16),legend.title.align=0.5) + 
  scale_fill_gradient2( low = "blue", mid = "white",
                        high = "red", midpoint = 0, space = "Lab",
                        guide = "colourbar", aesthetics = "fill",name="Ocean Nino\nIndex") 




ggpubr::ggarrange(z3b,z1b,ncol=1,nrow=2,align="hv",labels="auto",common.legend=T)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Landscape/TerraClim.StdAnom.GAM.Comparison.pdf",height=9,width=12)
