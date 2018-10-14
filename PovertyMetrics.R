#library(reshape)
library(gridExtra)
library(tidyverse)


setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")
#year="2014"
#season="1415"
#setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")

#source("/Volumes/ELDS/ECOLIMITS/R_codes/HelperFunctions/summarySE.R")

dF.pov<-data.frame(read.csv(paste0(getwd(),"/HouseholdSurvey/household_data.csv")),stringsAsFactors = F)
dF.income<-data.frame(read.csv(paste0(getwd(),"/Analysis/ES/Estimated.variability.income.csv")),stringsAsFactors = F)
dF<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot.mod_analysis_dataset.csv"))

dF$wereda<-"Yayu"
dF[dF$kebele=="Badessa"|dF$kebele=="Weyra","wereda"]<-"Doraani"
#add wereda
dF.income$wereda<-dF[match(dF.income$plotcode,dF$Plot),"wereda"]

#currency conversion rate birr to usd
usd=27.21

#plot different income quartiles in USD,[dF.pov$plotcode!="",]
dF.pov$Total.income.usd<-dF.pov$Total.income/usd
dF.pov <- dF.pov %>% arrange(Total.income.usd)
dF.pov$id<-1:nrow(dF.pov)

#plot different income quartiles in USD,[dF.pov$plotcode!="",]
dF.pov$Coffee.income.usd<-dF.pov$Coffee.income/usd
dF.pov <- dF.pov %>% arrange(Coffee.income.usd)
dF.pov$id<-1:nrow(dF.pov)

#take cut off incomes for quartiles
quarts <- dF.pov %>% group_by(Total.income.quartile) %>% summarise(min.usd=min(Total.income.usd,na.rm=T))
quarts.c <- dF.pov %>% group_by(Coffee.income.quartile) %>% summarise(min.usd=min(Coffee.income.usd,na.rm=T))

#open additional income estimates
yayu.2015<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.increase.Yayu.2015.csv"))
doraani.2016 <- read.csv(paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.csv"))

total.yayu<-yayu.2015 %>% group_by(Plot) %>% summarise(add.income.15=sum(new.income,na.rm=T)) %>% mutate(plotcode=Plot)
total.doraani <- doraani.2016 %>% group_by(Plot) %>% filter(variable=="CN.ratio1"|variable=="fruitset1") %>% 
  summarise(add.income.16=sum(new.income,na.rm=T)) %>% mutate(plotcode=Plot)

#add to income array
dF.income<-left_join(dF.income,total.yayu %>% select(plotcode,add.income.15),by="plotcode")
dF.income<-left_join(dF.income,total.doraani %>% select(plotcode,add.income.16),by="plotcode")

dF.income<-dF.income %>% group_by(plotcode) %>% mutate(a.2015=sum(add.income.15,y2015),a.2016=sum(add.income.16,y2016)) %>% mutate(a.14.15=(a.2015-y2014)/y2014,a.14.16=(a.2016-y2014)/y2014) %>%
  mutate(o.a.2015=Coffee.income/usd*(1+a.14.15),o.a.2016=Coffee.income/usd*(1+a.14.16))

#dF.pov$z.Food.security<-(dF.pov$Food.security-mean(dF.pov$Food.security))/sd(dF.pov$Food.security)
#quart<-ddply(dF.pov,.(Cocoa.income.quart),summarise,income=max(Cocoa.Income,na.rm=T))

#relationship between coffee and total income
ggplot(dF.pov,aes(Coffee.income/usd,Total.income/usd))+geom_point()+stat_smooth(method="lm") + geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlab("Income from Coffee [US$]") + ylab("Total Income [US$]")+
  ylim(0,17000)+xlim(0,10000)+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/Analysis/ES/Survey.totalincome.v.coffeeincome.pdf"))


ggplot(dF.pov,aes(id,Total.income.usd))+geom_bar(stat="identity") + geom_bar(data=dF.pov %>% filter(plotcode!=""), aes(id,Total.income.usd),color="red",stat="identity") + geom_vline(xintercept=60,linetype="dashed") + 
  geom_vline(xintercept=121,linetype="dashed") + geom_vline(xintercept=182,linetype="dashed") + xlab("Households\n(red = monitored plots)") + 
  ylab("Total Annual Income (USD)")+ggtitle("Distribution of Total Income\n(dashed lines = Income Quartiles)") +
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/Analysis/ES/Survey.totalincome.v.incomequartiles.pdf"))


#take cut off incomes for quartiles
#quarts <- dF.pov %>% group_by(Total.income.quartile) %>% summarise(min.usd=min(Total.income.usd,na.rm=T))

ggplot(dF.pov,aes(id,Coffee.income.usd))+geom_bar(stat="identity") + geom_bar(data=dF.pov %>% filter(plotcode!=""), aes(id,Coffee.income.usd),color="red",stat="identity") + geom_vline(xintercept=60,linetype="dashed") + 
  geom_vline(xintercept=121,linetype="dashed") + geom_vline(xintercept=182,linetype="dashed") + xlab("Households\n(red = monitored plots)") + 
  ylab("Annual Income from Coffee (USD)")+ggtitle("Distribution of Coffee Income\n(dashed lines = Income Quartiles)") +
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/Analysis/ES/Survey.coffeeincome.v.incomequartiles.pdf"))


#analyze poverty measures as predictors of quartiles, sanitation, electricity,food security (amount and variety), TV, access to extension
#basic necessities
x<-aov(glm(electricity~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results$quarts<-paste0(" ",row.names(results))
results$category<-"Electricity"
x<-aov(glm(sanitation~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Sanitation"
x<-aov(glm(drinking.water~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Water"

#Health (under 5 mortality)
x<-aov(glm(under5mortality~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Health"

#Education
x<-aov(glm(education~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Education"

#Assets
x<-aov(glm(TV~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"TV"

#Food Security
x<-aov(glm(food.amount~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Food Amount"

x<-aov(glm(food.variety~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Food Variety"

#Access to Extension
x<-aov(glm(extension~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Extension"

#Household Head Literacy
x<-aov(glm(literacy~factor(Coffee.income.quartile),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"HHold Head Literacy"

#save outcomes based on income quartile
write.csv(results,paste0(getwd(),"/Analysis/ES/Poverty.IncomeQuartile.anovas_yayu.csv"))

#do analysis using income continuous variable
#sanitation
x<-glm(sanitation~Coffee.income.usd,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(sanitation~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of education metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob.a","y2.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob.a","y3.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
}

sanit<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
sanit$id<-as.character(dF.income$plotcode)
sanit$Coffee.income<-dF.income$o.2014
sanit$wereda<- dF.income$wereda
write.csv(sanit,paste0(getwd(),"/Analysis/ES/Sanitation.probabilities.wincome.csv"))

output<-data.frame(cbind("Sanitation",mean(sanit$orig.prob,na.rm=T),mean(sanit$y2.prob,na.rm=T),mean(sanit$y3.prob,na.rm=T),mean(sanit$y2.prob.a,na.rm=T),mean(sanit$y3.prob.a,na.rm=T)),stringsAsFactors = F)
colnames(output)<-c("Measure","Original","Year2","Year3","Year2a","Year3a")

output.d<- sanit %>% filter(wereda=="Doraani") %>% summarise(Measure="Sanitation",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T))
#colnames(output.d)<-c("Measure","Original","Year2","Year3")
output.y<- sanit %>% filter(wereda=="Yayu") %>% summarise(Measure="Sanitation",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T))

#find mean probability before and after income shift
g1<-ggplot(sanit,aes(log(Coffee.income),orig.prob))+geom_point(aes(color=wereda))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=wereda))+
    #geom_point(data=sanit %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Coffee Income [USD]")+ylab("Probability")+ggtitle("Household Access to Sanitation")+
    geom_vline(xintercept = as.numeric(log(quarts[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts[3,2])),linetype="dashed")+
   geom_vline(xintercept = as.numeric(log(quarts[4,2])),linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top"
    ,legend.key=element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Probability.sanitation.income.survey.pdf"))

#electricity
x<-glm(electricity~Coffee.income.usd,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(electricity~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of education metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob.a","y2.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob.a","y3.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
}

elec<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
elec$id<-dF.income$plotcode
elec$Coffee.income<-dF.income$o.2014
elec$wereda<- dF.income$wereda
write.csv(elec,paste0(getwd(),"/Analysis/ES/Electricity.probabilities.wincome.csv"))

output[2,1:6]<-cbind("Electricity",mean(elec$orig.prob,na.rm=T),mean(elec$y2.prob,na.rm=T),mean(elec$y3.prob,na.rm=T),mean(elec$y2.prob.a,na.rm=T),mean(elec$y2.prob.a,na.rm=T))

output.d[2,1:6]<-cbind(elec %>% filter(wereda=="Doraani") %>% summarise(Measure="Electricity",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

#colnames(output.d)<-c("Measure","Original","Year2","Year3")
output.y[2,1:6]<-cbind(elec %>% filter(wereda=="Yayu") %>% summarise(Measure="Electricity",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

#find mean probability before and after income shift
g2<-ggplot(elec,aes(log(Coffee.income),orig.prob))+geom_point(aes(color=wereda))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=wereda))+
  #geom_point(data=elec %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") +
  xlab("Log of Coffee Income [USD]")+ylab("Probability")+ggtitle("Household Access to Electricity")+
  geom_vline(xintercept = as.numeric(log(quarts[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts[4,2])),linetype="dashed")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top"
    ,legend.key=element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Probability.electricity.income.survey.pdf"))


#do for TV asset
x<-glm(TV~Coffee.income.usd,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(TV~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob.a","y2.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob.a","y3.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
}

TV<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV$id<-as.character(dF.income$plotcode)
TV$Coffee.income<-dF.income$o.2014
TV$wereda<-dF.income$wereda
write.csv(TV,paste0(getwd(),"/Analysis/ES/Asset.tv.probabilities.wincome.csv"))

output[3,1:6]<-cbind("Asset (TV)",mean(TV$orig.prob,na.rm=T),mean(TV$y2.prob,na.rm=T),mean(TV$y3.prob,na.rm=T),mean(TV$y2.prob.a,na.rm=T),mean(TV$y2.prob.a,na.rm=T))

output.d[3,1:6]<-cbind(TV %>% filter(wereda=="Doraani") %>% summarise(Measure="TV",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

#colnames(output.d)<-c("Measure","Original","Year2","Year3")
output.y[3,1:6]<-cbind(TV %>% filter(wereda=="Yayu") %>% summarise(Measure="TV",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

g3<-ggplot(TV,aes(log(Coffee.income),orig.prob))+geom_point(aes(color=wereda))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=wereda))+
  #geom_point(data=TV %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Income [USD]")+ylab("Probability")+ggtitle("Household Owning a TV")+
  geom_vline(xintercept = as.numeric(log(quarts[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts[4,2])),linetype="dashed") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top"
    ,legend.key=element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Probability.assetTV.income.increase.survey.pdf"))

#do for food amount
x<-glm(food.amount~Coffee.income.usd,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(food.amount~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob.a","y2.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob.a","y3.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
}

food.amount<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
food.amount$id<-as.character(dF.income$plotcode)
food.amount$Coffee.income<-dF.income$o.2014
food.amount$wereda<-dF.income$wereda
write.csv(food.amount,paste0(getwd(),"/Analysis/ES/Asset.foodamount.probabilities.wincome.csv"))

output[4,1:6]<-cbind("Food security (amount)",mean(food.amount$orig.prob,na.rm=T),mean(food.amount$y2.prob,na.rm=T),mean(food.amount$y3.prob,na.rm=T),mean(food.amount$y2.prob.a,na.rm=T),mean(food.amount$y2.prob.a,na.rm=T))

output.d[4,1:6]<-cbind(food.amount %>% filter(wereda=="Doraani") %>% summarise(Measure="Food security (amount)",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

#colnames(output.d)<-c("Measure","Original","Year2","Year3")
output.y[4,1:6]<-cbind(food.amount %>% filter(wereda=="Yayu") %>% summarise(Measure="Food security (amount)",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

g4<-ggplot(food.amount,aes(log(Coffee.income),orig.prob))+geom_point(aes(color=wereda))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=wereda))+
  #geom_point(data=food.amount %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Income [USD]")+ylab("Probability")+ggtitle("Household Adequate\nAmount of Food")+
  geom_vline(xintercept = as.numeric(log(quarts[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts[4,2])),linetype="dashed") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top"
    ,legend.key=element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Probability.foodamount.income.survey.pdf"))


#do for food variety
x<-glm(food.variety~Coffee.income.usd,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(food.variety~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob.a","y2.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob.a","y3.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
}

food.variety<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
food.variety$id<-as.character(dF.income$plotcode)
food.variety$Coffee.income<-dF.income$o.2014
food.variety$wereda<-dF.income$wereda
write.csv(food.variety,paste0(getwd(),"/Analysis/ES/Asset.foodvariety.probabilities.wincome.csv"))

output[5,1:6]<-cbind("Food security (variety)",mean(food.variety$orig.prob,na.rm=T),mean(food.variety$y2.prob,na.rm=T),mean(food.variety$y3.prob,na.rm=T),mean(food.variety$y2.prob.a,na.rm=T),mean(food.variety$y2.prob.a,na.rm=T))

output.d[5,1:6]<-cbind(food.variety %>% filter(wereda=="Doraani") %>% summarise(Measure="Food security (variety)",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

#colnames(output.d)<-c("Measure","Original","Year2","Year3")
output.y[5,1:6]<-cbind(food.variety %>% filter(wereda=="Yayu") %>% summarise(Measure="Food security (variety)",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T),Year2a=mean(y2.prob.a,na.rm=T),Year3a=mean(y3.prob.a,na.rm=T)))

g5<-ggplot(food.variety,aes(log(Coffee.income),orig.prob))+geom_point(aes(color=wereda))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=wereda))+
  #geom_point(data=food.variety %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Income [USD]")+ylab("Probability")+ggtitle("Household Adequate\nVariety of Food")+
  geom_vline(xintercept = as.numeric(log(quarts[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts[4,2])),linetype="dashed") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top"
    ,legend.key=element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Probability.foodvariety.income.survey.pdf"))

#do for access to extension
x<-glm(extension~Coffee.income.usd,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(extension~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2014[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2015[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob.a","y2.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.income$o.a.2016[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob.a","y3.ci5.a","y3.ci95.a")
  y<-cbind(y,z)
  results[[i]]<-y
  
}

extension<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
extension$id<-as.character(dF.income$plotcode)
extension$Coffee.income<-dF.income$o.2014
extension$wereda<-dF.income$wereda
write.csv(extension,paste0(getwd(),"/Analysis/ES/Asset.extension.probabilities.wincome.csv"))

output[6,1:4]<-cbind("Extension",mean(extension$orig.prob,na.rm=T),mean(extension$y2.prob,na.rm=T),mean(extension$y3.prob,na.rm=T))
output.d[6,1:4]<-cbind(extension %>% filter(wereda=="Doraani") %>% summarise(Measure="Extension",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T)))

#colnames(output.d)<-c("Measure","Original","Year2","Year3")
output.y[6,1:4]<-cbind(extension %>% filter(wereda=="Yayu") %>% summarise(Measure="Extension",Original=mean(orig.prob,na.rm=T),Year2=mean(y2.prob,na.rm=T),Year3=mean(y3.prob,na.rm=T)))

g6<-ggplot(extension,aes(log(Coffee.income),orig.prob))+geom_point(aes(color=wereda))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=wereda))+
  #geom_point(data=extension %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Income [USD]")+ylab("Probability")+ggtitle("Household Access\nto Extension")+
  geom_vline(xintercept = as.numeric(log(quarts[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts[4,2])),linetype="dashed") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top"
    ,legend.key = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Probability.extension.income.survey.pdf"))


g7<-grid.arrange(g1,g2,g3,g4,g5,g6,ncol=3)

ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.vs.income.figures.pdf"),g7,width=12,height=8)


g8<-grid.arrange(g2,g3,g4,g5,ncol=2)

ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.vs.income.sigfigures.pdf"),g8,width=10,height=8)

write.csv(output,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.InterannualMeans.csv"))
write.csv(output.d,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.doraani.InterannualMeans.csv"))
write.csv(output.y,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.yayu.InterannualMeans.csv"))

output.d<-read.csv(paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.doraani.InterannualMeans.csv"))
output.y<-read.csv(paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.yayu.InterannualMeans.csv"))

#create figures of change in "community" mean per measure focusing on electicity, TV, food security measures
ggplot(output,aes("2014",as.numeric(Original),group=Measure)) + geom_point(aes(color=Measure)) + geom_point(data=output,aes("2015",as.numeric(Year2),group=Measure,color=Measure))+
  geom_point(data=output,aes("2016",as.numeric(Year3),group=Measure,color=Measure)) +ylim(0,1) + 
  geom_segment(data=output,aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2),group=Measure,color=Measure),size=1)+
  geom_segment(data=output,aes(x="2015",xend="2016",y=as.numeric(Year2),yend=as.numeric(Year3),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(yintercept=0.5,linetype="dashed") +
  xlab("Year of Monitoring") + ylab("Probability") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    #,legend.title=element_blank()
    ,legend.key=element_blank()
    ,legend.position="bottom"
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.communitymeans.pdf"),width=6,height=8)


g1<-ggplot(output.d,aes("2014",as.numeric(Original),group=Measure)) + geom_point(aes(color=Measure)) + geom_point(data=output.d,aes("2015",as.numeric(Year2),group=Measure,color=Measure))+
  geom_point(data=output.d,aes("2016",as.numeric(Year3),group=Measure,color=Measure)) +ylim(0,1) + 
  geom_segment(data=output.d,aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2),group=Measure,color=Measure),size=1)+
  geom_segment(data=output.d,aes(x="2015",xend="2016",y=as.numeric(Year2),yend=as.numeric(Year3),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(yintercept=0.5,linetype="dashed") + ggtitle("Doraani") +
  xlab("Year of Monitoring") + ylab("Probability") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    #,legend.title=element_blank()
    ,legend.key=element_blank()
    ,legend.position="none"
    ,text = element_text(size = 14))

g2<-ggplot(output.y,aes("2014",as.numeric(Original),group=Measure)) + geom_point(aes(color=Measure)) + geom_point(data=output.y,aes("2015",as.numeric(Year2),group=Measure,color=Measure))+
  geom_point(data=output.y,aes("2016",as.numeric(Year3),group=Measure,color=Measure)) +ylim(0,1) + 
  geom_segment(data=output.y,aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2),group=Measure,color=Measure),size=1)+
  geom_segment(data=output.y,aes(x="2015",xend="2016",y=as.numeric(Year2),yend=as.numeric(Year3),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(yintercept=0.5,linetype="dashed") + ggtitle("Yayu") +
  xlab("Year of Monitoring") + ylab("Probability") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    #,legend.title=element_blank()
    ,legend.key=element_blank()
    ,legend.position="none"
    ,text = element_text(size = 14))

g3<-grid.arrange(g1,g2,ncol=2)

ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.communitymeans.bywereda.pdf"),g3,width=8,height=6)

output.d<-read.csv(paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.doraani.InterannualMeans.csv"))
output.y<-read.csv(paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.yayu.InterannualMeans.csv"))

output.d[output.d$Measure=="Sanitation",5:7]<-NA
output.y[output.y$Measure=="Sanitation",5:7]<-NA

#redo for augmented income in Doraani and Yayu
g1<-ggplot(output.d[output.d$Measure!="Sanitation"&output.d$Measure!="Extension",],aes("2014",as.numeric(Original),group=Measure)) + geom_point(aes(color=Measure)) + 
  geom_point(data=output.d[output.d$Measure!="Sanitation"&output.d$Measure!="Extension",],aes("2015",as.numeric(Year2),group=Measure,color=Measure))+
  geom_point(data=output.d[output.d$Measure!="Sanitation"&output.d$Measure!="Extension",],aes("2016",as.numeric(Year3a),group=Measure,color=Measure)) +ylim(0,1) + 
  geom_segment(data=output.d[output.d$Measure!="Sanitation"&output.d$Measure!="Extension",],aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2),group=Measure,color=Measure),size=1)+
  geom_segment(data=output.d,aes(x="2016",xend="2016",y=as.numeric(Year3),yend=(as.numeric(Year3a)-0.01),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_segment(data=output.d[output.d$Measure!="Sanitation"&output.d$Measure!="Extension",],aes(x="2015",xend="2016",y=as.numeric(Year2),yend=as.numeric(Year3a),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(yintercept=0.5,linetype="dashed") + ggtitle("Doraani") +
  xlab("Year of Monitoring") + ylab("Probability") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    #,legend.title=element_blank()
    ,legend.key=element_blank()
    ,legend.position="none"
    ,text = element_text(size = 14))

g2<-ggplot(output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes("2014",as.numeric(Original),group=Measure)) + geom_point(aes(color=Measure)) + 
  geom_point(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes("2015",as.numeric(Year2a),group=Measure,color=Measure))+
  geom_point(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes("2016",as.numeric(Year3),group=Measure,color=Measure)) +ylim(0,1) + 
  geom_segment(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2a),group=Measure,color=Measure),size=1)+
  geom_segment(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes(x="2015",xend="2015",y=as.numeric(Year2),yend=(as.numeric(Year2a)-0.01),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_segment(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes(x="2015",xend="2016",y=as.numeric(Year2a),yend=as.numeric(Year3),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(yintercept=0.5,linetype="dashed") + ggtitle("Yayu") +
  xlab("Year of Monitoring") + ylab("Probability") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    #,legend.title=element_blank()
    ,legend.key=element_blank()
    ,legend.position="none"
    ,text = element_text(size = 14))

g3<-grid.arrange(g1,g2,ncol=2)

ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.communitymeans.bywereda.add.pdf"),g3,width=8,height=5)

ggplot(output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes("2014",as.numeric(Original),group=Measure)) + geom_point(aes(color=Measure)) + 
  geom_point(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes("2015",as.numeric(Year2),group=Measure,color=Measure))+
  geom_point(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes("2016",as.numeric(Year3),group=Measure,color=Measure)) +ylim(0,1) + 
  geom_segment(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2a),group=Measure,color=Measure),size=1)+
  #geom_segment(data=output.y,aes(x="2014",xend="2015",y=as.numeric(Original),yend=as.numeric(Year2),group=Measure,color=Measure),size=1)+
  geom_segment(data=output.y[output.y$Measure!="Sanitation"&output.y$Measure!="Extension",],aes(x="2015",xend="2016",y=as.numeric(Year2a),yend=as.numeric(Year3),group=Measure,color=Measure),size=1, arrow = arrow(length = unit(0.03, "npc")))+
  geom_hline(yintercept=0.5,linetype="dashed") + ggtitle("Yayu") +
  xlab("Year of Monitoring") + ylab("Probability") + theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    #,legend.title=element_blank()
    ,legend.key=element_blank()
    ,legend.position="bottom"
    ,text = element_text(size = 14))
ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.communitymeans.yayu.pdf"))

