library(gridExtra)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")
#setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")
#load input data for model
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
dF.hhold<-data.frame(read.csv(paste0(getwd(),"/HouseholdSurvey/household_data.csv")),stringsAsFactors = F)

#currency conversion rate birr to usd
usd=27.21

#coffee price (2014)
coffee_price=22

#calculate annual yields (kg/ha) and income
dF.1 <- dF.1 %>% group_by(Plot,year) %>% mutate(est.yield=Shrub.kg*density) %>% 
  mutate(est.harvest.kg=est.yield*coffee.area.ha,est.coffee.income=est.yield*coffee.area.ha*coffee_price) %>% 
  mutate(est.coffee.income.usd=est.coffee.income/usd) %>% ungroup()

#add survey household measures
dF.1 <- left_join(dF.1,dF.hhold %>% rename(Plot=plotcode) %>% select(-SUBMISSION,-coffee.landarea.ha),by="Plot")

ggplot(dF.1[dF.1$Shrub.kg>0&dF.1$year==2014,],aes(Shrub.kg,color="2014")) + geom_freqpoly(binwidth=0.1) + xlab("Estimated Shrub Yield [kg/shrub]") + ylab("Number of Farms")+
  geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year=="2015",],binwidth=0.05,aes(color="2015"))+geom_freqpoly(data=dF.1[dF.1$Shrub.kg>0&dF.1$year==2016,],binwidth=0.05,aes(color="2016"))+
  ggtitle("Estimated Yields of Fresh Cherries")+
  #scale_fill_discrete(aes(name="Year"))+
  theme_classic()+theme(text = element_text(size = 16)
    ,legend.key = element_blank()
    ,legend.position="bottom"
    ,legend.title = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Observed.yields.shrub.histo.pdf"),height=5,width=7)

#plot boxplots of income and yield
g1<-ggplot(dF.1,aes(wereda,est.yield,color=factor(year))) + geom_boxplot(width=0.5)+
  xlab("Wereda") + ylab("Mean Yield [kg/ha]")+scale_fill_discrete(name="Year")+
  theme_classic() + ylim(0,1000) + theme(text = element_text(size = 18)
                          ,legend.key = element_blank()
                          ,legend.title = element_blank()
                          ,legend.position="top"
                          ,plot.background = element_blank())
ggsave(paste0(getwd(),"/Analysis/ES/Variability.yield.by.wereda.pdf"),g1,width=8, height=4)

g2<-ggplot(dF.1,aes(wereda,est.coffee.income.usd,color=factor(year))) + geom_boxplot()+
  xlab("Wereda") + ylab("Mean Income [US$]")+scale_fill_discrete(name="Year")+ylim(0,2000)+
  theme_classic() + theme(text = element_text(size = 18)
                          ,legend.key = element_blank()
                          ,legend.position="none")
g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ES/Variability.yield.and.income.by.wereda.pdf"),g3,width=7,height=8)

#compare two measures of income
ggplot(dF.1 %>% filter(year==2014),aes(Coffee.income/usd,est.coffee.income.usd)) + geom_point(aes(color=wereda)) + geom_abline(slope=1,intercept=0,linetype="dashed") + 
  stat_smooth(method="lm")+ xlab("Reported Income from Coffee [US$]") + ylab("Estimated Income from Coffee [US$]") +
  #ylim(0,17000)+xlim(0,10000)+
  theme_classic() + theme(text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom") + coord_fixed(ratio=1)
ggsave(paste0(getwd(),"/Analysis/ES/Survey.estincome.v.reportedincome.final.pdf"))

#calculate total and proportional change in income from 2014
dF.summ <- dF.1 %>% group_by(Plot,Coffee.income.percent,coffee.area.ha,Coffee.income.quartile) %>% summarise(avg.harvest.kg=mean(est.yield),sd.harvest.kg=sd(est.yield) )  %>% #,
                                                 #avg.coffee.income=mean(est.coffee.income.usd,na.rm=T),sd.coffee.income=sd(est.coffee.income.usd,na.rm=T)) 
                                                
  ungroup()

dF.annual <- dF.1 %>% filter(year==2014) %>% select(Plot,est.yield) %>% 
  rename(yield.2014=est.yield)
dF.annual <- left_join(dF.annual, dF.1 %>% filter(year==2015) %>% select(Plot,est.yield) %>% 
                         rename(yield.2015=est.yield),by="Plot")
dF.annual <- left_join(dF.annual, dF.1 %>% filter(year==2016) %>% select(Plot,est.yield) %>% 
                         rename(yield.2016=est.yield),by="Plot") %>%
  group_by(Plot) %>% summarise_all(mean) %>% ungroup()

dF.summ <- left_join(dF.annual,dF.summ,by="Plot")
dF.summ <- dF.summ %>% group_by(Plot) %>% mutate(harvest.kg.2014=yield.2014*coffee.area.ha,
                                                 harvest.kg.2015=yield.2015*coffee.area.ha,
                                                 harvest.kg.2016=yield.2016*coffee.area.ha) %>% 
  mutate(income.2014=harvest.kg.2014*coffee_price/usd,income.2015=harvest.kg.2015*coffee_price/usd,
         income.2016=harvest.kg.2016*coffee_price/usd) %>% mutate(harvest.diff.2015=harvest.kg.2015-harvest.kg.2014,
                                                                  harvest.diff.2016=harvest.kg.2016-harvest.kg.2014,
                                                                  income.diff.2015=income.2015-income.2014,
                                                                  income.diff.2016=income.2016-income.2014) %>% 
  mutate(income.tot.2016=income.diff.2015+income.diff.2016,prop.harvest.2015=harvest.diff.2015/harvest.kg.2014,prop.harvest.2016=harvest.diff.2016/harvest.kg.2014,
         prop.income.2015=income.diff.2015/income.2014, prop.income.2016=income.diff.2016/income.2014) %>% 
  mutate(prop.tot=income.tot.2016/income.2014) %>% 
  ungroup()

#plot percent coffee income vs variability of income for 2015 and 2016
g1<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)&prop.income.2015<3), aes(prop.income.2015,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Proportional Change in Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("2015") +
  labs(color="Coffee\nIncome\nQuartile")

g2<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)), aes(prop.income.2016,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Proportional Change in Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("2016") +
  labs(color="Coffee\nIncome\nQuartile")

ggarrange(g1,g2,ncol=2,nrow=1,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/PropDiffIncome.PercentIncomeCoffee.pdf"),height=5,width=10)

#do for two years combined & add quadrant of "vulnerability
g1<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)&prop.tot<2), aes(prop.tot,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=Inf),fill="red",alpha=1/500) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Proportional Change in Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative for 2015 and 2016") +
  labs(color="Coffee\nIncome\nQuartile")

g2<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)&income.tot.2016>-1000), aes(income.tot.2016,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=Inf),fill="red",alpha=1/500) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Absolute Change in Income [US$]") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative for 2015 and 2016") +
  labs(color="Coffee\nIncome\nQuartile")

ggarrange(g1,g2,ncol=2,nrow=1,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/PropCumDiffIncome.PercentIncomeCoffee.pdf"),height=5,width=10)

#impacts on food security
quarts.c <- dF.hhold %>% group_by(Coffee.income.quartile) %>% summarise(min.usd=min(Coffee.income.usd,na.rm=T))

dF.hhold <- dF.hhold %>% group_by(SUBMISSION) %>% mutate(Coffee.income.usd=Coffee.income/usd) %>% 
  ungroup()
dF.summ <- left_join(dF.summ,dF.1 %>% select(Plot,Coffee.income,food.amount,food.variety,coffee.area.ha,Coffee.income.quartile),by=c("Plot","coffee.area.ha","Coffee.income.quartile"))

#calculate new incomes from proportional changes and survey reported coffee incomes
dF.summ <- dF.summ %>% group_by(Plot,coffee.area.ha,Coffee.income.quartile,Coffee.income.percent) %>% 
  summarise_all(mean) %>% 
  mutate(income.2015.survey.usd=(Coffee.income+Coffee.income*(prop.income.2015))/usd,
         income.2016.survey.usd=(Coffee.income+Coffee.income*(prop.income.2016))/usd) %>% 
  mutate(income.2015.survey.usd=replace(income.2015.survey.usd,income.2015.survey.usd<0|is.na(income.2015.survey.usd),0),
         income.2016.survey.usd=replace(income.2016.survey.usd,income.2016.survey.usd<0|is.na(income.2016.survey.usd),0))

#do for food amount
x<-glm(food.amount~Coffee.income.usd,family=binomial,data=dF.hhold)
summary(x)
#compare to intercept only 
x.reduced<-glm(food.amount~1,family=binomial,data=dF.hhold)
anova(x.reduced,x, test="Chisq")

results<-list()
for(i in 1:nrow(dF.summ)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$Coffee.income[i]/usd),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$Coffee.income[i]/usd), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2015.survey.usd[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2015.survey.usd[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2016.survey.usd[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2016.survey.usd[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}


food.amount<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
food.amount$Plot<-as.character(dF.summ$Plot)
food.amount$Coffee.income.usd<-dF.summ$Coffee.income/usd
food.amount$Coffee.income.quartile<-dF.summ$Coffee.income.quartile
food.amount<-left_join(food.amount,dF.1 %>% select(Plot,wereda,kebele.x,Coffee.income.quartile),by=c("Plot","Coffee.income.quartile")) %>%
  group_by(Plot,Coffee.income.usd,Coffee.income.quartile,wereda,kebele.x) %>% summarise_all(mean)
write.csv(food.amount,paste0(getwd(),"/Analysis/ES/Resilience.foodamount.probabilities.wincome.csv"))

#calculate vulnerability (>50% of income from coffee & cumulative income <0)
dF.summ$vulnerable<-0
dF.summ <- dF.summ %>% mutate(vulnerable=replace(vulnerable,Coffee.income.percent>=50&prop.tot<0,1))

#plot income variability and food security
food.amount<-read.csv(paste0(getwd(),"/Analysis/ES/Resilience.foodamount.probabilities.wincome.csv"))
food.amount1<-left_join(food.amount,dF.summ, by=c("Plot","Coffee.income.quartile"))

cols<-c("0" = "grey","1" = "red")

g1<-ggplot(food.amount1,aes(log(Coffee.income.usd),orig.prob))+geom_point(aes(color=factor(vulnerable)))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=factor(vulnerable)))+
  #geom_point(data=food.amount %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Income [US$]")+ylab("Probability")+ggtitle("Household Adequate\nAmount of Food")+
  geom_vline(xintercept = as.numeric(log(quarts.c[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts.c[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts.c[4,2])),linetype="dashed") + theme_classic() + theme(legend.title=element_blank()
    ,legend.position="top"
    ,legend.key=element_blank()
    ,text=element_text(size=14)) + geom_hline(yintercept=0.5,linetype="dashed") + ylim(0,1) + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable"))
#ggsave(paste0(getwd(),"/Analysis/ES/Probability.foodamount.income.survey.pdf"))

#do for food variety
x<-glm(food.variety~Coffee.income.usd,family=binomial,data=dF.hhold)
summary(x)
#compare to intercept only 
x.reduced<-glm(food.variety~1,family=binomial,data=dF.hhold)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

results<-list()
for(i in 1:nrow(dF.summ)){
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$Coffee.income[i]/usd),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$Coffee.income[i]/usd), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("orig.prob","orig.ci5","orig.ci95")
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2015.survey.usd[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2015.survey.usd[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y2.prob","y2.ci5","y2.ci95")
  y<-cbind(y,z)
  
  pi.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2016.survey.usd[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Coffee.income.usd=dF.summ$income.2016.survey.usd[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("y3.prob","y3.ci5","y3.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

food.variety<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
food.variety$Plot<-as.character(dF.summ$Plot)
food.variety$Coffee.income.usd<-dF.summ$Coffee.income/usd
food.variety$Coffee.income.quartile<-dF.summ$Coffee.income.quartile
food.variety<-left_join(food.variety,dF.1 %>% select(Plot,wereda,kebele.x,Coffee.income.quartile),by=c("Plot","Coffee.income.quartile"))  %>%
  group_by(Plot,Coffee.income.usd,Coffee.income.quartile,wereda,kebele.x) %>% summarise_all(mean)
write.csv(food.variety,paste0(getwd(),"/Analysis/ES/Resilience.foodvariety.probabilities.wincome.csv"))

food.variety<-read.csv(paste0(getwd(),"/Analysis/ES/Resilience.foodvariety.probabilities.wincome.csv"))
food.variety1<-left_join(food.variety,dF.summ, by=c("Plot","Coffee.income.quartile"))

g2<-ggplot(food.variety1,aes(log(Coffee.income.usd),orig.prob))+geom_point(aes(color=factor(vulnerable)))+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95,color=factor(vulnerable)))+
  #geom_point(data=food.amount %>% filter(id!=""),aes(log(Coffee.income),orig.prob),color="red") + 
  xlab("Log of Income [US$]")+ylab("Probability")+ggtitle("Household Adequate\nVariety of Food")+
  geom_vline(xintercept = as.numeric(log(quarts.c[2,2])),linetype="dashed")+geom_vline(xintercept = as.numeric(log(quarts.c[3,2])),linetype="dashed")+
  geom_vline(xintercept = as.numeric(log(quarts.c[4,2])),linetype="dashed") + theme_classic() + theme(legend.title=element_blank()
                                                                                                      ,legend.position="top"
                                                                                                      ,legend.key=element_blank()
                                                                                                      ,text=element_text(size=14)) + 
  geom_hline(yintercept=0.5,linetype="dashed") + ylim(0,1) + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable"))

#ggarrange(g1,g2,ncol=2,nrow=1,common.legend = T,font.label=list(size=14))
#ggsave(paste0(getwd(),"/Analysis/ES/FoodSecurity.Prob.OrigIncome.pdf"),height=5,width=10)

g3<-ggplot(food.amount1 %>% filter(prop.income.2015<2),aes(prop.income.2015,y2.prob,ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Proportional Change in Income") + ylab("Probability") +
  ggtitle("Household Adequate\nAmount of Food (2015)") + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) +
  theme(text=element_text(size=14))

g4<-ggplot(food.amount1 %>% filter(prop.income.2016<2),aes(prop.income.2016,y3.prob,ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Proportional Change in Income") + ylab("Probability") +
  ggtitle("Household Adequate\nAmount of Food (2016")  + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + theme(text=element_text(size=14))


g5<-ggplot(food.variety1 %>% filter(prop.income.2015<2),aes(prop.income.2015,y2.prob,ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Proportional Change in Income") + ylab("Probability") +
  ggtitle("Household Adequate\nVariety of Food (2015)") + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) +
  theme(text=element_text(size=14))

g6<-ggplot(food.variety1 %>% filter(prop.income.2016<2),aes(prop.income.2016,y3.prob,ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Proportional Change in Income") + ylab("Probability") +
  ggtitle("Household Adequate\nVariety of Food (2016")  + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + theme(text=element_text(size=14))

ggarrange(g1,g3,g4,g2,g5,g6,ncol=3,nrow=2,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/Resilience.FoodSecurity.pdf"),height=8,width=14)

######################Not used below here######################
#plot 2014 vs 2015 and 2014 vs 2016
g1<-ggplot(dF.2,aes(o.2014,o.2015)) + geom_point(aes(color=wereda)) + stat_smooth(method="lm") + geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlim(0,7500) + ylim(0,7500) + xlab("Reported Coffee Income in 2014")+ylab("Estimated Coffee Income in 2015")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")

g2<-ggplot(dF.2,aes(o.2014,o.2016)) + geom_point(aes(color=wereda)) + stat_smooth(method="lm") + geom_abline(slope=1,intercept=0,linetype="dashed")+
  xlim(0,7500) + ylim(0,7500) + xlab("Reported Coffee Income in 2014")+ylab("Estimated Coffee Income in 2016")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")

g3<-grid.arrange(g1,g2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ES/Survey.income2014.v.lateryears.pdf"),g3,height=5,width=10)

dF.1$Shrub.kg.1<- dF.1$Shrub.kg + .00001
dF.low<-dF.1[dF.1$kebele!="Badessa"&dF.1$kebele!="Weyra",]
dF.hi<-dF.1[dF.1$kebele=="Badessa"|dF.1$kebele=="Weyra",]

#dM.hi<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_doraani.lnorm_delta2.confint.csv"))
#dM.low<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_yayu.lnorm_delta2.confint.csv"))
inc.var<-read.csv(paste0(getwd(),"/Analysis/ES/Estimated.variability.income.csv"))

#yayu 2015
yayu.2015<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_yayu_2015.med.csv"))
#doraani 2016
doraani.2016<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_doraani_2016.med.csv"))
#differcnce of 2016 from 2014
diff.16.all<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.diffyield_norm.all.2014.16.csv"))

#calculate additional 2015 income, yayu
yayu.2015<-left_join(yayu.2015,dF.1 %>% filter(year==2015) %>% select(Plot,Shrub.kg,coffee.area.ha,density,coffee.price,obs.coffee.income.usd,est.coffee.income.usd),by="Plot")
yayu.2015 <- yayu.2015 %>% mutate(new.income=value*density*coffee.area.ha*coffee.price/usd)

#save income calculations
write.csv(yayu.2015,paste0(getwd(),"/Analysis/ES/Yield.increase.Yayu.2015.csv"))

#plot yayu 2015 increase yield
ggplot(yayu.2015,aes(Plot,value,group=variable)) + geom_bar(stat="identity",aes(fill=variable)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Yayu 2015")+
  scale_fill_discrete(labels=c("BA Legume","Fruitset"),name="Factor")+theme(
  plot.background = element_blank()
  ,panel.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.line.x = element_line(color = 'black')
  ,axis.line.y = element_line(color = 'black')
  ,axis.text.x = element_blank()
  ,text = element_text(size = 14)
  ,legend.key = element_blank()
  ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Yield.increase.Yayu.2015.pdf"))

#calculate additional 2016 income, doraani
doraani.2016<-left_join(doraani.2016,dF.1 %>% filter(year==2016) %>% select(Plot,Shrub.kg,coffee.area.ha,density,coffee.price,obs.coffee.income.usd,est.coffee.income.usd),by="Plot")
doraani.2016 <- doraani.2016 %>% mutate(new.income=value*density*coffee.area.ha*coffee.price/usd)
#save income calculations
write.csv(doraani.2016,paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.csv"))

#plot doraani 2016 increase yield
ggplot(doraani.2016,aes(Plot,value,group=variable)) + geom_bar(stat="identity",aes(fill=variable)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Doraani 2016")+
  scale_fill_discrete(labels=c("Soil C:N","Planting Density","Fruitset","GapDry"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.pdf"))

ggplot(doraani.2016[doraani.2016$variable=="fruitset1"|doraani.2016$variable=="CN.ratio1",],aes(Plot,value,group=variable)) + geom_bar(stat="identity",aes(fill=variable)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Doraani 2016")+
  scale_fill_discrete(labels=c("Soil C:N","Fruitset"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
ggsave(paste0(getwd(),"/Analysis/ES/Yield.increase.Doraani.2016.less.pdf"))

#calculate additional income in 2016, for yayu
diff.16.all<-left_join(diff.16.all,df.14 %>% select(Plot,Shrub.kg),by="Plot")
diff.16.all<-diff.16.all %>% mutate(new.kg.clr=Shrub.kg+prop.CLR,new.kg.patch=Shrub.kg+patcharea1)
#add 2016 yield, Shrub.kg.x = 2014 and Shrub.kg.y = 2016
#diff.16.all<-left_join(diff.16.all,df.16 %>% select(Plot,Shrub.kg),by="Plot")
diff.16.all<-left_join(diff.16.all,dF.1 %>% filter(year==2016) %>% select(Plot,Shrub.kg,coffee.area.ha,density,coffee.price,obs.coffee.income.usd,est.coffee.income.usd,wereda),by="Plot")
diff.16.all <- diff.16.all %>% mutate(clr.income=new.kg.clr*density*coffee.area.ha*coffee.price/usd,patch.income=new.kg.patch*density*coffee.area.ha*coffee.price/usd)

#plot reducing difference in 2016
g1<-ggplot(diff.16.all[!is.na(diff.16.all$wereda),],aes(Plot,new.kg.clr)) + geom_bar(stat="identity",aes(fill=wereda)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Yield Difference by Managing CLR (2016)")+
  #scale_fill_discrete(labels=c("Soil C:N","Planting Density","Fruitset","GapDry"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="none")
g2<-ggplot(diff.16.all[!is.na(diff.16.all$wereda),],aes(Plot,new.kg.patch)) + geom_bar(stat="identity",aes(fill=wereda)) + 
  xlab("Farm")+ylab("Additional Yield per Shrub [kg]")+ggtitle("Yield Difference by Location in Landscape (2016)")+
  #scale_fill_discrete(labels=c("Soil C:N","Planting Density","Fruitset","GapDry"),name="Factor")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,axis.text.x = element_blank()
    ,text = element_text(size = 14)
    ,legend.key = element_blank()
    ,legend.position="bottom")
g3<-grid.arrange(g1,g2,ncol=1)
ggsave(paste0(getwd(),"/Analysis/ES/Yield.difference.2016.pdf"),g3)

