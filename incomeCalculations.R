library(gridExtra)
library(tidyverse)
library(ggpubr)

setwd("/Volumes/ELDS/ECOLIMITS/Ethiopia/Yayu/")
#setwd("/users/alex/Documents/Research/Africa/ECOLIMITS/Data/Yayu/")
#load input data for model
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/ES.plot_analysis_dataset_wylddiff.csv"))
dF.hhold1<-data.frame(read.csv(paste0(getwd(),"/HouseholdSurvey/household_data_ext.csv")),stringsAsFactors = F)
dF.hhold<-data.frame(read.csv(paste0(getwd(),"/HouseholdSurvey/household_data.csv")),stringsAsFactors = F)

dF.hhold<-left_join(dF.hhold,dF.hhold1 %>% select(SUBMISSION,Number.of.income.sources),by="SUBMISSION")
dF.hhold<-dF.hhold %>% rename(income.sources=Number.of.income.sources)

rm(dF.hhold1)

#currency conversion rate birr to usd
usd=27.21

#coffee price (2014, 2015, 2016)
c_price=data.frame(cbind(c(20.5,18.5,22),c(2014,2015,2016)))
colnames(c_price)<-c("coffee_price","year")

dF.1<-left_join(dF.1,c_price,by="year")

#calculate annual yields (kg/ha) and income
dF.1 <- dF.1 %>% group_by(Plot,year) %>% mutate(est.yield=Shrub.kg*density) %>% 
  mutate(est.harvest.kg=est.yield*coffee.area.ha,est.coffee.income=est.yield*coffee.area.ha*coffee_price) %>% 
  mutate(est.coffee.income.usd=est.coffee.income/usd) %>% ungroup()

#add survey household measures
dF.1 <- left_join(dF.1,dF.hhold %>% rename(Plot=plotcode) %>% select(-SUBMISSION,-coffee.landarea.ha),by="Plot")
dF.1 <- dF.1 %>% distinct(Plot,year,Coffee.income,.keep_all=T)

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
dF.summ <- dF.1 %>% group_by(Plot,Coffee.income.percent,coffee.area.ha,Coffee.income.quartile,Coffee.income,income.sources,household.size,elevation,patcharea,Shannon.i,BA.legume,Total.labour,local,GapDry,GapWet,b.ffer) %>% summarise(avg.harvest.kg=mean(est.yield),sd.harvest.kg=sd(est.yield) )  %>% #,
                                                 #avg.coffee.income=mean(est.coffee.income.usd,na.rm=T),sd.coffee.income=sd(est.coffee.income.usd,na.rm=T)) 
                                                
  ungroup()

dF.annual <- dF.1 %>% filter(year==2014) %>% select(Plot,est.yield,Coffee.income,coffee_price) %>% 
  rename(yield.2014=est.yield,price.2014=coffee_price)
dF.annual <- left_join(dF.annual, dF.1 %>% filter(year==2015) %>% select(Plot,est.yield,Coffee.income,coffee_price) %>% 
                         rename(yield.2015=est.yield,price.2015=coffee_price),by=c("Plot","Coffee.income"))
dF.annual <- left_join(dF.annual, dF.1 %>% filter(year==2016) %>% select(Plot,est.yield,Coffee.income,coffee_price) %>% 
                         rename(yield.2016=est.yield,price.2016=coffee_price),by=c("Plot","Coffee.income")) %>%
  group_by(Plot,Coffee.income) %>% summarise_all(mean) %>% ungroup()

dF.summ <- left_join(dF.annual,dF.summ,by=c("Plot","Coffee.income"))
dF.summ <- dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T)

dF.summ <- dF.summ %>% group_by(Plot,Coffee.income) %>% mutate(harvest.kg.2014=yield.2014*coffee.area.ha,
                                                 harvest.kg.2015=yield.2015*coffee.area.ha,
                                                 harvest.kg.2016=yield.2016*coffee.area.ha) %>% 
  mutate(income.2014=harvest.kg.2014*price.2014/usd,income.2015=harvest.kg.2015*price.2015/usd,
         income.2016=harvest.kg.2016*price.2016/usd) %>% mutate(harvest.diff.2015=harvest.kg.2015-harvest.kg.2014,log.harv.diff.15=log(harvest.kg.2015/harvest.kg.2014),
                                                                  harvest.diff.2016=harvest.kg.2016-harvest.kg.2014,log.harv.diff.16=log(harvest.kg.2016/harvest.kg.2014),
                                                                  income.diff.2015=income.2015-income.2014,log.inc.diff.15=log(income.2015/income.2014),
                                                                  income.diff.2016=income.2016-income.2014,log.inc.diff.16=log(income.2016/income.2014)) %>% 
  mutate(income.tot.2016=income.diff.2015+income.diff.2016,prop.harvest.2015=harvest.diff.2015/harvest.kg.2014,prop.harvest.2016=harvest.diff.2016/harvest.kg.2014,
         prop.income.2015=income.2015/income.2014, prop.income.2016=income.2016/income.2014) %>% 
  mutate(prop.tot=income.tot.2016/income.2014,log.tot=log(sum(income.2015,income.2016,na.rm=T)/income.2014)) %>% 
  mutate(log.inc.diff.15=replace(log.inc.diff.15,log.inc.diff.15==-Inf,NA),log.inc.diff.16=replace(log.inc.diff.16,log.inc.diff.16==-Inf,NA)) %>% 
  ungroup()


#plot percent coffee income vs variability of income for 2015 and 2016
g1<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)), aes(log.inc.diff.15,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("2015") +
  labs(color="Coffee\nIncome\nQuartile")

g2<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)), aes(log.inc.diff.16,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("2016") +
  labs(color="Coffee\nIncome\nQuartile")

ggarrange(g1,g2,ncol=2,nrow=1,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/LogDiffIncome.PercentIncomeCoffee.pdf"),height=5,width=10)

#check if landscape variables predict drop in income, yep same as yield model
car::qqp(dF.summ$log.inc.diff.16)
im<-lm(log.inc.diff.15~arm::rescale(patcharea)+arm::rescale(elevation)+arm::rescale(Shannon.i)+arm::rescale(BA.legume),data=dF.summ)
summary(im)

dF.summ<-left_join(dF.summ,dF.1 %>% select(Plot,low.yield.bin,low.yield14.bin,low.yield15.bin,low.yield16.bin),by="Plot")
dF.summ <- dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T)

#do for two years combined & add quadrant of "vulnerability" and include consistently low yielding farms
g1<-ggplot(dF.summ %>% filter(!is.na(Coffee.income.quartile)), aes(log.tot,Coffee.income.percent)) + geom_point(aes(color=factor(Coffee.income.quartile))) +
  #xlim(-1,1.5) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=Inf),fill="lightred",alpha=0.0020) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=75,ymax=Inf),fill="red",alpha=1/500) + 
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative for 2015 and 2016") +
  labs(color="Coffee\nIncome\nQuartile")

g2<-ggplot(dF.summ, aes(log.tot,Coffee.income.percent)) + geom_point(aes(color=factor(low.yield.bin))) +
  #xlim(-1,1.5) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=75),fill="orange",alpha=1/500) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=75,ymax=Inf),fill="red",alpha=1/500) +
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative for 2015 and 2016") +
  labs(color="Low\nYielding\nFarms")

ggarrange(g1,g2,ncol=2,nrow=1,common.legend = F)
ggsave(paste0(getwd(),"/Analysis/ES/PropCumDiffIncome.PercentIncomeCoffee.pdf"),height=5,width=10)
ggsave(paste0(getwd(),"/Analysis/ES/PropCumLogDiffIncome.PercentIncomeCoffee.pdf"),g1,height=4,width=5)

#identify vulnerable households, -log ratio of income, greater than 50% dependence on coffee, below 4th income quartile
dF.summ <- dF.summ %>% group_by(Plot,coffee.area.ha) %>% mutate(vulnerable=0) %>% mutate(vulnerable=replace(vulnerable,log.tot<0&Coffee.income.quartile<4&!is.na(Coffee.income.quartile)&Coffee.income.percent>50,1))

g1<-ggplot(dF.summ %>% filter(low.yield.bin==1), aes(log.tot,Coffee.income.percent)) + geom_point(aes(color=factor(vulnerable))) +
  #xlim(-1,1.5) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=75),fill="orange",alpha=1/500) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=75,ymax=Inf),fill="red",alpha=1/500) +
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative Income Loss\n(Low Yielding Farms)") +
  labs(color="Vulnerable\nFarms") + scale_color_grey()

g2<-ggplot(dF.summ %>% filter(low.yield.bin==0), aes(log.tot,Coffee.income.percent)) + geom_point(aes(color=factor(vulnerable))) +
  #xlim(-1,1.5) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=50,ymax=75),fill="orange",alpha=1/500) + 
  #geom_rect(aes(xmin=-Inf,xmax=0,ymin=75,ymax=Inf),fill="red",alpha=1/500) +
  theme_classic() + geom_hline(yintercept=50,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") +
  xlab("Log Ratio of Income") + ylab("Percent of Income from Coffee [%]") + ggtitle("Cumulative Income Loss\n(Remaining Farms)") +
  labs(color="Vulnerable\nFarms") + scale_color_grey()
ggarrange(g1,g2,ncol=2,nrow=1,common.legend = T,labels="auto")
ggsave(paste0(getwd(),"/Analysis/ES/PropCumDiffIncome.Vulnerable.PercentIncomeCoffee.pdf"),height=5,width=10)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/PropCumDiffIncome.Vulnerable.PercentIncomeCoffee.pdf",height=5,width=10)


#impacts on food security
dF.hhold <- dF.hhold %>% group_by(SUBMISSION) %>% mutate(Coffee.income.usd=Coffee.income/usd) %>% 
  ungroup()

quarts.c <- dF.hhold %>% group_by(Coffee.income.quartile) %>% summarise(min.usd=min(Coffee.income.usd,na.rm=T))

dF.summ <- left_join(dF.summ,dF.1 %>% select(Plot,Coffee.income,food.amount,food.variety,coffee.area.ha,Coffee.income.quartile),by=c("Plot","coffee.area.ha","Coffee.income.quartile","Coffee.income"))
dF.summ<-dF.summ %>% distinct(Plot,Coffee.income,.keep_all=T)
#calculate new incomes from proportional changes and survey reported coffee incomes
dF.summ <- dF.summ %>% group_by(Plot,coffee.area.ha,Coffee.income.quartile,Coffee.income.percent) %>% 
  mutate(income.2015.survey.usd=(Coffee.income+Coffee.income*(prop.income.2015))/usd,
         income.2016.survey.usd=(Coffee.income+Coffee.income*(prop.income.2016))/usd) %>% 
  mutate(income.2015.survey.usd=replace(income.2015.survey.usd,income.2015.survey.usd<0|is.na(income.2015.survey.usd),0),
         income.2016.survey.usd=replace(income.2016.survey.usd,income.2016.survey.usd<0|is.na(income.2016.survey.usd),0))

#calculate vulnerability (>50% of income from coffee & cumulative income <0)
#dF.summ$vulnerable<-0
#dF.summ <- dF.summ %>% mutate(vulnerable=replace(vulnerable,Coffee.income.percent>=50&log.tot<0,1))

write.csv(dF.summ,paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

#assess whether investments in labour or whether a household is "local", in the buffer or not predicts vulnerability or placement in the landscape.
dF.summ<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

pv1<-glm(vulnerable~poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(pv1)

pv2<-glm(vulnerable~buffer,data=dF.summ,family=quasibinomial(link="logit"))
summary(pv2)

pv3<-glm(buffer~poly(elevation,2)+patcharea,data=dF.summ,family=quasibinomial(link="logit"))
summary(pv3)

lv1<-glm(vulnerable~local,data=dF.summ,family=quasibinomial(link="logit"))
summary(lv1)

#correlation between total labour and location in the buffer
cor(dF.summ$Total.labour[!is.na(dF.summ$Total.labour&dF.summ$Total.labour<max(dF.summ$Total.labour[!is.na(dF.summ$Total.labour)]))],dF.summ$buffer[!is.na(dF.summ$Total.labour)],method="pearson")
lv2<-lm(Total.labour~poly(elevation,2) + buffer,data=dF.summ)
summary(lv2)

ggplot(dF.summ %>% filter(Total.labour<1200),aes(elevation,Total.labour)) + geom_point() + stat_smooth(method="lm")
                                                                                                       #,formula=y~poly(x,2))

pv4<-lm(Total.labour~poly(elevation,2)+patcharea,data=dF.summ)
summary(pv4)


#####need to move to PovertyMetrics
library(ggpubr)

dF.summ<-read.csv(paste0(getwd(),"/Analysis/ES/Yield.income.input.variables.csv"))

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
food.amount<-left_join(food.amount,dF.1 %>% select(Plot,wereda,kebele.x,Coffee.income.quartile,elevation,patcharea,Shannon.i,low.yield.bin),by=c("Plot","Coffee.income.quartile")) %>%
  group_by(Plot,Coffee.income.usd,Coffee.income.quartile,wereda,kebele.x) %>% summarise_all(mean)
write.csv(food.amount,paste0(getwd(),"/Analysis/ES/Resilience.foodamount.probabilities.wincome.csv"))

#plot income variability and food security
food.amount<-read.csv(paste0(getwd(),"/Analysis/ES/Resilience.foodamount.probabilities.wincome.csv"))
food.amount1<-left_join(food.amount,dF.summ %>% select(-elevation,-patcharea,-Shannon.i), by=c("Plot","Coffee.income.quartile"))
food.amount1<-food.amount1 %>% distinct(Plot,Coffee.income.usd,.keep_all=T)

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
food.variety<-left_join(food.variety,dF.1 %>% select(Plot,wereda,kebele.x,Coffee.income.quartile,elevation,patcharea,Shannon.i,low.yield.bin),by=c("Plot","Coffee.income.quartile"))  %>%
  group_by(Plot,Coffee.income.usd,Coffee.income.quartile,wereda,kebele.x) %>% summarise_all(mean)
write.csv(food.variety,paste0(getwd(),"/Analysis/ES/Resilience.foodvariety.probabilities.wincome.csv"))

food.variety<-read.csv(paste0(getwd(),"/Analysis/ES/Resilience.foodvariety.probabilities.wincome.csv"))
food.variety1<-left_join(food.variety,dF.summ %>% select(-elevation,-patcharea,-Shannon.i), by=c("Plot","Coffee.income.quartile"))
food.variety1<-food.variety1 %>% distinct(Plot,Coffee.income.usd,.keep_all=T)

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

g3<-ggplot(food.amount1,aes(log.inc.diff.15,y2.prob,ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Log Ratio of Income") + ylab("Probability") +
  ggtitle("Household Adequate\nAmount of Food (2015)") + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) +
  theme(text=element_text(size=14))

g4<-ggplot(food.amount1,aes(log.inc.diff.16,y3.prob,ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Log Ratio of Income") + ylab("Probability") +
  ggtitle("Household Adequate\nAmount of Food (2016")  + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + theme(text=element_text(size=14))


g5<-ggplot(food.variety1,aes(log.inc.diff.15,y2.prob,ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Log Ratio of Income") + ylab("Probability") +
  ggtitle("Household Adequate\nVariety of Food (2015)") + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) +
  theme(text=element_text(size=14))

g6<-ggplot(food.variety1,aes(log.inc.diff.16,y3.prob,ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + xlab("Log Ratio of Income") + ylab("Probability") +
  ggtitle("Household Adequate\nVariety of Food (2016")  + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + 
  geom_hline(yintercept=0.5,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed") + labs(color="") +
  xlim(-1,2) + ylim(0,1) + theme(text=element_text(size=14))

ggarrange(g1,g3,g4,g2,g5,g6,ncol=3,nrow=2,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/Vulnerability.FoodSecurity.pdf"),height=8,width=14)

#do FS figures again but with landscape variables
g1<-ggplot(food.amount1,aes(elevation, orig.prob, ymin=orig.ci5,ymax=orig.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("Elevation [m]") + ggtitle("Household Adequate\nAmount of Food (2014)")

g2<-ggplot(food.amount1,aes(elevation, y2.prob, ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("Elevation [m]") + ggtitle("Household Adequate\nAmount of Food (2015)")

g3<-ggplot(food.amount1,aes(elevation, y3.prob, ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("Elevation [m]") + ggtitle("Household Adequate\nAmount of Food (2016)")

g4<-ggplot(food.variety1,aes(elevation, orig.prob, ymin=orig.ci5,ymax=orig.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("Elevation [m]") + ggtitle("Household Adequate\nVariety of Food (2014)")

g5<-ggplot(food.variety1,aes(elevation, y2.prob, ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("Elevation [m]") + ggtitle("Household Adequate\nVariety of Food (2015)")

g6<-ggplot(food.variety1,aes(elevation, y3.prob, ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("Elevation [m]") + ggtitle("Household Adequate\nVariety of Food (2016)")

ggarrange(g1,g2,g3,g4,g5,g6,ncol=3,nrow=2,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/Vulnerability.FoodSecurity.elevation.pdf"),height=8,width=14)

#patcharea
g1<-ggplot(food.amount1,aes(log(patcharea), orig.prob, ymin=orig.ci5,ymax=orig.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("log(Patcharea) [ha]") + ggtitle("Household Adequate\nAmount of Food (2014)")

g2<-ggplot(food.amount1,aes(log(patcharea), y2.prob, ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("log(Patcharea) [ha]") + ggtitle("Household Adequate\nAmount of Food (2015)")

g3<-ggplot(food.amount1,aes(log(patcharea), y3.prob, ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("log(Patcharea) [ha]") + ggtitle("Household Adequate\nAmount of Food (2016)")

g4<-ggplot(food.variety1,aes(log(patcharea), orig.prob, ymin=orig.ci5,ymax=orig.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("log(Patcharea) [ha]") + ggtitle("Household Adequate\nVariety of Food (2014)")

g5<-ggplot(food.variety1,aes(log(patcharea), y2.prob, ymin=y2.ci5,ymax=y2.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("log(Patcharea) [ha]") + ggtitle("Household Adequate\nVariety of Food (2015)")

g6<-ggplot(food.variety1,aes(log(patcharea), y3.prob, ymin=y3.ci5,ymax=y3.ci95)) + geom_point(aes(color=factor(vulnerable))) + geom_errorbar(aes(color=factor(vulnerable))) +
  theme_classic() + scale_colour_manual(values=cols,labels=c("Less Vulnerable","Vulnerable")) + theme(text=element_text(size=14))  + labs(color="") +
  ylab("Probability") + xlab("log(Patcharea) [ha]") + ggtitle("Household Adequate\nVariety of Food (2016)")

ggarrange(g1,g2,g3,g4,g5,g6,ncol=3,nrow=2,common.legend = T)
ggsave(paste0(getwd(),"/Analysis/ES/Vulnerability.FoodSecurity.patcharea.pdf"),height=8,width=14)

#check if patch area and elevation predicts food security
#food amount
x<-glm(food.amount~elevation*patcharea,family=binomial,data=dF.1)
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Resilience/FoodSecurity.landscape.txt")
summary(x)
sink()
#compare to intercept only 
x.reduced<-glm(food.amount~1,family=binomial,data=dF.1)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

elev<-seq(as.integer(min(dF.summ$elevation)),as.integer(max(dF.summ$elevation)),by=1)
patch<-seq(as.integer(min(dF.summ$patcharea)),as.integer(max(dF.summ$patcharea)),by=5)

z.fa<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(x, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.fa[i,j]<-pi.hat$fit
  }
}
colnames(z.fa)<-patch
z.fa$elevation<-elev

z_g.fa<-gather(z.fa,key="patch",value="food.amount",-elevation)
t1<-ggplot(z_g.fa, aes( as.numeric(patch), elevation, z = food.amount)) +geom_raster(aes(fill=food.amount)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle(" Adequate Food Amount") + theme(text=element_text(size=14))

#food variety
x<-glm(food.variety~elevation*patcharea,family=binomial,data=dF.1)
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/Coffee_ES/Resilience/FoodVariety.landscape.txt")
summary(x)
sink()

#compare to intercept only 
x.reduced<-glm(food.variety~1,family=binomial,data=dF.1)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

elev<-seq(as.integer(min(dF.summ$elevation)),as.integer(max(dF.summ$elevation)),by=1)
patch<-seq(as.integer(min(dF.summ$patcharea)),as.integer(max(dF.summ$patcharea)),by=5)

z.fs<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(x, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.fs[i,j]<-pi.hat$fit
  }
}
colnames(z.fs)<-patch
z.fs$elevation<-elev

z_g.fs<-gather(z.fs,key="patch",value="food.variety",-elevation)
t2<-ggplot(z_g.fs, aes( as.numeric(patch), elevation, z = food.variety)) +geom_raster(aes(fill=food.variety)) +
  scale_fill_viridis_c() + theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + ggtitle("Adequate Food Variety") + theme(text=element_text(size=14))

ggarrange(t1,t2,ncol=2,nrow=1,common.legend = T,legend = "right")
ggsave(paste0(getwd(),"/Analysis/ES/Resilience.FoodSecurity.landscape.pdf"),height=4,width=9)

#predict other household characteristics using landscape?
x.labour<-lm(labour~arm::rescale(elevation)*arm::rescale(patcharea),data=dF.1)
summary(x.labour)

x.income<-lm(Coffee.income~arm::rescale(elevation)*arm::rescale(patcharea),data=dF.1)
summary(x.income)

x.area<-lm(coffee.area.ha~arm::rescale(elevation)*arm::rescale(patcharea),data=dF.1)
summary(x.area)

x.pH<-lm(pH~arm::rescale(elevation)*arm::rescale(patcharea),data=dF.1)
summary(x.pH)

z.elev<-attributes(scale(dF.summ$elevation))
z.patch<-attributes(scale(dF.summ$patcharea))

#for soil pH
z.pH<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    z.pH[i,j] <- coef(x.pH)[1]+coef(x.pH)[2]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]]) + coef(x.pH)[3]*(patch[j]-z.patch[[2]])/(2*z.patch[[3]]) +
      coef(x.pH)[4]*(elev[i]-z.elev[[2]])/(2*z.elev[[3]])*(patch[j]-z.patch[[2]])/(2*z.patch[[3]])
  }
}
colnames(z.pH)<-patch
z.pH$elevation<-elev

z_g.pH<-gather(z.pH,key="patch",value="pH",-elevation)

ggplot(z_g.pH, aes( as.numeric(patch), elevation, z = pH)) +geom_raster(aes(fill=pH)) +
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Soil pH") + theme(text=element_text(size=14))
ggsave(paste0(getwd(),"/Analysis/ES/SoilpH.elev.vs.patcharea.pdf"),width=8,height=7)


#how well does low yielding plot predict vulnerability-not well
pv<-glm(vulnerable~low.yield.bin,data=dF.summ,family="binomial")
summary(pv)

pv1<-glm(vulnerable~poly(elevation,2)+patcharea,data=dF.summ,family="binomial")
sink("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.landscape.txt")
summary(pv1)
sink()

z.v<-data.frame()
for(i in 1:length(elev)){
  for(j in 1:length(patch)){
    pi.hat = predict.glm(pv1, data.frame(patcharea=patch[j],elevation=elev[i]),
                         type="response", se.fit=TRUE)
    z.v[i,j]<-pi.hat$fit  }
}
colnames(z.v)<-patch
z.v$elevation<-elev

z_g.v<-gather(z.v,key="patch",value="vulnerability",-elevation)

ggplot(z_g.v, aes( as.numeric(patch), elevation, z = vulnerability)) +geom_raster(aes(fill=vulnerability)) +
  scale_fill_viridis_c()+ theme_classic() + ylab("Elevation [m]") + xlab("Patch Area [ha]")+
  labs(fill="Probability") + theme(text=element_text(size=14)) + ggtitle("Vulnerability")
ggsave(paste0(getwd(),"/Analysis/ES/Vulnerability.elev.vs.patcharea.pdf"),width=6,height=5)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/ElNino/Coffee_ES/Resilience/Vulnerability.elev.vs.patcharea.pdf",width=6,height=5)


#how well does vulnerability predict food security - not well
fav1<-glm(food.amount~vulnerable+low.yield.bin,data=dF.summ,family="binomial")
summary(fav1)



