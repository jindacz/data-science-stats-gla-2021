library(ggplot2)
library(dplyr)
library(sjPlot)
library(stats)
library(janitor)
library(jtools)
library(moderndive)
library(gapminder)
library(skimr)
library(kableExtra)
library(gridExtra)
library(GGally)
library(MASS)
library(ROCR)
library(modelr)
library(olsrr)
library(car)


setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 3/Modelling\ the\ progression\ of\ world\ records\ in\ athletics\ -\ Events\ over\ middle\ distances")
Men1500mori=read.csv("Men1500m.csv")
Men1500m=read.csv("Men1500m.csv")
Men400m=read.csv("Men400m.csv")
Men800m=read.csv("Men800m.csv")
Women1500m=read.csv("Women1500m.csv")
Women400m=read.csv("Women400m.csv")
Women800m=read.csv("Women800m.csv")

Men1500m=transform(Men1500m,class="Men1500m",sex="M",Date=as.Date(Date,"%d/%m/%Y"),DOB=as.Date(DOB,"%d/%m/%Y"))
Men1500m=transform(Men1500m,age=as.integer(Date-as.Date(DOB))/365,Speed=1500/Time)

Men400m=transform(Men400m,class="Men400m",sex="M",Date=as.Date(Date,"%d/%m/%Y"),DOB=as.Date(DOB,"%d/%m/%Y"))
Men400m=transform(Men400m,age=as.integer(Date-as.Date(DOB))/365,Speed=400/Time)

Men800m=transform(Men800m,class="Men800m",sex="M",Date=as.Date(Date,"%d/%m/%Y"),DOB=as.Date(DOB,"%d/%m/%Y"))
Men800m=transform(Men800m,age=as.integer(Date-as.Date(DOB))/365,Speed=800/Time)

Women1500m=transform(Women1500m,class="Women1500m",sex="F",Date=as.Date(Date,"%d/%m/%Y"),DOB=as.Date(DOB,"%d/%m/%Y"))
Women1500m=transform(Women1500m,age=as.integer(Date-as.Date(DOB))/365,Speed=1500/Time)

Women400m=transform(Women400m,class="Women400m",sex="F",Date=as.Date(Date,"%d/%m/%Y"),DOB=as.Date(DOB,"%d/%m/%Y"))
Women400m=transform(Women400m,age=as.integer(Date-as.Date(DOB))/365,Speed=400/Time)

Women800m=transform(Women800m,class="Women800m",sex="F",Date=as.Date(Date,"%d/%m/%Y"),DOB=as.Date(DOB,"%d/%m/%Y"))
Women800m=transform(Women800m,age=as.integer(Date-as.Date(DOB))/365,Speed=800/Time)

all=rbind(Men1500m,Women1500m,
          Men400m,Women400m,
          Men800m,Women800m)
all1500m=rbind(Men1500m,Women1500m)
all400m=rbind(Men400m,Women400m)
all800m=rbind(Men800m,Women800m)

male=rbind(Men1500m,Men400m,Men800m)
female=rbind(Women1500m,Women400m,Women800m)

str(all)





#Time against date 


ggplot(all,mapping=aes(x=Date,y=Time))+
  geom_point()+
  facet_wrap(~class)+
  labs(x="Date",y="Time",title="Time against date for events over middle distance")

#for one
ggplot(Women1500m,mapping=aes(x=Date,y=Time))+
  geom_point()
factor(all$Venue)

#barplot by nationality
ggplot(all,aes(x=Country,y=Time))+
  geom_boxplot(fill="red3")+
  labs(x="Country",y="Time")

#explotary analysis
plot(all$Date,all$Time,lwd=1,pch=20,col=factor(all$class))
#color the dots by events

plot(all$age,all$Time,lwd=1,pch=20,col=factor(all$class),main="Time against Age for 6 events")
plot(all$Altitude,all$Time,lwd=1,pch=20,col=factor(all$class),main="Time against Altitude for 6 events")

plot(factor(all1500m$sex),all1500m$Time,lwd=1,pch=20)
plot(factor(all400m$sex),all400m$Time,lwd=1,pch=20)
plot(factor(all800m$sex),all800m$Time,lwd=1,pch=20)
#we could see that in 1500m,400m,800m events, male athletes seems outperform
#female athletes in speed

# potential altitude outliers
allrm=subset(all,all$Altitude<2000)
Men1500mrm=subset(Men1500m,Men1500m$Altitude<2000)
Women1500mrm=subset(Women1500m,Women1500m$Altitude<2000)
Men400mrm=subset(Men400m,Men400m$Altitude<2000)
Women400mrm=subset(Women400m,Women400m$Altitude<2000)
Men800mrm=subset(Men800m,Men800m$Altitude<2000)
Women800mrm=subset(Women800m,Women800m$Altitude<2000)

all=rbind(Men1500m,Women1500m,
          Men400m,Women400m,
          Men800m,Women800m)


all1500m=rbind(Men1500m,Women1500m)
all400m=rbind(Men400m,Women400m)
all800m=rbind(Men800m,Women800m)
male=rbind(Men1500m,Men400m,Men800m)
female=rbind(Women1500m,Women400m,Women800m)



#knitr to print statistics
all1500m %>%
  group_by(sex) %>%
  summarise(n=n(),Mean=round(mean(Time),digits=1), St.Dev=round(sd(Time),digits=1),
            Min=min(Time), Q1 = quantile(Time,0.25), Median=median(Time),
            Q3 = quantile(Time,0.75), Max=max(Time)) %>%
  kable(caption = '\\label{tab:summaries} Summary statistics on
  record Time by sex of 1500m events.') %>%
  kable_styling(latex_options = "hold_position")

all400m %>%
  group_by(sex) %>%
  summarise(n=n(),Mean=round(mean(Time),digits=1), St.Dev=round(sd(Time),digits=1),
            Min=min(Time), Q1 = quantile(Time,0.25), Median=median(Time),
            Q3 = quantile(Time,0.75), Max=max(Time)) %>%
  kable(caption = '\\label{tab:summaries} Summary statistics on
  record Time by sex of 400m events.') %>%
  kable_styling(latex_options = "hold_position")

all800m %>%
  group_by(sex) %>%
  summarise(n=n(),Mean=round(mean(Time),digits=1), St.Dev=round(sd(Time),digits=1),
            Min=min(Time), Q1 = quantile(Time,0.25), Median=median(Time),
            Q3 = quantile(Time,0.75), Max=max(Time)) %>%
  kable(caption = '\\label{tab:summaries} Summary statistics on
  record Time by sex of 800m events.') %>%
  kable_styling(latex_options = "hold_position")

ggplot(all, aes(x = class, y = Time)) +
  geom_boxplot() +
  labs(x = "class", y = "Time",
       title = "Middle distance World Records By sex")

lm1=lm(Time~Date,data=Men1500m)
lm2=lm(Time~Date,data=Men400m)
lm3=lm(Time~Date,data=Men800m)
lm4=lm(Time~Date,data=Women1500m)
lm5=lm(Time~Date,data=Women400m)
lm6=lm(Time~Date,data=Women800m)

par(mfcol=c(2,3))
plot(Men1500m$Date,Men1500m$Time)
abline(lm1$coefficients[1],lm1$coefficients[2],col="red",lwd=2)

plot(Men400m$Date,Men400m$Time)
abline(lm2$coefficients[1],lm2$coefficients[2],col="red",lwd=2)

plot(Men800m$Date,Men800m$Time)
abline(lm3$coefficients[1],lm3$coefficients[2],col="red",lwd=2)

plot(Women1500m$Date,Women1500m$Time)
abline(lm4$coefficients[1],lm4$coefficients[2],col="red",lwd=2)

plot(Women400m$Date,Women400m$Time)
abline(lm5$coefficients[1],lm5$coefficients[2],col="red",lwd=2)

plot(Women800m$Date,Women800m$Time)
abline(lm6$coefficients[1],lm6$coefficients[2],col="red",lwd=2)

par(mfcol=c(1,1))

#From the plot, we can see that linear model cannot fit the data well,
#as there seems to exist curvature, which can not be treated well by 
#linear model

#try the log world record time, the percentage of new to very first recorded world record time
llm1=lm(log(Time)~Date,data=Men1500m)
llm2=lm(log(Time)~Date,data=Men400m)
llm3=lm(log(Time)~Date,data=Men800m)
llm4=lm(log(Time)~Date,data=Women1500m)
llm5=lm(log(Time)~Date,data=Women400m)
llm6=lm(log(Time)~Date,data=Women800m)

#not good

par(mfcol=c(2,3))
plot(Men1500m$Date,log(Men1500m$Time))
abline(llm1$coefficients[1],llm1$coefficients[2],col="red",lwd=2)

plot(Men400m$Date,log(Men400m$Time))
abline(llm2$coefficients[1],llm2$coefficients[2],col="red",lwd=2)

plot(Men800m$Date,log(Men800m$Time))
abline(llm3$coefficients[1],llm3$coefficients[2],col="red",lwd=2)

plot(Women1500m$Date,log(Women1500m$Time))
abline(llm4$coefficients[1],llm4$coefficients[2],col="red",lwd=2)

plot(Women400m$Date,log(Women400m$Time))
abline(llm5$coefficients[1],llm5$coefficients[2],col="red",lwd=2)

plot(Women800m$Date,log(Women800m$Time))
abline(llm6$coefficients[1],llm6$coefficients[2],col="red",lwd=2)

par(mfcol=c(1,1))
#log transformation cannot fit women1500m well

?plot
#transform time to elpased data to fit polynomial regression and GAM
#try polynomial regression of degree 3(see world 1500m)
allelapair=allela[,-c(1,3,4,5,6)]
allelapair1=allelapair[,-c(1,2)]
ggpairs(allelapair1)

allela=transform(all,Date=as.integer(Date-as.Date('1900-1-1')))
all1500m=transform(all1500m,Date=as.integer(Date-as.Date('1900-1-1')))
all400m=transform(all400m,Date=as.integer(Date-as.Date('1900-1-1')))
all800m=transform(all800m,Date=as.integer(Date-as.Date('1900-1-1')))
Men1500m=transform(Men1500m,Date=as.integer(Date-as.Date('1900-1-1')))
Men400m=transform(Men400m,Date=as.integer(Date-as.Date('1900-1-1')))
Men800m=transform(Men800m,Date=as.integer(Date-as.Date('1900-1-1')))
Women1500m=transform(Women1500m,Date=as.integer(Date-as.Date('1900-1-1')))
Women400m=transform(Women400m,Date=as.integer(Date-as.Date('1900-1-1')))
Women800m=transform(Women800m,Date=as.integer(Date-as.Date('1900-1-1')))

male=rbind(Men1500m,Men400m,Men800m)
female=rbind(Women1500m,Women400m,Women800m)
#####################
pm1=lm((Time)~Date+I(Date^2)+I(Date^3),data=Men1500m)
pm2=lm((Time)~Date+I(Date^2)+I(Date^3),data=Men400m)
pm3=lm((Time)~Date+I(Date^2)+I(Date^3),data=Men800m)
pm4=lm((Time)~Date+I(Date^2)+I(Date^3),data=Women1500m)
pm5=lm((Time)~Date+I(Date^2)+I(Date^3),data=Women400m)
pm6=lm((Time)~Date+I(Date^2)+I(Date^3),data=Women800m)

plot(pm5)


par(mfcol=c(2,3))
plot(Men1500m$Date,Men1500m$Time)
y=pm1$coefficients[1]+pm1$coefficients[2]*Men1500m$Date+pm1$coefficients[3]*Men1500m$Date^2+
  pm1$coefficients[4]*Men1500m$Date^3
lines(Men1500m$Date,y,col="red",lwd=2)

plot(Men400m$Date,Men400m$Time)
y=pm2$coefficients[1]+pm2$coefficients[2]*Men400m$Date+pm2$coefficients[3]*Men400m$Date^2+
  pm2$coefficients[4]*Men400m$Date^3
lines(Men400m$Date,y,col="red",lwd=2)

plot(Men800m$Date,Men800m$Time)
y=pm3$coefficients[1]+pm3$coefficients[2]*Men800m$Date+pm3$coefficients[3]*Men800m$Date^2+
  pm3$coefficients[4]*Men800m$Date^3
lines(Men800m$Date,y,col="red",lwd=2)

plot(Women1500m$Date,Women1500m$Time)
y=pm4$coefficients[1]+pm4$coefficients[2]*Men1500m$Date+pm4$coefficients[3]*Men1500m$Date^2+
  pm4$coefficients[4]*Men1500m$Date^3
lines(Men1500m$Date,y,col="red",lwd=2)

plot(Women400m$Date,Women400m$Time)
y=pm5$coefficients[1]+pm5$coefficients[2]*Men400m$Date+pm5$coefficients[3]*Men400m$Date^2+
  pm5$coefficients[4]*Men400m$Date^3
lines(Men400m$Date,y,col="red",lwd=2)

plot(Women800m$Date,Women800m$Time)
y=pm6$coefficients[1]+pm6$coefficients[2]*Men800m$Date+pm6$coefficients[3]*Men800m$Date^2+
  pm6$coefficients[4]*Men800m$Date^3
lines(Men800m$Date,y,col="red",lwd=2)

par(mfcol=c(1,1))

#polynomial regresson fits most data well

#fit GAM model only with date

#need to fit a multivariate GAM model?

#Models of this form are a particular case of generalised additive models
#with normal errors and are discussed in detail in Hastie and Tibshirani
#(1990) and Wood (2006, 2017).

?gam
gam1=gam(Time~s(Date),data=Men1500m,method="REML")
summary(gam1)
gam21=gam(Time~s(Date)+s(age),data=Men1500m,method="REML")
summary(gam21)
gam21$aic
gam31=gam(Time~s(Date)+s(age)+s(Altitude),data=Men1500m,select=TRUE,method="REML")
summary(gam31)
gam31$aic

gam1=gam(Time~s(Date),data=Men1500m,method="REML")
gam1rm=gam(Time~s(Date),data=Men1500mrm,method="REML")
par(mfcol=c(1,2))
plot(gam1)
plot(gam1rm)
gam.check(gam1)



gam2=gam(Time~s(Date),data=Men400m,method="REML")
summary(gam2)
gam2$aic
gam22=gam(Time~s(Date)+s(age),data=Men400m,method="REML")
summary(gam22)
gam22$aic
gam32=gam(Time~s(Date)+s(age)+s(Altitude, bs = 'cr', k = 3),data=Men400m,method="REML")
gam32rm=gam(Time~s(Date)+s(age)+s(Altitude, bs = 'cr', k = 3),data=Men400mrm,method="REML")
summary(gam32)
gam32$aic
gam.check(gam3)

par(mfcol=c(1,2))
plot(gam32)
plot(gam32rm)


gam3=gam(Time~s(Date),data=Men800m,method="REML")
summary(gam3)
gam3$aic
gam23=gam(Time~s(Date)+s(age),data=Men800m,method="REML")
summary(gam23)
gam23$aic
gam33=gam(Time~s(Date)+s(age)+s(Altitude, bs = 'cr', k = 3),data=Men800m,method="REML")
summary(gam33)
gam33$aic
gam.check(gam3)

gam4=gam(Time~s(Date),data=Women1500m,method="REML")
summary(gam4)
gam4$aic
gam24=gam(Time~s(Date)+s(age, bs = 'cr', k = 3),data=Women1500m,method="REML")
summary(gam24)
gam24$formula
gam24$aic
gam34=gam(Time~s(Date)+s(age, bs = 'cr', k = 3)+s(Altitude, bs = 'cr', k = 3),data=Women1500m,method="REML")
summary(gam34)
gam34$aic
gam.check(gam34)


gam5=gam(Time~s(Date),data=Women400m,method="REML")
summary(gam5)
gam5$aic
gam25=gam(Time~s(Date)+s(age),data=Women400m,method="REML")
summary(gam25)
gam25$aic
gam35=gam(Time~s(Date)+s(age)+s(Altitude, bs = 'cr', k = 3),data=Women400m,method="REML")
summary(gam35)
gam35$aic
gam.check(gam25)

gam6=gam(Time~s(Date),data=Women800m,method="REML")
summary(gam6)
gam6$aic
gam26=gam(Time~s(Date)+s(age),data=Women800m,method="REML")
summary(gam26)
gam26$aic
gam36=gam(Time~s(Date)+s(age)+s(Altitude, bs = 'cr', k = 3),data=Women800m,method="REML")
summary(gam36)
gam36$aic
gam.check(gam26)
#Since df(gam1)!=1,cannot fit a straight line, so smooth terms necessary




#prediction
pred1=gam(Date~s(Time),data=Men1500m)
Time.pred1=seq(200,205,length.out = 5)
predict.gam(pred1,data.frame(Time = Time.pred1))

pred1=gam(Date~s(Time),data=Men1500m)


#Use the best-fitting model type to compare the patterns of progress in the three events for en and women separately
?gam

gammale1=gam(Speed~s(Date,by=factor(class)),data=male,method="REML")
gammale2=gam(Speed~s(Date,by=factor(class),k=20),data=male,method="REML")
gammale3=gam(Speed~s(Date,by=factor(class))+s(Altitude),data=male,method="REML")
gammale3=gam(Speed~s(Date,by=factor(class))+s(Altitude)+s(age),data=male,method="REML")
plot.gam(gammale1)
summary(gammale1)
plot.gam(gammale12)
summary(gammale12)
gam.check(gammale2)




predict(gammale)
?predict
gamfemale1=gam(Speed~s(Date,by=factor(class)),data=female,method="REML")
gamfemale2=gam(Speed~s(Date,by=factor(class))+s(Altitude),data=female,method="REML")
gamfemalefull=gam(Speed~s(Date,by=factor(class))+s(Altitude)+s(age),data=female,method="REML")

summary(gamfemale1)
summary(gamfemale2)
summary(gamfemalefull)

gamfemale1$aic
gamfemale2$aic
gamfemalefull$aic

gam.check(gamfemale1)
gam.check(gamfemalefull)

anova(gamfemale1,gamfemale2,test="Chisq")
?anova

print(gamfemale)

plot(gammale,residuals=TRUE,cex=10)
summary(gammale1)
#difference between men400m and men800m is small
#cross validation @ cite source CVgam
gam.check(gammale1)
gam.check(gammale2)
#p=0.38>0.05, k=9, how to fix, and interpret?
gam.check(gamfemale)
gam.check(gamfemale2)
gam.check(gamfemalefull)
?gam
library(mgcv)
library(gam)
scope1=list( 
  "lcavol" = ~1 + Speed~s(Date,by=factor(class))+s(Altitude), 
  " lweight" = ~1 + Speed~s(Date,by=factor(class))+s(Altitude)+s(age)
)
step.object <- step.gam(gamfemalefull, scope1)
?step.gam


#edf of s(Altitude) is one, indicates linear fit

plot(gammale)


CVgam(formula=Speed~s(Date,by=factor(class)),
      data = male, nfold = 10, debug.level = 0, method = "REML",
      printit = TRUE, cvparts = NULL, gamma = 1)
#speed against date


#MSE is large 
CVgam(formula=Speed~s(Date,by=factor(class)),
      data = female, nfold = 10, debug.level = 0, method = "REML",
      printit = TRUE, cvparts = NULL, gamma = 1)

#Use the best-fitting model type to compare the patterns of progress for men and women in each event separately

gam1500m=gam(Time~s(Date,by=factor(sex)),data=all1500m,method="REML")
gam1500m2=gam(Time~s(Date,by=factor(sex))+s(Altitude),data=all1500m,method="REML")
gam1500m3=gam(Time~s(Date,by=factor(sex))+s(age),data=all1500m,method="REML")

plot(gam1500m,main="1500m world records data",ylim=c(0,50))
plot(gam400m,main="400m world records data",ylim=c(0,50))
plot(gam800m,main="800m world records data",ylim=c(0,50))

# women does not have data at early period until 1941, so the second part
#matters

#different trend for men and women: straight lines
gam400m=gam(Time~s(Date,by=factor(sex)),data=all400m,method="REML")

gam800m=gam(Time~s(Date,by=factor(sex)),data=all800m,method="REML")

par(mfcol=c(1,2))
plot(gam1500m)
par(mfcol=c(1,2))
plot(gam400m)
par(mfcol=c(1,2))
plot(gam800m)

gam.check(gam1500m)
gam.check(gam400m)
gam.check(gam800m)

CVgam(formula=Time~s(Date,by=factor(sex)),
      data = all1500m, nfold = 10, debug.level = 0, method = "REML",
      printit = TRUE, cvparts = NULL, gamma = 1)

#MSE is large 
CVgam(formula=Time~s(Date,by=factor(class)),
      data = all400m, nfold = 10, debug.level = 0, method = "REML",
      printit = TRUE, cvparts = NULL, gamma = 1)

CVgam(formula=Time~s(Date,by=factor(class)),
      data = all800m, nfold = 10, debug.level = 0, method = "REML",
      printit = TRUE, cvparts = NULL, gamma = 1)




#model compare for all 6 data

#p value
