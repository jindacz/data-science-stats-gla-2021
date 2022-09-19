### Fitting Linear Model in R 
#Lecture 8

### Please download crime.csv and 01_heights_weights_genders.csv from moodle

### Save these two files in your working directory

### If you do not know your working directory run
getwd()
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/Data\ sets-20210205/")

###Load crime data set
crime=read.csv("crime.csv")
head(crime)

###simple plot of data
plot(crime$Dropout,crime$Crime)

###ggplot
library(ggplot2)
ggplot(crime,aes(x=Dropout,y=Crime))+
  geom_point()

###fit a simple linear regression model
model=lm(Crime~Dropout,data=crime)

###print summary of model
summary(model)

###load weight, height and gender data set
htwtgen=read.csv(file="01_heights_weights_genders.csv")

##simple plot-weight v gender
plot(htwtgen$Gender,htwtgen$Weight) #Notice the plot function automatically plots box plots
boxplot(Weight~Gender,data=htwtgen)

###simple plot-weight v height
plot(htwtgen$Height,htwtgen$Weight,col=unclass(factor(htwtgen$Gender)))

##ggplots
ggplot(htwtgen,aes(y=Weight,x=Gender))+
  geom_boxplot()
ggplot(htwtgen,aes(y=Weight,x=Height,color=Gender))+
  geom_point()

###fit a multiple linear regression
model=lm(Weight~Gender+Height,data=htwtgen)
summary(model)

### Now lets load Month data - notice that we do not have a data frame already 
#written but we can create one.
response=c(0.690,1.028,0.507,1.689,-1.80,2.966,0.653,3.423,1.476,0.540)
observation=1:10
Month=c(1,3,2,3,1,2,1,3,2,3)
data=data.frame(Observation=observation,Response=response,Month=Month)

#plot data
plot(data$Month,data$Response)

### Specify Month as factor
data$Month=as.factor(data$Month)
head(data)

### plot data
plot(data$Month,data$Response)

###ggplot
ggplot(data,aes(x=Month,y=Response))+
  geom_boxplot()

### fit a simople linear regression
model_1=lm(Response~Month,data=data)
summary(model_1)

### What happens if we """change the baseline""" category from Januray to February?
data$Month=relevel(data$Month,ref=2)
data
?relevel

### Fit a simple linear regression
model_2<-lm(Response ~ Month, data=data)
summary(model_2)

###relevel









