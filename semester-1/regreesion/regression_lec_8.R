### Fitting Linear Model in R

### Please download crime.csv and 01_heights_weights_genders.csv from moodle

### Save these two files in your working directory

### If you do not know your working directory run
getwd()


### Load crime data set
crime<-read.csv("crime.csv")
head(crime)

### Simple plot of data
plot(crime$Dropout, crime$Crime)

### ggplot
library(ggplot2)
ggplot(crime, aes(x=Dropout, y=Crime)) + 
  geom_point()

### Fit a simple linear regression model
model<-lm(Crime ~ Dropout, data=crime)

### Print summary of model
summary(model)


### Load weight, height and gender data set
htwtgen <- read.csv(file = "01_heights_weights_genders.csv")

### Simple plot - Weight v Gender
plot(htwtgen$Gender, htwtgen$Weight) #Notice the plot function automatically plots boxplots
boxplot(Weight~Gender, data=htwtgen)

### Simple plot - Weight v Height
plot(htwtgen$Height, htwtgen$Weight, col=htwtgen$Gender)

### ggplots
ggplot(htwtgen, aes(y=Weight, x=Gender)) + 
  geom_boxplot()

ggplot(htwtgen, aes(y=Weight, x=Height, color=Gender)) + 
  geom_point()

### Fit a multiple linear regression
model<-lm(Weight~Gender + Height, data=htwtgen)
summary(model)


### Now lets load Month data - notice that we do not have a data frame already written but we can create one.
response<-c(0.690,1.028,0.507,1.689,-1.80,2.966,0.653,3.423,1.476,0.540)
observation<-1:10
Month<-c(1,3,2,3,1,2,1,3,2,3)
data<-data.frame(Observation=observation,Response=response,Month=Month)

### Plot data 
plot(data$Month, data$Response)

### Specify Month as factor
data$Month<-as.factor(data$Month)
head(data)

### Plot data 
plot(data$Month, data$Response)

### ggplot

ggplot(data, aes(x=Month, y=Response)) +
  geom_boxplot()

### Fit a simple linear regression
model_1<-lm(Response ~ Month, data=data)
summary(model_1)


### What happens if we change the baseline category from Januray to February?
data$Month<-relevel(data$Month,ref=2)
data

### Fit a simple linear regression
model_2<-lm(Response ~ Month, data=data)
summary(model_2)
