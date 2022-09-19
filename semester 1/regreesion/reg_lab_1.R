setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion\ code/Data\ sets-20210205")
phys <- read.csv("phys1.csv")
names(phys)
View(phys)
plot(Power1~Weight,data=phys)
plot(Power1~Weight,data=phys,xlab="Weight(kgs)",ylab="Power Output(Watts)")
plot(Power1~Weight,data=phys,xlab="Weight(kgs)",ylab="Power Output(Watts)",cex=1.5)
plot(Power1~Weight,data=phys,xlab="Weight(kgs)",ylab="Power Output(Watts)",
     cex=1.5,pch=19)
plot(Power1~Weight,data=phys,xlab="Weight(kgs)",ylab="Power Output(Watts)",
     cex=1.5,pch=19,col="blue")
library(ggplot2)
ggplot(data=phys,aes(y=Power1,x=Weight))+ #Specify x and y axes
geom_point(size=3,color="blue")+ #Specify to plot points size 2 coloured blue
labs(w="Weight(kgs)",y="Power Output(Watts)")+ #Specify axes labels
theme(text=element_text(size=20))

#我们可以发现，ggplot的绘图有以下几个特点：第一，有明确的起始（以ggplot函数开始）
#与终止（一句语句一幅图）；其二，图层之间的叠加是靠“+”号实现的，越后面其图层越高。
ggplot(data=phys, aes(y=Power1,x= Weight))+ #Specify x and y axes
  geom_point(size = 3, color="blue") + #Specify to plot points size 2 coloured blue
  labs(x="Weight (kgs)", y="Power Output (Watts)")+ #Specify axes labels
  theme(text = element_text(size=20)) #Change size of axes labels

###Now suppose we are interested in the relationship between power, weight and gender.
###Specify points to be coloured by gender.
ggplot(data=phys, aes(y=Power1,x= Weight,color=Gender))+ #Specify x and y axes
  geom_point(size = 3) + #Specify to plot points size 2 coloured blue
  labs(x="Weight (kgs)", y="Power Output (Watts)")+ #Specify axes labels
  theme(text = element_text(size=20))

###Comment on the relationship between power and 
###weight and does this relationship differ for males and females?

###Ans: Weights increase, the higher power output increase, there is a postive linear
#realtionship btw power and weight

###We can estimate the sample correlation
cor(phys$Power1,phys$Weight)

#there is a positive correlation btw power and weight

#Therefore, there is a strong positive linear relationship between weight and power. Taking
#this one step further, we can test the null hypothesis that the population correlations is zero and
#provide a confidence interval for the correlation 
#(i.e. a range of plausible values for the true population correlation). 
#Remember we reject the null hypothesis, that the population correlation is
#zero, if the p-value < 0.05.

cor.test(phys$Power1,phys$Weight)

#Notice this interval is centered around our estimate of 0.889. This interval is also entirely positive,
#and does not contain zero. If this interval did contain zero then that would suggest zero to be
#a plausible value. Since this interval does not contain zero, this suggests zero is not a plausible
#value. In summary, we have evidence of a significant strong positive 
#correlation between power and weight.

#Fitting a simple linear regression
model1=lm(Power1~Weight,data=phys)
summary(model1)
# E(Power1|Weight) = −517.956 + 25.226 × Weight

#comment on the value of R^2
#We can also fit a multiple linear regression model to assess the 
#relationship between power, weight and gender
model2=lm(Power1~Weight+Gender,data=phys)
summary(model2)

#Based on this output,
#E(Power1|Weight, Male) = 13.391 + 249.708 + 14.995 × Weight
#E(Power1|Weight, Female) = 13.391 + 14.995 × Weight

#Lastly, we can fit two separate regression lines to assess the relationship 
#between power and weight for males and females separately.
#Try now including an interaction between weight and gender
model3=lm(Power1~Weight*Gender,data=phys)
summary(model3)

#Notice in this model the additional term 'Weight:GenderMale' this value, in addition to 'Weight' gives the slope estimate for males.
#E(Power|Weight,Male)=401.931-416.113+8.102Weight+10.751Weight
#E(Power|Weight,Female)=401.931+8.102Weight

#Make plot with these two separate fitted lines included from model3
?rep
?predict
seq(40,90,length=50)
newdata<-data.frame(Weight=rep(seq(40,90,length=50),2), Gender=rep(c("Male","Female"),rep(50,2)) )
pred3<-predict(model3, newdata)
newdata1<-data.frame(newdata, pred=pred3)
#Specify points to be coloured by gender.
ggplot(data=phys, aes(y=Power1,x= Weight,color=Gender)) + 
  geom_point(size = 3) + 
  labs(x="Weight (kgs)", y="Power Output (Watts)") + 
  theme(text = element_text(size=20)) +
  geom_line(aes(y=pred,x= Weight,color=Gender), data=newdata1,size=2)

#Make plot with these two separate fitted lines included from model2
newdata=data.frame(Weight=rep(seq(40,90,length=50),2),
                   Gender=rep(c("Male","Female"),rep(50,2)))
###先组内rep50次，再组间
pred2=predict(model2,newdata)
newdata1=data.frame(newdata,pred=pred2)
#specifiy points to be colored by gender
ggplot(data=phys,aes(y=Power1, x=Weight, color=Gender))+
  geom_point(size=3)+
  labs(x="weight(kgs)", y="power outputs(watts)")+
  theme(text=element_text(size=20))+
  geom_line(aes(y=pred,x=Weight,color=Gender),data=newdata1,size=2)

#Based on these two plots, we can see that the intercept terms are different for males and females because males lie
#'above' females, that is the green cloud of points lie aboive the red cloud of points.
#Both lines have fairly similar positive slope parameters and so it may not be neccessary to have differet slope parameters.




###Hubble’s Constant
#Load data
hubble <- read.csv("hubble.csv")
#Check column names
names(hubble)
#View data
View(hubble)

#Describe the relationship between velocity and distance.
#positively correlated

model=lm(Velocity~Distance,data=hubble)
plot(Velocity~Distance,data=hubble,cex=1.5,col="blue",pch=19,cex.lab=1.5,
     xlab="distance(mgaparsecs)",ylab="velocity(km/sec)")
#ggplot
ggplot(data=hubble,aes(y=Velocity,x=Distance))+
  geom_point(size=3,color="blue")+
  labs(x="distance",y="velocity(km/sec)")+
  theme(text=element_text(size=20))

#This function adds the fitted line from a simple linear regression.
model=lm(Velocity~Distance,data=hubble)
summary(model)
#E(Velocity|Distance)=-40.78 + 454.16Distance

#add fitted line to plot
plot(Velocity~Distance,data=hubble,cex=1.5,col="blue",pch=19,cex.lab=1.5,
     xlab="Distance (megaparsecs)", ylab="Velocity (km/sec)")
abline(model,lwd=2)
#With ggplot
ggplot(data=hubble, aes(y=Velocity,x=Distance))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Distance (megaparsecs)", y="Velocity (km/sec)")+ 
  theme(text = element_text(size=20))+
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black")

#Notice this intercept from this model looks quite close to zero. 
#Try fitting a regression model with no intercept term.

#In most R regression packages, y ~ 1 means "fit an intercept only".
#So in the case of linear regression, this is exactly the same as mean(y). 
#For glm's, it depends on the link function.
#Also, y ~ x - 1 means "regress x on y, but leave out the intercept".
model2=lm(Velocity~Distance-1,data=hubble)
summary(model2)
#E(Velocity|Distance)=423.94Distance
plot(Velocity~Distance,data=hubble)
plot(Velocity~Distance, data=hubble, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Distance (megaparsecs)", ylab="Velocity (km/sec)")
abline(model,col="blue")
abline(model2,col="red")

#With ggplot
ggplot(data=hubble, aes(y=Velocity,x=Distance))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Distance (megaparsecs)", y="Velocity (km/sec)")+ 
  theme(text = element_text(size=20))+
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black") +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="red",formula=y~x-1)

#Comparing the two fitted lines, is the model that passes through the origin plausible?
#yes?
#There is very little difference between the two fitted lines.  In general, go for the 'simplier' of the two models.
#that is the model with the least parameters to estimate. 


#publishing history
#1. Produce a plot of the data with number of books on the y-axis and year on the x-axis.
books=read.csv("books.csv")
#Check column names
names(books)
#View data
View(books)


plot(Number.of.Books~Year,data=books,cex=1.5,
     col="blue",pch=19,cex.lab=1.5,xlab="Year", ylab="Number of Books Published")
#ggplot
ggplot(data=books, aes(y=Number.of.Books,x=Year))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Year", y="Number of Books Published")+ 
  theme(text = element_text(size=20))

#2. Describe the relationship between number of books published and year.
#We can see the number of book published increase over time. 
#The relationship does not look linear.

#Fit a simple linear regression
model0 <- lm(Number.of.Books~Year, data=books)
summary(model0)
#E(Number.of.Books|Year)=-50.1011 + 4.2599Year

#3 quad. model
?poly
model=lm(Number.of.Books~poly(Year,2),data=books)
summary(model)
#4 write down the fitted line of the form 
#E(Number.of.Books|Year)=248.092 + 2058.861Year + 597.132Year^2

#Add fitted line to plot
plot(Number.of.Books~Year, data=books, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Year", ylab="Number of Books Published")
abline(model0,lwd=2,lty=2)

#Add line from quadratic model
newdata<-data.frame(Year=0:145)
pred<-predict(model, newdata)
newdata1<-data.frame(newdata, pred=pred)
lines(newdata1$Year,newdata1$pred,lwd=2)

#ggplot
ggplot(data=books, aes(y=Number.of.Books,x=Year))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Year", y="Number of Books Published")+ 
  theme(text = element_text(size=20))+
  geom_smooth(method="lm", fill=NA, fullrange=TRUE,color="black") +
  geom_line(aes(y=pred,x= Year), data=newdata1,size=2)




#5 plot the data with fitted line
lines(fitted(model))
?I

?lines

#6Do you think this model provides a good fit to these data?
The quadratic model seems to fit the data more closely to the simple linear regression.



#7 the taste of Cheese
cheese <- read.csv("cheese.csv")
#Check column names
names(cheese)
#View data
View(cheese)
#1.Produce plots of response variable Taste and each of the three 
#chemicals Acetic Acid, H2S and Lactic Acid.
plot(Acetic.Acid~Taste,data=cheese, cex=1.5,pch=19,col="blue", cex.lab=1.5,
     xlab="Acetic Acid", ylab="Taste")
#ggplot
ggplot(data=cheese, aes(y=Taste,x=Acetic.Acid))+ 
  geom_point(size = 3, color="blue") + 
  labs(x="Taste", y="Acetic Acid")+ 
  theme(text = element_text(size=20))
#There appears to be a weak postive correlation between taste and acetic acid.


plot(Taste~Acetic.Acid,data=cheese)
plot(Taste~H2S,data=cheese)
plot(Taste~Lactic.Acid,data=cheese)

#2 There appears to be a moderately strong postive correlation between taste and Lactic Acid.
#I will choose Lactic Acid

#3 Fit a simple linear regression with your choosen chemical.
model=lm(Taste~Lactic.Acid,data=cheese)
summary(model)
#E(Taste|Lactic Acid)=-29.859 + 37.720Lactic.Acid

attach(cheese)
plot(Lactic.Acid, Taste, cex=1.5, col="blue", pch=19, cex.lab=1.5,
     xlab="Lactic Acid", ylab="Taste") 
#4 fitted line
abline(model,col="red",lwd=2)
p
#5 reject H0
#Correlation between taste and lactic acid
cor.test(cheese$Taste,cheese$Lactic.Acid)
#Since the p-value < 0.05, we can reject the null hypothesis that the population correlation coefficient is zero.
#We have evidence in favour or a strong postive correlation, estimated as 0.704, between taste and lactic acid.






