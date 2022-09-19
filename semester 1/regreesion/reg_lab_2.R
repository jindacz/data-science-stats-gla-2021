# Regression_lab2 Models Checking Assumptions
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/Data\ sets-20210219/")
diamonds=read.csv("diamonds.csv")
plot(Price~Weight,data=diamonds)

library(ggplot2)
ggplot(diamonds,aes(x=Weight,y=Price))+
  geom_point()

model1=lm(Price~Weight,data=diamonds)
summary(model1)

#E(Price|Weight) = −259.63 + 3721.02 × Weight

#Checking model assumptions
residuals(model1)
rstandard(model1)

par(mfrow=c(2,2))
#plot residuals against fitted values
plot(rstandard(model1)~fitted(model1))
#QQ plot
qqnorm(rstandard(model1))
#Histogram of residuals
hist(rstandard(model1))

par(mfrow=c(2,2))
#plot residuals against fitted values
plot(model1)

#Lastly, we can produce residuals plots using ggplot
library(ggfortify)
autoplot(model1)

#From the above plots
#1. It seems valid that the residuals have mean zero (from residuals against fitted values).
#2. It seems valid that the residuals have constant variance (from residuals against fitted values).
#3. It seems valid that the residuals are normally distribution (from QQ plot and histogram).
#4. We will assume measurements were recorded without error.
#5. Lastly, we may also want to consider checking independence.

#Create data frame with fitted values and residuals
model.fit<-data.frame(fit=model1$fitted.values,
                      res=model1$residuals,
                      res1=c(model1$residuals[2:nrow(diamonds)],NA))


#Plot residuals against the previous residuals (lag 1)
ggplot(model.fit, aes(x=res1,y=res)) +
  geom_point() +
  labs(y="Residuals", x="Residuals lag 1", title="Residual Independence")

#Since we observe no clear relationship, then we may assume residuals are independent

#Regression output
model1=lm(Price~Weight,data=diamonds)
summary(model1)
#Anova
anova(model1)
#R^2is the percentage of variation in price that is explained by the linear regression model with
#weight as a predictor. Hence 97.8% of the variation in the price is explained by taking account of
#weight using a simple linear regression model. Therefore, the model gives an excellent fit to the data.



#part 1
cheese <- read.csv("cheese.csv")
name(cheese)
str(cheese)
# If continuous variables appear as 'Factor' then we convert them to 'numeric' using:
cheese$Acetic.Acid <- as.numeric(as.character(cheese$Acetic.Acid))

#1 Produce a scatterplot of taste score versus H2S to remind yourselves of the 
#relationship between these two variables.
plot(Taste~H2S,data=cheese)

#2 Fit a simple linear regression model to predict the taste score from H2S. Write down your
#fitted model.
Model2=lm(Taste~H2S,data=cheese)
plot(Taste~H2S, data=cheese)
abline(Model2)
summary(Model2)
#y=20.26+0.001583*x1

#3 Produce plots of standardised residuals versus fits, a histogram of standardised residuals
#and a normal probability plot (Q-Q plot) of standardised residuals and use these to comment
#on the assumptions for the simple linear regression model.
library(ggfortify)
autoplot(Model2)
#or
par(mfrow=c(2,2))
## residual plots
plot(rstandard(Model2)~fitted(Model2))
qqnorm(rstandard(Model2))
hist(rstandard(Model2))


#4. Repeat the above using log(H2S) (where log is natural log, loge) and compare the goodness
#of fit of the model and the residual plots. This model is much more appropriate for the data.
## Transforming H2S:
cheese$lH2S <- log(cheese$H2S)
## Exploratory analysis with log HS2
plot(Taste~lH2S, data=cheese)
## Simple linear regression
Model2 <- lm(Taste~lH2S, data=cheese)
plot(Taste~lH2S, data=cheese)
abline(Model2)
summary(Model2)
#y=-9.79+5.78*x1
library(ggfortify)
autoplot(model_2)

#or
par(mfrow=c(2,2))
## residual plots
plot(rstandard(Model2)~fitted(Model2))
qqnorm(rstandard(Model2))
hist(rstandard(Model2))



#5. Fit a multiple linear regression model to predict taste from log H2S and log lactic acid. Examine the fit of the model (R2
#) and residual plots.
llactic <- log(cheese$Lactic.Acid)
Model3=lm(Taste~lH2S+log(Lactic.Acid),data=cheese)
summary(Model3)
#Multiple R-squared:  0.9783,	Adjusted R-squared:  0.9778 

par(mfrow=c(2,2))
## residual plots
plot(rstandard(Model3)~fitted(Model3))
qqnorm(rstandard(Model3))
hist(rstandard(Model3))

model.fit3<-data.frame(fit=model_3$fitted.values,
                      res=model_3$residuals,
                      res1=c(model_3$residuals[2:nrow(cheese)],NA))

ggplot(model.fit, aes(x=res1,y=res)) +
  geom_point() +
  labs(y="Residuals", x="Residuals lag 1", title="Residual Independence")



#part 2
trees1 <- read.csv("TREES.csv")
#利用$直接增加行数
trees1$logvol<-log(trees1$Volume)
trees1$logdiam<-log(trees1$Diameter)
trees1$loght<-log(trees1$Height)

#Exploratory analysis
#A matrix plot with scatterplots between each pair of variables can be used to 
#gain an initial impression. This can be produced by using the following command in R.
pairs(trees1[,c("logvol","logdiam","loght")],lower.panel=NULL)

#Statistical Analysis
#Fit multiple linear regression
Model1=lm(logvol~logdiam+loght,data=trees1)
#Model diagnostics
autoplot(Model1)

summary(Model1)
anova(Model1)

#When given a single model, as in the case here, it produces a table which tests 
#whether the model terms are significant.We can test our multiple linear 
#regression to the a model without any explanatory variables.

#Model 0= null model?
Model0<-lm(logvol~1, data=trees1)

#In this case, the null hypothesis being tested is β and γ are both 0.
#In this case we can reject the null hypothesis.
anova(Model0,Model1)

qt(p=0.975, df=28)
qt(p=0.025,df=28)

#1. Construct a 95% confidence interval for the coefficients β and γ and interpret th
#t.test(cheese)
##CI
beta.hat<-Model1$coefficients[2] 
ese.beta.hat<-summary(Model1)$coefficients[2,2]
#Interval 
beta.hat - qt(p=0.975, df=28)*ese.beta.hat
beta.hat + qt(p=0.975, df=28)*ese.beta.hat

gamma.hat<-Model1$coefficients[3] 
ese.gamma.hat<-summary(Model1)$coefficients[3,2]
#Interval 
gamma.hat - qt(p=0.975, df=28)*ese.gamma.hat
gamma.hat + qt(p=0.975, df=28)*ese.gamma.hat

##R has a built in function to compute such intervals
confint(Model1)


