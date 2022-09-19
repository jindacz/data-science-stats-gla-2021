#reg_lec9_code
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/Data\ sets-20210219/")
protein=read.csv("PROTEIN.csv",header=T)
protein.lm=lm(Protein~Gestation,data=protein)
summary(protein.lm)

#Print ANOVA table
anova(protein.lm)

#Plot the response variable protein against explanatory variable gestation
#with fitted line.
ggplot(protein,aes(x=Gestation,y=Protein))+
  geom_point(size=3.2,alpha=0.4,col="blue")+
  ggtitle("Protein in Pregnancy")+
  geom_smooth(method="lm",fullrange=T,color="black",size=2,se=F)

library(ggfortify)
library(gridExtra)
autoplot(protein.lm, which=1:2)

#Create data frame with fitted values and residuals
protein.fit<-data.frame(fit=protein.lm$fitted.values,
                        res=protein.lm$residuals,
                        ges=protein$Gestation,
                        res1=c(protein.lm$residuals[2:19],NA))
#Plot fitted values against gestation
plot1<-ggplot(protein.fit, aes(x=ges,y=res)) +
  geom_point() +
  labs(x="Gestation", y="Residuals", title="Residuals vs Gestation")
#Plot residuals against the previous residuals (lag 1)
plot3<-ggplot(protein.fit, aes(x=res1,y=res)) +
  geom_point() +
  labs(y="Residuals", x="Residuals lag 1", title="Residual Independence")
#Create a grid with the two plots side-by-side
grid.arrange(plot1, plot3, ncol=2)

#lecture 10_code
trees=read.csv("TREES.csv",header=T)
trees.lm=lm(Volume~Diameter+Height,data=trees)
autoplot(trees.lm,1:2)

#this curvature (and the underlying geometric model) suggest that using a log transformation
#is appropriate for these data. (The log transform will produce, an additive, linear model from a
#multiplicative one.)

tree.lm=lm(formula=log(Volume)~log(Height)+log(Diameter),data=trees)
summary(tree.lm)
#why use log transformation?
#blc underlying geometric model outlined earlier.
#97.6% of the variability in log volume
#can be explained by its dependence on log diameter and log height.

load("rodent.rda")
?qplot
a=qplot(Mass,Speed,data=rodent)
b=qplot(log(Mass),log(Speed),data=rodent)
grid.arrange(a,b,ncol=2)

#Model with all data not transformed
coef.1m1a=as.numeric(coef(lm(Speed~Mass,data=rodent)))
#Model removing North American Porcupine not transformed
coef.lm2a=coef(lm(Speed~Mass,data=rodent[-1,]))

a<-qplot(rodent$Mass,rodent$Speed,main="Mass and Speed of Rodents",
         xlab="Mass",ylab="Speed") +
  geom_text( aes(x=6, y=1.1, label="North American Porcupine",color="red"),
             show.legend = FALSE) +
  geom_point( aes(x=rodent[1,1], y=rodent[1,2],color="red"),
              shape=21,size=5,show.legend = FALSE,alpha=1)+
  scale_shape(solid = FALSE) +
  stat_smooth(method = "lm", se = FALSE, col="red") +
  geom_abline(intercept=coef.lm2a[1], slope=coef.lm2a[2], color = "blue")

#Model with all data
coef.lm1<-as.numeric(coef(lm(log(Speed)~log(Mass),data=rodent)))
#Model removing North American Porcupine
coef.lm2<-coef(lm(log(Speed)~log(Mass),data=rodent[-1,]))
b<-qplot(log(rodent$Mass),log(rodent$Speed),main="log Mass and Speed of Rodents",
         xlab="log(Mass)",ylab="log(Speed)") +
  geom_text( aes(x=-0.3, y=1.1, label="North American Porcupine",color="red"),
             show.legend = FALSE) +
  geom_point( aes(x=log(rodent[1,1]), y=log(rodent[1,2]),color="red"),
              shape=21,size=5,show.legend = FALSE,alpha=1)+
  scale_shape(solid = FALSE) +
  stat_smooth(method = "lm", se = FALSE, col="red") +
  geom_abline(intercept=coef.lm2[1], slope=coef.lm2[2], color = "blue")
grid.arrange(a,b,ncol=2)

#蓝色的是校准的，右边的是log

#Cook’s distance
#In R one can use cooks distance which is available using the diagnostics of a linear model fit
model<-lm(log(Speed)~log(Mass),data=rodent)
autoplot(model, which = 4)

###or
cd_cont_pos <- function(leverage, level, model)
{sqrt(level*length(coef(model))*(1-leverage)/leverage)}
cd_cont_neg <- function(leverage, level, model)
{-cd_cont_pos(leverage, level, model)}
autoplot(model, which = 5) +
  stat_function(fun = cd_cont_pos, args = list(level = 0.5, model = model),
                xlim = c(0, 0.4), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 0.5, model = model),
                xlim = c(0, 0.4), lty = 2, colour = "red") +
  stat_function(fun = cd_cont_pos, args = list(level = 1, model = model),
                xlim = c(0, 0.4), lty = 3, colour = "red") +
  stat_function(fun = cd_cont_neg, args = list(level = 1, model = model),
                xlim = c(0, 0.4), lty = 3, colour = "red") +
  scale_y_continuous(limits = c(-5, 3.5))


###The R library car also has a function called outlierTest which 
#performs a formal test for detecting an outlier.
library(car)
outlierTest(lm(log(Mass)~log(Speed),data=rodent))

?car::outlierTest


