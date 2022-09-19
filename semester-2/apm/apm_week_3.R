#apm_week_3
library(ggplot2)
library(class)
?predict
beetles <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/beetles.csv"))
beetles

beetles$propkilled <- beetles$killed / beetles$number
p1 <- ggplot(beetles, aes(x = dose, y = propkilled))+
  geom_point(size = 1) + xlab ("Dose") + ylab ("Proportion killed")

#Since the response variable 푌푖 can only take one of two values (killed or not), we need to apply a generalised linear model. This takes the form 푔(푝푖) = 훽0 + 훽1푥푖 where 푥푖
#is the dose for  We start by considering the logit link function
m1 <- glm(cbind(killed, number-killed) ~ dose, family = binomial(link = 'logit'),
          data=beetles)
summary(m1)
qchisq(df=1,p=0.95)

qchisq(df=6,p=0.95)

#For the logit model the fitted values can be obtained by taking the predicted probabilities, 푝ˆ푖
#, and multiplying them by the corresponding total number of beetles for 푖 = 1, . . . , 8:
p.hat=predict(m1,type="response")
fitted=beetles$number*p.hat

cbind(beetles$killed,round(fitted,2))

#model 2
m2 <- glm(cbind(killed, number-killed) ~ dose, family = binomial(link = 'probit'),
          data=beetles)
summary(m2)

m3 <- glm(cbind(killed, number-killed) ~ dose, family = binomial(link = 'cloglog'),
          data=beetles)
summary(m3)

beet_p <- data.frame(beetles = beetles,
                     logit = fitted(m1),
                     probit = fitted(m2),
                     cloglog = fitted(m3))
p2 <- ggplot(beet_p, aes(x = beetles$dose, y = beetles$propkilled)) +
  geom_point() + xlab("Dose") + ylab("Proportion killed") +
  geom_line(aes(x = beetles$dose, y = logit, colour = "Logit")) +
  geom_line(aes(x = beetles$dose, y = probit, colour = "Probit")) +
  geom_line(aes(x = beetles$dose, y = cloglog, colour = "C log-log")) +
  guides(colour = guide_legend("Method"))

#example_2
install.packages("faraway")
library(faraway)
head(orings)

p1<- ggplot(orings, aes(x=temp, y=damage/6)) +
  geom_point()+ xlim (c(25,85)) + ylim(c(0,1)) +
  xlab ("Temperature (F)") + ylab("Probability of damage")
p1

# fit a binomial regression model to the data, trying out the logit,probit,
# and complementary log-log options for the link function
lmod=glm(cbind(damage,6-damage)~temp,family=binomial,data=orings)
summary(lmod)

pmod=glm(cbind(damage,6-damage)~temp,family=binomial(link="probit"),data=orings)
summary(lmod)

cmod=glm(cbind(damage,6-damage)~temp,family=binomial(link="cloglog"),data=orings)
summary(cmod)

#Task 2
#Superimpose the fitted probabilities from each of the three models on the above plot
?fitted
pred1=fitted(lmod)
pred1 <- predict(lmod, newdata=data.frame(temp=seq(25,85,length.out=23)), type="response")
pred2 <- predict(pmod, newdata=data.frame(temp=seq(25,85,length.out=23)), type="response")
pred3 <- predict(cmod, newdata=data.frame(temp=seq(25,85,length.out=23)), type="response")
pred=data.frame(logit=pred1,probit=pred2,cloglog=pred3,px=seq(25,85,le=23),orings)
p1.1 <- ggplot(pred, aes(x=orings$temp, y= orings$damage/6)) +
  geom_point(size = 1)+ xlim (c(25,85)) + ylim(c(0,1)) +
  xlab ("Temperature (F)") + ylab("Probability of damage") +
  geom_line(aes(x = px, y = logit, color = "Logit")) +
  geom_line(aes(x = px, y = probit, color = "Probit"))+
  geom_line(aes(x = px, y = cloglog, color = "Complementary log-log"))
p1.1

#Task 3.
#Calculate a point estimate of the probability of damage to the O-rings when the temperature is 31 degrees
#Fahrenheit using each of the three models.

#We can obtain the predicted probabilities using the model equation:
exp(11.6630-0.2162*31)/(1+exp(11.6630-0.2162*31))

#or
?predict
predict(lmod,newdata=data.frame(temp=31),type="response")

#Similarly, we can obtain the prediction for the probit model using the cumulative distribution function of a normal
#distribution:
pnorm(5.5915-0.1058*31)

#or by using the predict() function:
predict(pmod, newdata=data.frame(temp=31), type="response")

#Finally for the complementary log-log model the predicted probability is
predict(cmod, newdata=data.frame(temp=31), type="response")

yl<- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/yl53.csv"))
head(yl)
yl$hear=factor(yl$hear)

yl.plot1=ggplot(yl,aes(y=age,x=hear))
?element_rect
yl.plot1 + geom_boxplot()+ xlab("What do you hear?") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

install.packages("sjPlot")
library("sjPlot")
plot_xtab(yl$hear,yl$gender,show.values=F,show.total=F,
          axis.labels = c("Laurel", "Yanny"),
          axis.titles=c("What do you hear?"))

#logistic model
mod.yl=glm(hear~age,family=binomial,data=yl)
summary(mod.yl)

#Finally we can plot the predicted probabilities from this model as a function of age and, as expected, we see that the predicted probability of
#hearing “Yanny” decreases with age:
library(sjPlot)
plot_model(mod.yl,type="pred",terms=c("age"),axis.title=c("Age","Prob(hear Yanny)"),
           title="",ci.lvl=NA)

#Task 4.
#Fit appropriate logistic regression models to explore if gender is related to whether people hear “Yanny”
#or “Laurel”.
mod.yl2 <- glm(hear ~ gender, family=binomial, data=yl)
summary(mod.yl2)

#or we can add gender to the model with age:
mod.yl3 <- glm(hear ~ gender+age, family=binomial, data=yl)
summary(mod.yl3)
#in both cases we see that there is no significant gender effect

##titanic
library(ggplot2)
library(sjPlot)
titanic=read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/titanic.csv"))
titanic$passenger.class=factor(titanic$passenger.class)
head(titanic)

??plot_xtab
plot_xtab(titanic$survived,titanic$gender, show.values = FALSE,
          show.total = FALSE, axis.labels = c("Died", "Survived"),
          legend.title = "Gender")

#There is a clear pattern here with the proportion surviving much higher for women than for men
plot_xtab(titanic$survived,titanic$passenger.class, show.values = FALSE,
          show.total = FALSE, axis.labels = c("Died", "Survived"),
          legend.title = "Class")

mod.titan=glm(survived~gender+passenger.class+age,
              family=binomial(link="logit"),data=titanic)
summary(mod.titan)

#To quantify the effect of each of these predictors, we look at odds ratios which can be computed as exp(훽ˆ).
#These are shown in the plot below.
??plot_model
plot_model(mod.titan,show.values=T)

plot_model(mod.titan,type="pred",terms=c("age","passenger.class","gender"))
#We see the gender and class differences in survival we have already discussed, and also that survival
#probabilities decrease by age.









