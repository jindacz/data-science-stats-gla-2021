#reg_w8
#L13
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/Data\ sets-20210219")

trees<-read.csv("TREES.csv")
trees.llm=lm(log(Volume)~log(Diameter)+log(Height),data=trees)
summary(trees.llm)
##
## Call:
## lm(formula = log(Volume) ~ log(Diameter) + log(Height), data = trees)
##
## Residuals:
## Min 1Q Median 3Q Max
## -0.168561 -0.048488 0.002431 0.063637 0.129223
##
## Coefficients:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) -6.63162 0.79979 -8.292 5.06e-09 ***
## log(Diameter) 1.98265 0.07501 26.432 < 2e-16 ***
## log(Height) 1.11712 0.20444 5.464 7.81e-06 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.08139 on 28 degrees of freedom
## Multiple R-squared: 0.9777, Adjusted R-squared: 0.9761
## F-statistic: 613.2 on 2 and 28 DF, p-value: < 2.2e-16
confint(trees.llm)
## 2.5 % 97.5 %
## (Intercept) -8.269912 -4.993322
## log(Diameter) 1.828998 2.136302
## log(Height) 0.698353 1.535894

#similar model
summary(lm(log(Volume)~log(Diameter),data=trees))

#Regression Analysis: log volume versus log height
summary(lm(log(Volume)~log(Height),data=trees))

#Giving in the Church of England.
cofe<-read.csv("cofe.CSV")
library(GGally)
ggpairs(cofe) +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1))

#Regression Analysis: Annual giving versus Employment rate and % of Attendance
giving.lm=lm(giving~employ+attend,data=cofe)
summary(giving.lm)
print(anova(giving.lm))

#Regression Analysis: Annual giving versus Employment
giving.lm2<-lm(giving~employ,data=cofe)
print(summary(giving.lm2),concise=TRUE)

#Regression Analysis: Annual giving versus % of Attendance
giving.lm3<-lm(giving~attend,data=cofe)
print(summary(giving.lm3),concise=TRUE)

#confidence interval
confint(giving.lm)

ggcoef(giving.lm,vline_color = "red",
       vline_linetype = "solid",
       errorbar_color = "blue",
       errorbar_height = .25,exclude_intercept = TRUE)+
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1))

confint(giving.lm2)
confint(giving.lm3)

#lec_16
?mtcars
model <- lm(mpg~., data = mtcars)
summary(model)

library(GGally)
ggpairs(mtcars,upper=list(continuous=wrap("cor",size=3,col="black")))

model2 <- lm(mpg ~ hp, data = mtcars)
summary(model2)

#hp coefficient estimate from full model
summary(model)$coefficient["hp",]

#hp coefficient estimate from simple linear regression
summary(model2)$coefficient["hp",]

library(knitr)
library(kableExtra)
library(olsrr)
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
model.selection<-ols_step_all_possible(model)
model.selection

plot(model.selection)

model <- lm(mpg ~ disp + hp + wt +
              qsec, data = mtcars)
model.selection <- ols_step_best_subset(model)
library(data.table)
data.table(Model = model.selection$predictors,
           AIC = model.selection$aic,
           Cp = model.selection$cp, BIC = model.selection$sbc,
           R2 = model.selection$rsquare,
           R2adj = model.selection$adjr)

