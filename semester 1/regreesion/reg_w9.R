#reg_w9
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/Data\ sets-20210219")
library(GGally)

lake=read.csv("lakes.csv")
ggpairs(lake)

lakes.all <- lm(ltotalbio ~ lAltitude + lMeanDepth + lRettime + lAlkalinity + lColour +
                  lTotalP, data = lakes)
summary(lakes.all)
ggcoef(lakes.all,exclude_intercept = TRUE)

lakes.all <- lm(ltotalbio ~ lAltitude + lMeanDepth + lRettime + lAlkalinity + lColour +
                  lTotalP, data = lakes)
print(summary(lakes.all),concise=TRUE)

ggcoef(lakes.all,vline_color = "red",
       vline_linetype = "solid",
       errorbar_color = "blue",
       errorbar_height = .25,exclude_intercept = TRUE)

#We will do a model selection to see if we can reduce the model. We use the step function
#which by default uses the AIC criterion for model selection
step(lakes.all)

# Reduced Model
lakes.step <- lm(ltotalbio ~ lAlkalinity + lColour + lTotalP, data = lakes)
summary(lakes.step)

#We notice that the predictor lColour can be dropped based on the p-value criterion. One can
#fit a model removing lColour
lakes.stepred <- lm(ltotalbio ~ lAlkalinity + lTotalP, data = lakes)
summary(lakes.stepred)

#p-values
library(olsrr)
ols_back_p<-ols_step_backward_p(lakes.all)
summary(ols_back_p$model)

#Notice that in this model, some variables still have p-values >0.05. This is because the default
#parameters specific a much higher cut off point of 0.3.
ols_back_p2<-ols_step_backward_p(lakes.all,prem=0.05)
summary(ols_back_p2$model)

ols_back_p2[c("removed","aic","sbc","sbic","adjr","rmse","mallows_cp")]

#forward selection
ols_forward_aic<-ols_step_forward_aic(lakes.all)
summary(ols_forward_aic$model)

ols_forward_aic[c("predictors","aics","arsq","steps")]

ols_forward_p2<-ols_step_forward_p(lakes.all,penter=0.05)
summary(ols_forward_p2$model)

ols_forward_p2[c("predictors","aic","sbc","sbic","adjr","rmse","mallows_cp")]


#Lec 18
lakes <- read.csv("lakes.csv")
lakes.all <- lm(ltotalbio ~ lAltitude + lMeanDepth + lRettime + lAlkalinity + lColour +
                  lTotalP, data = lakes)
FB_selection<-ols_step_both_aic(lakes.all,details=TRUE)

summary(FB_selection$model)

FB_selection[c("predictors","method","aic","rss","arsq","steps")]

#We can repeat this using p-values, this time specifying both an entry p-value and a removal
#p-value
FB_selection_p<-ols_step_both_p(lakes.all,details=TRUE,pent=0.05,prem=0.05)







