#W7_lec_13
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/regreesion/Data\ sets-20210219/")
trout=read.csv("TROUT.csv")
trout$Group=factor(trout$Group)

library(ggplot2)
ggplot(data=trout, aes(y=Energy,x= Ration, group=Group, color=Group,shape= Group)) +
  geom_point(size = 3, alpha = .8) +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE)+
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1), legend.position = c(0.15,0.85),
    legend.background = element_rect(fill="transparent", size=0.5,
                                     linetype="solid")) +
  scale_shape_discrete(breaks=c("1", "2"),
                       labels=c( "Dominant","Subordinate")) +
  scale_color_discrete(breaks=c("1", "2"),
                       labels=c( "Dominant","Subordinate"))

trout.lm<-lm(Energy~Ration+Group,data=trout)
trout.lm.single<-lm(Energy~Ration,data=trout)
summary(trout.lm)

#The first indicator that we should keep the two parallel lines is the fact that the term Group is
#significant in the summary of the output of model

#We can obtain the same conclusion using the anova function. In this case we have two competing models. 
#Model 1 is the model including Ration and Group and Model 2 is the model including
#only Ration. When given a sequence of models in this way, anova tests the models against one another. The null model is the simplier of the two (Model 2). Given the p-value of 0.003958, we have
#evidence to reject this model.

print(anova(trout.lm,trout.lm.single))
## Analysis of Variance Table
##
## Model 1: Energy ~ Ration + Group
## Model 2: Energy ~ Ration
## Res.Df RSS Df Sum of Sq F Pr(>F)
## 1 17 1582.1
## 2 18 2614.5 -1 -1032.5 11.094 0.003958 **
#. When given a sequence of models in this way, anova tests the models against one another. The null model is the simplier of the two (Model 2). Given the p-value of 0.003958, we have
#evidence to reject this model.


fit<-predict(trout.lm, interval="confidence")
trout.fit<-data.frame(trout,fit)
ggplot(data=trout.fit, aes(x=Ration,y= Energy, colour = Group, shape= Group))+
  geom_point(size = 3, alpha = .8) +
  geom_line(aes(x=Ration,y=fit),size=1.5)+
  geom_ribbon(aes(ymin = lwr, ymax = upr), linetype = "dotted", alpha = I(0.2)) + #For CI
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1), legend.position = c(0.15,0.85),
    legend.background = element_rect(fill="transparent", size=0.5, linetype="solid")) +
  guides(color = guide_legend(label.position = "right") )

#rds
rds=read.csv("rds.csv")
#* ==two different slope
resp.separate <- lm(Lrate~GA*RDS,data=rds)
summary(resp.separate)
print(anova(resp.separate))

#Neither the confidence interval and p-value for the interaction terms provide enough evidence
#to conclude that the two seperate regressions model is appropriate, so we will instead try to fit the
#“parallel lines” model to the data

#+ == two same slope
resp.parallel<- lm(Lrate~GA+RDS,data=rds)
summary(resp.parallel)
print(anova(resp.parallel))

#There is therefore no evidence, from both the confidence interval (-0.58, 0.8) and p-value for
#RDS (0.743), of a difference between the regression lines. We conclude that there is insufficient
#evidence of a difference in the rate of blood flow through pores between babies who suffer from
#RDS and those who do not and finally conclude that a single line is a good model for the data.


resp.single <- lm(Lrate~GA,data=rds)
print(anova(resp.separate,resp.parallel,resp.single))



hab=read.csv("HAB.csv")
ggplot(data=hab, aes(x=HAB.length,y= Weight, colour = Month, shape= Month))+
  geom_point(size = 3, alpha = .8) +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE) +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1), legend.position = c(0.15,0.85),
    legend.background = element_rect(fill="transparent", size=0.5, linetype="solid")) +
  guides(color = guide_legend(label.position = "right") )

hab.separate <- lm(Weight~HAB.length*Month,data=hab)
print(anova(hab.separate))


## Analysis of Variance Table
##
## Response: Weight
## Df Sum Sq Mean Sq F value Pr(>F)
## HAB.length 1 145746 145746 38.9985 4.248e-06 ***
## Month 1 25126 25126 6.7231 0.01739 *
## HAB.length:Month 1 14696 14696 3.9324 0.06127 .
## Residuals 20 74745 3737
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#The term HAB.length:Month is the interaction term. If this has a small p-value (i.e. typically <
#0.05) then it suggests that the model with completely separate regression lines is the most appropriate i.e. there is a statistically significant interaction. If this p-value is not < 0.05 then we should
#consider the parallel lines model.

hab.parallel <- lm(Weight~HAB.length+Month,data=hab)
summary(hab.parallel)
##
## Call:
## lm(formula = Weight ~ HAB.length + Month, data = hab)
##
## Residuals:
## Min 1Q Median 3Q Max
## -103.384 -46.786 -3.306 36.083 109.840
##
## Coefficients:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1293.553 361.884 -3.574 0.00179 **
## HAB.length 18.196 3.214 5.662 1.28e-05 ***
## Month 65.797 27.090 2.429 0.02421 *
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 65.26 on 21 degrees of freedom
## Multiple R-squared: 0.6564, Adjusted R-squared: 0.6237
## F-statistic: 20.06 on 2 and 21 DF, p-value: 1.344e-05
#If the p-value for the factor (which is Month here) is small (typically < 0.05) then we conclude
#that the parallel lines model is the most appropriate. However, if the p-value is > 0.05 it suggests
#that there is insufficient evidence of a difference between the two regression lines and one line
#may be appropriate i.e. the relationship is the same regardless of the group

hab.single <- lm(Weight~HAB.length,data=hab)
print(anova(hab.separate,hab.parallel,hab.single))
## Analysis of Variance Table
##
## Model 1: Weight ~ HAB.length * Month
## Model 2: Weight ~ HAB.length + Month
## Model 3: Weight ~ HAB.length
## Res.Df RSS Df Sum of Sq F Pr(>F)
## 1 20 74745
## 2 21 89441 -1 -14696 3.9324 0.06127 .
## 3 22 114566 -1 -25126 6.7231 0.01739 *

#model 3 is the null model, P<0.05 reject model 3
#We could have also considered the model selection using the anova function and compare the
#two separate, two parallel and single line. and as the p-value < 0.05 we do not have evidencew
#that the interaction term is significant, therefore we drop the interaction term and use the simpler
#parallel lines model.


