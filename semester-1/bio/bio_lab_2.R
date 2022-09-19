#bio_stats_lab_2
#What impact do each of the covariates have on survival time, 
#especially the type of care they received?
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/bio/")
library(survival)
dat=read.csv(file="Palliativecare.csv")
head(dat)

#compute the Kaplan-Meier estimator of the survival function for the entire population.
surv1=survfit(Surv(Time,Status==1)~1,
              conf.type="log-log",
              data=dat)
summary(surv1)
#The analysis conducted is a complete case analysis since 1 missing observation is deleted.
#Column 1 shows the uncensored event times t(i) Column 2 shows the
#number of people at risk just before t(i)
# Column 3 shows the number of people who experience
#the event of interest at t(i)
# Column 4 shows the probability of surviving up to t(i)
# Column 5 shows the standard error followed by the 95% CI in Column 6 and 7. The survival probability in
#the last row of the table is not zero, meaning that there must be censored observations beyond
#the survival probability decreases sharply within the first 100 days and plateaus at around 30%.

plot(surv1,main = "Estimated survival function",
     xlab = "Survival time in days",
     ylab = "Survival probability")

#Task 1
#Summarize above output in your own words. In particular comment on the presence/absence of
#ties in the data and on every column in the table (as well as the plot of course).
#The table shows 262 uncensored event times. The transformation used to compute the survival
#probability and its 95% CI is the log(− log) transformation. This transformation is needed to
#ensure that the survival times are within the [0, 1] range.
#The analysis conducted is a complete case analysis since 1 missing observation is deleted.
#Column 1 shows the uncensored event times t(i)
#Column 2 shows the number of people at risk just before t(i)
#Column 3 shows the number of people who experience the event of interest at t(i)
#Column 4 shows the probability of surviving up to t(i)
#Column 5 shows the standard error followed by the 95% CI in Column 6 and 7. 
#The survival probability in the last row of the table is not zero, meaning that 
#there must be censored observations beyond t262; in this example, there are 39 such observations.
#The survival plot shows a sharp decrease followed by a plateauing that starts at around day 100.
#Hence the survival probability decreases sharply within the first 100 days and plateaus at around 30%.



#As the most important covariate is care type, the Kaplan-Meier estimate should be drawn separately for
#each care type, which can be done by specifying a factor variable as covariate. Note, R will still include an
#intercept by default.

surv2 <- survfit(Surv(Time,Status == 1) ~ factor(Care),
                 conf.type = "log-log",
                 data = dat)
plot(surv2,
     col = c("red", "blue", "darkseagreen", "purple"),
     main = "Estimated survival function by care type",
     xlab = "Survival time in days",
     ylab = "Survival probability")
legend("bottomleft",
       col = c("red", "blue", "darkseagreen", "purple"),
       lty = 1,
       legend = c("Discharged", "Inpatient", "MacMillan", "OutPatient"))

#Task 2
#Comment on the plot above
#The survival functions cross for the different care types. This suggests that 
#the proportional hazard assumptions may not hold. To assess this, one can draw
#log-log plot of the survival function for each care type


#The log-log plot of the survival function for each care type is plotted below.
plot(surv2,
     fun = "cloglog",
     col = c("red", "blue", "darkseagreen", "purple"),
     xlab = "Survival time in days",
     ylab = "ln(-ln(KM))",
     main = "Assessing proportional hazards by care type")
legend("topleft",
       col = c("red", "blue", "darkseagreen", "purple"),
       lty = 1,
       legend = c("Discharged", "Inpatient", "MacMillan", "OutPatient"))

#Task 3 Comment on the plot above
#sol: the plot shows the log-log survival function cross at least once for each
#care type, suggesting that the proportional hazard assumption is not tenable. This 
#can be overcame by combining the McMillan and Outpatient together, which are most
#similar to each other

#Task 4
#In order to ensure all necessary assumptions are met, the decision was taken to combine the
#MacMillan and OutPatient care types. This piece of code is hidden from you. Write your own
#R code that achieves that. Call the new 3 level factor Care3 and make sure your code also
#attaches it to the dataset.
Care3=dat$Care
Care3[Care3=="MacMillan"]="Other"
Care3[Care3=="Outpt Care"]="Other"


surv2 <- survfit(Surv(Time,Status == 1) ~ factor(Care3),
                 conf.type = "log-log",
                 data = dat)
plot(surv2,
     col = c("red", "blue", "darkseagreen"),
     main = "Estimated survival function by care type",
     xlab = "Survival time in days",
     ylab = "Survival probability")
legend("bottomleft",
       col = c("red", "blue", "darkseagreen"),
       lty = 1,
       legend = c("Discharged", "InPatient", "Other"))


plot(surv2,
     fun = "cloglog",
     col = c("red", "blue", "darkseagreen"),
     xlab="Survival time in days",
     ylab="ln(-ln(KM))",
     main="Assessing proportional hazards by care type")
legend("topleft",
       col=c("red", "blue", "darkseagreen"),
       lty = 1,
       legend = c("Discharged", "Inpatient", "Other"))

#Task 5
#Based on the above plots, is the proportional hazards assumption met and which of the
#treatments appear to be associated with the longest and shortest survival times? Explain your
#reasoning.
#This time the lines for different care types do not cross, suggesting that the 
#proportional hazard assuption is met. It appears as if discharged patients 
#experience the longest survival time while the inpatient care group experience 
#the shortest survial time. This is only an initial impression and must be
#backed up by a formal statistial test
#[Note, you also have to check the proportional hazards assumption of all other variables you intend to
#include in the PHM. Since this is a simple example to illustrate the steps of a survival analysis, those plots
#are skipped here.]


#The next step is to assess whether there is a significant difference in 
#survival distribution by care type,which can be done using a log rank test.
survdiff(Surv(Time, Status == 1) ~ Care3, data = dat)

#Task 6
#Interpret the log rank test above. Note that the squared difference of observed and expected
#values can be devided by the expected values or a variance meassure called V. For the purpose of
#this course, we will stick to the definition of the χ2 test and devide by E.

#The p-value is much less than 0.05, suggesting that care type is sig. corr. w/
#survival. while the log rank test established a sig. diff. in survival times
#btw treatments, it does not reval anything about size of this effect. To establish
#effect sizes a Cox proportional hazard model can be fit.



mod1=coxph(Surv(Time,Status==1)~factor(Care3)+factor(Age)+factor(Sex),
           data=dat)
summary(mod1)



#Task 7
#Interpret the output above in detail. Make sure you comment on all the information the output
#provides to you and clearly state your reasoning. Not all statistics reported have been discussed
#in the notes and I encourage you to use the R help function as well as books and online
#resources to research what you don’t yet know.

#The summary output first shows the model that was fit. It then shows the number of
#observations, events and missing values. This dataset contains 332 observations and 1 missing
#value. A complete case analysis is conducted and therefore the missing value is not considered
#in the model. 228 of the 332 non-missing observations are uncensored.
#All variables in the model are factors and therefore have a baseline specified. By default, R uses
#the Discharged group as baseline for the variable Care3, since it is alphabetically the first
#category (D, before I and O) and 31-50 as baseline for the variable Age since it starts with the
#smallest number. Due to similar reasoning Female is the baseline of the variable Sex. All
#conclusions drawn from the output are relative to the baseline level.

#The coef column gives the coefficient (denoted βˆ in the lecture notes) on the log scale. It also
#gives the exponential of the coefficient, since the hazard ratios are defined on the exponential
#scale. The standard error and the z-score can be used to compute the p-value. Hence you may
#look at the standard error and z-score or at the p-value - both give the same information.
#Let’s look closer at the Inpatient Care level of the Care3 factor. The coefficient is 2.9, which
#is greater than 0 and on the exponential scale 19.9, which is greater than 1. Therefore the
#Inpatient Care group is associated with a higher hazard and a shorter survival than the
#Discharged group. The z-score is 6.9 with a standard error of 0.4. In order to get a 95% CI we
#need to add / substract 1.96 times the standard error to from the z-score. A quick calculation in
#our heads by using 2 rather than 1.96 gives a z-score that is easily entirely positive. That is
#reflected by a very significant p-value of 0.000000000003.

#We can interpret the hazards ratio of experiencing the event of interest (i.e. dying) as being on
#average 19.9 times greater in the Inpatient Care group compared to the Discharged group. It
#is possible to flip this statement around and say that the hazard of death in the Disharged
#group is on average 0.05 times the hazard of death in the Inpatient Care group. 95% CI on
#the exponential scale are given as well.
#Concordance is a measure for the predictive ability of the model. It gives the proportion of
#predicted observations that have the same ranking as observed observations, for all observations
#where ranking is possible. For example, assume death d1 has occurred at time point t1 and death
#d2 at time point t2 such that t1 < t2. If both deaths are uncensored or if d2 is censored we can
#rank the deaths and say that d1 has occurred before d2. If d1 is censored however, this ranking is
#not possible anymore and this pair of observations would not be used to compute the concordance statistic.
#The Likelihood ratio test and Wald test are two methods to compare the fitted model to the
#intercept only model. The significant p-values show that the model with covariates is
#significantly better at explaining the variation in the data than the intercept-only model.
#Finally, the log rank test indicates a significant difference between at least two subgroups in the
#data.





mod2 <- coxph(Surv(Time, Status == 1) ~ factor(Care3), data = dat)
summary(mod2)



#Task 8
#Quantify the hazard ratio between Inpatient Care and Other. You will need to compute the
#estimate and variance of this difference. You might find following well known result useful:
#Ans
est=mod2$coefficients[1]-mod2$coefficients[2]
var=mod2$var[1,1]+mod2$var[2,2]-2*mod2$var[1,2]


#Task 9
#Answer the research question posed at the beginning of the Problem. Make sure to write full
#sentences in clear and easy-to-understand language. Don’t forget to include a measure of
#uncertainty.

#The only variable that appears to have an impact on survival time is the type of care received.
#There is a statstically significant difference between all three levels of care in the model - those
#are discharged, inpatient care and other (consisting of Macmillan nursing and outpatient care).
#The largest of those differences appears to be between inpatient care and discharged with a
#hazard ratio of 18.4 (7.9, 42.5) followed by the hazard ratio between other and discharged of 6.8
#(3.0, 15.5). The smallest, yet significant, difference can be observed between inpatient care and
#other with a hazard ratio of 2.7 (2.0, 3.6). The intervals given are 95% CI intervals.















