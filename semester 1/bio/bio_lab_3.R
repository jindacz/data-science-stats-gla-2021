#bio_lab_3
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/bio")
dat <- read.csv(file = "Air pollution and health.csv")
head(dat)

#Task 1
#Compute the SMR and attach it as a variable to the dataset. Call the variable SMR.
dat=transform(dat,SMR=YhospResp/EhospResp)
dat
#or
dat$SMR=dat$YhospResp/dat$EhospResp

# xi denotes a vector of covariates. Multicollinearity is undesired in a regression model and you should
#consider to only include a subset of the potential covariates listed above.

#Task 2
#Produce a plot and a table showing the correlations between all variables that can be included in
#the model. Make sure to use the logarithm of the SMR, since the logarithm is the link function
#in Poisson regression. Interpret the output.
pairs(cbind(log(dat$SMR),dat[,5:12]))
cor((cbind(log(dat$SMR),dat[,5:12])))

#income and employment; high positve correlation
#...


#Task 3
#Fit the Poisson model discussed above. Justify the covariates you include. Produce summary
#output and interpret it.
?offset
?glm
formula=YhospResp~offset(log(EhospResp))+income+crime+access+no2
mod1=glm(formula=formula,family="poisson",dat=dat) #possion familiy
summary(mod1)

#possion model
#log(SMR)=log(YhospResp/EhospResp)=log(YhospResp)-log(EhospResp)
#=bo+b1*income+b2*....

#relative risk(income ): every one unit incre. in income, the average visit increase
#by exp(3.528e-02)
(exp(3.528e-02)-1)*100 #relative risk
#percent


#Task 4
#Compute the relative change in risk for
#• a one unit increase in no2;
exp(6.296e-03) #change
#1.006316
(exp(6.296e-03)-1)*100 #RR
###incrase in  0.6315861 percent for every unit in no2

#• a one standard deviation increase in no2.
sdno2=sd(dat$no2)
(exp(sdno2*6.296e-03)-1)*100
###incrase by 3.5 percent for every standard deviation increase in no2


#Why might we prefer using the standard deviation?



#Task 5
#In Task 4 you have produced two risk estimates. Do those estimates take confounding into
#account?
#yes the estimate is produced when all other variables are constant,
#so confounding is taking care of


#Task 6
#95% CIs of the SMR are usually produced on the log scale and are then exponentiated. That is
#partly due to the fact that Poisson regression produces estimates on the log scale anyway (due to                                                                                           its natural link function), but furthermore the distribution of log(SMR) tends to look more
#Gaussian. Produce histograms of the SMR and the log(SMR) to verify that.
hist(mod1$fitted.values/dat$EhospResp,main="Histogram of the SMR",
     xlab="SMR")
hist(log(mod1$fitted.values/dat$EhospResp),main="Histogram of the SMR",
     xlab="SMR")



#Task 7
#Compute the risk of being admitted to hospital in an IZ with following covariate values.
#Interpret that risk in a full sentence.
linPred=mod1$coeff[1]+mod1$coeff[2]*8+
  mod1$coeff[3]*350+mod1$coeff[4]*10+mod1$coeff[5]*5
linPred

risk=exp(linPred)
risk

#SE of estimate is the sum of standard errors
SE=summary(mod1)$coeff[1,2]+summary(mod1)$coeff[2,2]*8+
  summary(mod1)$coeff[3,2]*350+summary(mod1)$coeff[4,2]*10+
  summary(mod1)$coeff[5,2]*5
SE

lower=exp(linPred-1.96*SE)
upper=exp(linPred+1.96*SE)
c(lower,upper)

#Task 8
#The lower quartile of no2 is 5.807, hence Task 7 uses a low, but still realistic pollution value.
#Repeat the risk calculation for the value of the upper quartile and the maximum of no2. You
#don’t have to produce 95% CI for this task. Interpret your results.
summary(dat$no2)
riskUpperQuart=exp(mod1$coeff[1]+mod1$coeff[2]*8+
  mod1$coeff[3]*350+mod1$coeff[4]*10+mod1$coeff[5]*14.044)
risk
riskUpperQuart

riskMax=exp(mod1$coeff[1]+mod1$coeff[2]*8+
          mod1$coeff[3]*350+mod1$coeff[4]*10+mod1$coeff[5]*38.291)
riskMax

#with no2:5 estimate is 0.8555, no2=14, risk=0.905, no2=39, risk close to 1



#Task 9
#Task 8 showed that even when air pollution increases to levels equal to the upper quartile, i.e. to
#rather high values, the SMR remains below 1. That might seem counterintuitve at first - explain
#why that is the case

#Task 10
#Criticise following statement: ‘The above analysis shows that hospital admission due to
#respiratory disease is caused, at least partly, by air pollution. These results are statistically
#significant.’










