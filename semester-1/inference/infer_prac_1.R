## Statistical Inference Practical 1 ##



#########################################

## Lines in an R script that start with # are comments and 
## will not be run by R.


###################################


### Section 1 - Nonparametric Tests

############################

### Wilcoxon Signed Ranks Test

## Example 1: Discretionary spending

## The Data

spending <- c(5, 4, 12, 7, 3, -12, 14, 11, 6, -2, 8, -1, 10, 17,-9)

## Use the commands:

## boxplot()
## summary()
## median()
## quantile()

## applied to these data to plot and summarise the data

## A wilcoxon signed ranks test can be carried out using the following command
## to test the null hypothesis that the median difference is 0.

#1 Produce a boxplot of the data.
q1=boxplot(spending,main="Discretionary spending")

#2 Compute the median and quantiles of the data
median(spending)
quantile(spending)

#3 Use a Wilcoxon Signed Ranks test to investigate the hypothesis that the median
#difference is 0.
model1=wilcox.test(spending, mu=0,exact=F)
model1

#4 write down your conclusions from this test
#reject H0 since p value<0.05



## This command has an argument called 'alternative' which specifies whether the
## test is one or two-sided.  The default for the command is 'alternative = "two.sided"'.
## For a one-sided test the 'alternative' argument can be set to "less" or "greater",
## depending on the hypotheses specified.
## i.e. wilcox.test(spending, mu=0, alternative="less")

## Explore the function using ?wilcox.test


#########################


## Mann-Whitney U Test

### Example 2: Sway data

## The data
#Eight elderly and eight young people were subjects in an experiment. Each in turn stood
#barefoot on a ‘force platform’ and tried to maintain a stable and upright position while
#reacting as quickly as possible to an unpredictable noise, by pressing a hand-held button.
#The platform automatically measured how much each subject swayed (in mm) in the
#forward-backward direction, and these data are recorded in the R script. The sway vector
#in R contains all of the data with the group identifiers provided in the vector group.
sway <- c(19, 30, 20, 19, 29, 25, 21, 24, 25, 21, 17, 15, 14, 14, 22, 17)
group <- c(rep("E",8),rep("Y",8))
q2=data.frame(sway,group)
q2_1=subset(q2,q2$group=="E")
q2_2=subset(q2,q2$group=="Y")


#1 Produce a boxplot of the data for each group.
boxplot(q2_1$sway,q2_2$sway,names=c('E','Y'))
######## or
boxplot(sway~group)
?cat
cat("IQR E",IQR(sway[group=="E"]),"\n")
cat("IQR Y",IQR(sway[group=="Y"]),"\n")
IQR(sway[group=="E"])/IQR(sway[group=="Y"])



##2 The Mann-Whitney U test can be carried out using the following command,
## to test the null hypothesis that the population median sway is the same for each group.

wilcox.test(sway~group,exact=F)

#3 conclusion
#p value>0.05, cannot reject H0

##############################


## One-sample t-test

### Example 3: House price

## The data

price <-c(120, 110, 108, 100, 150, 106, 100, 100, 114, 130, 122, 100, 120, 130, 115, 112, 126, 110, 120, 128)



##1 Use the histogram command hist() to plot a histogram of the data.
hist(price)


##2 Use the following command to perform a one-sample t-test, testing the null hypothesis that the population mean is 118.
t.test(price, mu=118)
#p-value = 0.5069

## Explore the function using ?t.test 
?t.test

##3 Use the wilcox.test() command to compare the results to a wilcoxon signed ranks test.
## For this test, you should test the null hypothesis that the population median=118.
wilcox.test(price,mu=118,exact=F)
#p-value = 0.3594

#4 conclusion
# for t test, cannot reject H0; for wilcox test, reject H0

############################



## Two-sample t-test

## Example 4: Salary data
#These data are the starting salaries of 50 Scottish adults who are administrative assistants;
#25 of whom work in the public sector and 25 work in the private sector. The Salary
#vector in R contains all of the data in £1000’s with group identifiers provided in the vector
#Sector, 0 for public and 1 for private.

## The Data
Salary <- c(18.9,10.5,	17.5,	13.1,	13.0,	18.2,	22.0,	13.0,	25.0,	12.2,  10.3,15.5,	24.4,	11.8,	15.0,	25.6,	11.8,	22.8,	19.4,	12.3, 22.7,	27.3,	16.0,	11.0,	12.6,	17.7,	17.2, 20.2,	34.0,	36.4,	11.3,	24.0,	17.6,	26.0,	25.7,	17.2,	14.1,	22.0,	17.2,	20.9,	16.8,	19.3,	15.8,	27.0,	20.4,	25.5,	30.1, 28.3,	29.5,	31.6)
Sector<- c(rep(0,25), rep(1,25))

## Use the following commands to plot histograms of the data for each group
###利用判断语句直接分组，不需要创建data frame
hist(Salary[Sector==0])
hist(Salary[Sector==1])

## Use the following command to perform the two-sample t-test.  This assumes that the population variances for each population are equal.
## We are testing the null hypothesis that the population mean for group 1 = population mean for group 2.

t.test(Salary~Sector, var.equal=TRUE)
#p-value = 0.001392

## Since the normality assumptions is dubious from the histograms 
## use the wilcox.test() command to perform a Mann-Whitney test and compare your results
## to the two-sample t-test.
wilcox.test(Salary~Sector, exact=F)
#p-value = 0.002547



###################################



## Likelihood

#### Example 5

## The air conditioning failures data

aircond <- c(50,44,102,72,22,39,3,15,197,188,79,88,46,5,5,36,22,139,210,97,30,23,13,14)


## Use the commmands provided to plot the log-likelihood. 
n=length(aircond)
thetahat=1/mean(aircond)
theta=seq(0.005,0.025,length=50)
loglik = n * log(theta) - theta * sum(aircond)
plot(theta, loglik, type = "l")
## Develop code to compute the relative log-likelihood.

rela=(n * log(theta) - theta * sum(aircond))-( n * log(thetahat) - thetahat * sum(aircond))

## Plot the relative log-likelihood for different values of theta.
plot(theta,rela,type="l")

## Add lines to the plot to help to estimate 10%, 14.65% and 50% likelihood intervals.
abline(h = log(0.10))
abline(h = log(0.1465))
abline(h = log(0.5))







