###Lab 3###

###Firstly tell R, where to look for data
###This is my directory, you use your own one
###Make sure you use "/" and not "\" 

###设置dictionary

setwd("/Users/kurisuuu/Documents/2020-stats/introtoR/Week3LabData")
###You can also click on Session->Set Working Directory

############
###Task 1###
############

##1) Read health data in
health<-read.table("health.txt",header=TRUE)
str(health)
head(health)
###What will you use?
read.table() ### option 1 CORRECT
read.csv()   ### option 2
load()       ### option 3
##2) Read cia data in
cia   <- read.csv("cia.csv",na.strings = "?")
str(cia)
head(cia)
###What will you use?
read.table() ### option 1
read.csv()   ### option 2 CORRECT
load()       ### option 3  

############
###Task 2###
############

###Save the data, make sure:
###Separated by comma
###Comlumn names - YES
###Row names - NO
###Missing values recorded as *

write.table(health,file="health.csv",sep=",",row.names = FALSE,
            col.names = TRUE, na="*")


############
###Task 3###
############

###Copied from the question:####

##学习怎么用 cut(speed,breaks=c(),lables=c())

speed             <- c(34, 49, 52, 24, 60, 74, 55)
speed.discretised <- cut(speed, breaks=3)
speed.discretised
speed.discretised <- cut(speed, breaks=c(0,40,60,100),
                         labels=c("slow", "medium", "fast"))
speed.discretised

###Now add a new column ExpectancyGroup to health data, which
###discretises LifeExpectancy variable with categories
###low, medium and high with breaks at 40 and 70

###用transform来新增一列transform(health,xxx)

health <- transform(health, 
                    ExpectancyGroup=cut(health$LifeExpectancy,
                                        breaks=c(0,40,70,Inf),
                                        labels=c("low","medium","high")
                                        )
)

str(health$ExpectancyGroup)
head(health$ExpectancyGroup)

###How do we set an upper bound for the "high" group?
### Use:
### 1) 1e6 CORRECT
### 2) Inf CORRECT
### 3) LOADS

############
###Task 4###
############

###0) Read the data in
maternity <- read.csv("maternity.csv", header=TRUE)


###Determine:

###1)for each Health Authority the proportion of smoking
###  pregnant women and the proportion of breastfeeding mothers
###  create two new columns called "smokeprop" and "bfprop"
###  respectively

###学习怎么用transform来增加列

maternity <- transform(maternity,
                       smokeprop=Smoking/(Maternities-SmokingUnknown),
                       bfprop=Breastfeeding/(Maternities-BreastfeedingUnknown))
maternity$smokeprop
maternity$bfprop

###2) the name of the Health Authority which both the smallest and 
###   the largest proportion of smoking
###   pregnant women / breastfeeding mothers

###学会用which.min, which.max 来定位最小最大
###Determines the location, i.e., 
###index of the (first) minimum or maximum of a numeric (or logical) vector.

### Smoking
extremes <- c(which.min(maternity$smokeprop),which.max(maternity$smokeprop))
extremes
maternity[extremes,c("HealthAuthority","smokeprop")]

### Breastfeeding

extremes <- c(which.min(maternity$bfprop),which.max(maternity$bfprop))
maternity[extremes,c("HealthAuthority","bfprop")]


###3)the percentage of smoking pregnant women / breastfeeding 
###  mothers in the North West and in London

###学会用subset来subset(x, ...)
###Return subsets of vectors, matrices or data frames which meet conditions.

maternity.london<-subset(maternity,Region=="London")
maternity.nw    <-subset(maternity,Region=="North West")


mean(maternity.london$smokeprop)
mean(maternity.london$bfprop)
mean(maternity.nw$smokeprop)
mean(maternity.nw$bfprop)

###OR
colMeans(subset(maternity, Region=="London")[,c("smokeprop","bfprop")])
colMeans(subset(maternity, Region=="North West")[,c("smokeprop","bfprop")])

###4)the percentage of smoking pregnant women / breastfeeding 
###  mothers for Health Authorities with an average deprivation 
###  score of at most 10 and at least 40

colMeans(subset(maternity,Deprivation <=10)[,c("smokeprop","bfprop")])
colMeans(subset(maternity,Deprivation >=40)[,c("smokeprop","bfprop")])

###5)the percentage of breastfeeding mothers for Health 
###  Authorities with more than 25% (and less than 15%)
###  smoking pregnant women.

###Many smokers:
subset(maternity, smokeprop>0.25)$bfprop
mean(subset(maternity, smokeprop>0.25)$bfprop)

###Few smokers:
mean(subset(maternity, smokeprop<0.15)$bfprop)

############
###Task 5###
############

###Cia data

###1)Delete all observations for which the population 
###  is missing
###subset(),Return subsets of vectors, matrices or data frames which meet conditions.

cia <- subset(cia,!is.na(Population))

###2)Create a data frame called small where the rows 
###  correspond to countries that have a population of less
###  than 10,000 inhabitants?

small<- subset(cia, Population < 1e4)

###3)Create a vector called lowexp which contains the names 
###  of the countries that have a military expenditure
###  of at least 8% of the GDP

lowexp<-subset(cia, MilitaryExpenditure>0.08*GDP)$Country
lowexp

###4)Ignoring missing values, what is the combined GDP of all 
###  European countries?

cia.europe<-subset(cia,Continent=="Europe" & !is.na(GDP))
sum(cia.europe$GDP)

###5a)Create a new column GDPPerCapita, which contains the 
###   per capita GDP
###5b)Create a new column MilitaryExpPerCapita, which contains
###   the per capita military expenditure

cia <- transform(cia,GDPPerCapita=GDP/Population,
                 MilitaryExpPerCapita=MilitaryExpenditure/Population)

###6)Which country has the highest life expectancy?
cia[order(cia$Life,decreasing = TRUE)[1],]

cia[which.max(cia$Life),]

###7)Create a data frame called top10 which contains the rows
###  corresponding to the ten countries with the
###  highest life expectancy.
top10 <- cia[order(cia$Life, decreasing = TRUE)[1:10], ]

############
###Task 5###
############

###0) Load in the alligator data

alligator<-read.csv("alligator.csv")


###1)Create x and y (remember the log!)
x <- log(alligator$Length) # Define x
y <- log(alligator$Weight) # Define y
###2.1)Find betahat0 and betahat1
sxx <- sum((x - mean(x))^2) # Compute sums of squares
sxy <- sum((x - mean(x)) * (y - mean(y)))
beta.1 <- sxy / sxx # Compute estimates
beta.0 <- mean(y) - beta.1 * mean(x)
c(beta.0,beta.1)
###2.2)Create a vector of predictions y.hat
y.hat <- beta.0 + beta.1 * x
###2.3)Compute the estimated variance of the residuals
sigma2 <- sum((y.hat-y)^2) / (length(y) - 2)
###2.4)Compute the coefficient of determination
R2 <- 1 - sum((y.hat-y)^2) / sum((y-mean(y))^2)
###3.1)Create the design matrix
X <- cbind(1,x)
###3.2)Create XtX and Xty
XtX <- t(X)%*%X # Prepare calculation of beta
Xty <- t(X)%*%y
###3.3)Solve the system of equations
beta <- solve(XtX,Xty) # Compute beta
beta
###3.4)Compute the fitted values y.hat
y.hat <- X%*%beta