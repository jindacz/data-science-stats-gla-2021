#mining_week_1

#W1_example_1+task_6
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/mining/Datasets\ for\ week\ 1")
wine=read.csv("wine.data.csv")
str(wine,vec.len=2)

summary(wine)

#1
#skim gives an alternative to summary
install.packages("skimr")
library(skimr)
skim(wine)

# If you want specific columns to disappear from the output, you set them to
# NULL as follows
my_skim=skim_with(base=sfl(n=length),numeric=sfl(p0=NULL,p100=NULL,
                                                 hist=NULL))
my_skim(wine)

install.packages("knitr")
library(knitr)
knit_print(my_skim(wine))

#The first thing we always want to do with data is plot them to see if there is 
#any unusual features (particularly outliers and skewness which could adversely affect PCA).

#2
pairs(wine[,c(1:2,5:8,14)],pch=20,lower.panel=NULL)


#It’s a little hard to see anything clearly but we can see that the first variable is not continuous (it only
#takes three values). We can also see potential outliers in a couple of scatterplots e.g. lower right corner of
#Flavanoids and Proline plot. There is evidence of some strong linear relationships between some pairs of
#variables, e.g. Total phenols and Flavanoids in particular. We can use the base command plot AND the
#identify command to look at the Flanavoids and Proline graph and identify the outlier (on the far right).
plot(wine$Flavanoids,wine$Proanthocyanins,xlab="Flanavoids",ylab="Proline")

#Use the mouse to click on the point you want and the Esc key to stop the command
outlier=identify(cbind(wine$Flavanoids,wine$Proline))

#We can now remove the outlier (i.e. the 122th observation) and the first variable (i.e. Class) from our
#dataset and look at a correlation matrix.
wine.new=wine[-122,-1]
install.packages("corrplot")
library(corrplot)

M=cor(wine.new)
corrplot(M,method="number",type="upper")

#4 Well, looking at the variances we can see huge differences between the variables (e.g. Ash and Proline,
#etc) which strongly suggests we use the correlation matrix.
wine.pca=princomp(wine.new,cor=T)
wine.pca

#As expected, since we had 13 variables to start with (and 177 observations), we get 13 components. The
#standard deviation of the first component is large compared to the others but it is hard to see from this
#how many components we should retain.

#In order to make this decision we have to decide which of the three methods we wish to use to decide
#on the number of retained components. For this example, we’ll see all of them but remember, in practice,
#we should only ever have used one.

#Proportion of variance: Let’s say we wanted to decide on this based on proportion of variance and we
#wanted to have at least 90% of the original variability explained. We need to see the cumulative proportions of variance for all the components which we get using the summary command.

summary(wine.pca)

###We can see here that if we wanted at least 90%, the smallest number of components that will give us that
#is 8 (as 7 only explain 89%).

#Cattell’s method
plot(wine.pca)

#This isn’t an unambiguous plot, unfortunately. There are a couple of different place that we could say
#represent a change in rate of decrease: between components 3 and 4, or 5 and 6 or 7 and 8. If we wanted
#to really reduce the dimensionality we could argue the first one and say we are retaining 3 components.

#Kaiser’s method: Here we need to look at finding the average eigenvalue to discover which set of components have variation above it that we will retain. Because we used the correlation matrix, we know the
#average should be 1 but let’s check.

#Extract the component standard deviations
sd.pca=wine.pca$sdev
#Find the average var,take the mean after squaring them
ave.var=mean((sd.pca^2))
ave.var

#Find which components have higher than average variance (TRUE)
sd.pca^2>ave.var

#So based on this, we would retain the first 3 PCs.

#task_3
#Some social scientists use Joliffe’s rule, which says that for a PCA run on correlation, only those PCs with
#variation above 0.6 should be retained. Using a version of the previous code to pull out the variances of
#the PCs, find out how many PCs should be retained according to this rule.
wine.pca$sdev^2
wine.pca$sdev^2>0.6

#example_3
#loadings
wine.pca$loadings[,1:8]

#We are usually interested in examining what the data look like in the new reduced space. The scores for
#the PCs on the original data are automatically produced by princomp. So we can look at the pairs plot of
#the 3 PCs we decided to retain.
scores.wine=wine.pca$scores[,1:3]
pairs(scores.wine,pch=20,lower.panel=NULL)

#Interestingly, we see evidence of potentially two or more groups in the data (rather than a single homogeneous population). There may also be a couple of outliers. We could iterate this process by finding the
#outliers, removing them from the data and running PCA again. Often PCA is an iterative process, rather
#than a one and done kind of deal.

#task_5
#By hand using R as a calculator
new.x<-matrix(c(12,4,3,25,100,2,1,0.4,2,4,1,2,600),nrow=1)
colnames(new.x)<-colnames(wine.new)
centre.wine<-wine.pca$center
scale.wine<-wine.pca$scale
first.load<-wine.pca$loadings[,1]

new.x.cent<-new.x-centre.wine
new.x.stand<-new.x.cent/scale.wine
new.x.stand%*%first.load

#use the predict command
predict(wine.pca,as.data.frame(new.x))

#example_5
employ=read.table("eurojob.txt",header=T,row.names=1);head(employ,4)

#task_6
#1. Produce numerical summaries and comment on them
summary(employ)
knit_print(my_skim(employ))

#Comment: All variables are continuous variables, so it is suitable to apply PCA. The standard deviation vary across
#variables, which suggest that we should use the correlation matrix in PCA (This will be formally checked shortly).

#The next step of exploratory analysis is to produce some graphical summaries. 
#Here we could use the pairs plot in R

#2. Produce a sensible plot or plots for these data and comment on them.
pairs(employ,pch=20,lower.panel=NULL)

#Comment: There are a couple of possible outliers. (We’ll leave these for the moment and see if they appear again in
#our scores plot in the end for whether we need to re-run PCA without them included.) The data doesn’t appear to fall
#into groups. There seem to be some linear relationships between pairs of variables but they are not very strong.

#Display the correlation matrix to 2 d.p.s
#library(matrixcalc)
install.packages("matrixcalc")
library(matrixcalc)

#•3 Produce two important numerical summaries for deciding on how to run PCA and 
#to tell how successful it is likely to be. Comment on these.
round(cor(employ),2)

#Look at the standard deviations of the variables
round(sqrt(diag(cov(employ))),1)

#Comment: There are some strong correlation (around -0.7) but a lot of weak (close to zero) correlations as well. It may
#be possible to achieve some dimension reduction here but not a lot. The variation for some variables is much bigger
#than for others, therefore we should use the correlation matrix and not the covariance matrix in PCA.

#4. Run PCA on the appropriate matrix and look at the output
employ.pca=princomp(employ,cor=T)
employ.pca

#5.Assuming we are most concerned with preserving information, how many coefficients should we
#retain if we want to have 90% of the original variability kept?
summary(employ.pca)

#Looking at the Cumulative Proportion line, it is clear that we need to keep 5 components to retain 90% variability.

#6. For Cattell’s method we will need to produce a scree plot.
plot(employ.pca)

#7. For Kaiser’s method we need to look at finding the average eigenvalue to discover which set of components have
#variation above it that we will retain.
employ.pca$sdev^2>1

#Here we have cheated a bit, since we know the average variance is going to be 1 whenever we use the correlation
#matrix. We can see that we would retain the first 3 components in this case

#Let’s now have a look at the loadings from each component.
employ.pca$loadings


#8. Assuming we have decided to retain 2 components, is there any useful interpretation to be had for
#these?

#Component 1 seems to be the difference between the average of manufacturing, power, construction, service,
#social and transporation industries, and the agricultural industry. So this new variable will distinguish between
#countries with agricultural economies and those with industrial economies.

#Component 2 seems to be the difference between the average of mining, manufacturing, power and transportation industries, and the average of service, finance and social industries. So this new variable distinguishes
#between contries with relatively large and relatively small service sectors.

#task_7 
#we can calculate the scores using the following code
obs1<-c(5.1,0.5,32.3,0.8,8.1,16.7,4.3,21.2,6.3)
obs2<-c(4.2,0.7,25.4,0.7,9.3,15.0,5.8,31.0,6.9)
newdata<-rbind(obs1,obs2);colnames(newdata)<-colnames(employ)
new.data<-as.data.frame(newdata);
new.data.scores<-predict(employ.pca,new.data)
new.data.scores[, 1:6] #first 6 new variables

#Let’s produce the scatterplot first
employ.scores2=as.data.frame(employ.pca$scores[,1:2])
plot(employ.scores2$Comp.1,employ.scores2$Comp.2,xlab="Comp.1", ylab="Comp.2")
#There definitely seems to be an issue with at least one outlier. It would be worth identifying and removing it/them
#and re-running PCA to see if it affects the results.

#Task_8
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/mining/Datasets\ for\ week\ 1")
install.packages("TeachingDemos")
install.packages("lattice")
install.packages("ade4")
library(lattice)

#Setting the random generator seed to ensure similar responses when re-running code
set.seed(135)
##################################
#Reading in and preparing the data
##################################
#Open the library ade4 where the data is

library(ade4)
#Load the tortues dataset
data("tortues")
#Look at the first few lines of the data
head(tortues)
#Extract the females turtles data into a new dataset called fem.turt
fem.turt=tortues[tortues[,4]=="F",-4]

#Take the log of all the variables in the new dataset
log.fem.turt=log(fem.turt)
#name the variables
colnames(log.fem.turt)=c("log.length","log.width","log.breadth")

##############
#Summary Plots
##############
#create a pairsplot of the data
pairs(log.fem.turt,pch=20,lower.panel=NULL)
#create a 3-d scatter plot of the data
library(lattice)
cloud(log.length~log.width*log.breadth,data=log.fem.turt)
#Rotate the 3-d scatterplot of the data
library(TeachingDemos)
#Use your mouse to drag the sliders to change the plot
rotate.cloud(log.length~log.width*log.breadth,data=log.fem.turt)

####################
#Numerical Summaries
####################
#Correlation matrix
round(cor(log.fem.turt),2)
#the more correlation, the greater the reduction; 0.97 is high
#Standard deviations
apply(log.fem.turt,2,sd)
#similar sd, use cov matrix
#if sd's are very different, use cor=T in princomp

#############################
#Principal Component Analysis
#############################
pca.turt=princomp(log.fem.turt);pca.turt
#what is standard deviation of comp1,2,3 refers to???
#Change princomp to prcomp and use the help page to find out loadings, scores etc.

######################
#Looking at the scores
######################
head(pca.turt$scores);plot(pca.turt$scores,pch=20)
#spot an outlier
outlier=identify(pca.turt$scores) #empty???
#warning: no point within 0.25 inches
?identify

#####################################
#Run PCA on dataset excluding outlier
#####################################
pca.turt.new=princomp(log.fem.turt[-10,]);pca.turt.new

####################################
#Deciding on number of PCs to retain
####################################
#cattell's method
plot(pca.turt.new)
#cumulative method
summary(pca.turt.new)
#sd: square root of variance==eigenvalue
#proportion of var

#Kaiser's method
sd.pca=summary(pca.turt.new)$sdev
tot.var=sum(sd.pca^2)
ave.var=tot.var/ncol(log.fem.turt) #average variance
ave.var
sd.pca^2>ave.var

##########################
#Interpreting the loadings
##########################
pca.turt.new$loadings
#loading=weights
#if weight>0.4 === all 3 variables have same size of weight, same sign
#pc1 have more or less an average of those three varibles
#pc2 contrast between log.breatdth btw log.length and log.width
#pc3 blank space, close to 0, take it as zero; difference between log.length and log.width
#signs can be swapped; 但是contrast sign才有意义
#log interpretation


#######################
#Calculating new scores
#######################
new.data<-data.frame(log.length=c(4.8),log.width=c(4.7),log.breadth=c(3.9))
predict(pca.turt.new,new.data)


#task_8 use prcomp instead of princomp
#Setting the random generator seed to ensure similar responses when re-running code
set.seed(135)
#############################
#Principal Component Analysis
#############################
pca.turt.2<-prcomp(log.fem.turt[-10,]);pca.turt.2
####################################
#Deciding on number of PCs to retain
####################################
plot(pca.turt.2);summary(pca.turt.2)
sd.pca<-summary(pca.turt.2)$sdev
tot.var<-sum(sd.pca^2)
ave.var<-tot.var/ncol(log.fem.turt)
ave.var
sd.pca^2>ave.var
#####################################################
#Interpreting the loadings and calculating new scores
#####################################################
pca.turt.2$rotation #loading matrix
# pca.turt.2$x #PC scores
new.data<-data.frame(log.length=c(4.8),log.width=c(4.7),log.breadth=c(3.9))
predict(pca.turt.2,new.data)







