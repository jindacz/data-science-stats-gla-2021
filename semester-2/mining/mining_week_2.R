#data_mining_week2
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

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

round(cor(log.fem.turt),2)
apply(log.fem.turt,2,sd) #use cov matrix

biplot(pca.turt)

setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/semester\ 2/mining/Datasets\ for\ week\ 2-20210520")
pairs(USArrests,pch=20,lower.panel=NULL)

data("USArrests");str(USArrests,vec.len=2)

library(skimr);library(knitr)
knit_print(skim(USArrests))

#Task 1.
#Based on these summaries, would you recommend using the covariance matrix or the correlation matrix
#for PCA?
  
#Example 2.
#Now, we can have a look at the correlation matrix
cor.arrest=cor(USArrests)
round(cor.arrest,2)

#Task 2.
#Based on the correlation information, do you think PCA will work well as a dimension reduction tool in
#this case?

set.seed(1)
arrest.pca=princomp(USArrests)
summary(arrest.pca)

#Since the first two components represent 99% of the variability, they should present most of the relevant
#information in this data. Therefore, we take a look at the biplot of the first 2 PCs.

biplot(arrest.pca,xlim=c(-0.3,0.4))

#example_4
#By default, biplot has used the rownames of the dataset as labels for the points. This makes the plot a
#bit cluttered so we’ll take this information out using the xlabs argument. Also, to get a better look at the
#smaller arrows, we’ll reduce the text size using cex and reduce the arrow size using arrow.len.
biplot(arrest.pca,xlim=c(-0.3,0.4),xlabs=rep("*",nrow(USArrests)), cex=c(1,0.5),
       arrow.len=0.05)

#By default the biplot always shows the first 2 PCs. If we are interested in other PCs, say PC3 and PC4,
#we can visualise by using choices as shown below.
biplot(arrest.pca,xlabs=rep("*",nrow(USArrests)), cex=c(1,0.5),arrow.len=0.05,
       choices=c(3,4))

#Finally, if we want to look at a ggplot style version of a biplot we can use the command ggbiplot. The
#library containing this can be loaded in the following way:
library(devtools)
install_github("vqv/ggbiplot",force=T)

library(ggbiplot)
gb <- ggbiplot(arrest.pca,circle = T)

#Here the circle added with circle=T is simply a circle around the origin with radius equal to the longest
#arrow (which in this case, the variable with the longest arrow is Assault). This helps us compare the lengths
#of the other arrows with this one more easily

#Task 3.
#• Which variable has the second largest variability?
#• How related are Rape and Urban Population to each other?
#• Which component does Urban Population have its highest loading on?

#example_5
#The euro dataset provides the road distances (in kilometers) between 21 cities in Europe. We are interested in using the classical MDS to recover the coordinates of these cities and display them in two
#dimensions.
euromat=eurodist
euro.cmds=cmdscale(euromat,k=2)
plot(euro.cmds[,1], euro.cmds[,2], xlab="Coordinate 1",
     ylab="Coordinate 2", main="classical MDS", type="n",asp=1)
text(euro.cmds[,1], euro.cmds[,2],labels = labels(euromat),cex=.7)

#The plot allows us to represent the distances between cities in a two-dimensional space. However, the
#representation is not identical to a geographical map of Europe: Athens is in the north while Stockholm
#is in the south. This “anomaly” reflects the fact that the representation is not unique; the distances are
#equally good whether we take a mirror image or rotate it. We could simply invert the vertical axis to get
#a more sensible map.

#example_6
#Let’s look at the US crime data from another perspective. Here we have the correlation information
#between seven types of crime. Our goal is to represent the seven crimes by seven points in a geometric
#space so that the closer two points lie in the MDS solution, the higher the correlation of the crimes that
#they represent.
install.packages("smacof")
library(smacof);crimes

#As the data is a correlation matrix, we need to first convert it into a dissimilarity matrix. After that, we
#can apply distance-based metric scaling to preserve these pairwise information
crime.dist=sim2diss(crimes,method="corr")
set.seed(1)
?mds
crime.mds=mds(crime.dist,ndim=2,type="interval")
# crime.mds$conf #matrix of fitted configurations
plot(crime.mds,xlab="Axis 1", ylab="Axis 2")

#What has been gained by analysing the crime data via MDS? First, instead of 21 correlations, we get a
#simple picture of the interrelations, which is much easier to explore the structure of the correlations. By
#looking along the first axis, we see that the crimes form certain neighborhoods in MDS: violent crimes
#(murder, assault, rape) emerge in one such neighborhood, and property crimes (auto theft, larceny, burglary) form another neighborhood.
#The second axis is difficult to interpret. So, one can ask whether it suffices to represent the given data in
#a one-dimensional MDS space.

crime.mds1=mds(crime.dist,ndim=1,type="interval")
plot(crime.mds1$conf,rep(0,7),xlab="axis 1",ylab="",yaxt="n")
text(crime.mds1$conf,0.5+rep(0,7),labels=names(crimes),cex=.7)

#The 1D scale also seems to make sense: it orders the crimes in terms of increasing violence and brutality.
#To decide which one to use, we may compare the Shepard diagrams.
plot(crime.mds, plot.type="Shepard", main="Shepard diag. 2D")
plot(crime.mds1, plot.type="Shepard", main="Shepard diag. 1D")

#Clearly, the points adhere to the straight line more in the 2D case. We could also calculate the Pearson
#correlation between the fitted distances and original correlation matrix. The correlation equals to -0.99
#when using two dimensions and -0.87 when using one dimension. Therefore, for this dataset, selecting
#two dimensions can well preserve the original correlation information
cor(as.matrix(crime.mds1$confdist)[lower.tri(crime.mds1$confdist)],
    as.matrix(crimes)[lower.tri(crimes)])

cor(as.matrix(crime.mds$confdist)[lower.tri(crime.mds1$confdist)],
    as.matrix(crimes)[lower.tri(crimes)])

#Task 4.
#Perform the Sammon mapping on the crimes dataset using the command sammon from the MASS library
#and comment on the scatterplot.
library(MASS);library(smacof)
crime.dist=sim2diss(crimes,method="corr")
set.seed(1)
crime.sm=sammon(crime.dist,k=2)

plot(crime.sm$points,type="n",asp=1)
text(crime.sm$points,labels=names(crimes))
#The plot is quite similar to the result obtained from metric MDS, except that the distance between Murder and Assault
#increases slightly.
#To select between metric MDS and Sammon mapping, we can again make use of the Shepard diagram. As sammon is
#not from the smacof library, we cannot easily plot the Shepard diagram by setting plot.type="Shepard". Instead,
#an additional command Shepard is needed to calculate
set.seed(1)
crime.mds=mds(crime.dist,ndim=2,type="interval")
par(mfrow=c(1,2))
plot(as.dist(crime.dist),crime.mds$confdist,pch=16,xlab="Dissimilarities",
     ylab="Configuration Distances", main="Shepard diag.(metric MDS)")
crime.mds.fit=lm(as.vector(crime.mds$confdist)~
                   as.vector(as.dist(crime.dist)))
?as.dist
?dist
abline(crime.mds.fit,col="red",lty=2)
plot(as.dist(crime.dist),dist(crime.sm$points),pch=16,xlab="Dissimilarities",
     ylab="Configuration Distances", main="Shepard diag.(Sammon)")
crime.sm.fit <- lm(as.vector(dist(crime.sm$points))~as.vector(as.dist(crime.dist)))
abline(crime.sm.fit,col="red",lty=2)
par(mfrow=c(1,1))

#Configuration points found from metric MDS aligns closer to the straight line compared to Sammon mapping. Therefore, the metric MDS method would be more suitable for this dataset.

#example_7
library(smacof);wish

diss=sim2diss(wish,method=7) #sim. to dissim. by subtracting from 7
set.seed(1)
res=mds(diss,type="ordinal")
plot(res,asp=1)

#example_9
n=100
x=sort(runif(n)) # Create 100 values between 0 and 1.
y=5*x^2-x+0.1*rnorm(n) # Create simulated response.
plot(x,y) # Plot the data.
reg.model=lm(y~x+I(x^2)) #fit the regression model
lines(x,fitted(reg.model),col="red") #plot the fitted function

#What if we change the range of the 𝑥𝑖 from [0, 1] to [1000, 1001]? According to the theory of the linear
#model the fitted values should be exactly the same.
z=x+1000 #add 1000 to each x
plot(x,y) #plot the data
reg.model=lm(y~z+I(z^2)) #fit the reg model
lines(x,fitted(reg.model),col="red") #plot the fitted function

eigen(crossprod(cbind(1,x,x^2)))$values
eigen(crossprod(cbind(1,z,z^2)))$values
#The third eigenvalue is almost zero, making the matrix 𝒁𝑇𝒁 almost singular

#So the linear model is not suitable if we have more covariates than observations or if the covariates are highly correlated. To understand how we can address these problems we need to look at the eigendecomposition and the singular
#value decomposition.

#video_2
mod=lm(log.length~log.width+log.breadth,data=log.fem.turt)
mod

#regress log length on the two pcs
pca.1=princomp(log.fem.turt[,2:3]) #use cov cuz cor is high, redunduncy
mod1=lm(log.fem.turt$log.length~pca.1$scores) 
mod1
pca.1$loadings%*%mod1$coefficients[-1] #get back the original variable

#first component 96 var
mod2=lm(log.fem.turt$log.length~pca.1$scores[,1])
mod2
pca.1$loadings[,1]*mod2$coefficients[2]

#example_10
prostate <- read.csv("prostate.csv");pairs(prostate,lower.panel = NULL)

#Looking at the pairs plot, some correlations with lpsa are evident, but a good predictive model is difficult
#to construct by eye. First, we will fit a standard regression model of lpsa on all other variables.
mod=lm(lpsa~.,data=prostate)
#recall the ~. notation means use all other variables apart from lpsa
mod

#Next we run a PCA on the explanatory variables and use the scores as input to a new regression model.

#we remove the 9th variable which is our outcome
prostate.pca<-princomp(prostate[,-9])
mod.pcr<-lm(prostate$lpsa~prostate.pca$scores)
coef(mod.pcr)

t(prostate.pca$loadings%*%mod.pcr$coefficients[-1])
#we leave out the first coefficient (i.e. the intercept) which we are uninterested in.

#So have we really achieved anything here? We got the same coefficients doing the regression one way as we did
#another. However, in cases where there is multicollinearity, we may not trust the regression on the original variables,
#while we can trust the estimation on the uncorrelated components to be stable.
#Note that here we used PCA with the covariance matrix. If we had used the correlation matrix, in addition to taking
#the inner product with the loadings we would also have to re-scale the slopes (due to the standardisation done in the
                                                                               PCA).
#Task 6.
#Run the original variables regression and the all PCs regression on the USArrests data (from earlier in
#the biplot example) to prove to yourself that you will get the same slope coefficients back from PC regression as regular regression when retaining all components and using the loadings to back transform
#the estimated slopes. Use Murder as the outcome variable in both cases. Try doing it for PCA using the
#covariance matrix and using the correlation matrix (note the previous comment in the latter case).
data("USArrests")
mod1<-lm(Murder~.,data=USArrests)
mod1
pca=princomp(USArrests[,-1]) #for cov matrix
mod2=lm(USArrests$Murder~pca$scores)
mod2
(pca$loadings)%*%mod2$coefficients[-1]

pca.cor=princomp(USArrests[,-1],cor=T) #for correlation matrix
mod3=lm(USArrest$Murder~pca.cor$scores)
mod3

(pca.cor$loadings/pca.cor$scale)%*%mod3$coefficients[-1]


#example_11
p<-ncol(prostate)-1 #we "lose" one of the outcome variables
MSE<-numeric(p)
prostate.pca<-princomp(prostate[,-9])
for (q in 1:p) {
  temp<-lm(prostate$lpsa~prostate.pca$scores[,1:q,drop=F])
  MSE[q]<-sum((prostate$lpsa-temp$fitted)^2)
}
which.min(MSE)
plot(c(1:p),MSE,type="b",xlab="No. of PCs")

#The best MSE is obtained for 8 PCs, the same as the number of the original explanatory variables. So no
#dimension reduction or regularisation takes place in this case.
#If we looked at the % of variability and chose the number of components to represent 99% of the original
#variability and ran a regression on this subset we would get the following:
n_pc <- which(cumsum((prostate.pca$sdev^2)/sum(prostate.pca$sdev^2))>0.99)[1]
mod.reduced<-lm(prostate$lpsa~prostate.pca$scores[,1:n_pc])
t((prostate.pca$loadings[,1:n_pc])%*%mod.reduced$coefficients[-1])

mod.full=lm(lpsa~.,data=prostate)
mod.full
#We see that when we use only 2 PCs we no longer get the same coefficients back as the original model.
#We note that in absolute terms the absolute values of the slope coefficients for our reduced model are
#all closer to zero than in the original. This is often the case when we use a regularised model.

#Task 7.
#In the USArrests data, look at the coefficients for the regression of Murder on all other variables. Now
#look at the regression of the Murder variables on the number of components (with PCA on the covariance
#matrix) that represent 99% of the variabililty in the model. Compare the coefficients of the two models.
#Compare the MSE for the two models. Which model is better in terms of MSE?
data("USArrests")
mod1=lm(Murder~.,data=USArrests)
mod1

pca<-princomp(USArrests[,-1])
summary(pca)

mod2<-lm(USArrests$Murder~pca$scores[,1:2])
t((pca$loadings[,1:2])%*%mod2$coefficients[-1])

MSE.1<-sum((USArrests$Murder-mod1$fitted.values)^2)
MSE.1

MSE.2<-sum((USArrests$Murder-mod2$fitted.values)^2)
MSE.2

#######
#mining tut 2


