#intro_to_R_week 7
n=1e7
x=rnorm(n)
y=rnorm(n)

system.time(z<-x+y)

#A slightly less efficient approach would consist of creating an empty vector z 
#and setting its entries zi = xi +yi one by one using a for loop:
system.time({
  z=numeric(n)
  for(i in 1:n)
    z[i]=x[i]+y[i]
})

#A slightly less efficient approach would consist of creating an empty vector z and setting its entries zi = xi +yi
#one by one using a for loop:
system.time({
  z=numeric(n)
  for(i in 1:n)
    z[i]=x[i]+y[i]
})

#slower
system.time({
  z=c()
  for(i in 1:n)
    z=c(z,x[i]+y[i])
})

#example 2
#In this example we compute the vector of increments di = xi+1 − xi. Our first approach uses a loop:
  
differences.slow <- function(x) {
  n <- length(x)
  d <- numeric(n-1)
  for (i in 1:(n-1))
    d[i] <- x[i+1] - x[i]
  d
}
system.time(differences.slow(x))

#We can also use vectorised code for this task . . .
difference.fast=function(x){
  n=length(x)
  x[2:n]-x[1:(n-1)]
}
system.time(difference.fast(x))

#the function apply
#Suppose you want to find the maximal values of the four numeric columns of Anderson’s iris data set. We
#can do this using a loop:
iris.maxi=numeric(4)
for (i in 1:4)
  iris.maxi[i]=max(iris[,i])
#The apply function allows us to do this in a single line
iris.maxi=apply(iris[,1:4],2,max)

#The syntax of the function apply is
#apply(X, MARGIN, FUN, ...)
#The arguments are:
#  X is the matrix or data frame (or array in more general) to be used.
#MARGIN indicates whether the function should be applied row-wise (MARGIN = 1) or column-wise (MARGIN = 2).
#FUN is the function to be applied. Additional arguments to the function can be supplied using the ... argument.
#Thus apply(iris[,1:4], 2, max) computes the column-wise maxima of the first four columns of the iris
#dataset.
#apply is a very versatile function, we can use it for example to remove all observations with missing values
#from a dataset.

#Example 4
library(MASS)
line.has.na=apply(is.na(Cars93),1,any)
cars.no.has=Cars93[!line.has.na,]
?any

#Example 5
#Suppose we want to compute the coefficient of variation which is the ratio 
#of the standard deviation and the mean.
cv=function(x){
  sd(x)/mean(x)
}
apply(iris[,1:4],2,cv)
#or
apply(iris[,1:4],2,function(x) sd(x)/mean(x))

#Example 6
#We are interested in the question whether the voting pattern is independent of the ethnicity of the voter. This
#can be investigated using a χ2-test. When using Nij to denote the observed counts, then the test statistic is
#defined as
N=rbind(c(392,598,230),c(484,139,177))
colnames(N) <- c("Obama","Romney", "No preference")
rownames(N) <- c("caucasian", "other")

Ni=apply(N,1,sum) #row totals
Nj=apply(N,2,sum) #column totals
total=sum(N)      #overall total
E=Ni%*%t(Nj)/total #expected counts
statstic=sum((N-E)^2/E)
?apply
?lapply
?sapply

#The functions sweep and scale

#Example 7
#Suppose we want to centre the first four columns of the iris dataset by subtracting their means. We can do
#this using a loop
iris.means=apply(iris[,1:4],2,mean)
iris.zeromean=iris[,1:4]
for(i in 1:4)
  iris.zeromean[,i]=iris.zeromean[,i]-iris.means[i]

#However, we can avoid the loop by using the function sweep:
iris.means=apply(iris[,1:4],2,mean)
iris.zeromean=sweep(iris[,1:4],2,iris.means,"-")

#The general syntax of the function sweep is
#sweep(X, MARGIN, STATS, FUN, ..
sweep(iris[,1:4], iris.means, 1, "-")
?sweep

#R has a built in function to standardise data:scale. We can substract the means
#of each column using 
?scale
#Scaling and Centering of Matrix-like Objects
scale(iris[,1:4],center=T,scale=F)

#The function by
#Suppose you want to compute the average values of the four measurements for the three different species in
#the iris data set.
head(iris)
colMeans(iris[iris$Species=="setosa",1:4])
colMeans(iris[iris$Species=="versicolor",1:4])
colMeans(iris[iris$Species=="virginica",1:4])

#Suppose you want to compute the average values of the four measurements for the three different species in
#the iris data set.
for (species in levels(iris$Species))
  print(colMeans(iris[iris$Species==species, 1:4]))

#or
by(iris[,1:4],iris$Species,colMeans)
#The general syntax of the function by is
#by(data, INDICES, FUN, ...)
#The arguments of by are data is the matrix or data frame to be used.
#INDICES contains the variable (a factor, or coercible to a factor) to be used to group the data.
#FUN is the function to be applied to each group. Additional arguments to the function can be supplied using
#the additional ... argument.
#Just like apply, we can use by together with user-defined functions

#Example 9
#The code below computes separate regressions of the petal length against the 
#sepal length for each species
do.regression=function(x){
  lm(Petal.Length~Sepal.Length,data=x)
}
by(iris,iris$Species,do.regression)






























