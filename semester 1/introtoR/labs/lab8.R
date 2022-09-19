#intro_lab_8
load(url("http://www.stats.gla.ac.uk/~rhaggarty/rp/p8_2020.Rdata"))

#task 1
#(a) The code below (supposedly) computes the variance of x. Find the mistakes in the following code.
compute.variance <- function(x) {
  x.bar <- mean(x)
  n <- length(x)
  variance <- sum((x-x.bar)^2) / (n-1)
  variance
}
compute.variance(c(20,30,40))

#(b) Consider the function What does the function my.function do?
my.function <- function(z) {
  m <- Inf
  for (i in 1:length(z))
    if (z[i]<m)
      m <- z[i]
    m
}
my.function(c(300,20,2,40))
#output the min of z; i.e find the smallest value in

#task 2
data(cars)
plot(cars)
y=cars$dist
x=cars$speed

X=cbind(1,x)

least.squares=function(y,X){
  #conditions if(length(y)!=nrow(X)|ncol(X)>nrow(X)) stop("Error")
  y.hat=X%*%solve(t(X)%*%X,t(X)%*%y)
  y.hat
}
y.hat=least.squares(y,cbind(1,x))

#(b) The linear model requires that the length of y equals the number of rows of X and that X has not more
#columns than rows.
#Add one (or more) if statements to the above definition of your function least.squares that check whether
#these conditions hold and, in the case at least one does not hold, produce a meaningful error message.

least.squares=function(y,X,plot=FALSE){
  if(length(y)!=nrow(X)|ncol(X)>nrow(X)) stop("Error") #增加条件
  y.hat=X%*%solve(t(X)%*%X,t(X)%*%y)
  y.hat
  if(plot){
    plot(X[,2],y,col=2)
    lines(X[,2],y.hat,col=2)
  }
  return(y.hat)
}

y.hat=least.squares(y,cbind(1,x),plot=TRUE)
lines(x,y.hat,col=2)


#task 3

#(a) Define two variables nprovince and nregion using the R functions unique and length which contain
#how many different provinces there are and how many different regions there are respectively.
str(canada)
nprovince=length(unique(canada$province))
nregion=length(unique(canada$region))

#(b) Define a vector called icy which contains the proportion of the year where each station has a temperature
#below zero. The vector should have the same length as the number of stations and each element should
#correspond to a single station.

#for one station
tmp=canada[1,] #利用tmp解决问题
tmp=tmp[6:length(tmp)]
mean(tmp<0) #mean is proportion, but we want to know how to for all stations

icy<-rowMeans(canada[,6:17]<0)
#or
temps=canada[,6:ncol(canada)]
icy=apply(temps<0,1,mean)


#(c) Use the function split to create a list named regional where each element of the list contains a subset
#of the canada dataframe corresponding to a single region (i.e. the list should have as many elements as there
#are unique regions)
?split
regional=split(canada,canada$region)
#利用split函数区别species
#x=apply(regional[[1]][6:17],2,mean)

#(d) Define a matrix region.temp where the rows correspond to the average temperature in each region.
#Your matrix should have 12 columns, each corresponding to a different month and as many rows as there are
#unique regions.

region.temp=matrix(NA,nrow=4,ncol=12)
for(i in 1:4)
  region.temp[i,]=apply(regional[[i]][6:17],2,mean)
###or
f=function(x) colMeans(x[,6:17])
#lapply returns a list
#sapply will simplify the output
region.temp=t(sapply(regional,f)) #怎么对list使用函数？使用sapply,same as lapply(list apply)


#(e) Produce a plot of the average regional temperatures. Your plot should look like the one below where
#each line corresponds to a different region. Each line should be a different colour. Your plot should have an
#appropriate title, axis labels and a legend.
?matplot
matplot(t(region.temp), type="l", lty=1, xlab="Month", ylim=c(-30,30),
        ylab="Temperature", main="Canadian Regional Temperature Profiles")
#注意原图ylim，选择适当的limitation
legend("topleft",legend=rownames(region.temp),bty="n",col=1:4,lty=1)


#task 4
#(a) Write a function z.transform that takes r as its only argument and which returns ζ(r). Your function
#should return an error message if r 6∈ (−1, 1).
z.transform=function(r){
  if(r<=-1|r>=1) stop("r scope error")
  0.5*log((1+r)*(1-r)^-1)
}
z.transform(0.5)

#(b) Write a function inverse.z.transform that takes z as its only argument and that returns ζ −1(z).
inverse.z.transform=function(z){
  (exp(2*z)-1)*(exp(2*z)+1)^-1
}

#(c) Write a function cor.ci which takes the observed correlation ρˆ, n, and α as arguments and that returns
#the asymptotic (1 − α)-confidence interval for ρ. The default value of α should be 5%. Hint: You will need to
#call the functions z.transform and inverse.z.transform you defined in parts (a) and (b).
cor.ci=function(r.hat,n,alpha=0.05){
  if(n<4) stop("scope error")
  x1=inverse.z.transform(z.transform(r.hat)-qnorm(1-alpha/2)/sqrt(n-3))
  x2=inverse.z.transform(z.transform(r.hat)+qnorm(1-alpha/2)/sqrt(n-3))
  c(x1,x2)
}

#or
cor.ci <- function(rho, n, alpha=0.05) {
  if (n<4) stop("Formula not valid if n<4") # Check whether n is large enough
  transformed <- z.transform(rho) # Compute z transform
  transformed.ci <- transformed + c(-1,1) * qnorm(1 - alpha/2) / sqrt(n-3)
  # Create symmetric CI in the transformed domain
  original.ci <- inverse.z.transform(transformed.ci)
  # Transform back to the original domain
  original.ci # Return this transformed CI
}


#(d) Write a function compute.cor.ci which takes two vectors x and y as well as α as its arguments and
#which returns a (1 − α)-confidence interval for the correlation between x and y. Hint: You can use the built-in
#function cor to compute the correlation between x and y. Then use the function $cor.ci* from part (c) to
#compute the confidence interval.
compute.cor.ci=function(x,y,alpha=0.05){
  if(length(x)!=length(y)) stop("x and y must be of the same length")
  cor.ci(cor(x,y),n=length(x),alpha=0.05)
}


#(e) In this part you will use the mammals data from the library MASS.
#Use your function compute.cor.ci to compute a 95% confidence interval for the correlation between the
#body weight (body) and the weight of the brain (brain) of the mammals data.
#Use your function compute.cor.ci to also compute a 95% confidence interval for the correlation between the
#logarithm of the body weight (body) and the logarithm of the weight of the brain (brain) of the mammals
#data.
library(MASS)
data("mammals")
compute.cor.ci(mammals$body,mammals$brain)
compute.cor.ci(mammals$body,log(mammals$brain))









