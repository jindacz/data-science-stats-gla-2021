#R_exam_2
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR")

#1.Create a plot of the function f defined as
#f(x)=sin(x)−0.2x2,
#for −5≤x≤5. The axis labels should be "x" and "f(x)".
x=seq(-5,5,length.out=1000)
plot(x,sin(x)-0.2*x^2,xlab="x",ylab="f(x)",type="l")

#2 Add a horizontal red dashed line at 0. Your plot should look like the one below.
f.prime <- function(x) { 
cos(x) - 0.4*x
}
plot(x, f.prime(x), type="l", xlab="x", ylab="Derivative of f(x)")
abline(h = 0, lty = 2, col = 2)

#3 Define the recursive sequence xh=f′(xh−1)+xh−1, for h≥2, with x1=0. 
#Write a loop that computes xh for h=2,…,100.  Save all the 100 values in a vector x. 
#Plot x. Your plot should look like the one below.
x=numeric(100)
x[1]=0
for(h in 2:100){
  x[h]=cos(x[h-1])-0.4*x[h-1]+x[h-1]
}
h=c(1:100)
x
plot(h,x,type="l",col="red",lwd=2)
###or(不用考虑横坐标？)
x = rep(NA, 100)
x[1] = 0
for (h in 2:100) {
  x[h] = f.prime(x[h-1]) + x[h-1]
}
plot(x, type = 'l', xlab = 'h', col = 2)

#4 The sequence xh was constructed using an algorithm called Banach fixed-point iteration.
#This algorithm is finding the solution to f′(x)=0. In other words, the Banach fixed-point
#iteration can be used to find the maximum of f. Adapt (some of) your previous code to 
#compute xh until |xh−xh−1|<10−10 or h exceeds 100, whichever comes first.
x=rep(NA,100)
x[1]=0
for(h in 2:100){
  x[h]=cos(x[h-1])-0.4*x[h-1]+x[h-1]
  old=x[h-1]
  new=x[h]
  if (abs(old-new)<1e-10) 
    break
}
xh=x[!is.na(x)]
xh
#or
xmax <- 0 
for (h in 2:100) {
  old.x <- xmax 
  xmax <- f.prime(xmax)+xmax 
  if (abs(old.x-xmax)<1e-10) 
    break
}
xmax

#5 Suppose you found that the local maximum is at xmax=1.11. 
#Add a vertical dashed line at xmax to the plots of f and f′ you have previously created.
#Your plots should then look like the ones below.
x=seq(-5,5,length.out=1000)
plot(x,sin(x)-0.2*x^2,xlab="x",ylab="f(x)",type="l",
     xlim=c(-5,5))
abline(v=1.11,lty=2,col="red",lwd=2)

x=seq(-5,5,length.out=1000)
plot(x,cos(x)-0.4*x,xlab="x",ylab="Derivative of f(x)",
     type="l",xlim=c(-5,5))
abline(h=0,lty=2,col="red",lwd=2)
abline(v=1.11,lty=2,col="red",lwd=2)

#6 Use R to read in the file datasaurus_dozen.txt correctly and save it as a 
#data frame called datasaurus_dozen.
datasaurus_dozen=read.table("datasaurus_dozen.txt",header=T)

#7 Create a vector called data.types that contains the names of the 12 unique 
#sub-datasets in the datasaurus_dozen data frame.
data.types = unique(datasaurus_dozen$dataset)

#8 Use a loop to compute the five statistics previously mentioned for each type of
#sub-dataset contained in the data.types vector you created. Summarise your results
#in a 12×6 data frame called five.stats that contains the following 6 columns
data.type = unique(datasaurus_dozen$dataset)
mean.x=mean.y=sd.x=sd.y=corr.xy=rep(NA,length(data.types)) #多重赋予初值
for(i in 1:length(data.type)){
  #利用tmp来每一次第i个data types,select different species
  tmp = datasaurus_dozen[datasaurus_dozen$dataset == data.types[i], ]
  mean.x[i]=mean(tmp$x)
  mean.y[i]=mean(tmp$y)
  sd.x[i]=sd(tmp$x)
  sd.y[i]=sd(tmp$y)
  corr.xy[i]=cor(tmp$x,tmp$y)
}
five.stats=data.frame(data.type=data.type,mean.x=mean.x,mean.y=mean.y,
                      sd.x=sd.x,sd.y=sd.y,corr.xy=corr.xy)
#先生成6个数列，然后把他们组成data frame
#而不是先生成matrix，然后往里面填
five.stats

#9 Use a loop to produce 12 plots containing scatterplots of each sub-dataset. 
#Use different colours for each sub-dataset. Display all 12 plots in the same 
#plotting window using par(mfrow = c(3,4), pty = 's', mar = c(2,1,1,1)). 
par(mfrow = c(3,4), pty = 's', mar = c(2,1,1,1))
for(i in 1:length(data.types)){
  #利用tmp来subset是关键
  tmp=datasaurus_dozen[datasaurus_dozen$dataset==data.types[i],]
  plot(tmp$x,tmp$y,col=i)
}

#10 (a) [2 marks] Create a function called f that takes x, a and b as arguments and computes
f = function(x, a, b){
  (b^a/gamma(a))*x^(a-1)*exp(-b*x)
  # dgamma(x, shape = a, rate = b)
}

g = function(x, lambda){
  lambda*exp(-lambda*x)
  # dexp(x, rate = lambda)
}


gen.gamma = function(n, mu, sigma){
  b      = mu/sigma^2
  a      = mu*b
  lambda = b/a
  x.star = a/b
  c      = f(x.star, a, b)/g(x.star, lambda)
  i      = 1
  x      = rep(NA, n)
  
  while(i < n){
    y     = rexp(1, rate = lambda)
    u     = runif(1)
    alpha = f(y, a,b)/(c*g(y, lambda))
    if(u < alpha){
      x[i] = y
      i    = i + 1
    }
  }
  x
}

mu    = 47
sigma = 26
x     = gen.gamma(1000, mu, sigma)
b     = mu/sigma^2
a     = mu*b

# Here we add the theoretical density to the histogram (not needed)

par(cex.main = .8, cex.lab = .7, cex.axis = .6)

hist(x, freq = F,
     main = 'Histogram of generated Gamma values and the theoretical density', 
     ylim = c(0, 0.02))
curve(dgamma(x, shape = a, rate = b), from = 0.0001, to = 150, col = 2, lwd = 2, add = T)


