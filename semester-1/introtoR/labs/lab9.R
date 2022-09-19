#intro_to_R lab 9
load(url("http://www.stats.gla.ac.uk/~rhaggarty/rp/p9_2020.RData"))

x
?lgamma

#estimated density of the data
par(mfrow=c(2,1))
plot(density(x))
hist(x,freq=T)
#

#task 1
#(a) Write three functions loglik, loglik.d, and loglik.dd that take θ as the first argument and the sample
#x = (x1, . . . , xn) as second argument, and that return `(θ), `respectively.


loglik<-function(theta,x){
  return((theta-1)*sum(log(x))-(1/theta)*sum(x^theta))
}

loglik.d <- function(theta, x){
  sum(log(x)) + theta^-2 * sum(x^theta) - theta^-1 * sum(x^theta *log(x))
}

loglik.dd <- function(theta, x){
  -theta^-1*sum(x^theta*log(x)^2) + 2*theta^-2*sum(x^theta*log(x))
  -2*theta^-3*sum(x^theta)
  
}

#(b) Implement Newton’s method to find the maximum likelihood estimate of θ. Use the function optimize
#to check the result.
?rexp
x=rexp(1e3)
theta.range <- 10:500/100
#To get an idea of how the objective function looks like we can plot it
plot(theta.range, loglik(theta.range, x), type="l", lwd=2,
     xlab=expression(theta), ylab=expression(loglik(theta)))

#We can implement Newton’s method in R using
theta <- 1
for (h in 1:100) {
  old.theta <- theta
  theta <- theta - loglik.d(theta, x) / loglik.dd(theta, x)
  if (abs(theta-old.theta)<1e-10)
    break
}
theta

#check using optimize
?optimize
optimize(loglik, interval=c(0.1,10), x, maximum=TRUE)
#$maximum
#[1] 0.984396


#(c) The lower and upper limit of the 95% likelihood interval (“Wilks interval”) for θ can be found by solving
#the equation
?uniroot
loglik.minus.q=function(theta,theta.hat,x,alpha=0.01){
  loglik(theta.hat,x)-loglik(theta,x)-0.5*qchisq(1-alpha,df=1)
} #two solutions
#we know what kind of the value the sol can or cannot have
theta.hat=theta
theta.left=uniroot(loglik.minus.q,c(0.1,0.98),
                   theta.hat=theta.hat,x=x)$root
theta.right=uniroot(loglik.minus.q,c(0.98,3),
                   theta.hat=theta.hat,x=x)$root
c(theta.left,theta.right)

theta.seq=seq(0.001,10,length.out=100)
plot(theta.seq,sapply(theta.seq,loglik.minus.q,theta.hat=theta.hat,
                      x=x),type="l",)
abline(h=0,lty=2,col=2)

#Task 2
#Use the function rexp to create a random vector x of size 10, 000 from the Expo(1) distribution.
#(a) Compute the mean of x and median of x.
?rexp
set.seed(16032021)
rexp(10000,rate=1)
plot(density(x))
mean(x)
median(x)

#(b) Use the function optimize to find the value m minimising the objective function
#Hint: Create a function which takes the arguments m and x as arguments (in that order), and which returns
#the value of the objective function. Then use optimize to find its minimum
op=function(m){
  sum((x-m)^2)
}
#by regression, m is the mean(x)
optimize(op, interval=c(0.1,10),maximum=F)
#$minimum 
#[1] 0.9941284
#same as mean(x)
#minimize L2 loss-average

#(c) Next use the function optimize to find the value m minimising the objective function
op1=function(m){
  sum(abs(x-m))
}
optimize(op1, interval=c(0.1,10),  maximum=F)
#same as mean(x)
#minimize L1 loss-median

#task 3
#(a) Use the function uniroot to compute a solution to the above equation.\ Hint: Define a function which
#takes z as argument and which returns Φ(z) − 0.95 and apply uniroot to it. You can compare your result to
#the quantile obtained using qnorm(0.95).

#remember that to use uniroot(), we must pass the function f in the form f(whatever) = 0
findz=function(z){
  pnorm(z)-0.95
}
uniroot(findz,c(-10,10))
#$root
#[1] 1.644854
qnorm(0.95)
#same

#(b) Implement Newton’s method to solve the above equation.\ Hint: Φ(z) = φ(z), 
#which can be found in R using the function dnorm
?dnorm
z=seq(-2,2,length.out=100)
plot(z,pnorm(z),type="l")
abline(h=0.95,lty=2,col=2)

theta.new <- 1
for (i in 1:100){
  theta.old <- theta.new
  theta.new <- theta.old - (pnorm(theta.old)-0.95)/dnorm(theta.old)
  
  if(abs(theta.new-theta.old)<1E-8){
    print(i)
    print(theta.new)
    break
  }
}

theta.new

#task 4
#(a) Write a function gmm.loglik which takes the parameter vector par = (p, µ1, σ1, µ2, σ2) as first argument
#and the data x as second argument and which returns the loglikelihood `(p, µ1, σ1, µ2, σ2).
plot(density(mixture.data))

gmm.loglik=function(par,x){
  p=par[1]
  mu1=par[2]
  sigma1=par[3]
  mu2=par[4]
  sigma2=par[5]
  sum(log(p*dnorm(x,mu1,sigma1^2)+(1-p)*dnorm(x,mu2,sigma2^2)))
}
gmm.loglik(c(0.8,-1,1,3,1),mixture.data)

#(b) Use the function optim to maximise the loglikelihood function `(p, µ1, σ1, µ2, σ2) over the parameter vector
#θ = (p, µ1, σ1, µ2, σ2). Use the data contained in the vector mixture.data (available in p9_2020.RData) as
#data x.
?optim
par0=c(0.8,-1,1,3,1) #testing vector
opt.par=optim(par=par0,
              fn=gmm.loglik, 
              x=mixture.data, #pass the second function of the argument
              control=list(fnscale=-1,maxit=1000))
#par starting point
#by default, optim will minimize the function
#use control=list(fnscale=-1),to find max
#maxit (iteration=1000)
#abstol 1e10 

#(c) Using the estimated parameters plot the estimated p.d.f. of X






