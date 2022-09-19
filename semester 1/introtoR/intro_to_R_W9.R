#intro_week_9_optimization

#example 1 MLE
loglik=function(theta,x){
  -sum(x)+(theta-1)*sum(log(x))-length(x)*lgamma(theta)
}

loglik.d=function(theta,x){
  sum(log(x))-length(x)*digamma(theta)
}

loglik.dd=function(theta,x){
  -length(x)*trigamma(theta)
}

x=rexp(1e3)

theta.range=1:500/100
plot(theta.range,loglik(theta.range,x),type="l",lwd=2,
     xlab=expression(theta), ylab=expression(loglik(theta)))

#newton's method
theta=1.5
for(h in 1:100){
  theta=theta-loglik.d(theta,x)/loglik.dd(theta,x)
}
theta

#The above example carries out a fixed number of iterations. It would be better to check convergence at every
#iteration and abort if the change in θ is small enough.
theta=1.5
for(h in 1:100){
  old.theta=theta
  theta=theta-loglik.d(theta,x)/loglik.dd(theta,x)
  if(abs(theta-old.theta)<1e-10)
    break
}
theta

f=function(theta){
  exp(-2*theta)-2
}
f.d=function(theta){
  -2*exp(-2*theta)
}

#In its simplest form Newton’s method can now be implemented as follows:
theta=0
for(h in 1:20){
  theta=theta-f(theta)/f.d(theta)
}

#The R function uniroot makes use of a more powerful version of the bisection method, Brent’s method. The
#syntax of uniroot 

#uniroot(f,interval=c(from....to),...)
uniroot(f,c(-1,1))

#Optimisation in more than one dimension
#A sophisticated version of Newton’s method which can work for multi-parameter optimisation is implemented
#in R’s function optim. Its syntax is
#optim(par, fn, gr, method="BFGS", ...)
#par are the initial values for the parameters to be optimised over.
#• fn is the function to be optimised over its first argument.
#• gr is the gradient (first derivative) of the function (optional - if omitted optim will approximate the gradient).
#• Additional optional arguments (...) are passed on to fn and gr.


#example 4

#We start with defining the Rosenbrock function in R. In order to be able to use optim later on, we need to
#combine θ1 and θ2 into a single parameter

rosenbrock=function(theta,trace=F){
  if(trace)
    points(t(theta))
  100*(theta[2]-theta[1]^2)^2+(1-theta[1])^2
}

rosenbrock.gradient <- function(theta, ...) {
  c(-400*theta[1]*(theta[2]-theta[1]^2) - 2*(1-theta[1]), 200*(theta[2]-theta[1]^2))
}

theta1=seq(-1.5,1.5,length.out=100)
theta2=seq(-1.5,1.5,length.out=100)
val=matrix(nrow=length(theta1),ncol=length(theta2))
for(i in seq_along(theta1))   # Evaluate function on grid
  for(j in seq_along(theta2))
    val[i,j]=rosenbrock(c(theta1[i],theta2[j]))
image(theta1,theta2,val,col=topo.colors(128)) # Draw image
sol=optim(c(0,0),fn=rosenbrock,gr=rosenbrock.gradient,
          method="BFGS",trace=T)
sol
points(sol$par[1],sol$par[2],cex=2,pch=3,col="white") # Mark minimum found (should be (1,1))

#Global optimisation
#A usually more promising approach is to use stochastic optimisation methods. One such method is
#simulated annealing. (not covered here)

#Numerical integration in R
#integrate(f=f, lower=a, upper=b, ...)

#example 6
integrate(function(x) dexp(x,rate=2)*x,lower=0,upper=+Inf)

#integrate can only compute univariate integrals. The function adaptIntegrate from the package cubature
#can be used to compute integrals in higher dimension. High-dimension integrals are however typically easier
#to compute using Monte-Carlo methods (not covered here).




