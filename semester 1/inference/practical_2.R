### Statistical Inference Practical 2



### Likelihood, Likelihood Intervals and Optimisation


#----------------------------------------------------------------
#	Example 1 - Air Conditioning Failures (Chapter 2 and 3)
#----------------------------------------------------------------

aircond <- c(50,44,102,72,22,39,3,15,197,188,79,88,46,5,5,36,22,139,210,97,30,23,13,14)


## The number of observations is given by:

n <- length(aircond)


##  The maximum likelihood estimate (MLE) was found to be:

thetahat <- 1/mean(aircond)


##  To plot the log likelihood set up a sequence of values for theta around the MLE

theta  <- seq(0.005, 0.025, length = 50)


## The log likelihood can be found using:

loglik <- n * log(theta) - theta * sum(aircond)


########################################

## Task 1: plot the relative log likelihood against theta

## The relative log likelihood is given by:

rloglik <- loglik - n * log(thetahat) + thetahat * sum(aircond)


plot(theta, rloglik, type = "l")

##########################################


## Lines can be added to this plot to provide approximate intervals

## Task 2: Firstly, a Wilks interval can be displayed by adding a 
## horizontal line at h=-1.92

abline(h=-1.92, lwd=3, col=2)

#The 'lwd' argument changes the line width and 'col' changes the colour

########################################

##########################################

## Initial estimates for each interval can be estimated by looking at
## the minimum value of theta that is greater than or equal to the line and
## the maximum value of theta that is greater than or equal to the line.

## Task 3: Run the following commands compute this to give the initial estimates for 
## a Wilks interval.

min(theta[rloglik>=-1.92])
max(theta[rloglik>=-1.92])


###################################


### Task 4: Type in the commands from the handout to explore the rpanel library.

library(rpanel)
rp.likelihood("sum(log(dexp(data, theta)))", aircond, 0.005, 0.03)


## The command produces a panel which plots the likelihood and log likelihood
## function for the air conditioning data over a range of theta values which start at
## 0.005 and end at 0.03.  These values can be altered, you can try different values
## here.  The values were only chosen to provide a range of values either side of
## the MLE, which we know is 0.016 from previous examples.

## Try the different options in the panel to display the maximum likelihood estimate,
## the relative likelihood function, a wilks (ci) confidence interval, a wald (quadratic) confidence interval, 
## and move the threshold line to investigate alternative likelihood intervals. 







#----------------------------------------------------------------
#	Example 2 - New Haven temperature data (Chapter 3)
#----------------------------------------------------------------

temp <- c(49.9, 52.3, 49.4, 51.1, 49.4, 47.9, 49.8, 50.9, 49.3, 51.9, 50.8, 49.6, 49.3, 50.6,
          48.4, 50.7, 50.9, 50.6, 51.5, 52.8, 51.8, 51.1, 49.8, 50.2, 50.4, 51.6, 51.8, 50.9,
          48.8, 51.7, 51.0, 50.6, 51.7, 51.5, 52.1, 51.3, 51.0, 54.0, 51.4, 52.7, 53.1, 54.6,
          52.0, 52.0, 50.9, 52.6, 50.2, 52.6, 51.6, 51.9, 50.5, 50.9, 51.7, 51.4, 51.7, 50.8,
          51.9, 51.8, 51.9, 53.0)



## Task: Type in the commands from the handout to explore the likelihood and 
## confidence intervals using rpanel.

rp.likelihood("sum(log(dnorm(data, theta[1], theta[2])))",
              temp, c(50, 1), c(52, 3))


## The log likelihood function for a normal distribution is plotted for the temperature data.
## The two parameters for the mean and variance are denoted by theta[1] and theta[2] here.  
## A range of values for theta[1] and theta[2] are given in the last two vectors.
## theta[1] goes fromk 50 to 52 and theta[2] goes from 1 to 3.  These values were based on our previous
## experience of this example and can be changed.

#----------------------------------------------------------------
#	Example 3 - Numerical Optimisation (Air Conditioning)
#----------------------------------------------------------------

## The air-conditioning data, we have calculated by hand that theta = 0.016
## for an exponential model

## The data, number of data points and a range of values for theta are given below:

aircond <- c(50,44,102,72,22,39,3,15,197,188,79,88,46,5,5,36,22,139,210,97,30,23,13,14)

n <- length(aircond)

theta  <- seq(0.005, 0.025, length = 50)



## The log-likelihood can be taken from Example 1 and turned into a function:


loglikfn <- function(theta,x,n){n*log(theta)-theta*sum(x)}


optim(par=0.01, fn=loglikfn, method="BFGS",control=list(fnscale= -1), x=aircond, n=n)






#----------------------------------------------------------------
#	Example 4 - Numerical Optimisation (Heliconia plants, tutorial sheet 2)
#----------------------------------------------------------------

Heliconia <- c(9, 14, 3, 3, 8, 7, 7, 6, 7, 0, 6, 0, 5, 1, 3, 12, 2, 4, 0, 11)

## Type in the commands from the handout to find the maximum likelihood
## estimate.

n <- length(Heliconia)

theta <- seq(0.4, 1, length=100)

LogLik <- (sum(Heliconia))*log(theta)+n*log(1-theta)

plot(theta, LogLik)

Helc <- function(theta,y, n){
  
  (sum(y))*log(theta)+n*log(1-theta)
  
}


optim(par=0.82, fn=Helc, method="BFGS",control=list(fnscale= -1), y=Heliconia, n=20)
