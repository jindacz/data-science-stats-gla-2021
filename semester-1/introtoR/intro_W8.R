#intro_to_R W8
#Example 1. In this example we will compute confidence bounds for a proportion induced by the confidence
#bounds for the log-odds.

# STEP 2.1
# Compute the logit-transform
# Arguments: theta (proportion)
# Returns: phi=logit(theta)
logit.transform=function(theta){
  log(theta/(1-theta))
}

# STEP 3
# Compute the inverse logit-transform
# Arguments: phi (log-odds)
# Returns: theta=inverse logit(phi)
inverse.logit.transform=function(phi){
  exp(phi)/(1+exp(phi))
}

## STEP 2.3 (and therefore 2.2)
# Compute an asymptotic confidence interval for the log-odds
# Arguments: theta (observed proportion), n (sample size), alpha (1-significance level)
# returns: vector of length 2 containing the lower and upper bound of the CI
ci.logodds=function(theta,n,alpha=0.05){
  phi=logit.transofrm(theta) #compute log odds
  sd.phi=sqrt(1/(n*theta)+1/(n*(1-theta))) #se of log-odds,step 2.2
  ci=phi+c(-1,1)*qnorm(1-alpha/2)*sd.phi #CI
  names(ci)=c("lower","upper") #label the two entries
  ci
}

#Finally we will write the main function to compute the induced confidence interval for proportions:
# Compute the induced asymptotic confidence interval for a proportion
# Arguments: x (observed number of "successes"), n (sample size), alpha (1-significance level)
# Returns: vector of length 2 containing the lower and upper bound of the CI
ci.proportion.induced=function(x,n,alpha=0.05){
  theta=x/n
  ci.phi=ci.logodds(theta,n,alpha)
  ci.theta=inverse.logit.transform(ci,phi)
  ci.theta
}

#Unit testing

#Debugging
#warnings
x=log(-1:1)
## Warning in log(-1:1): NaNs produced
x
## [1] NaN -Inf 0
x <- 1:3
y <- 4:5
z <- x+y
## Warning in x + y: longer object length is not a multiple of shorter object
## length
z
## [1] 5 7 7

#Errors:
#• syntax errors like mismatched parenthesis,
sin(x[i)]
    ## Error: <text>:1:8: unexpected ')'
    ## 1: sin(x[i)
    ## ^
    #• trying to use a variable that does not exist,
    print(I.do.not.exist)
    ## Error in print(I.do.not.exist): object 'I.do.not.exist' not found
    #• calling a function with the wrong arguments.
    rnorm(sample.size=10)
    ## Error in rnorm(sample.size = 10): unused argument (sample.size = 10)

#Basic debugging in R
#The basic principle is to increasingly narrow down where the error might have occurred
    
#example 2
golden.ratio <- function(n=100) {
  x <- rep(1,n)
  for (i in 1:n) {
    x[i] <- 1 + 1 / x[i-1]
  }
  x[n]
}
golden.ratio()
    
golden.ratio <- function(n=100) {
  x <- rep(1,n)
  cat("Starting the loop ... \n")
  for (i in 1:n) {
    cat("Setting x[",i,"]\n")
    x[i] <- 1 + 1 / x[i-1]
  }
  x[n]
}
golden.ratio()
    
#fix
golden.ratio <- function(n=100) {
  x <- rep(1,n)
  for (i in 2:n) {
    x[i] <- 1 + 1 / x[i-1]
  }
  x[n]
}
golden.ratio()

#Using warning and stop in your code
# Compute an asymptotic confidence interval for the log-odds
## (with error handling)
# Arguments:
## theta (observed proportion),
## n (sample size)
## alpha (1-significance level)
# Returns: vector of length 2 containing the lower and
## upper bound of the CI
ci.logodds=function(theta,n,alpha=0.05){
  if((theta==0)|(theta==1))
    stop("Propotion theta cannot be 0 or 1")
  phi=logit.transform(tehta)
  sd.phi=sqrt(1/(n*theta)+1/(n*(1-theta)))
  ci=phi+c(-1,1)*qnorm(1-alpha/2)*sd.phi
  names(ci)=c("lower","upper")
  ci
}

# Compute the induced asymptotic confidence interval
## for a proportion (now checking n*theta)
# Arguments:
## x (observed number of "successes") or theta (proportion of "successess",
## n (sample size)
## alpha (1-significance level)
# Returns: vector of length 2 containing the lower
## and upper bound of the CI
ci.proportion.induced <- function(x, theta=x/n, n, alpha=0.05) {
  if (abs(n*theta-round(n*theta))>1e-10) # Warn the user if n*theta is not an integer
    warning("n*theta is not a an integer. Check observed proportion theta and sample size n.")
  ci.phi <- ci.logodds(theta, n, alpha) # Get CI for log-odds
  ci.theta <- inverse.logit.transform(ci.phi) # Transform back to theta-domain
  ci.theta
}










