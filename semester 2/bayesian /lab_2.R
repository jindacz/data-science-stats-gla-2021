rm(list=ls())
install.packages("nimble") ## Install nimble
## nimble is an R package, which can do everything that WinBUGS does
## For further information about nimble, see https://r-nimble.org/
## User manual can be found here: https://r-nimble.org/documentation-2
## 
library(nimble)
## Data 
x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)
y <- c(6, 13, 18, 28, 52, 53, 61, 60)
N <- length(y)

## Model 1: use the WinBUGS model code directly
code1 <- nimbleCode({
  for (j in 1:N) {
    y[j] ~ dbin(theta[j], n[j])
    logit(theta[j]) <- alpha + beta * x[j]
    yhat[j] <- n[j] * theta[j]
  }
  alpha ~ dnorm(0.0, tau = 1e-6)
  beta  ~ dnorm(0.0, tau = 1e-6)
})

## Initial values, data and constants
consts <- list(N = N, x = x, n = n)
inits<- list(alpha = 0, beta = 0)
data <- list(y = y)

## Build nimble model
model1 <- nimbleModel(code = code1, constants = consts, data = data, inits = inits)

## Compile the model
Cmodel1 <- compileNimble(model1)

## Set up to generate posterior samples
monitors1 <- c("alpha", "beta")
MCMCconf1 <- configureMCMC(model = model1, monitors = monitors1) 
MCMC1 <- buildMCMC(MCMCconf1)
cMCMC1 <- compileNimble(MCMC1, project = model1)

## Now generate posterior samples
runTime1 <- system.time(output1 <- runMCMC(mcmc = cMCMC1,
                                           nburnin = 100,
                                           niter = 1000,
                                           samplesAsCodaMCMC = TRUE))

## Check the posterior samples: what do you think?
plot(output1[,1]) 
plot(output1[,2])

##---------------------------------------------------------------------------------
## Model 2: add centering
code2 <- nimbleCode({
  for (j in 1:N) {
    y[j] ~ dbin(theta[j], n[j])
    logit(theta[j]) <- alpha.star + beta * (x[j] - mean(x[1:N]))
    yhat[j] <- n[j] * theta[j]
  }
  alpha <- alpha.star - beta * mean(x[1:N])
  alpha.star ~ dnorm(0.0, tau = 1e-6)
  beta ~ dnorm(0.0, tau = 1e-6)
})
## Initial values, data and constants
consts <- list(N = N, x = x, n = n)
inits <- list(alpha.star = 0, beta = 0)
data <- list(y = y)

## Build nimble model
model2 <- nimbleModel(code = code2, constants = consts, data = data, inits = inits)

## Compile the model
Cmodel2 <- compileNimble(model2)

## Set up to generate posterior samples
monitors2 <- c("alpha.star", "beta", "alpha", "theta")
MCMCconf2 <- configureMCMC(model = model2, monitors = monitors2) 
MCMC2 <- buildMCMC(MCMCconf2)
cMCMC2 <- compileNimble(MCMC2, project = model2)

## Now generate posterior samples
runTime2 <- system.time(output2 <- runMCMC(mcmc = cMCMC2,
                                           nburnin = 100,
                                           niter = 1000,
                                           samplesAsCodaMCMC = TRUE))
## Check the posterior samples
colnames(output2)
plot(output2[,1]) 
plot(output2[,2]) ##...

## Get various values (beta here for example)
mean(output2[,3])
median(output2[,3])
sd(output2[,3])
quantile(output2[,3], c(0.025, 0.975))

## Get estimtes and posterior intervals of alpha.star and beta
## summ2 <- summary(output2)

##---------------------------------------------------------------------------------
## Model 3: add prediction
code3 <- nimbleCode({
  for (j in 1:N) {
    y[j] ~ dbin(theta[j], n[j])
    logit(theta[j]) <- alpha.star + beta * (x[j] - mean(x[1:N]))
    yhat[j] <- n[j] * theta[j]
  }
  alpha <- alpha.star - beta * mean(x[1:N])
  alpha.star ~ dnorm(0.0, tau = 1e-6)
  beta ~ dnorm(0.0, tau = 1e-6)
  xtilde <- 1.7
  ytilde ~ dbin(theta.tilde, 60)
  logit(theta.tilde) <- alpha.star + beta * (xtilde - mean(x[1:N]))
})

## Initial values, data and constants
consts <- list(N = N, x = x, n = n)
inits <- list(alpha.star = 0, beta = 0, ytilde = 5)
data <- list(y = y)

## Build nimble model
model3 <- nimbleModel(code = code3, constants = consts, data = data, inits = inits)

## Compile the model
Cmodel3 <- compileNimble(model3)

## Set up to generate posterior samples
monitors3 <- c("alpha.star", "beta", "alpha", "ytilde", "theta")
MCMCconf3 <- configureMCMC(model = model3, monitors = monitors3) 
MCMC3 <- buildMCMC(MCMCconf3)
cMCMC3 <- compileNimble(MCMC3, project = model3)

## Now generate posterior samples
runTime3 <- system.time(output3 <- runMCMC(mcmc = cMCMC3,
                                           nburnin = 100,
                                           niter = 1000,
                                           samplesAsCodaMCMC = TRUE))
## Check the posterior samples
colnames(output3)
plot(output3[,1]) 
plot(output3[,2])

## Get predictions of theta7 (10th column of output3)
mean(output3[,10])
median(output3[,10])
quantile(output3[,10], c(0.025, 0.975))

## For the probit and cloglog link functions, just change the function used in the model code
## and please give it a try. 