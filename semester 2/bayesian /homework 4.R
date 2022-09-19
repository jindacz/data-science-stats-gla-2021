#P61
#Appendix: R code for the Leukaemia data example
remistime <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
hist(remistime, xlab="Times of remission", col="lightblue",
     main="Time of remission - control sample",
     prob=TRUE, ylim=c(0, 0.1))
time <- seq(0.5, 25, length=200)
lines(time, dexp(time, 1/mean(remistime)))
quantiles <- qexp(seq(0.5/21, 20.5/21, by=1/21), 1/mean(remistime))
plot(sort(remistime), quantiles, pch=16,
     xlab="Sorted remission times",
     ylab="Quantiles of Exponential distribution")
n <- length(remistime)
sumy <- sum(remistime)
alpha <- 1
beta <- 0
nsamp <- 100000
set.seed(321)
theta.draws <- rgamma(nsamp, alpha + n, beta + sumy)
histpost <- function(draws, ...)
{
  quantiles <- quantile(draws, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  hist(draws, col="lightblue", ...)
  lines(quantiles[c(2,4)], rep(0, 2), lwd=6, col="red")
  lines(quantiles[c(1,5)], rep(0, 2), lwd=3, col="blue")
  points(quantiles[3], 0, pch=16, cex=1.2)
  return(invisible(quantiles))
}
# histogram of draws from posterior of theta
histpost(theta.draws, breaks=30,
         prob=TRUE, main="Posterior of theta",
         xlab="Rate (theta) draws", ylab="Density")
# histogram of draws from posterior of 1/theta
histpost(1/theta.draws, breaks=30, prob=TRUE, main="Posterior of 1/theta",
         xlab="Mean (1/theta) draws", ylab="Density")
# histogram of draws from the posterior predictive
postpred.draws <- rexp(nsamp, theta.draws)
histpost(postpred.draws, breaks=seq(0,max(postpred.draws)+2, by=2),
         prob=TRUE, main="Posterior predictive distribution",
         xlab="Remission time", ylab="Density")

#homework 4 
#problem 1
year=1976:1985
accidents=c(24,25,31,31,22,21,26,20,16,22)
deaths <- c(734, 516, 754, 877, 814, 362, 764, 809, 223, 1066)
deathrate <- c(19, 12, 15, 16, 14, 6, 13, 13, 3, 15) / 100

#In order to print the data in a table, let's put the data in a data frame:
fatalities <- data.frame(Year = year, Accidents = accidents,
                         Deaths = deaths, Death.rate = deathrate)
fatalities

#(a) Let yi = number of accidents in ith year and assume yi
#i.i.d∼ Poi(θ), with θ ∼ Ga(α, β).
#We choose α = 1 and β = 0, which corresponds to an improper 
#at prior on θ. The posterior distribution of θ is θ|y ∼ Ga(α + ny, β + n), from which we can easily sample:
alpha <- 1; beta <- 0; nsamp <- 100000
y <- accidents; n <- length(y)
alpha1 <- alpha + sum(y); beta1 <- beta + n
set.seed(678)
theta.draws <- rgamma(nsamp, alpha1, beta1)

#The draws can then be plotted and summaries computed using the function histpost:
theta.quant <- histpost(theta.draws, prob=T, xlab="theta", ylab="Density")

#To compute a 95% predicitive interval for the number of accidents in 1986, we need to
#generate from the posterior predictive distribution of a future observation. This is easily
#done by generating Poisson random variables with rates equal to the draws from the
#posterior of θ:
#???
postpred.draws <- rpois(nsamp, theta.draws)
postpred.quant <- histpost(postpred.draws, prob=TRUE, 100, xlab="Accidents",
                           ylab="Density",
                           main="Posterior predictive of no. of accidents")
postpred.quant <- histpost(postpred.draws)
postpred.quant

#(b) The number of passenger miles in hundreds of millions, can be obtained by dividing the
#nunber of deaths by the death-rate (which is dened as the number of passenger deaths
#per 100 million passenger miles). We further divide by 1000, to obtain passenger miles in
#units of 1011 miles.
miles <- round(deaths / (deathrate * 1000), 2)
miles

alpha <- 1; beta <- 0
x <- miles
alpha1 <- alpha + sum(y); beta1 <- beta + sum(x)
theta.draws <- rgamma(nsamp, alpha1, beta1)
theta.quant <- histpost(theta.draws)
theta.quant

#To produce a 95% predictive interval for the number of fatal accidents, we need an estimate
#of the exposure (number of passenger miles) in 1986: this is given as 8 × 1011 passenger
#miles. Then, we can simulate from Poisson distribution with parameters equal to the
#exposure multiplied by the draws from the posterior of θ:
miles86 <- 8
postpred.draws <- rpois(nsamp, theta.draws * miles86 )
postpred.quant <- histpost(postpred.draws)
postpred.quant

#(c) A model for the number of passenger deaths, using the same approach as in part (a).
alpha <- 1; beta <- 0; nsamp <- 100000
y <- deaths; n <- length(y)
alpha1 <- alpha + sum(y); beta1 <- beta + n
theta.draws <- rgamma(nsamp, alpha1, beta1)
theta.quant <- histpost(theta.draws)
theta.quant

#(d) A model for the number of passenger deaths, using the same approach as in part (b).
miles <- round(deaths / (deathrate * 1000), 2)
alpha <- 1; beta <- 0
x <- miles
alpha1 <- alpha + sum(y); beta1 <- beta + sum(x)
theta.draws <- rgamma(nsamp, alpha1, beta1)
theta.quant <- histpost(theta.draws)
theta.quant

miles86 <- 8
postpred.draws <- rpois(nsamp, theta.draws * miles86 )
postpred.quant <- histpost(postpred.draws)
postpred.quant

#(e) Assumption of independence is likely violated in models (c) and (d), since passenger deaths
#do not occur independently, but in clusters. Among models (a) and (b) which one do you
#think is more plausible? Can you think of a modication of the models in parts (a) and
#(b) to predict the number of passenger deaths?










