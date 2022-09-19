rat.tumors <- read.table(file="rats.txt", header=TRUE)
y <- rat.tumors$y
n <- rat.tumors$N
rm(rat.tumors)
y
n

# Rat tumor data - GCSR Chp. 5, p. 118
# > y
# [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  3  2  2
# [26]  2  2  2  2  2  2  2  1  5  2  5  2  7  7  3  3  2  9 10  4  4  4  4  4  4
# [51]  4 10  4  4  4  5 11 12  5  5  6  5  6  6  6  6 16 15 15  9  4
# > n
#  [1] 20 20 20 20 20 20 20 19 19 19 19 18 18 17 20 20 20 20 19 19 18 18 27 25 24
# [26] 23 20 20 20 20 20 20 10 49 19 46 17 49 47 20 20 13 48 50 20 20 20 20 20 20
# [51] 20 48 19 19 19 22 46 49 20 20 23 19 22 20 20 20 52 46 47 24 14

drawBetaPlt <- function(nsamp, alpha, beta, n, y, xlabel="", plotdens=TRUE, ...)
{
  alpha1 <- alpha + y
  beta1 <-  beta + n - y
  draws <- rbeta(nsamp, alpha1, beta1)
  quantiles <- quantile(draws, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  hist(draws, breaks=seq(0, 1, by=0.02), prob=TRUE, col="lightblue",
       xlab=xlabel, ylab="Posterior density", ...)
  lines(quantiles[c(2,4)], rep(0, 2), lwd=6, col="red")
  lines(quantiles[c(1,5)], rep(0, 2), lwd=3, col="blue")
  points(quantiles[3], 0, pch=16, cex=1.2)
  theta <- seq(min(draws), max(draws), length=500)
  if(plotdens) lines(theta, dbeta(theta, alpha1, beta1))
  return(invisible())
}
                     
# inference about rat 71, using Be(1,1) prior
nsamp <- 10000
drawBetaPlt(nsamp, alpha=1, beta=1, n=n[71], y=y[71],
            xlabel="theta_71", main="")

# empirical Bayes approach: hyperpars alpha, beta estimated from
# raw tumor rates for rats no. 1 to 70
# equate mean and var of Be(alpha, beta) to mean and var of raw rates
rawRates <- y[1:70] / n[1:70] 
m <- mean(rawRates)
v <- var(rawRates)
alpha <- (1-m) * m * m / v - m
beta <- alpha * ( 1/m - 1)
# alpha
# [1] 1.356149
# beta
# [1] 8.615058
# round these to first decimal alpha=1.4, beta=8.6
alpha <- round(alpha, 1)
beta <- round(beta, 1)
drawBetaPlt(nsamp, alpha=1.4, beta=8.6, n=n[71], y=y[71],
            xlabel="theta_71", main="")

# first attempt at computation on a grid of (alpha, beta) values
ngrid <- 100
mingrd <- 0.1
maxgrd <- 10
betagrd <- alphagrd <- seq(mingrd, maxgrd, length=ngrid)
alpbetgrd <- expand.grid(alphagrd, betagrd)
                       # Independent Exp(1) priors
lambda <- 1
alpbetprior <- lambda^2 * exp(- lambda * (alpbetgrd[,1] + alpbetgrd[,2]))
contour(alphagrd, betagrd, matrix(alpbetprior, nrow=ngrid),
        levels=c(.95, 0.5, 0.25, 0.1, 0.01, 0.001) * max(alpbetprior),
        drawlabels=FALSE, xlab="alpha", ylab="beta",
        main="Independent Exp(1) Priors")

                       # GCSR prior
#alpbetprior <- (alpbetgrd[,1] + alpbetgrd[,2])^(-5/2)
#contour(alphagrd, betagrd, matrix(alpbetprior, nrow=ngrid),
#        levels=c(.95, 0.5, 0.25, 0.1, 0.01, 0.001) * max(alpbetprior),
#        drawlabels=FALSE, xlab="alpha", ylab="beta", main="GCSR Prior")

postAlpBet <- function(alpbetgrd, n, y, lambda)
{
# evaluates on a grid the log-posterior of the hyperpars alpha, beta
#
  alpha <- alpbetgrd[,1]
  beta <- alpbetgrd[,2]
                          # logprior - iid Exp(lambda) on alpha, beta 
  logpost <- 2*log(lambda) - lambda * (alpha + beta)
                          # logprior - GCSR
  # logpost <- - (5/2) * log(alpha + beta) 
  for (j in (1:length(n))) {
    logpost <- ( logpost
                + lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta)
                + lgamma(alpha + y[j]) + lgamma(beta + n[j] - y[j])
                - lgamma(alpha + beta + n[j]) )
  }
  return(logpost)
}

plotPostAlpBet <- function(ngrid, minalpha, maxalpha, minbeta, maxbeta,
                           PostAlpBet, n, y, lambda, maintitle)
{
  alphagrd <- seq(minalpha, maxalpha, length=ngrid)
  betagrd <- seq(minbeta, maxbeta, length=ngrid)
  alpbetgrd <- expand.grid(alphagrd, betagrd)
  logpost <- postAlpBet(alpbetgrd, n, y, lambda)
  postab <- exp(logpost - max(logpost))
  contour(alphagrd, betagrd, matrix(postab, nrow=ngrid),
          levels=seq(0.05, 0.95, by=0.1), drawlabels=FALSE,
          xlab="alpha", ylab="beta", main=maintitle)
  return(invisible(list(alpbetgrd=alpbetgrd, postab=postab))) 
}

outPost <- plotPostAlpBet(ngrid=100, minalpha=0.1, maxalpha=10, minbeta=0.1,
                          maxbeta=10, PostAlpBet, n, y, lambda=1,
                          maintitle="Posterior - first grid")

# revised grid
outPost <- plotPostAlpBet(ngrid=100, minalpha=0.1, maxalpha=3, minbeta=2,
                          maxbeta=20, PostAlpBet, n, y, lambda=1,
                          maintitle="Posterior - second grid")

# final revision with increased number of grid points
outPost <- plotPostAlpBet(ngrid=150, minalpha=0.4, maxalpha=3, minbeta=2,
                          maxbeta=16, PostAlpBet, n, y, lambda=1,
                          maintitle="Posterior - final grid")

# sampling from p(alpha, beta | y)
nsamp <- 10000
alpbetgrd <- outPost$alpbetgrd
postab <- outPost$postab
indexes <- sample(x = 1:nrow(alpbetgrd), size = nsamp, replace = TRUE,
                  prob = postab)
draws <- alpbetgrd[indexes,]
plot(draws, pch=".", xlim=c(0.4, 3), ylim=c(2, 16))

alphainc <- alpbetgrd[2, 1] - alpbetgrd[1, 1]
betagrd <- unique(alpbetgrd[,2])
betainc <- betagrd[2] - betagrd[1]
alpha.drws <- draws[,1] + runif(nsamp, - alphainc/2, alphainc/2)
beta.drws <- draws[,2] + runif(nsamp, - betainc/2, betainc/2)
plot(alpha.drws, beta.drws, pch=".", xlim=c(0.4, 3), ylim=c(2, 16))

# comparison with Fig. 5.3 (b) in GCSR p. 130
lograt <- log(alpha.drws / beta.drws)
logsum <- log(alpha.drws + beta.drws)
plot(lograt, logsum, pch=".", xlab="log(alpha.drws / beta.drws)",
     ylab="log(alpha.drws + beta.drws)")

# 95% Posterior intervals for theta's - as Fig. 5.4 in GCSR p.131
theta.drws <- matrix(0, nsamp, length(n))
for (j in (1:length(n))) 
  theta.drws[,j] <- rbeta(nsamp, alpha.drws + y[j], beta.drws + n[j] - y[j])
par(pty="s")
plot(c(0, 0.47), c(0, 0.47), type="n", xlab="observed rate, y(i) / n(i)",
     ylab="95% posterior interval for theta(i)", cex=0.6)
abline(0, 1)
for (j in (1:length(n))) {
  quantiles <- quantile(theta.drws[,j], probs=c(0.025, 0.5, 0.975))
  jitter <- runif(1, -0.01, 0.01)
  lines(rep(y[j]/n[j], 2) + jitter, quantiles[c(1,3)])
  points(y[j]/n[j] + jitter, quantiles[2], pch=16, cex=0.6)
}

par(pty="m")
drawBetaPlt(nsamp=length(alpha.drws), alpha=alpha.drws, beta=beta.drws,
            n=n[71], y=y[71], xlabel="theta_71", plotdens=FALSE, main="")