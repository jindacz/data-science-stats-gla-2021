#data
x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)
y <- c(6, 13, 18, 28, 52, 53, 61, 60)

#x grid
X <- seq(1.6, 1.95, 0.001)

# logit fit
al <- -61.22
bl <-  34.56
ll <- al + bl*X
tl <- exp(ll)/(1+exp(ll))
plot(X, tl, ty='l', lwd=2, xlab="Dose", ylab="Prob. of death")


#probit fit
ap <- -35.08
bp <- 19.81
lp <- ap + bp*X
tp <- pnorm(lp)
lines(X, tp, ty='l', lwd=2, col=2)

#cloglog fit
all <- -39.82
bll <- 22.18
lll <- all + bll*X
tll <- 1-exp(-exp(lll))
lines(X, tll, ty='l', lwd=3, col=3)

# raw data
points(x, y/n)
legend(1.85, 0.2, c("logit", "probit", "cloglog"), col=c(1,2,3), lty=c(1,1,1), lwd=c(2,2,2))
