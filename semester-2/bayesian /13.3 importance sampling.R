#impotance sampling
#https://rpubs.com/mathetal/importance

#graph
curve(exp(-x+cos(x))*exp(-x), 0, 2.5, n = 101, add = FALSE, type = "l",
      ylab = NULL, log = NULL, lwd=3)
curve(exp(-x),0,2.5, add = TRUE, col = "violet", lwd=3)
curve(2*exp(-2*x),0,2.5, add = TRUE, col = "orange", lwd=3)

#importance sampling
m=10000
X=rep(m,rate=2)
weights=c()
mu=c()

for(i in 1:m){
  weights[i] = dexp(X[i])/dexp(X[i], rate = 2)
  mu[i] = weights[i]*exp((-1*(X[i]))+cos(X[i]))
}

#estimated mu
mean(mu)
sqrt(var(mu)/m)

#naive MC
X  =  rexp(m, 1)

#Naive Monte Carlo mean
mean(exp(-1*X+cos(X)))


