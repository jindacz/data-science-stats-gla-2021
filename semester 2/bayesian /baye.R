#baye_hw_1
#Q1
y=seq(-7,10,by=0.01)
plot(y,0.5*(dnorm(y,1,2)+dnorm(y,2,2)),type="l",ylab="Density")
title(main="0.5 N(y|1, 2) + 0.5 N(y|2, 2)")

#Q3
theta=rexp(n=100000,rate=1)
psi=sqrt(theta)*log(theta)/(1+theta)
hist(psi,100)
mean(psi)
var(psi)
mean(psi>0)
quantile(psi,probs=c(0.025,0.975)) # using R’s quantile function
sort(psi)[100000*c(0.025,0.975)+c(0,1)] #why plus 0,1???????

#Chapter 3
#plot posterior distribution of beta(3,14)
yi=c(0,0,0,0,0,0,1,0,0,0,0,0,0,1,0)
n=length(yi)
theta=seq(0.0001,0.9999,length=100)

alpha1=beta1=1
y=0

dens=dbeta(theta,alpha1,beta1)
plot(theta,dens,type="l")

for(i in (1:n)){
  y=y+yi[i]
  dens=dbeta(theta,alpha1+y,beta1+i-y)
  plot(theta,dens,type="l")
}

#plot attributes of beta(3,14)
y=sum(yi)
alphaU=alpha1+y
betaU=beta1+n-y

dens=dbeta(theta,alphaU,betaU)
plot(theta,dens,type="l")

modtheta=(alphaU-1)/(alphaU+betaU-2) #mode????
lines(rep(modtheta,2),c(0,0.2)) #add mode

lower=qbeta(p=0.025,alphaU,betaU) #central 95
upper=qbeta(p=0.975,alphaU,betaU) #posterior interval
lines(c(lower,upper),rep(0,2)) #add interval to plot
#what is rep(0,2) in lines
?lines

#
set.seed(432) #repordicible results

nsamp=10000
draws=rbeta(nsamp,alphaU,betaU)
hist(draws,breaks=seq(0,1,by=0.01),prob=T)

sorted=sort(draws)
lower95=sorted[nsamp*0.025]
upper95=sorted[nsamp*0.0975+1]
lower50=sorted[nsamp*0.025]
upper50=sorted[nsamp*0.75+1]
med=median(sorted)

lines(c(lower95,upper95),rep(0,2),lwd=10)
lines(c(lower50,upper50),rep(0,2),lwd=4)
points(med,0,pch=16,cex=1)

#baye_lab_1
install.packages('R2OpenBUGS',type='source')
library(R2OpenBUGS)

model{theta~dunif(0,1)}

#HW2_problem 4
nsamp=100000
theta=rbeta(nsamp,17,5)
mean(theta>0.5&theta<0.7)  # Pr[0.5 < theta < 0.7 | y]
mean(theta) # E[theta | y]
median(theta)  # median(theta | y)
quantile(theta,probs=c(0.025,0.975))  # 95% central posterior
# interval for theta

#In this example the posterior is a standard distribution, so the quantities in question can also
#be computed exactly:
pbeta(0.7, 17, 5) - pbeta(0.5, 17, 5) # Pr[0.5 < theta < 0.7 | y]
17/ (17 + 5) # E[theta | y]
qbeta(0.5, 17, 5) # median(theta | y)
qbeta(c(0.025, 0.975), 17, 5) # 95% central posterior
#0.5809340 0.9178241 # interval for theta

#yielding α = 3, β = 2. With a Be(3, 2) prior, the posterior is θ|y ∼ Be(19, 6) Then, we can
#proceed as done above. The results using simulation are
theta <- rbeta(nsamp, 19, 6)
mean(theta > 0.5 & theta < 0.7) # Pr[0.5 < theta < 0.7 | y]
mean(theta) # E[theta | y]
median(theta) # median(theta | y)
quantile(theta, probs = c(0.025, 0.975)) # 95% central posterior
#2.5% 97.5% # interval for theta







