## Simulation study comparing two designs from Chapter 6 in terms of accuracy of inference.

N<-12
truebeta<-c(3,2,-2)
truesigma2<-1

dB<-rep(c(-1,0,1),each=4)
dA<-rep(c(-1,-1/3,1/3,1),each=3)

reps<-10000
betaB<-matrix(0,nrow=reps,ncol=3)
betaA<-matrix(0,nrow=reps,ncol=3)
for(i in 1:reps){
  
  eps<-rnorm(n=N,mean=0,sd=sqrt(truesigma2)) #error term
  
  yA<-truebeta[1]+dA*truebeta[2]+(dA^2)*truebeta[3]+eps
  yB<-truebeta[1]+dB*truebeta[2]+(dB^2)*truebeta[3]+eps
  
  modA<-lm(yA~dA+I(dA^2))
  modB<-lm(yB~dB+I(dB^2))
  
  betaA[i,]<-modA$coefficients
  betaB[i,]<-modB$coefficients}

plot(density(betaA[,1]))
lines(density(betaB[,1]),col=2)
legend(x=4,y=0.8,legend=c("dA","dB"),col=c(1,2),lty=c(1,1))

plot(density(betaA[,2]))
lines(density(betaB[,2]),col=2)
legend(x=3,y=0.8,legend=c("dA","dB"),col=c(1,2),lty=c(1,1))

plot(density(betaA[,3]))
lines(density(betaB[,3]),col=2)
legend(x=-0.5,y=0.5,legend=c("dA","dB"),col=c(1,2),lty=c(1,1))

var(betaA)
var(betaB)

det(var(betaA))
det(var(betaB))

(det(var(betaB))/det(var(betaA)))^(1/3)


