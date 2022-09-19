#assignment_3
setwd("/Users/kurisuuu/Downloads")

#Q1
#1 Use R to read in the file trees.txt correctly and save it as a data frame called trees.
trees=read.table("trees.txt",header=T)

#2
tree.age=function(diameter,rate.of.growth,cm=F){
  if(cm==F){
    diameter=2.54*diameter
  }
  diameter/rate.of.growth
}

#3
mean(tree.age(trees$Diameter,2.5,cm=F))

#4
median=median(trees$Height) #76
trees=transform(trees,HeightGroup=ifelse(trees$Height<=median,"short","tall"))
plot(trees$Diameter,trees$Volume,col=unclass(factor(trees$HeightGroup)),
     xlab="Diameter",ylab="Volume")
legend("topleft",legend=c("Short","Tall"),
       pch=1,col=c("black","red"),title="Height")

#Q2
#1
load("tmax1993.Rdata")

#2
for(i in 1:length(tmax)){
  tmax[[i]]$julian=NULL
  tmax[[i]]$id=NULL
  tmax[[i]]$proc=NULL
  tmax[[i]]$date=NULL
}
View(tmax)

#3 
df=matrix(NA,nrow=133,ncol=6)
df=as.data.frame(df)
for(i in 1:length(tmax)){
  df[i,]=as.matrix(tmax[[i]],ncol=6)[1,]
}
colnames(df)=c("year","month","day","z","lat","lon")
df

#4
plot(df$lon,df$lat,xlab="Longitude",ylab="Latitude",main="1st of May, 1993",
     col=unclass(factor(df$z)),pch=20,cex=1.5)

#Q3
#1
?runif
?rnorm
x=runif(500,min=1,max=6)
x=x[order(x)]
y=rnorm(500,mean=2+5*x,sd=sqrt(25))

#2
nlog.lik=function(beta,y,x){
  b0=beta[1]
  b1=beta[2]
  f=(1/sqrt(2*pi*5*5))*exp(-(y-(b0+b1*x))^2/(2*5*5))
  -sum(log(f))
}

#3
?optim
beta.par=optim(fn=nlog.lik, par=c(2,5), y=y,x=x,
      control=list(maxit=10000))
beta.hat=beta.par$par
beta.hat

#4
plot(x,y)
abline(2,5,col="red",lty=1,lwd=3)
abline(beta.hat[1],beta.hat[2],col="green",lty=2,lwd=3)
legend("topleft",legend=c("True line","Estimated line"),lty=c(1,2),
       col=c("red","green"),lwd=c(3,3))

#Q4
#1
cov.mc=function(n){
  if(n<1){ #1.1
    stop("n must be greater than 1")
  }
  u=runif(n) 
  e=exp(u) #1.2
  UE=mean(u*e) #1.3
  U=mean(u)
  E=mean(e)
  UE-U*E #1.4
}


#2
theta.hat=cov.mc(5000)
abs(theta.hat-(3/2-exp(1)/2))

#3
#start by do sampleN once
sampleN.once=function(k){
  n=0
  v=1 #1.1
  u=runif(1)
  f=prod(u,v)
  while (f>=exp(-3)){
    v=f
    n=n+1
    u=runif(1)
    f=prod(u,v)
  }
  n
}
#sampleN function
sampleN=function(k){ #sampleN
  x=replicate(k,sampleN.once(k))
}


#4
Nsamp=sampleN(20000)
sum(Nsamp==3)/20000






























































