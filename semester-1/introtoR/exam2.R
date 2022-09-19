setwd("/Users/kurisuuu/Downloads")
#1 
x=seq(-5,5,length.out=1000)
plot(x,sin(x)-0.2*x^2,xlab="x",ylab="f(x)",type="l")

#2
x=seq(-5,5,length.out=1000)
plot(x,cos(x)-0.4*x,xlab="x",ylab="Derivative of f(x)",
      type="l",xlim=c(-5,5))
abline(h=0,lty=2,col="red",lwd=2)

#3
#Define the recursive sequence xh=f′(xh−1)+xh−1, for h≥2, with x1=0. 
#Write a loop that computes xh for h=2,…,100.  Save all the 100 values in a vector 
#x. Plot x. Your plot should look like the one below.

x=numeric(100)
x[1]=0
for(h in 2:100){
  x[h]=cos(x[h-1])-0.4*x[h-1]+x[h-1]
}
h=c(1:100)
x
plot(h,x,type="l",col="red",lwd=2)

#4
x=rep(NA,100)
x[1]=0
for(h in 2:100){
  x[h]=cos(x[h-1])-0.4*x[h-1]+x[h-1]
  old=x[h-1]
  new=x[h]
  if (abs(old-new)<1e-10) 
    break
}
xh=x[!is.na(x)]
xh


#5
#Your *plots* should then look like the *ones* below.
#you can draw 2 seperate without par
#par(mfrow=c(2,1)) 
x=seq(-5,5,length.out=1000)
plot(x,sin(x)-0.2*x^2,xlab="x",ylab="f(x)",type="l",
     xlim=c(-5,5))
abline(v=1.11,lty=2,col="red",lwd=2)

x=seq(-5,5,length.out=1000)
plot(x,cos(x)-0.4*x,xlab="x",ylab="Derivative of f(x)",
     type="l",xlim=c(-5,5))
abline(h=0,lty=2,col="red",lwd=2)
abline(v=1.11,lty=2,col="red",lwd=2)

#6
datasaurus_dozen=read.table("datasaurus_dozen.txt",header=T)

#7
data.types=c(unique(datasaurus_dozen$dataset))
data.types

#8
#make sure you copy all the codes
five.stats=matrix(NA,nrow=12,ncol=6)
five.stats=as.data.frame(five.stats)
names(five.stats)=c("data.type","mean.x","mean.y",
                    "sd.x","sd.y","corr.xy")
d1=subset(datasaurus_dozen,datasaurus_dozen$dataset=="dino")
d2=subset(datasaurus_dozen,datasaurus_dozen$dataset=="away")
d3=subset(datasaurus_dozen,datasaurus_dozen$dataset=="h_lines")
d4=subset(datasaurus_dozen,datasaurus_dozen$dataset=="v_lines")
d5=subset(datasaurus_dozen,datasaurus_dozen$dataset=="x_shape")
d6=subset(datasaurus_dozen,datasaurus_dozen$dataset=="star")
d7=subset(datasaurus_dozen,datasaurus_dozen$dataset=="dots")
d8=subset(datasaurus_dozen,datasaurus_dozen$dataset=="circle")
d9=subset(datasaurus_dozen,datasaurus_dozen$dataset=="bullseye")
d10=subset(datasaurus_dozen,datasaurus_dozen$dataset=="slant_up")
d11=subset(datasaurus_dozen,datasaurus_dozen$dataset=="slant_down")
d12=subset(datasaurus_dozen,datasaurus_dozen$dataset=="wide_lines")
col2=c(mean(d1$x),mean(d2$x),mean(d3$x),mean(d4$x),
       mean(d5$x),mean(d6$x),mean(d7$x),mean(d8$x),
       mean(d9$x),mean(d10$x),mean(d11$x),mean(d12$x))
col3=c(mean(d1$y),mean(d2$y),mean(d3$y),mean(d4$y),
       mean(d5$y),mean(d6$y),mean(d7$y),mean(d8$y),
       mean(d9$y),mean(d10$y),mean(d11$y),mean(d12$y))
col4=c(sd(d1$x),sd(d2$x),sd(d3$x),sd(d4$x),
       sd(d5$x),sd(d6$x),sd(d7$x),sd(d8$x),
       sd(d9$x),sd(d10$x),sd(d11$x),sd(d12$x))
col5=c(sd(d1$y),sd(d2$y),sd(d3$y),sd(d4$y),
       sd(d5$y),sd(d6$y),sd(d7$y),sd(d8$y),
       sd(d9$y),sd(d10$y),sd(d11$y),sd(d12$y))
col6=c(cor(d1$x,d1$y),cor(d2$x,d2$y),cor(d3$x,d3$y),cor(d4$x,d4$y),
       cor(d5$x,d5$y),cor(d6$x,d6$y),cor(d7$x,d7$y),cor(d8$x,d8$y),
       cor(d9$x,d9$y),cor(d10$x,d10$y),cor(d11$x,d11$y),cor(d12$x,d12$y))
#one loop only
for(i in 1:nrow(five.stats)){
  five.stats[i,1]=data.types[i]
  five.stats[i,2]=col2[i]
  five.stats[i,3]=col3[i]
  five.stats[i,4]=col4[i]
  five.stats[i,5]=col5[i]
  five.stats[i,6]=col6[i]
}
five.stats

#9
d=list(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
par(mfrow = c(3,4), pty = 's', mar = c(2,1,1,1))
#one loop!
for(i in 1:12){
  plot(d[[i]][[2]],d[[i]][[3]],col=i)
}

#10
#a
f=function(x,a,b){
  (b^a)*(gamma(a))^(-1)*x^(a-1)*exp(-b*x)
}


#b
g=function(x,lambda){
  lambda*exp(-lambda*x)
}


#c
#we start from doing the function once!!!
gen.gamma.once=function(n,mu,sigma){ 
  x=numeric(n)
  a=mu^2/sigma^2 #1
  b=mu/sigma^2
  lambda=b/a #2
  x.star=a/b
  c=f(x.star,a,b)/g(x.star,lambda) #3
  
  while(TRUE){
    y=rexp(1, rate = lambda) #4.1
    u=runif(1) #4.2
    alpha=f(y,a,b)/(c*g(y,lambda)) #4.3
    if(u<alpha) break
  }
  x=y #4.4
}



gen.gamma=function(n,mu,sigma){ #gen.gamma
  x=replicate(n,gen.gamma.once(n,mu,sigma))
}


#d
x=gen.gamma(1000,47,26)
x
hist(x,freq=F,ylim=c(0,0.03))
#use ylim for reading numbers







