Sys.setenv(LANG="en")
#biostats_lab1
#q1
diff=0.25
sd=0.99
power=0.9
siglvl=0.05
samplesize=2*(sd^2)*(diff^(-2))*(qnorm(1-siglvl/2)+qnorm(power))^2;samplesize

diff=0.25
sd=0.99
power=0.8
siglvl=0.05
samplesize1=2*(sd^2)*(diff^(-2))*(qnorm(1-siglvl/2)+qnorm(power))^2;samplesize1
#the smaller the power is, the less N is needed

diff=0.25
sd=1.5
power=0.9
siglvl=0.05
samplesize2=2*(sd^2)*(diff^(-2))*(qnorm(1-siglvl/2)+qnorm(power))^2;samplesize2
#the more sample sd, the more N is needed.

diff=0.2
sd=0.99
power=0.9
siglvl=0.05
samplesize3=2*(sd^2)*(diff^(-2))*(qnorm(1-siglvl/2)+qnorm(power))^2;samplesize3
#the less difference is, the more N is needed.

ta=0.15
tb=0.25
sd=0.99
power=0.95
siglvl=0.05
N=(ta*(1-ta)+tb*(1-tb))*(ta-tb)^(-2)*(qnorm(1-siglvl/2)+qnorm(power));N

##correct answer
samplesize.cts=function(dif,std,siglev,pwr){
  2*(std^2)*(qnorm(1-siglev/2)+qnorm(pwr))^2/(dif^2)
}
samplesize.cts(0.25,0.99,0.05,0.9)
samplesize.cts(0.25,0.99,0.05,0.8)
samplesize.cts(0.25,1.5,0.05,0.9)
samplesize.cts(0.2,0.99,0.05,0.9)

power.t.test(n=NULL,delta=0.25,sd=0.99,sig.level=0.05,power=0.9,alternative=c("two.sided"))

samplesize.bin=function(thetaA,thetaB,signlev,pwr){
  (thetaA*(1-thetaA)+thetaB*(1-thetaB))*((qnorm(1-siglev/2)+qnrom(power)^2)/((thetaA-thetaB)^2)
}
samplesize.bin(0.15,0.25,0.05,0.95)



#Q2
#part1
sample1=sample(c("A","B"),660,replace=T) #replace=T means replacement
sample1

#part2
#the number of group A and B are not equally distributed
table(sample1)

#part3?
#|na-nb|>10
x=1:660
px=dbinom(x,660,0.5)
1-sum(px[x%in%325:335])

plot(x, px, main = "PDF of binomial distribution",
     xlab = "number of people assigned to one of the groups",
     ylab = "density")



#part4?
permutation1=sample(c(1:110),size=110,replace=F)
permutation1
block1=rep("A",110)
block1[permutation1>55]="B"
block1

permutation2=sample(c(1:110),size=110,replace=F)
block2=rep("A",110)
block2[permutation1>55]="B"

permutation3=sample(c(1:110),size=110,replace=F)
block3=rep("A",110)
block3[permutation1>55]="B"

permutation4=sample(c(1:110),size=110,replace=F)
block4=rep("A",110)
block4[permutation1>55]="B"

permutation5=sample(c(1:110),size=110,replace=F)
block5=rep("A",110)
block5[permutation1>55]="B"

permutation6=sample(c(1:110),size=110,replace=F)
block6=rep("A",110)
block6[permutation1>55]="B"

sample2=c(block1,block2,block3,block4,block5,block6)
table(sample2)

#use set.seed to fix 
set.seed(2023)
table(sample(c("A","B"),660,replace=T))


#part5

#Q3
getwd()
setwd("/Users/kurisuuu/Downloads/statslab/biostatslab")
dat=read.csv("/Users/kurisuuu/Downloads/statslab/biostatslab/vitaminD.csv")
head(dat)
#part1

par(mfrow=c(1,2))
boxplot(calcium~treatment,data=dat[dat$feeding=="Art",],main="arti fed babies")
boxplot(calcium~treatment,data=dat[dat$feeding=="Br",],main="br fed babies")

t.test(calcium~treatment,data=dat[dat$feeding=="Art",])
t.test(calcium~treatment,data=dat[dat$feeding=="Br",])
#part2
interaction.plot(dat$treatment,dat$feeding,dat$calcium,main=
                   "inter plot of gps and type of feed",
                 xlab="treatment",
                 ylab="mean cal lvl")
#no interaction btw groups
model1=lm(calcium~feeding*treatment,data=dat)
summary(model1)

model2=lm(calcium~feeding + treatment,data=dat)
summary(model2)

cbind(model2$coefficients,confint(model2))








