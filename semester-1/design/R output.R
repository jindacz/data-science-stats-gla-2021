#week 2
## SIMPLE COMPARATIVE EXPERIMENT ##
## Loading the data
data = PlantGrowth
?PlantGrowth

boxplot(PlantGrowth$weight~PlantGrowth$group)
## There seems to be a difference between the groups

## Fitting the model
mod <- lm(PlantGrowth$weight~factor(PlantGrowth$group))
summary(mod)
## No statistical significant difference between the control group and the two treatments

amod<-anova(mod)
amod
## However the variable group appears to be significant

## Multiple comparisons
r <- 10
sigma <- amod$`Mean Sq`[2]
t12 <- abs(mean(subset(PlantGrowth$weight, PlantGrowth$group == "ctrl"))-mean(subset(PlantGrowth$weight, PlantGrowth$group == "trt1")))/(sigma*sqrt(1/r+1/r))
t13 <- abs(mean(subset(PlantGrowth$weight, PlantGrowth$group == "ctrl"))-mean(subset(PlantGrowth$weight, PlantGrowth$group == "trt2")))/(sigma*sqrt(1/r+1/r))
t23 <- abs(mean(subset(PlantGrowth$weight, PlantGrowth$group == "trt1"))-mean(subset(PlantGrowth$weight, PlantGrowth$group == "trt2")))/(sigma*sqrt(1/r+1/r))
qtukey(0.95,3,27)
## Only t23>qtukey and thus only trt1 and trt2 are statistically significant different.



## SIMPLE COMPARATIVE EXPERIMENT WITH BLOCKING ##
## Creation of the dataset for the tyre experiment
y<-c(238,196,254,238,213,312,279,334,421,308,367,412)
compound<-rep(c("A","B","C","D"),each=3)
tyre<-c("one","two","three","one","two","four","one","three","four","two","three","four")
compound<-factor(compound,levels=c("D","A","B","C"))
tyre<-factor(tyre,levels=c("four","one","two","three"))


## Fit of the full model
modfull<-lm(y ~ compound + tyre)

## Parameter estimates for the full model
modfull$coefficients[1:4]

## Covariance matrix of beta
round(summary(modfull)$cov.unscaled,2)[1:4,1:4]

## Fit of the model with the blocking term only
modblock<-lm(y ~ tyre)

### ANOVA tables
afull<-anova(modfull)
ablock<-anova(modblock)

afull
ablock

## Test statistic for hypothesis b1=b2=....=bp
num<-(sum(ablock$`Sum Sq`)-afull$`Sum Sq`[3]-ablock$`Sum Sq`[1])/(length(levels(compound))-1)
den<-afull$`Mean Sq`[3]
test<- num/den
qf(0.95,3,5)
## Since test>qf we reject the null hypothesis that all treatments are equal

## Multiple comparisons
k <- 3
lambda <-2
p<-length(levels(compound))
sigma <- afull$`Mean Sq`[3]

den <- 2*k*sigma/(lambda*p)

t12=abs(modfull$coefficients[2]-modfull$coefficients[3])/sqrt(den)
t13=abs(modfull$coefficients[2]-modfull$coefficients[4])/sqrt(den)
t14=abs(modfull$coefficients[2])/sqrt(den)
t23=abs(modfull$coefficients[3]-modfull$coefficients[4])/sqrt(den)
t24=abs(modfull$coefficients[3])/sqrt(den)
t34=abs(modfull$coefficients[4])/sqrt(den)

qtukey(0.95,4,5)/sqrt(2)
## t_ij > qtukey for A vs C, A vs D, B vs C and B vs D, and consequently we reject the hypothesis that these pairs of treatments are equal

#week 3
## Analysis of the desylilathion experiment ## 

## Creation of all possible treatments
x1<-rep(c(-1,1),each=8)
x2<-rep(rep(c(-1,1),each=4),2)
x3<-rep(rep(c(-1,1),each=2),4)
x4<-rep(c(-1,1),8)

## Values of the response
y<-c(82.947, 88.667, 77.193, 84.873, 88.073, 92.993, 83.587, 88.707, 94.053, 94.293,
     93.007, 94.247, 93.967, 93.407, 94.373, 94.653)

## Fit of the normal model for factorial experiments. The power allows for considering all possible interactions
mod<-lm(y~(x1+x2+x3+x4)^4)

summary(mod)
## Notice that since the experiment is unreplicated there is no estimation of the error and no statistical tests can be run

## Factorial effects:
theta<-2*mod$coefficients[-1]
theta

## Normal effects plot to assess which factorial effects might be significant (as in the slides)
otheta<-sort(theta)
qq<-qnorm(((1:15)-0.5)/15)
names(qq)<-names(otheta)

plot(otheta,qq, xlab="Ordered estimates", ylab="Ordered quantiles", ylim=c(-3,3))
sets<-c("x1","x2","x3","x4","x1:x2","x1:x3","x1:x4")
text(x=otheta[sets],y=qq[sets]+0.15,labels=sets)


#week 5
# Yield Experiment 
time<-c(-1,-1,1,1,0,0)
temp<-c(-1,1,-1,1,0,0)
yield<-c(65.6,45.6,78.7,63.0,64.8,64.3)

# Test for curvature
first<-lm(yield~time+temp)
first
abs(mean(yield[1:4])- mean(yield[5:6]))
# As the curvature is much smaller than linear effects, it is unlikely to be important

# After a steepest ascent the following data was collected
time<-c(-1,-1,1,1,0,0)
temp<-c(-1,1,-1,1,0,0)
yield<-c(91.2,94.2,87.5,94.4,93.0,93.1)

# Test of curvature
first<-lm(yield~time+temp)
first
abs(mean(yield[1:4])- mean(yield[5:6]))
# As the curvature is of the same magnitude as the linear effects, it is likely to be important

# Central cuboid design
time<-c(-1,-1,1,1,0,0,-1.41,1.41,0,0)
temp<-c(-1,1,-1,1,0,0,0,0,-1.41,1.41)
yield<-c(91.2,94.2,87.5,94.4,93.0,93.1,93.6,91.2,88.7,95.1)

# Second-order model 
second<-lm(yield~time*temp+I(time^2)+I(temp^2))
second$coefficients

# Identification of the optimum
b <- second$coefficients[2:3]
B<-matrix(c(second$coefficients[4],second$coefficients[6]/2,second$coefficients[6]/2,second$coefficients[5]),2,2)

x_s <- -0.5*solve(B)%*%b
y_s <- second$coefficients[1]-0.25*b%*%solve(B)%*%b


#week 6
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
  
  eps<-rnorm(n=N,mean=0,sd=sqrt(truesigma2))
  
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


