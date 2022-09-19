#2019 CT2 Q1
getwd()
setwd("/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/2019_CT2-20210210/M/")
load('ca-fires.Rdata')
#Q1. [2 marks] Identify the year with the highest number of acres burned.

fires$year[which.max(fires$acres.burned)]

#Q2. [2 marks] Create a variable called dec which contains a sequence from 1930 
#to 2020 increasing in units of 10.
dec=seq(1930,2020,by=10)

#***Q3 [3 marks] Using the sequence dec and the cut function create a new variable called decade which
#contains for each row the decade that year falls in, i.e. for 1933 the decade will be 1930, for 1990 the decade
#will be 1990, etc.
decade=cut(fires$year,breaks=dec,right=F,labels=dec[-10]) 
?cut

#***Q4. [2 marks] Define a vector s which contains the total number of fires per decade.
s=by(fires$year,decade,sum)
?by
#Function by is an object-oriented wrapper for tapply applied to data frames.
#Usage by(data, INDICES, FUN, ..., simplify = TRUE)

# Q5. [2 marks] Using s, find the decade with the highest number of fires. 
#Note: If, for example, the highest number of fires occurred in the 
#decade of the 1930s (i.e., in any year from 1930 till 1939), then you should report 1930.
names(which.max(s))

#Q6. [3 marks] Plot the number of fires versus the cost (dollar damage) in log-scale using a different colour
#for each decade. Your plot should look like the one below, which was produced using colour i for the i-th
#decade, i = 1, 2, . . . , 9.
#fires=transform(fires,logd=log(dollar.damage))
#plot(fires$n.of.fires,fires$logd,
#     main="Number of fires versus cost (log scale)",
#     xlab="number of fires",ylab="Cost (log scale)",
#     pch=19,col=1+unclass(decade))
### or
plot(fires$n.of.fires,log(fires$dollar.damage),col=decade,pch=20,
     xlab = 'Number of fires',
     ylab = 'Cost (log scale)', main = 'Number of fires versus cost (log scale)')


#Q7. [2 marks] Create a new variable called year.group, which takes the following values
?ifelse
year.group=ifelse(fires$year<1974,1,2)
#Correct

#Q8. [3 marks] Compute t given below (t is known as the Welch’s t-test)
x1 = fires$acres.burned[year.group == 1] #定位year group 1 
x2 = fires$acres.burned[year.group == 2] #定位year group 2
n1=length(x1)
n2=length(x2)
s1.2=var(x1)
s2.2=var(x2)
delta=sqrt(s1.2/n1+s2.2/n2)
t=(mean(x1)-mean(x2))/delta;t
t.test(x1,x2)

#Q9. [2 marks] The test rejects the null hypothesis of equal means if t > t? = 1.99. Define a variable called
#out.t.test which is TRUE if t > t? and FALSE otherwise
tstar=1.99
out.t.test=ifelse(t>tstar,T,F)
#or
out.t.test = t > 1.99; out.t.test

#Task 2: The generalized extreme-value distribution
#Q10. [4 marks] Create a function called pgev that receives four arguments: z, µ, σ, and ξ. Your function
#should return G(z) when σ > 0 and z ∈ Z. If z / ∈ Z, i.e., if z is such that 1+ ξ ·  z−σµ ≤ 0, then your function
#should return 0. If σ ≤ 0, then your function should return a warning message saying invalid scale.
pgev=function(z,mu,sigma,xi){
  z=1+xi*(z-mu)/sigma
  if(sigma>0&z>0){
    return(exp(-z^(-1/xi)))
  }else if(z<=0){
    return(0)
  }else{
    return(warning('invaloid scale'))
  }
}

#Q11. [3 marks] The portpirie.Rdata data frame has 65 rows and 2 columns. The second column gives
#annual maximum sea levels recorded at Port Pirie, South Australia, from 1923 to 1987. The first column
#gives the corresponding years. Set your working directory to the location where your files are stored and run
#the line of code below to read the portpirie.Rdata data frame into R
load('portpirie.Rdata')
?sapply
pgev.portpirie=sapply(portpirie$SeaLevel,pgev,mu=3.87,sigma=0.198,xi=-0.05)

#Task 3: Generating samples from a Gamma distribution
#Q12. [4 marks] Using the argument above, write a function called sample.gamma which take n, k, and θ as
#arguments and which returns a sample of size n from the Ga(k, θ) distribution. No marks will be awarded for
#this question if the rgamma function is used here.
?rexp
sample.gamma=function(n,k,theta){
  out=rep(NA,n)
  for(i in 1:n)
    out[i]=sum(rexp(k,theta))
  out
}

#Q13. [2 marks] Run the line of code below
#Then use your function sample.gamma to draw a sample of size n = 100 from the Ga(5, 2) distribution.
set.seed(123)
sample.gamma(100, 5, 2)

#Q14. [6 marks] Using rejection sampling, write a function called sampleGammaRej which takes the desired
#sample size n as well as the parameters α and θ as arguments and which returns a sample of size n from the
#Ga(α, θ) distribution. Your function should stop and return the message invalid parameters if θ ≤ 1 or
#α ≤ 1.
sampleGammaRej=function(n,alpha,theta){
  if(theta<=1|alpha<=1) {stop("invalid parameters")}
  else{
    iter=0 #初始化loop
    y=NULL #初始化
    repeat{
      iter=iter+1
      k=floor(alpha)
      x=sample.gama(1,k,theta)
      u=runif(1)
      M = dgamma(alpha-k, shape = alpha, rate = theta)/dgamma(alpha-k, shape = alpha,
                                                              rate = (theta-1))
      p = dgamma(x, shape = alpha, rate = theta)/(M * dgamma(x, shape = alpha,
                                                             rate = (theta-1)))
      if(u<p)
        y=c(y,x)
      if(iter==n) break
    }
    return(y)
  }
}









