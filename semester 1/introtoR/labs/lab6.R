#intro_to_R_lab_6
#Task 1
#Read the help file of the function dbinom, which evaluates the p.m.f. of the Bi(n, θ) distribution.
?dbinom
#a) Use the function dbinom to evaluate the p.m.f of the Bi(10, 0.5) distribution at x = 3. Specify the
#arguments to dbinom once using the named form and once using the positional form. You should obtain
#the number 0.1171875.
#dbinom(x, size, prob, log = FALSE)
dbinom(x=3,10,0.5)

#b) What does dbinom(size=4, 1, 0.5) compute?
dbinom(size=4, 1, 0.5)
#0.25

#Task 2
#p1
binomial.coefficient=function(n,k){
  factorial(n)*(factorial(k)*factorial(n-k))^(-1)
}
binomial.coefficient(6,3)
#p2
binary.entropy=function(p){
  -p*log(p)-(1-p)*log(1-p)
}
binary.entropy(0.5)
#p3
approx.lbincoef=function(n,k){
  n*(binary.entropy(k/n))
}
#p4
?lchoose
log(binomial.coefficient(9000, 4000)) # fails (numerically unstable)
approx.lbincoef(9000,4000)
lchoose(9000,4000)

#Task3
#1
quadratic=function(a,b,c){
  delta=b*b-4*a*c
  if(a==0&&b==0)
    return(NA)
  if(a==0&&b!=0)
    return(-c/b)
  if(abs(delta)<1e-10) #allow for rounding errors
    return(0)
  if(delta==0)
    return(-b/(2*a))
  if(delta>0){
    x1=(b+sqrt(delta))/(-2*a)
    x2=(b-sqrt(delta))/(-2*a)
    return(c(x1,x2))
  }
}
###or
quadratic <- function(a, b, c) {
  if (abs(a)<1e-10) { # Linear case (for really small a the quadratic
    # formula is unstable, so we use the linear one)
    if (abs(b)<1e-10) # Constant case (no solution, or entire real line if c=0)
      return(NA)
    return(-c/b)
  }
  delta <- b^2 - 4*a*c # Discriminant
  if (abs(delta)<1e-10) { # Exactly one solution (allowing for rounding errors)
    return(-b/(2*a))
  }
  if (delta>0) { # Two real solutions
    sol1 <- -(b+sqrt(delta)) / (2*a)
    sol2 <- -(b-sqrt(delta)) / (2*a)
    return(c(sol1, sol2))
  }
  return(c()) # Otherwise no solution
}

#Task 4
#Your function should be named corr_mat, take the covariance matrix sigma as only argument and should
#return the matrix of correlations.
corr_matrix=function(sigma){
  sds=sqrt(diag(sigma))
  R=matrix(nrow=nrow(sigma),ncol=ncol(sigma))
    for(i in 1:nrow(R)){
      for(j in 1:ncol(R)){
        R[i,j]=sigma[i,j]/(sds[i]*sds[j])
      }
    }
  R
}
test=rbind(c(16,4.5),c(4.5,9))
corr_matrix(test)

#Task 5
#(a) Write a function lcg, which takes the required number of pseudo-random numbers n, the parameters
#a, M and c, and the initial value z0 as arguments. The function should return a sequence of n
#pseudo-random numbers generated from the above algorithm. The default values of the arguments
#should be a = 216 + 3, M = 231
#, c = 0 and z0 = 1.

#%% for congruence
lcg=function(n,a=2^16+3,M=2^31,c=0,z0=1){
  z=numeric(n)
  z[1]=z0
  for(i in 2:n){
    z[i]=((a*z[i-1])+c)%%M
    x[i]=z[i]/M  
    }
  return(x)
}
lcg(10)

#task 5b
x=lcg(300000)

#task 5c
X=matrix(data = x, ncol = 3, byrow = TRUE)

#task 5d
library(rgl)
plot3d(X)

#Task 6
#Write a function box.muller, which takes no arguments, 
#and which returns a pair of numbers (z1, z2) calculated as follows:
box.muller=function(){
  c=runif(2)
  theta=2*pi*c[1]
  r=sqrt(-2*log(c[2]))
  z1=r*sin(theta)
  z2=r*cos(theta)
  return(c(z1,z2))
}
box.muller()
?runif

#part b
#Draw two realisations from the standard normal distribution using the polar Marsaglia method and
#return a vector of length two containing the random numbers

polar.marsaglia=function(){
  while(TRUE){
    u = runif(2,-1, 1)
    s = u[1]^2+u[2]^2
    if(s < 1) break
  }
  rho = sqrt(-2*log(s)*s^(-1))
  z1 = rho*u[1]
  z2 = rho*u[2]
  return(c(z1,z2))
}
polar.marsaglia()

#part c
simulate.normal=function(n, mu=0, sigma=1, method=box.muller){
  n.even=ifelse(n %% 2 == 0, n, n+1)
  z=numeric(n.even)
  for (i in 1:(n.even/2)){
    new.z=method()
    z[c(2*i-1, 2*i)]=new.z
  }
  return(mu + sigma*z[1:n])
}
###or
simulate.normal <- function(n, mu=0, sigma=1, method=box.muller) {
  result <- matrix(nrow=ceiling(n/2), ncol=2) #round number
  for (i in 1:nrow(result))
    result[i,] <- mu+sigma*method()
  result[1:n]
}
?ceiling

#part d
test <- simulate.normal(1000, mu=2, sigma=3)
mean(test)
## [1] 2.056727
sd(test)
## [1] 2.926947








