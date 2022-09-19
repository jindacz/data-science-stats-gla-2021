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
#1
binomial.coefficient=function(n,k){
  factorial(n)*(factorial(k)*factorial(n-k))^(-1)
}
binomial.coefficient(6,3)
#2
binary.entropy=function(p){
  -p*log(p)-(1-p)*log(1-p)
}
binary.entropy(0.5)
#3
approx.lbincoef=function(n,k){
  n*(binary.entropy(k/n))
}
#4
approx.lbincoef(9000,4000)

#Task3
#1
quadratic=function(a,b,c){
  delta=b*b-4*a*c
  if(a==0&&b==0)
    return(NA)
  if(a==0&&b!=0)
    return(-c/b)
  if(delta<0)
    return(0)
  if(delta==0)
    return(-b/(2*a))
  if(delta>0){
    x1=(b+sqrt(delta))/(-2*a)
    x2=(b-sqrt(delta))/(-2*a)
    return(c(x1,x2))
  }
}
quadratic(1,-5,1)
quadratic(0,0,1)
quadratic(0,5,1)

#Task 4
#Your function should be named corr_mat, take the covariance matrix sigma as only argument and should
#return the matrix of correlations.
corr_mat(sigma){
  
}

#Task 5



