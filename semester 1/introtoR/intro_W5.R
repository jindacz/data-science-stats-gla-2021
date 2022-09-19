#intro week 5

x=rnorm(10)
x[x<0]=0
x

x=rnorm(10)
ifelse(x<0,0,x)

#if (condition) {
#statement11
#...
#statement1m
#} else {
#  statement21
#  ...
#  statement2n
#}

#We can use the logical operators !, & &&, | and || as well as the functions such as all, any and xor in
#condition to combine logical expressions.

#Note that the condition of an if statement cannot be a vector (of length > 1). If we want to carry out a
#conditional operation on a vector, we need to either subset it, use loops or the ifelse function.

#example 1
x=2
if (x==2){
  print('x is 2')
}else{
  print{'x is not 2'}
}

#example 2
x=rnorm(1)
if(x>0){
  x=sqrt(x)
}else{x=sqrt(-x)}

#or
y=sign(x)*sqrt(abs(x))

#for loop
x=1
for(i in 1:10)
  x=x/2+1/x

#for (variable in sequence) {
#  statement1
#  ...
#  statementn
#}

for(i in 1:3)
  print(i)

for(day in c('m','tu','w','tu','f'))
  print(day)

#The function seq_along(x) does exactly the same as 1:length(x),
#except that it handles the case of a vector of length zero correctly.
x=rnorm(10)
for(i in seq_along(x))
  if(x[i]<0)
    x[i]=0
x

#ex1
n=1000
x=numeric(x)
x[1]=rnorm(1)
for(i in 2:n){
  ep=rnorm(1)
  x[i]=0.8*x[i-1]+0.6*ep
}
plot(x,type='l')

#ex2 Create a vector x containing some missing values using
x=rnorm(100) # Generate white noise
x[sample(100,10)]=NA # Sneak in 10 missing values

#Task 1 Create two variables x and y containing one random number each. Use an if statement to set the
#smaller of the two variables to the value of the larger variable.
x=rnorm(1)
y=rnorm(1)
ifelse(x<y,x<-y,y<-x);x;y

#Task 2 Create a vector x containing some missing values using
x <- rnorm(100) # Generate white noise
x[sample(100,10)] <- NA # Sneak in 10 missing values
#Use a loop to create a vector y which contains the entries of x, however with missing values replaced by 0.
#(Remember, you can use the function is.na to test whether a value is missing.) Can you do the same without
#a loop?
x=rnorm(100)
x[sample(100,10)]=NA
y=numeric(length(x))
for (i in seq_along(x))
  if(!is.na(x[i])){
    y[i]=x[i]
  }else{
    y[i]=0
  }
y;
#or
y=x
y[is.na(y)]=0


#Task 3 In the setting of the previous task, suppose that rather than setting the missing values to 0 we want
#to omit them from y. We can do so by setting
y=c()
for(i in seq_along(x))
  if(!is.na(x[i]))
    y=c(y,x[i])
y


#Nested loops
for(i in 1:2)
  for(j in 1:3)
    print(c(i,j))


#Example 8 When we looked at the standard plotting functions in R we created an image plot of the density
#of the bivariate normal distribution. We created a matrix Z = (Zij )ij with
x=seq(from=-3,to=3,length.out=50)
y=seq(from=-3,to=3,length.out=50)
x;y
z=matrix(nrow=length(x),ncol=length(y))
for(i in seq_along(x))
  for(j in seq_along(y))
    z[i,j]=dnorm(x[i])*dnorm(y[j])
?dnorm
?persp
persp(x,y,z,theta=30,phi=30,col='yellow',shade=0.5)

#break and next
#A for-loop repeats the statements in it a fixed number of times. The break statement gives additional
#flexibility and allows for aborting the loop immediately and before the sequence of indices has been finished.
#It is typically used inside an if statement.

x=1
for(i in 1:10)
  x=x/2+1/x

x=1
for(i in 1:100){
  x.old=x
  x=x/2+1/x
  if (abs(x-x.old)<1e-8)
    break
}
x

#Task 4 In Example 9 we have used a variable x to store the current value of x in the recursive sequence. We
#had to introduce x.old to store the old value of x, so that we can compare to the current value in order to
#check for convergence.
#Instead we could have used a vector x of length 100 and stored the value at the i-th iteration xi in the i-th
#entry of x. After the end of the loop the required value would then be in x[i]. Rewrite the loop in that way.
x=100
x=numeric(n)
x[1]=1
for(i in 2:100){
  x[i]=x[i-1]/2+1/x[i-1]
  if(abs(x[i]-x[i-1])<1e-8)
    break
}




#exmaple 10
#next halts the processing of the current iteration and goes back to the begin of the body of the loop (using
#the next value of sequence in a for loop). next is the R equivalent of continue in C or Java.
for (x in 1:10){
  if(x%%2==0)
    next
  print(x)
}

#while loops
#while (condition) {
#  statement1
#  ...
#  statementn
#}

#example 11
x=1
x.old=0
while(abs(x-x.old)>1e-8){
  x.old=x
  x=x/2+1/x
}
x

#example 12 
for(i in 1:3)
  print(i)

###
i=1
while(i<=3){
  print(i)
  i=i+1
}

#The function ifelse
#result=ifelse(condition,yes,no)

#example 13
x=rnorm(10)
for (i in seq_along(x))
  if (x[i]<0)
    x[i]=0

x=ifelse(x<0,0,x)

#example 14
n=5
x=sample(n)
y=sample(n)

#Suppose you want to the set i-th entry of a new vector z to zi = max{xi, yi}. 
#You can do this by placing an if statement inside a for loop.

z=numeric(n)
for(i in 1:n)
  if (x[i]>y[i]){
    z[i]=x[i]
  }else{
    z[i]=y[i]
  }
###or
z=ifelse(x>y,x,y)

#Note that we could have also used subsetting
z <- x # Start with a copy of x
select <- y>x # Find out for which entries y is larger than x
z[select] <- y[select] # Set these to y

#Task 5 Without running the code in R, determine what ifelse returns in the code snippet below.
x <- c(1,2,9)
y <- c(2,6,4)
z <- c(3,5,7)
ifelse(x<4, y, z)
#return (y1,y2,z3)=(2,6,7)

#Task 6 What does the following loop do?
  x <- rnorm(10) # Generate some white noise
out <- numeric(length(x))
for (i in seq_along(x)) {
  if (x[i]>0) {
    out[i] <- x[i]
  } else {
    out[i] <- -x[i]
  }
}
#out <- ifelse(x>0, x, -x)
#We could, of course, also have simply set
#out <- abs(x)

#Avoiding loops
n=1e5
x=rnorm(n)
y=rnorm(n)
system.time(z <- x + y)

system.time( {
  z <- numeric(n) # Create vector of correct size
  for (i in 1:n) # Set entries one-by-one
    z[i] <- x[i]+y[i]
} )

system.time( {
  z <- c() # Create an empty vector and let R extend it
  for (i in 1:n) # Set entries one-by-one
    z[i] <- x[i]+y[i]
})

#Example 16 Suppose we compute the vector of increments di = xi+1 − xi. Our first approach uses a loop.
#system.time( {
  n <- length(x)
  d <- numeric(n-1)
  for (i in 1:(n-1))
    d[i] <- x[i+1] - x[i]
} )
## user system elapsed
## 0.012 0.000 0.013
#We cannot simply set d to the difference of x and x, as we subtract xi from xi+1. Essentially we need to
#offset the two copies of x before we subtract them. We can do this using
#system.time( {
  n <- length(x)
  d <- x[-1] - x[-n]
} )
## user system elapsed
## 0.001 0.000 0.001



