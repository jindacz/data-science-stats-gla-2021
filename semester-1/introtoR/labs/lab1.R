#intro_lab_1
#Q1:Use R code below to compute variance using the two formulae
n <- 1000 # Set sample size.
mu <- 1e7 # Set mean to something very large.
sigma=1e-1 # Set standard deviation.
x=rnorm(n,mu,sigma) # Simulate the data.
sum((x-mean(x))^2)/n # Formula on left hand side.
sum(x^2)/n - mean(x)^2 # Formula on right hand side.
var(x)

#Q2: Use the following code to fit a quadratic regression to the simulated data.
n <- 100# Create 100 values between 0 and 1.
x <- sort(runif(n)) # Create simulated response.
y <- 5*x^2 - x + 0.1*rnorm(n)# Plot the data.
plot(x,y)# Fit the regression model
reg.model <- lm(y~x+I(x^2))
lines(x,fitted(reg.model),col="red")# Plot the fitted function
x <- x+1000
plot(x,y)
reg.model <- lm(y~x+I(x^2))
lines(x,fitted(reg.model),col="red")
#we did not change the value of y


#Q3
5-7/8
(5-7)/8
pi-333/123
256^0.25
exp(1)
(1+1/1000)^1000
(1/56)^1/4

#Q4. The “Babylonian method” provides a way of approximating √2. The sequence defined recursive
x=1# Define a new variable x taking the value 1.
x=(x/2)+(1/x)#Update x
x
x=(x/2)+(1/x)
x
x=(x/2)+(1/x)
x
x=(x/2)+(1/x)
x
#Repeat the update from the previous part. You should see that x tends to √2. (You will learn
#later on in this course how to use loops to do this more efficiently.)

#Q5 Use the help files built in to R to explore what the following functions do and explain what each line of
#code does

#(a)
?sample
test <- sample(x=c(1,2,3,4,5,6,7,8,9,10), size=4, replace=TRUE)
#replace means duplicate
#(b)
n=20
k=rnorm(n=n,mean=1,sd=0.8)
k
#crete 20 random number with mean=1,standard deviation=0.8
  
#Q6:
?rnorm
a <- sample(x=c(TRUE, FALSE), size=1) 
#sample: where do you choose, choose one of them
b <- sample(x=c(TRUE, FALSE), size=1)
a
b
c=(a|b)==(a&b)
c
c=a==b
c

#Q7: (a) Define a variable named ‘zeven’ which is assigned the value TRUE if z is even and FALSE if z is odd.
#Use the code below to define a variable named y. (Note the code 1:100 is the sequence of values from 1 to
#100 in increments of 1 (i.e. 1,2,3,. . . 100.) which we will explore in more detail next week)

z=sample(1:10,1)
z
zeven=z%%2==0
zeven

y=sample(1:100,1)
y
yedevis=y%%z==0
yedevis
 
ev_rem=((y%%z)%%2==0)
ev_rem
  
14 %/% 4 
#%/% : for integer division
#%% : for the remainder after integer division


