###Intro to R Lab 2###

###Question 1###
###part 1)
sum(1:10)

###part 2)
###For the second part, which option is correct?
sum((1:100)^2) #option 1 CORRECT
sum(1:100)^2   #option 2
sum(1:100^2)   #option 3
###vote in the chat

###Question 2###
x <- seq(1, 5, by=0.3)
?seq
x
###1) finding the mean:
mean(x)

###2) finding standard deviation
sd(x)

###3) standardise x
x.standardised<-(x-mean(x))/sd(x)

###4) check that the mean is zero
mean(x.standardised)

###5) check that standard deviation is 1
sd(x.standardised)

###Question 3
x <- rnorm(100, mean=1, sd=1)
###Things we need
###1) sample size n
n<-100
###2) mean of x x.bar
x.bar<-1/n*sum(x)
x.bar<-mean(x)
###3) the difference between x and x.bar
differences<- x-x.bar
###4) Sx^2
sx2<-1/(n-1)*sum(differences^2)
sx2<-1/(n-1)*sum((x-x.bar)^2)
###5) calculate t! 
t<- sqrt(n)*x.bar/sqrt(sx2)
t

t.test(x)

###Question 4

###part 1)
sum(2^(0:9)) ###option 1 CORRECT
sum(2^0:9)   ###option 2
sum(2)^(0:9) ###option 3

###part 2)

sum((1/2)^(1:1e5)) ###option 1 CORRECT, because top fraction has 1
sum(1/2^(1:1e5))   ###option 2 CORRECT
sum(1^2:1e5/2)     ###option 3

###Question 5
#1. Compute the mean and the median of x. Re-run the above line of code and re-compute the mean and
#the median five times. What can you observe?
#2. Rather than generating a sample of size 100 from the standard normal distribution we will now generate
#a sample from the Cauchy distribution using
#3. One way of “robustifying” the mean is to compute the so-called 20%-trimmed mean by removing the 10
#smallest and the 10 largest observations and computing the mean of the remaining 80 observations.
#Compute the 20%-trimmed mean of both a sample from the standard normal distribution and the
#Cauchy distribution. Your code should automatically find the 10 smallest and largest values using the
#function order or sort.

###part 1)
x<-rnorm(100)
mean(x)
median(x)

###no much change in mean and median

###part 2)
x <- rcauchy(100)
mean(x)
median(x)

###sometimes a drastic change in mean, but not median

###part 3)
###cut off the first 10 and the last 10 observations 
##and then calculate mean
x.trimmed <- sort(x)[11:90]
mean(x.trimmed)
###OR###
mean(x,trim=0.1)

###Question 6 Create a vector u containing all integers between 1 and 100. Replace all elements from u that are
#less than 55 by the number 0.
###1) create the vector u
u<-1:100
###OR###
u<-seq(1,100,by=1)
###2) replace all values that are smaller than 55 by 0
u[u<=55]<-0 ###option 1
u[u<55]==0  ###option 2
u[u<55]<-0  ###option 3 CORRECT


###Question 7
###Look at the table at the bottom of your Week 1 
###Lecture notes page 11

a <- c(TRUE,FALSE)
b <- c(FALSE,FALSE)
c <- (a & !b)
c
d <- !(a | b)
d

###Question 8
###sequence 1
1:10
###sequence 2
rep(1:3, times = 4)
###sequence 3
rep(1:3,each = 2)
###sequence 4
c(1:20,19:1)
###sequence 5
(1:10)^2
###sequence 6
2^(0:10)


###Question 9

###1) create P
P <- diag(c(1,5,1,7,9))
P[1,4] <- -1
P[3,1] <- 3
P
###2) print the first row and the second column of P
P[1,]
P[,2]
###3) print the submatrix that consists of the first three 
###rows and first two columns of P
P[1:3,1:2]
###BONUS: print rows 1 and 3
P[c(1,3),]
###4) compute transpose of P
t(P)
###if I wanted to save it:
P<-t(P) ###overwrite my original P
P_transposed<-t(P) ###original P will stay the same 
###5) compute inverse of P
solve(P)
###6) replace the first row of P by (1, 2, 3, 4, 5)
P[1,]<-1:5
P
###7) replace all non-zero entries of the matrix P by 1
P[P!=0]<-1
P

###Question 10

A <- matrix(rnorm(9e6), nrow=3e3) # Create matrix A with random entries (3000x3000)
B <- matrix(rnorm(9e6), nrow=3e3) # Create matrix B with random entries (3000x3000)
x <- rnorm(3e3) # Create vector x with random entries (3000x1)
(A%*%B)%*%x
A%*%(B%*%x)

###Multiplying two matrices is much slower than computing 
###the product of a matrix (of the same size)
###and a vector. The first line of code multiplies the two matrices
### A and B first (which takes very long), and
###then multiplies the result by the vector x. 
###The second line of code never multiplies two matrices. 
##The result of B ? x is another vector of length 1000. 
###A is then multiplied by this vector.

###If you want to time your code you can use
ptm <- proc.time()
(A%*%B)%*%x
proc.time() - ptm
ptm <- proc.time()
A%*%(B%*%x)
proc.time() - ptm

###Question 11

###1) create A and b
A <- rbind(c( 1, 2, 3),
           c( 2, 20, 26),
           c(3, 26, 70)) 
b <- c(4, 52, 31) 

###2) use solve() to find z
solve(A, b)
solve(A)%*%b ###the same thing, but slower

###3) use chol and t to compute the Choleski factor L
L<-t(chol(A))

###4) verify that LL^T is (except for rounding errors) identical to A
L%*%t(L)

###5) calculate z using previous steps (still need to get v!)


###6) compute v
v<-solve(L)%*%b
v <- forwardsolve(L, b) ##faster 
v

###7) compute z
z <- solve(t(L))%*%v
z

###8) compute the square of the product of the diagonal entries of L
prod(diag(L))^2
det(A)

