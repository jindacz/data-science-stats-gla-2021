#intro to R W6(vow power)
dnorm(x,mean=0,sd=1,log=F)
#To evaluate the p.d.f. of the N(0.5, 1) distribution at x = −2 we can use any of 
#the following lines (and many other variants on the theme):
dnorm(-2,mean=0.5,sd=1)
dnorm(-2,0.5,1)
dnorm(0.5,1,x=-2)
dnorm(x=-2,mean=0.5)
#Named form: Arguments are given in the form name=value, i.e. dnorm(x=-2, mean=0.5). Arguments
#in named form can be in any order. The name of the argument does not need to be given in full as long
#as it is unique, i.e. you can use dnorm(x=-2, m=0.5): in this case R will interpret m as mean, as there
#are no other arguments starting with the letter “m”.

#Positional form: In positional form no names are given. In this case, R matches the arguments by their
#position. For dnorm for example, the first argument is, according to the function definition, x, and the
#second argument is mean, thus R interprets dnorm(-2, 0.5) as dnorm(x=-2, mean=0.5).

#Some arguments have default values and only need to be specified if you want to set this argument to a
#different value. This is typically (but not always) visible in the function declaration, e.g. sd of dnorm defaults
#to 1 (“sd=1”). Thus if we do not specify sd, we implicitly set sd=1.

#Task 1
func <- function(a, b=0, c=1) {
  (a+2*b)^c
}
#Rewrite the calls below into named form (i.e. using parameter=value) and give the output of func without
#running it in R.
func(1, 2, 3)==func(a=1, b=2, c=3)
func(4)==func(a=4, b=0, c=1)
func(c=3, 2, 1)==func(a=4, b=0, c=1)

#function.name <- function(argument1, argument2, ...) {
#  statement1
#  ...
#  statementn
#}

par(pty="square") # Set the plotting region to be square
plot(c(-1,1),c(-1,1),type="n") # Set up canvas
x=0;y=0;r=1 #x,y cor. of centre, radius
t=seq(0,2*pi,length=200)
lines(x+r*cos(t),y+r*sin(t)) # Draw circle
###or
circle=function(x,y,r){
  t=seq(0,2*pi,length=250)
  lines(x+r*cos(t),y+r*sin(t))
}

###example 3
#Suppose we want to use the function circle to draw circles in different colours and using different line types
#and widths using arguments like col, lty and lwd. We can then pass on these arguments when we call the
#lines command.
circle=function(x,y,r=1,...){
  t=seq(0,2*pi,length=250)
  lines(x+r*cos(t),y+r*sin(t),...)
}
par(pty="s") # Set the plotting region to be square
plot(c(-1,1), c(-1,1), type="n") # Set up a canvas
circle(-0.5, -0.5, r=0.5, col="red", lwd=2)
circle(-0.5, 0.5, r=0.5, col="orange")
circle( 0.5, -0.5, r=0.5, col="green")
circle( 0.5, 0.5, r=0.5, col="yellow", lwd=2)
#use... to pass para. to lines function

#returning objects
#Often we want functions not only to perform certain tasks, but also to return a value (e.g. the result of a
#calculation). We can return values using the function return(object). The function return terminates the
#function and returns object. When we return object from the function we can “capture” this value from
#outside the function and store it in a variable. If we do not assign the returned result from a function to a
#variable it will be printed on the screen.
#example 4
stirling=function(n){
  approx=sqrt(2*pi*n)*n^n*exp(-n)
  return(approx)
}
approx=stirling(10)

#Actually, there is no need to use return in the above example, as an R function will always return the value
#of the last statement. Thus we could have used as well
stirling <- function(n) {
  sqrt(2*pi*n) * n^n * exp(-n)
}

exact=factorial(10)
exact
exact-approx

#example 5
stirling.bounds <- function(n) {
  approx <- sqrt(2*pi*n) * n^n * exp(-n)
  list(lower=approx * exp(1/(12*n+1)),
       upper=approx * exp(1/(12*n)))
}
stirling.bounds(10)

bounds=stirling.bounds(10)
bounds$upper-bounds$lower

#scoping
#Every variable that exists outside the function can be referenced and used inside the function. However,
#as soon as you change any of these variables, a local copy will be created: the changes you make to this
#variable will be only temporary. Once the function has finished running, the changes will be rolled back.
#Similarly, if you create a new variable, it will only be created temporarily and will be deleted once you exit
#from the function, unless you return it using the function return or by putting the object in the last line of
#the function body.
#In R, all arguments (with the exception of environments) are always passed on to
#the function by value
test=function(a){
  print(b)
  b=a
  print(b)
}
b=19
test(13)
b

#Good practice
#Functions should be self-contained units, so all inputs should be passed on as arguments and all outputs
#should be returned. Thus when writing a function, you should use inside the function only
#• variables that are passed on to the function as arguments, and
#• local variables you have defined inside the function.
#You can access variables available only in the workspace, but this is typically (and for good reason) considered
#to be bad programming style. The following example illustrates why.

x=1:10
n=length(x)
my.mean=function(x){
  x.bar=sum(x)/n
  x.bar
}
my.mean(x)
y=1:3
my.mean(y)

#rather than 3, the correct value. Why did this happen? When looking at the implementation of my.mean we
#computed the length n outside the function, thus we assumed that there is a variable n in the workspace which
#holds the length of the vector whose mean we want to compute. This was the case for x, but is not the case
#for y.
#According to the advice given above, we should not have used n in the function without having it first set to
#the length of x inside the function. Thus we should have coded the function my.mean as follows:
  my.mean <- function(x) {
    n <- length(x) # Determine length of x - now inside function
    x.bar <- sum(x) / n # Compute mean
    x.bar
  }



