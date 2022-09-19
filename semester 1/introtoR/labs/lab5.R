#intro to R lab 5

#task 1
x=c(1,2,9)
y=c(2,6,4)
z=vector(mode="integer",length=3)
###or
#z=rep(NA,3)
for (i in 1:3){
  z[i]=x[i]*y[i]
}
z
x*y
#same!
#breaks
for(i in 1:3){
  tmp=x[i]+y[i]
  print(tmp)
  if(tmp>6)
    break
  z[i]=tmp
}
#while()
#while (something happens){
#  do stuff
#}
z=rep(NA,3)
tem=0
while(sum(x)<=25){
  tmp=x+y
  print(sum(x))
  x=tmp
}

#repeat()


#task 2
x=c(1,2,9)
x=rnorm(100)
x=sample(1:100,100,replace=T)
cumsum(x)
cumsum.x=vector(length=length(x))
#cumsum.x=rep(NA,length(x))
#cumsum.x=numeric(length(x))
cumsum.x[1]=x[1]
for(i in 2:length(cumsum.x)){
  cumsum.x[i]=cumsum.x[i-1]+x[i]
}
sum(cumsum.x != cumsum(x)) #need integers to check
sum(abs(cumsum.x - cumsum(x)))

#task 3
#p1 Write a loop that approximates the golden ratio by computing x50.
a=sample(1:100,1)
x[1]=a
for(n in 2:50){
  x[n]=1+1/(x[n-1])
}
x-(1+sqrt(5))/2
#or
x=1
for(i in 1:50){
  x=1+1/x
}
#or using while
n=1
x=1
while(n<=50){
  x=1+1/x
  n=n+1
}
#p2 Modify your code from part 1 such that the loop stops as soon as either 
#|xn − xn−1| < 10−10 or 50
#iterations haven been carried out (whichever occurs first).
x[1]=sample(1:100,1)
for(n in 2:100){
  x[n]=1+1/(x[n-1])
  if (abs(x[n]-x[n-1])<1e-10)
    break
}
x-(1+sqrt(5))/2


##########
# Task 4 #
##########
b = rep(0, 4096)
t <- (1/4096)*c(1:2048,2048:1)
w = 1/2



for(i in 1:10){
  b = b + t
  t = w*c(t[seq(2, 4096, by = 2)],t[seq(2, 4096, by = 2)]) # w*(t[2], t[4],...,t[4096], t[2], t[4], ...t[4096])
}
x = (1/4096)*1:4096
plot(x, b, type = 'l', ylab = 'b(x)')





#Task 5
#part 1 Connect all pairs of points using blue lines. Your plot should look like the one shown below (except for
#the thick red line). Hint: You will need two nested for loops
n=10
set.seed(16022021)
coords=matrix(rnorm(2*n),ncol=2)
plot(coords)
coords
?lines

n <- 10 # Simulate points
coords <- matrix(rnorm(2*n), ncol=2)
plot(coords, pch=16, xlab="x", ylab="y") # Draw the points
for (i in 1:n) # For all pairs of points ...
  for (j in 1:n)
    lines(coords[c(i,j),], col="blue") # Connect i-th and j-th point
#用lines(coords[c(i,y),],col="blue")来连结
#part 2 
#Connect the two closest points with a thick red line. Your code should find the two closest
#points automatically. Your plot should now look like the plot shown below (it will depend on your
#simulated values). Hint: You need to loop through all pairs of points and identify the pair with smallest
#Euclidean distance.

closest.pair <- c(NA,NA) # Initialize closest pair
closest.distance <- Inf # Initialize closest distance
for (i in 1:(n-1)) # Go through all pairs of points
  for (j in (i+1):n){
    dist <- sum((coords[i,]-coords[j,])^2) # Compute (squared) distance
    if (dist<closest.distance) { # If we find a pair which is closer ...

      closest.pair <- c(i,j) # ... store it ...
      closest.distance <- dist # ... along with the distance
    }
  }
lines(coords[closest.pair,], col="red", lwd=4)



