#17_CT
setwd('/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/2017_CT2-20210210')
#(a) Read the data into R and store in an object called diamonds. 
#Ensure missing values are read in correctly.
?sapply
diamonds  <-read.csv("diamonds.csv", na.strings="*")

#(b) Delete all observations for which the clarity is missing.
diamonds=subset(diamonds,!is.na(clarity))
#or
diamonds <- diamonds[!is.na(diamonds$clarity),]

#(c) What proportion of diamonds have a cut grade of ‘Very Good’?
nrow(diamonds[diamonds$cut=="Very Good",])/nrow(diamonds)
###or
(sum(diamonds$cut=="Very Good")/nrow(diamonds))

#(d) For diamonds that are exactly one carat in weight, produce boxplots of 
#the diamond price for each of the cut grades. 
boxplot(diamonds[diamonds$carat==1,]$price~diamonds[diamonds$carat==1,]$cut)
#用boxplot函数

#(e) Find the minimum price of any diamond in the dataset which is 
#Premium cut and has the IF clarity grade
#diamonds[diamonds$cut=="Premium"&diamonds$clarity=="IF",]
#which.min(diamonds[diamonds$cut=="Premium"&diamonds$clarity=="IF"])
#Wrong,多个条件变体 用min直接求最小值，which.min返回index
min(diamonds$price[diamonds$cut=="Premium"&diamonds$clarity=="IF"])
#or
min(diamonds[diamonds$cut=="Premium"&diamonds$clarity=="IF",]$price)


#(f) Add a new column to the dataset, priceGBP, with contains the 
#price of diamonds in pounds (1 USD = 0.76 GBP).
diamonds=transform(diamonds,priceGBP=price*0.76)


#(g) Create a scatter plot of carat against priceGBP. Set the title of the plot to “Diamond Prices”. The x and y
#axis labels should be “Carat Weight” and “Price in GBP”, respectively
plot(priceGBP~carat,data=diamonds,title="Diamond Prices",
     xlab="Carat Weight",ylab="Price in GBP")

#(h) Modify the scatter plot from part (g) by using the plotting symbol and colour to represent the clarity grade.
#Add a legend to your plot.
# 对于plot，分组概念仍然混淆
plot(priceGBP~carat,data=diamonds,main="Diamond Prices",
     xlab="Carat Weight",ylab="Price in GBP",
     col=unclass(factor(clarity)),
     pch=unclass(factor(clarity)))
legend("topleft",legend=c("IF","SI","VS","VVS"),col=1:4,pch=1:4)

#(i) A regression line of the form y = α + β1x + β2x2 has been used to model how carat weight affects the price
#of diamonds. The coefficients of the fitted model were estimated to be α = 130:9; β1 = 220:4 and β2 = 3398:3.
#Add this fitted regression line to your scatterplot. [4 marks]
#Your plot should look similar to the one overleaf
#Wrong
#利用lines画线
xs=seq(0.2,1,length.out=100) #先把x坐标分割一下，越多的点，线越平滑
lines(x=xs,y=130.9+220.4*xs+3398.3*xs^2,lwd=2) #直接利用lines(x,y)画图



#task 2
#(a) Create vector t containing an equally-spaced sequence from 0 to 2π of length 15
n=15
t=seq(0,2*pi,length.out=n)
#correct

#(b) Create a matrix or data frame with two columns x and y and 15 rows, such that xi = sin(ti) and yi = cos(ti).
#Use the round function to round the values to two significant figures (i.e. 2 places after the decimal point)
#x=c(round(sin(t),2))
#y=c(round(cos(t),2))
#?round
#create a dataframe with 2 vector
x=data.frame(x=sin(t), y=cos(t))
data=round(x,2) #round函数

#(c) Create a plot of the data points from the matrix or data frame from part (b). The axis labels should be
#“Horizontal axis” and “Vertical axis”, respectively
par(pty="s") #当你需要正方形绘画区域的时候，用par(pty="s")
?par
###plot是先x后y的，然后lm是先y～再x
plot(data, xlab="Horizontal axis", ylab="Vertical axis")
#plot一个两行matrix的时候，正好对应好x和y

#(d) Connect all pairs of points in the plot from part (c) with a line
#?lines
#lines(data)
#######Wrong!!!
for(i in 1:n-1){ #用double loop来连结每一个点，1:n-1来连接i+1到n
  for(j in (i+1):n){ 
    lines(data[c(i,j),])
  }
}
#or
for(i in 1:n){
  for(j in 1:n){
    lines(data[c(i,j),])
  }
}

#(e) By adapting your code from part (d) draw over each of the horizontal 
#lines with a thicker red line
#怎么写horizontal line？不明白，只知道用line function
for(i in 1:n){
  for(j in 1:n){
    lines(data[c(i,j),])
    c2=data[i,2]==data[j,2] #y轴相等
    c3=data[i,1]!=data[j,1] #x轴不相等
    if(c2&c3){lines(data[c(i,j),],col=2,lwd=3)} #同时满足的时候，call line func.
  }
}

#task 3
#1. Draw X1; : : : ; Xk from the Expo(θ) distribution.
#Hint: You can draw realisations from the Expo(θ) distribution using the function rexp with the argument rate
#set to θ.
#2. Compute Y = X1 + : : : + Xk.
#wrong!!!
sample.gamma=function(n,k,theta){ 
  result=numeric(n) #初始化，result vector
  for(i in 1:n){
    result[i]=sum(rexp(k,theta)) #对于result(y)，里的每一个元素，都是k个x的和
  }
  result
}
sample.gamma(100,5,2)
?rexp 
#rexp自带 number of obersvations
#模拟100次和，n是模拟的次数！！！

#task 4
#a
#(α) Write a function weighted.ls which takes the vectors x, y, and w as arguments and which returns the
#weighted least squares estimate β^.
#weighted.ls=function(x,y,w){
#  beta_hat=solve(t(X)%*%W%*%X)%*%t(x)%*%W%*%Y
#}
#Wrong
weighted.ls=function(x,y,w){
  X=cbind(1,x)
  W=diag(w)
  solve(t(X)%*%W%*%X,t(X)%*%W%*%y)
}

#(β)Write a function predict.wls, which takes x, y, w and x0 as arguments, 
#and returns the least squares prediction y^0. Your function should first 
#call the function weighted.ls to compute β^ (from x, y and w) and then compute
#y0 using equation (1)
predict.wls=function(x,y,w,x0){
  beta=numeric(2) #不用初始化数组。。。
  beta=weighted.ls(x,y,w) 
  y0_hat=beta[1]+beta[2]*x0
}

#4b
#Write a function compute.weights which takes the vector x, the new observation x0 (a single number), and
#the parameter rho as arguments, and which computes the weights w.
#compute.weights=function(x,x0,rho){
#  n=length(x)
#  for(i in 1:n)
#     w=exp((-rho*x[i]-x0)^2)
#}
###甚至不用初始化新的vector，直接算一下拉倒！！！
compute.weights=function(x,x0,rho){
  exp(-rho*(x-x0)^2)
}

#4c
predict.locally.linear=function(x,y,x0,rho=0.2){
  w=compute.weights(x,x0,rho)
  predict.wls(x,y,w,x0)
}


################(not required)
library(MASS)
plot(mcycle)
x <- seq(1, 60, 0.5)
f.hat <- numeric(length(x))
for (i in 1:length(x))
  f.hat[i] <- predict.locally.linear(mcycle$times, mcycle$accel, x[i], 0.2 )
lines(x, f.hat)










