#16_CT_p2

#(a) Read both data files into R and store them in a data frame each.
setwd('/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/2016_CT2-20210210')
stations=read.csv("stations.csv")
trips=read.csv("trips.csv")
#correct

#(b) How many docks are there in the entire system? 
#How many docks are there om the city of Mountain View?
#怎么选中行啊？
mount=subset(stations,stations$city=="Mountain View")
sum(mount$docks)
#correct; or
sum(subset(stations, city=="Mountain View")$docks)

#(c) What proportion of trips were made by subscribers and what 
#proportion of trips were made by customers?
#a=subset(trips,trips$subscription_type=="Subscriber")
#b=subset(trips,trips$subscription_type=="Customer")
#c=sum(!is.na(trips$subscription_type)) #怎么计数character的数量啊？
#d=sum(!is.na(a$subscription_type))
#e=sum(!is.na(b$subscription_type))
#propSub=d/c
#probCus=e/c
#correct，但是太繁琐，可以学习计数char的方法
#T=1,F=0，直接求mean
mean(trips$subscription_type=='Subscriber') 
mean(trips$subscription_type=='Customer')

#(d) Find the station name of the station with the fewest docks
stations[which.min(stations$docks),]$name

#(e) Create a plot of the GPS coordinates of the stations in San Francisco. 
#The label of the x-axis should be “Longitude” and
#the label of the y-axis should be “Latitude”. The title of the 
#plot should be “Bicycle trips in San Francisco”

###注意一下pch,cex wrong!
?plot
sfstations=subset(stations,stations$city=="San Francisco")
plot(lat~long,data=sfstations,xlab='Longitude',ylab='Latitude',
     main="Bicycle trips in San Francisco",pch=16,cex=2)

#(f) Create a so-called origin-destination matrix. The matrix should be 70 × 70 and the (i; j)-th entry should contain the
#number of trips made from station i to station j
###???????????????????????????????? wrong
od=matrix(0,nrow(stations),nrow(stations)) 
for(i in 1:nrow(od)){
  for(j in 1:ncol(od)){
    od[i,j]=nrow(subset(trips,start_id==i&end_id==j)) #matrix assignment
    #每次运行这个循环，都要进行一次subset，然后求出有多少行
  }
}

###or
od=matrix(0,nrow(stations),nrow(stations)) #initial matrix before using it!
for(i in 1:nrow(trips)){
  od[trips$start_id[i],trips$end_id[i]]=od[trips$start_id[i],trips$end_id[i]]+1
}
#对于trips的每一行，e.g.15行，使得od[33,45]的数值+1，从而实现了od计数
#此方法在于，不是遍历od，而是遍历trip，然后分配给指定的od
trips$start_id[15] #33
trips$end_id[15] #45

#(g) Add lines to your plot representing the number of trips between the stations in San Francisco. Do not show lines corresponding to trips involving stations outside San Francisco.
#If there are less than 5 trips between i and j (either way), do not show a line on your plot.
#If there are between 5 and 9 trips between i and j (either way), draw a line of width 1.
#If there are between 10 and 19 trips between i and j (either way), draw a line of width 2.
#If there are more than 20 trips between i and j (either way), draw a line of width 3.
#Your plot should look similar to the plot shown overleaf.
#If your code from part (f) is not working you can create a fake origin-destination matrix using the code below.
#R 1 od <- matrix(rpois(4900, 5), ncol=70)
?segments
?points
for (i in 1:nrow(sfstations)){ #why double loop? general method to go through matrix
  for (j in 1:nrow(sfstations)){
    if (i<j) { #if i<j只是一个条件，我写if(T) 一样成功，防止多余循环，应该有用
      n.trips=od[sfstations$id[i],sfstations$id[j]]+ #算出从A到B，从B到A
        od[sfstations$id[j],sfstations$id[i]]  #i.e两地所有通讯数量，调用od数据来算
      if (n.trips>=5) { #大于等于5是大前提，因为小于5不画图！
        lwd <- 1
        if (n.trips>=10) #嵌套if，要先满足大前提
          lwd <- 2
        if (n.trips>=20) #嵌套if，要先满足大前提
          lwd <- 3
        segments(sfstations$long[i], sfstations$lat[i], sfstations$long[j], 
                 sfstations$lat[j], col="grey", lwd=lwd)
        #连结sfstations$long[1], sfstations$lat[1]，和sfstations$long[2], sfstations$lat[2]
        #因为在大循环里，所以这一条命令会遍历i.e连结所有pairs of i,j
      }
    }
  }
}
points(sfstations$long, sfstations$lat, pch=16, cex=2) 
#让points再涂一点
#od[sfstations$id[1],sfstations$id[2]]+od[sfstations$id[2],sfstations$id[1]]
sfstations$long[1];sfstations$lat[1];sfstations$long[2];
sfstations$lat[2]
#再次plot points,用来加深points,使用points(x,y,pch="",cex="")函数


#2 (a) The number of ways in which we can draw n objects from a population of N objects is given by N!
#(N − n)!, assuming that
#we are retaining the order. For large N this formula suffers from numerical overflow.
#Write a function approx.permut, which takes N and n as arguments and which returns the approximation
#return (approx) 记得打括号 
#or 不需要返回变量
approx.permut = function(N, n) {
        sqrt(N/(N-n)) * (N/(N-n))^N * (N-n)^n * exp(-n)
}


#b 
#exact.permut=function(N,n){
#  for(i in 1:(n-1)){
#    q3_1<-N
#    q3_2<-q3_1*(N-i)}
#  return (q3_2)
#}
exact.permut(10,5)
### wrong 结果呢 是彻底wrong的，因为返回的数值，result不能写在循环里！！！
exact.permut=function(N,n){
  result=N
  for(i in 1:(n-1))
    result=result*(N-i)
  result
}
###or 使用prod函数求连积
exact.permut=function(N,n){
  prod((N-n+1):N)
}

#c
approx.permut(200,10)
exact.permut(200,10)
exp(lgamma(200+1)-lgamma(200-10+1))

#Q3  Pascal’s triangle is an illustration of Pascal’s rule for binomial coefficients:
#Use your function to compute the first 10 rows of Pascal’s triangle
###Ans
pascal=function(n){
  triangle=list(1) #初始化，因为第一层只有1
  if(n>1)
    for(k in 2:n) #每层循环
      triangle[[k]]=c(0,triangle[[k-1]])+c(triangle[[k-1]],0) 
    triangle #返回       #双括号来引用list元素，算式是known的                                    
}
pascal(10)


#The figure in the question sheet can be drawn as follows (not required)
draw.hexagon <- function(x, y, r=0.5/sin(pi/3)) {
   t <- seq(0,2*pi, len=7)
  lines(x+r*sin(t), y+r*cos(t))
   }
 draw.pascal <- function(pa) {
   par(mar=rep(0.1,4))
  n <- length(pa)
  yscale <- sin(pi/3)
  plot(NULL, xlim=c(0,n+1), ylim=yscale*c(-n-1,0), bty='n', xaxt='n', yaxt='n')
  x.centre <- (n+1)/2
   for (i in 1:n) {
    p <- length(pa[[i]])
    text(x.centre-p/2+1:p,-i*yscale,pa[[i]])
    for (j in 1:p)
       draw.hexagon(x.centre-p/2+j,-i*yscale)
     }
   }
draw.pascal(pascal(10))

#(4)
#(a) Write a function simulate.sir which simulates from the SIR model as set out above. The function should take the
#parameters N, initial.infected (I0), alpha (α), beta (β) and T as arguments. It should returns a data frame with
#T + 1 rows, in which the (t + 1)-st row contains the values of St, It and Rt.
#Your function should check whether 0 ≤ I0 ≤ N, 0 < α < 1 and β > 0 and produce an error message if this is not the
#case.
#The default values should be N = 1000, I0 = 0:01N (rounded to the nearest integer), α = 10 1 , β = 1 4 and T = 100.
#Hint: You can draw one realisation from the Bi(n; θ) distribution using the R function rbinom(1, n, theta). [10 marks]
simulate.sir=function(N=1000,initial.infected=round(0.01*N),alpha=0.1,beta=0.25,T=100){
  if((initial.infected<0)|(initial.infected>N))
    stop("invalid...")
  if((alpha<=0)|(alpha>=1))
    stop("invalid...")
  if(beta<0)
    stop("invalid")
  result <- data.frame(susceptible=numeric(T+1), infected=numeric(T+1), recovered=numeric(T+1))
  #让结果成为一个dataframe，比vector更灵活
  result$susceptible[1] <- N - initial.infected #1
  result$infected[1] <- initial.infected  #1
  result$recovered[1] <- 0 #1
  for(t in 1:T){
    Delta.S <- rbinom(1, result$susceptible[t], 1-exp(-beta*result$infected[t]/N)) #2.1
    Delta.I <- rbinom(1, result$infected[t], alpha) #2.2
    result$susceptible[t+1] <- result$susceptible[t] - Delta.S #2.3
    result$infected[t+1] <- result$infected[t] + Delta.S - Delta.I #2.3
    result$recovered[t+1] <- result$recovered[t] + Delta.I #2.3
  }
  result #output
}

#(b) Use your function simulate.sir to create one realisation from the SIR model (using the above default values for the
#parameters) and plot the three columns as three curves in one plot as shown below. The curves should use a different
#colour and/or line type. Add a legend to your plot.
?matplot #used to plot matrix
data=simulate.sir()
time <- 1:nrow(data)-1
matplot(time, data, type="l",ylim=c(0,1000), xlab="Time", ylab="Population")
  legend("right", col=1:3, lty=1:3, c("Susceptible", "Infected ", "Recovered"))
#用matplot来输出结果
#Instead of matplot we could have used
plot(time, data[,1], ylim=c(0,1000), xlab="Time" , ylab="Population",
     type="l",lty=1,col=1)
lines(time, data[,2], lty=2, col=2)
lines(time, data[,3], lty=3, col=3)


#(c) At the end of the epidemic (which we assume is at time T = 100) not every individual will have had the infection. It is of
#epidemiological interest to determine the proportion of individuals who have had the infection, R100/N.
#Create 50 realisations from the above model, each created by calling the function simulate.sir from part (a), again using
#the above default values for the parameters. Calculate the mean value of R100/N from the simulations.
results=numeric(50) #initilize一个长度为50的vector
for(i in 1:length(results)){
  data=simulate.sir()
  results[i]=simulate.sir()$recovered[101]/1000
}
mean(results)



