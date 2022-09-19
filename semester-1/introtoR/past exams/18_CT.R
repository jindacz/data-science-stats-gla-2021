#18_CT
#Task1
setwd('/Users/kurisuuu/Documents/glasgow_stats_2021/introtoR/2018_CT1-20210210/M')

#1. [1 mark] Read the data file houseprices.csv correctly into a data frame called houseprices.
houseprices=read.csv("houseprices.csv")
head(houseprices)
str(houseprices)

#2. [1 mark] What is the average house price in August 2014?
mean(subset(houseprices,houseprices$Month==8)$Price)
#correct

#3  [2 marks] Create a dataset called houseprices.summer including the transactions occurred between
#July 15th and August 15th. How many transactions occurred in that period?
#怎么弄时间区间呢？
#houseprices.summer=subset(houseprices,houseprice)
###wrong 用nrow，给行数量计数
houseprices.summer=subset(houseprices,(Month==7&Day>=15)|(Month==8&Day<=15))
nrow(houseprices.summer)

#4. [1 mark] Which house sold for the lowest price?
houseprices[which.min(houseprices$Price),]

#5. [1 mark] Transform the column Lon to include the longitude of the properties expressed in radians,
#i.e. divide the longitude by 180◦ and multiply by π. Repeat the same process for Lat.
change=transform(houseprices,lonRD=houseprice$Lon/180,latRD=houseprice$Lat/180)
#要赋值才可以改变，不然只是tranform没意义
#Wrong
houseprices$Lon=houseprices$Lon*pi/180
houseprices$Lat=houseprices$Lat*pi/180

#6 Create a new variable Dist2University which contains the distance to the University in
#kilometres. Consider two locations with longitudes λ1 and λ2 and latitudes φ1 and φ2 expressed in
#radians.
#deltaL=L2-(-4.2886/180)
#deltaP=p2-(55.8711/180)
#alpha=sin(deltaP/2)^2+cos(p1)*cos(p2)*sin(deltaL/2)^2
#d=12742*atan2(sqrt(a)/sqrt(1-a))
#Dist2University=d
#wrong
###不太懂是干嘛，建模？
###要直接应用上去，直接算出来！！！
Lambda1 <- -4.2886 / 180 * pi #固定点的坐标是已知的
Phi1 <- 55.8711 / 180 * pi
DeltaLambda <- houseprices$Lon- Lambda1
DeltaPhi <- houseprices$Lat - Phi1
alpha <- sin(DeltaPhi/2)^2 + cos(Phi1)*cos(houseprices$Lat)*sin(DeltaLambda/2)^2
Dist2University <- 12742 * atan2(sqrt(alpha),sqrt(1-alpha))

#7. [1 mark] What was the average price of properties which are within 1km of the University?
houseprice_y=transform(houseprices,Dis2U=Dist2University)
houseprice_yu=subset(houseprice_y,Dis2U<=1)
mean(houseprice_yu$Price)
#correct
mean(subset(houseprices,Dist2University<= 1)$Price)

#8 Plot the density of the price of properties which cost less than one million. Include the title
#Density of Price and the label for the x-axis Price. Your plot should like like the following:
houseprices_1e6=subset(houseprices,houseprices$Price<=1e6)
#用density function画密度图
plot(density(houseprices_1e6$Price),main="density of price",xlab="price")

#Task 2
#1. [1 mark] Read the data correctly into R and store it in the data frame hearth.
?read.table
hearth=read.table("hearth.txt",header=T,na.string=";")

#2. [1 mark] Remove all rows containing missing values from heart.
#Wrong,注意remove all 用na.omit
hearth=na.omit(hearth)

#3. [2 marks] Add the column difference including MF - SV and the column mean including (MF+ SV)/2
#to the data frame hearth
hearth=transform(hearth,difference=MF-SV)
hearth=transform(hearth,mean=(MF+SV)/2)
###or
#hearth$difference <- hearth$MF - hearth$SV
#hearth$mean <- (hearth$MF + hearth$SV)/2

#4. [2 marks] Suppose we are interested in informally assessing whether there is a systematic difference
#between the two techniques. This is often done using what is called the Bland-Altman plot, which
#is a scatter plot of mean against difference. Produce such a plot and label the x-axis Mean of
#measurements and the y-axis Difference of measurements.
plot(hearth$mean,hearth$difference,,xlab="Mean of measurements",
     ylab="Difference of measurements")
#Correct 不能用data=...，要记得用data$x,data$y

#5. [2 marks] Add a solid horizontal line at the average value of difference and a pair of dashed lines
#one standard deviation above and below the solid horizontal line. Your plot should look similar to the
#one below.
#?????? 怎么画线我又忘记了？？？
?abline
#abline()
#	a,b the intercept and slope, single values.
#untf	 logical asking whether to untransform. See ‘Details’.
#h the y-value(s) for horizontal line(s).
#v	the x-value(s) for vertical line(s).
abline(h=mean(hearth$difference))
#lty=2，虚线，用abline(h=)画横线
abline(h=mean(hearth$difference)+sd(hearth$difference),lty=2)
abline(h=mean(hearth$difference)-sd(hearth$difference),lty=2)

#Task 3
#1. [1 mark] Read the file potus.txt into R and store into a data frame called potus
potus=read.table("potus.txt",header=T,sep=",")
#correct

#2. [2 marks] What is the average median household income in counties in which Donald Trump received
#at least three times as many votes as Hillary Clinton?
potus_2=subset(potus,VotesTrump>=3*VotesClinton)
mean(potus_2$HIncome)

#3. [1 mark] How many votes have been cast for Hillary Clinton in the state of California?
#potus_3=subset(potus,State=="California")
#sum(potus_3$VotesClinton)
#Wrong，为什么出来的NA？
sum(subset(potus,State=="California")$VotesClinton)
#牢记：条件等号，要用==

#4. [1 mark] Add a column to potus named Hillary.Wins which is TRUE for counties where Hillary
#Clinton has more votes than Donald Trump and FALSE otherwise.
potus=transform(potus,Hillary.Wins=VotesClinton>VotesTrump)
#correct

#5. [3 marks] Create a plot of HIncome against PercWhite. Counties in which Hillary Clinton obtained
#more votes than Donald Trump should be plotted in red, the others in blue.
#plot(potus$PercWhite,potus$HIncome,col=unclass(factor(potus$Hillary.Wins))
#    ,lwd=2)
#Wrong!!! 要用mycolor来自己指定颜色！！！
mycols=c("blue","red")
plot(HIncome~PercWhite,data=potus,col=mycols[unclass(factor(Hillary.Wins))],
     pch=19)

#6. [1 mark] Add a legend to the plot from part 5. Your final plot should look similar to the one below.
legend("topleft",pch=19,col=c("red","blue"),legend=c("Clinton","Trump"))
#legend直接用c就好



     